setwd("/home/wwu227/my_packages/sl3_densratio")
devtools::load_all()
setwd("/home/wwu227/my_packages/densratio")
devtools::load_all()
library(msm)

# define a function to set the data
# w = |w1| + w2 where w1 ~ N(0,4), w2 ~ Gamma(7.5, 1)
# a|w ~ Bernoulli(0.4 - 0.15I(w>7.5) + 0.15I(w>9) + 0.25I(w<6) - 0.1I(w<3))
# m|w,a=1 ~ Beta(0.5w, 0.8w)
# m|w,a=0 ~ Beta(0.8w, 0.5w)
# y|a, m, w ~ N(5 + 1.5a + 2m + 5m^2 + 0.3w, 4)
setdata <- function(n){
    w <- rtnorm(n, mean = 5, sd = 0.5, lower = 2, upper = 8)
    a <- rbinom(n, 1, 0.4 - 0.15*I(w>7.5) + 0.25*I(w<6) - 0.35*I(w<4))
    m <- (a == 1) * rbeta(n,  0.6 * w + 1, 0.7 * w) + (a == 0) * rtnorm(n, 0.1 * w, 0.25, lower = 0, upper = 1)
    y <- rnorm(n, 5 + 1.5 * a + 2 * m + 5 * m^2 + 0.3 * w, sd = 2)
    df <- data.frame(w = w, a = a, m = m, y = y)
}

# define a grid of parameters for the simulation
# 10 replicates at each sample size in each setting
params = expand.grid(seed = 101:200,
                     n = c(100, 500, 1000, 2000))



# define classification learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_glm$new()
cl3 <- Lrnr_hal9001$new()
cl4 <- Lrnr_gam$new()
cl5 <- Lrnr_xgboost$new()

# define ratio learners
lr1 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 200, alpha = 0.8, name = 'lr1'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 200, alpha = 0.8, name = '', stage2 = TRUE))
lr2 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 200, alpha = 0.2, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 200, alpha = 0.5, name = '', stage2 = TRUE))
lr3 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_gam), name = 'lr3'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_gam), stage2 = TRUE, name = ''))
lr4 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_bayesglm), name = 'lr4'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_bayesglm), stage2 = TRUE, name = ''))
lr5 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_hal9001), name = 'lr5'), 
                 Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_hal9001), stage2 = TRUE, name = ''))

# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2, cl3, cl4, cl5)
csl <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))


# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))



# define a function to calculate the two on-step estimations
# based on two ways of density ratio estimation
onestep_estimator <- function(df){
    
    # sequential regression
    # use HAL to run regressions
    hal <- Lrnr_hal9001$new()
    
    # estimate E[Y|A, M, W] and get E[Y|A = 0, M, W]
    reg_task1 <- sl3_Task$new(data = df, covariates = c('a', 'm', 'w'), outcome = 'y')
    model1 <- hal$train(reg_task1)
    new_df1 <- df
    new_df1$a <- 0
    reg_task1_pre <- sl3_Task$new(data = new_df1, covariates = c('a', 'm', 'w'))
    df$mu_hat <- model1$predict(task = reg_task1_pre)
    
    # estimate E[E[Y|A = 0, M, W]|A=1, W]] on A=1 subset
    # then predict on the whole data set
    sub_df <- df[df$a == 1, ]
    reg_task2 <- sl3_Task$new(data = sub_df, covariates = 'w', outcome = 'mu_hat')
    model2 <- hal$train(reg_task2)
    reg_task2_pre <- sl3_Task$new(data = df, covariates = 'w')
    df$theta_hat <- model2$predict(task = reg_task2_pre)
    
    # get theta_bar, the plug-in estimate
    # E[E[E[Y|A = 0, M, W]|A=1, W]]]
    df$theta_bar <- mean(df$theta_hat)
    
    
    # calculate the bias correction terms
    # estimate propensity scores p(A|W)
    ps_task <- sl3_Task$new(data = df, covariates = 'w', outcome = 'a')
    model3 <- hal$train(ps_task)
    df$ps1 <- model3$predict()
    df$ps0 <- 1 - df$ps1
    
    # estimate the density ratios
    df$indicator <- df$a
    # standardize the data
    sd_m <- sd(df$m)
    sd_w <- sd(df$w)
    df$m_sdd <- df$m / sd_m
    df$w_sdd <- df$w / sd_w
    # define the tasks
    task1 <- sl3_Task$new(data = df, covariates = c('m_sdd', 'w_sdd'), outcome = 'indicator', folds = 5)
    task2 <- sl3_Task$new(data = df, covariates = c('w_sdd'), outcome = 'indicator', folds = 5)
    # train the super learners
    csl1_fit <- csl$train(task1)
    csl2_fit <- csl$train(task2)
    sl_fit <- sl$train(task1)
    
    # get the predictions of p(m|a=1,w) / p(m|a=0, w)
    csl_pres <- (csl1_fit$predict() / (1 - csl1_fit$predict())) /
        (csl2_fit$predict() / (1 - csl2_fit$predict()))
    sl_pres <- sl_fit$predict()
    df$ratio_csl <- csl_pres
    df$ratio_sl <- sl_pres
    
    # get the final bias correction terms
    df$bias_sl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_sl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    df$bias_csl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_csl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    
    # get the final one-step estimates
    est_sl <- mean(df$bias_sl) + mean(df$theta_hat)
    est_csl <- mean(df$bias_csl) + mean(df$theta_hat)
    return(c(est_sl, est_csl))
}

# import environment parameter
# get iter from array
iter = Sys.getenv("SLURM_ARRAY_TASK_ID")
iter = as.numeric(iter)
# get nloop
args = commandArgs(trailingOnly = TRUE)
nloop = as.numeric(args[1])
# get task id
max_jobs = 100
task_id = max_jobs*(nloop-1) + iter
seed = as.numeric(params[task_id,][1])
n = as.numeric(params[task_id,][2])
set.seed(seed)


# generate a data
data <- setdata(n)
results <- c(onestep_estimator(data), seed)

# store the results
setwd('/projects/dbenkes/winn/mediation1')
filename = paste0("result_seed", seed, "_n", n, "_", ".rds")
saveRDS(results, file = filename)





