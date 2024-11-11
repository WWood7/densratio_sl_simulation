setwd("/home/wwu227/my_packages/sl3_densratio")
devtools::load_all()
setwd("/home/wwu227/my_packages/densratio")
devtools::load_all()

# define a function to set the data
# w1 ~ binomial(1, 0.5)
# w2 ~ N(0.1 * w1 + 1.65, 0.04)
# a|w1, w2 ~ binomial(1, sigmoid((2.5 - w2) * 1.2 * w1))
# m1|a, w1, w2 ~ N(a * w1 + 0.5 * a + 0.5 * w1 + 2 *w2, 0.25)
# m2|a, w1, w2 ~ binomial(1, sigmoid(w2 - 1.65 - a))
# y|m1, m2, a, w1, w2 ~ N(w2 + 0.1 * m2 * a + 0.05 * m1 + 0.05 * a - 0.3 * w1 * m2, 0.09)
setdata <- function(n){
    w1 <- rbinom(n, 1, 0.5)
    w2 <- rnorm(n, 0.1 * w1 + 1.65, sd = 0.2)
    a <- rbinom(n, 1, sigmoid::sigmoid((2.5 - w2) * 1.2 * w1))
    m1 <- rnorm(n, a * w1 + 0.5 * a + 0.5 * w1 + 2 * w2, sd = 0.5)
    m2 <- rbinom(n, 1, sigmoid::sigmoid(w2 - 1.65 - a))
    y <- rnorm(n, w2 + 0.1 * m2 * a + 0.05 * m1 + 0.05 * a - 0.3 * w1 * m2, sd = 0.3)
    df <- data.frame(w1 = w1, w2 = w2, a = a, m1 = m1, m2 = m2, y = y)
}

# define a grid of parameters for the simulation
# 10 replicates at each sample size in each setting
params = expand.grid(seed = 1:100,
                     n = c(100, 500, 1000, 2000))


# define a function to calculate the two on-step estimations
# based on two ways of density ratio estimation
onestep_estimator <- function(df){
    
    # use HAL to run the sequential regression and the PS model
    hal <- Lrnr_hal9001$new()
    
    # estimate E[Y|A, M, W] and get E[Y|A = 0, M, W]
    # define the task
    reg_task1 <- sl3_Task$new(data = df, covariates = c('a', 'm1', 'm2', 'w1', 'w2'), outcome = 'y')
    # get the estimate
    model1 <- hal$train(reg_task1)
    new_df1 <- df
    new_df1$a <- 0
    reg_task1_pre <- sl3_Task$new(data = new_df1, covariates = c('a', 'm1', 'm2', 'w1', 'w2'))
    df$mu_hat <- model1$predict(task = reg_task1_pre)
    
    # estimate E[E[Y|A = 0, M, W]|A=1, W]] on A=1 subset
    # then predict on the whole data set
    sub_df <- df[df$a == 1, ]
    reg_task2 <- sl3_Task$new(data = sub_df, covariates = c('w1', 'w2'), outcome = 'mu_hat')
    model2 <- hal$train(reg_task2)
    reg_task2_pre <- sl3_Task$new(data = df, covariates = c('w1', 'w2'))
    df$theta_hat <- model2$predict(task = reg_task2_pre)
    
    # get theta_bar, the plug-in estimate
    # E[E[E[Y|A = 0, M, W]|A=1, W]]]
    df$theta_bar <- mean(df$theta_hat)
    
    
    # calculate the bias correction terms
    # estimate propensity scores p(A|W)
    ps_task <- sl3_Task$new(data = df, covariates = c('w1', 'w2'), outcome = 'a')
    model3 <- hal$train(ps_task)
    df$ps1 <- model3$predict()
    df$ps0 <- 1 - df$ps1
    
    # estimate the density ratios
    df$indicator <- df$a
    # standardize the data
    sd_m1 <- sd(df$m1)
    sd_m2 <- sd(df$m2)
    sd_w1 <- sd(df$w1)
    sd_w2 <- sd(df$w2)
    df$m1_sdd <- df$m1 / sd_m1
    df$m2_sdd <- df$m2 / sd_m2
    df$w1_sdd <- df$w1 / sd_w1
    df$w2_sdd <- df$w2 / sd_w2
    # define the tasks
    task1 <- sl3_Task$new(data = df, covariates = c('m1_sdd', 'm2_sdd', 'w1_sdd', 'w2_sdd'), outcome = 'indicator', folds = 5)
    # train the super learners
    sl_fit <- sl$train(task1)
    
    
    # get the predictions of p(m|a=1,w) / p(m|a=0, w)
    # for the separate classification sl, manually calculate the 2 odds
    csl_pres <- sl_fit$learner_fits$`Pipeline(lr6->)`$predict()
    sl_pres <- sl_fit$predict()
    df$ratio_csl <- csl_pres
    df$ratio_sl <- sl_pres
    # df$true_ratio <- dbeta(df$m, 0.6 *df$w + 1, 0.7 *df$w) / dtnorm(df$m, 0.1 * df$w, 1, lower = 0, upper = 1)
    
    # get the final bias correction terms
    df$bias_sl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_sl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    df$bias_csl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_csl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    
    # get the final one-step estimates
    est_sl <- mean(df$bias_sl) + mean(df$theta_hat)
    est_csl <- mean(df$bias_csl) + mean(df$theta_hat)
    
    # get the average risk
    risk_sl <- sl_fit$cv_risk(loss_weighted_loglik_densratio)$WNLL[7]
    risk_csl <- sl_fit$cv_risk(loss_weighted_loglik_densratio)$WNLL[6]
    
    return(c(est_sl, est_csl, risk_sl, risk_csl))
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


# define classification learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_glm$new()
cl3 <- Lrnr_hal9001$new()
cl4 <- Lrnr_gam$new()
cl5 <- Lrnr_xgboost$new()
# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2, cl3, cl4, cl5)
csl <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))

# define ratio learners
lr1 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl1, name = 'lr1'), 
                    Lrnr_densratio_classification$new(classifier = cl3, conditional_set = c('w1_sdd', 'w2_sdd')))
lr2 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl3, name = 'lr2'), 
                    Lrnr_densratio_classification$new(classifier = cl3, conditional_set = c('w1_sdd', 'w2_sdd')))
lr3 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl4, name = 'lr3'), 
                    Lrnr_densratio_classification$new(classifier = cl5, conditional_set = c('w1_sdd', 'w2_sdd')))
lr4 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl5, name = 'lr4'), 
                    Lrnr_densratio_classification$new(classifier = cl3, conditional_set = c('w1_sdd', 'w2_sdd')))
lr5 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl2, name = 'lr5'), 
                    Lrnr_densratio_classification$new(classifier = cl2, conditional_set = c('w1_sdd', 'w2_sdd')))
lr6 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = csl, name = 'lr6'), 
                    Lrnr_densratio_classification$new(classifier = csl, name = '', conditional_set = c('w1_sdd', 'w2_sdd')))

# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))


# generate a data
data <- setdata(n)
results <- c(onestep_estimator(data))

# store the results
setwd('/projects/dbenkes/winn/mediation10')
filename = paste0("result_seed", seed, "_n", n, "_", ".rds")
saveRDS(results, file = filename)





