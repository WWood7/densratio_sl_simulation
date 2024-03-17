setwd("/home/wwu227/my_packages/sl3_densratio")
devtools::load_all()
setwd("/home/wwu227/my_packages/densratio")
devtools::load_all()
library(msm)

# define a function to set the data
# w ~ N(5, 4), truncated into [2, 8]
# a|w ~ Bernoulli(0.6 - 0.15I(w>7.5) + 0.05I(w<6) - 0.35I(w<4) - 0.15I(w>5))
# m|w,a=1 ~ Beta(0.6w + 1, 0.7w)
# m|w,a=0 ~ N(0.1w, 1), truncated into [0, 1]
# y|a, m, w ~ N(5 + 1.5a + 2m + 5m^2 + 0.3w, 4)
setdata <- function(n){
    w <- rtnorm(n, mean = 5, sd = 2, lower = 2, upper = 8)
    a <- rbinom(n, 1, 0.6 - 0.15*I(w>7) + 0.05*I(w<6) - 0.35*I(w<4) - 0.15*I(w>5))
    m <- (a == 1) * rbeta(n,  0.6 * w + 1, 0.7 * w) + (a == 0) * rtnorm(n, 0.1 * w, 1, lower = 0, upper = 1)
    y <- rnorm(n, 5 + 1.5 * a + 2 * m + 5 * m^2 + 0.3 * w, sd = 2)
    df <- data.frame(w = w, a = a, m = m, y = y)
}

# generate test data (for computing hold-out risk)
set.seed(777)
test <- setdata(10000)

# define a function for calculating hold-out risk
holdout_risk <- function(lr, df){
    # create a task
    test_task <- sl3_Task$new(data = df, covariates = c('m_sdd', 'w_sdd'))
    # get the prediction on the hold-out set
    pred <- lr$predict(test_task)
    # calculate risk
    out <- -log(bound(pred)) * (df$a - 0.5) * 2
    mean(out)
}

# define a grid of parameters for the simulation
# 10 replicates at each sample size in each setting
params = expand.grid(seed = 1:100,
                    n = c(100, 500, 1000, 2000, 5000))

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
train <- setdata(n)
train$indicator <- train$a

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
lr1 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.8, name = 'lr1'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.8, name = '', stage2 = TRUE))
lr2 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.5, name = '', stage2 = TRUE))
lr3 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = 'lr3'), 
                    Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = '', stage2 = TRUE))
# use Lrnr_glm for the second stage classification
lr4 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = csl, name = 'csl'), 
                    Lrnr_densratio_classification$new(classifier = csl, stage2 = TRUE, name = ''))


# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))

# normalize the data
sd_m <- sd(train$m)
sd_w <- sd(train$w)
train$m_sdd <- train$m / sd_m
train$w_sdd <- train$w / sd_w
# define a task
task <- sl3_Task$new(data = train, covariates = c('m_sdd', 'w_sdd'), outcome = 'indicator', folds = 5)
# train the super learner
sl_fit <- sl$train(task)


# standardize test data using sd's from training set
test$m_sdd <- test$m / sd_m
test$w_sdd <- test$w / sd_w
# calculate the hold-out risks
sl_risk <- holdout_risk(sl_fit, test)
lr1_risk <- holdout_risk(sl_fit$learner_fits$`Pipeline(lr1->)`, test)
lr2_risk <- holdout_risk(sl_fit$learner_fits$`Pipeline(lr2->)`, test)
lr3_risk <- holdout_risk(sl_fit$learner_fits$`Pipeline(lr3->)`, test)
csl_risk <- holdout_risk(sl_fit$learner_fits$`Pipeline(csl->)`, test)
result <- c(sl_risk, lr1_risk, lr2_risk, lr3_risk, csl_risk)

# store the results
setwd('/projects/dbenkes/winn/holdout')
filename = paste0("holdoutrisk_seed", seed, "_n", n, "_", ".rds")
saveRDS(result, file = filename)


