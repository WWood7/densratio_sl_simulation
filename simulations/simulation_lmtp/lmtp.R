setwd("/home/wwu227/my_packages/sl3_densratio")
devtools::load_all()
setwd("/home/wwu227/my_packages/densratio")
devtools::load_all()

# introduce functions
source('/home/wwu227/simulation_lmtp/functions.R')

# generate test data (for computing hold-out risk)
set.seed(100)
test <- generate_data(10000)


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



train <- generate_data(n)


# standardize the l's in the training set
l1_sd <- sd(train$l1)
l2_sd <- sd(train$l2)
l3_sd <- sd(train$l3)
l4_sd <- sd(train$l4)
train$l1_sdd <- train$l1 / l1_sd
train$l2_sdd <- train$l2 / l2_sd
train$l3_sdd <- train$l3 / l3_sd
train$l4_sdd <- train$l4 / l4_sd
# standardize the test set
test$l1_sdd <- test$l1 / l1_sd
test$l2_sdd <- test$l2 / l2_sd
test$l3_sdd <- test$l3 / l3_sd
test$l4_sdd <- test$l4 / l4_sd


# define classification learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_glm$new()
cl3 <- Lrnr_hal9001$new()
cl4 <- Lrnr_gam$new()
cl5 <- Lrnr_xgboost$new()
# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2, cl3, cl4, cl5)
cls <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))

# define ratio learners
lr1 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.8, name = 'lr1')
lr2 <- Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = 'lr2')
lr3 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.5, name = 'lr3')
lr4 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.2, name = 'lr4')
lr5 <- Lrnr_densratio_classification$new(classifier = cl1, name = 'cl1')
lr6 <- Lrnr_densratio_classification$new(classifier = cl2, name = 'cl2')
lr7 <- Lrnr_densratio_classification$new(classifier = cl3, name = 'cl3')
lr8 <- Lrnr_densratio_classification$new(classifier = cl4, name = 'cl4')
lr9 <- Lrnr_densratio_classification$new(classifier = cl5, name = 'cl5')
csl <- Lrnr_densratio_classification$new(classifier = cls, name = 'csl')

# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))


# t = 1
train1 <- augment_data1(train)
# standardize a1
a1_sd1 <- sd(train1$a1)
train1$a1_sdd <- train1$a1 / a1_sd1
test$a1_sdd <- test$a1 / a1_sd1
covs <- c('l1_sdd', 'a1_sdd')
# train the learners
train_task1 <- sl3_Task$new(data = train1, covariates = covs, outcome = 'indicator')
pre_task1 <- sl3_Task$new(data = test, covariates = covs)
sl_fit1 <- sl$train(task = train_task1)
csl_fit1 <- csl$train(task = train_task1)
# get the predictions
sl_pre1 <- sl_fit1$predict(task = pre_task1)
csl_pre1 <- csl_fit1$predict(task = pre_task1)
sl_risk1 <- mean(log(bound(sl_pre1)))
csl_risk1 <- mean(log(bound(csl_pre1)))



# t = 2
train2 <- augment_data2(train)
# standardize a2
a2_sd2 <- sd(train2$a2)
train2$a2_sdd <- train2$a2 / a2_sd2
test$a2_sdd <- test$a2 / a2_sd2
# re-standardize a1
a1_sd <- sd(train2$a1)
train2$a1_sdd <- train2$a1 / a1_sd
test$a1_sdd <- test$a1_sdd * a1_sd1 / a1_sd
covs <- c('l1_sdd', 'l2_sdd', 'a1_sdd', 'a2_sdd')
# train the learners
train_task2 <- sl3_Task$new(data = train2, covariates = covs, outcome = 'indicator')
pre_task2 <- sl3_Task$new(data = test, covariates = covs)
sl_fit2 <- sl$train(task = train_task2)
csl_fit2 <- csl$train(task = train_task2)
# get the predictions
sl_pre2 <- sl_fit2$predict(task = pre_task2)
csl_pre2 <- csl_fit2$predict(task = pre_task2)
sl_risk2 <- mean(log(bound(sl_pre2)))
csl_risk2 <- mean(log(bound(csl_pre2)))


# t = 3
train3 <- augment_data3(train)
# standardize a3
a3_sd3 <- sd(train3$a3)
train3$a3_sdd <- train3$a3 / a3_sd3
test$a3_sdd <- test$a3 / a3_sd3
# re-standardize a1, a2
train3$a1_sdd <- train3$a1 / a1_sd
a2_sd <- sd(train3$a2)
train3$a2_sdd <- train3$a2 / a2_sd
test$a2_sdd <- test$a2_sdd * a2_sd2 / a2_sd
covs <- c('l1_sdd', 'l2_sdd', 'l3_sdd', 'a1_sdd', 'a2_sdd', 'a3_sdd')
# covs <- c('l1', 'l2', 'l3', 'a1', 'a2', 'a3')
# train the learners
train_task3 <- sl3_Task$new(data = train3, covariates = covs, outcome = 'indicator')
pre_task3 <- sl3_Task$new(data = test, covariates = covs)
sl_fit3 <- sl$train(task = train_task3)
csl_fit3 <- csl$train(task = train_task3)
# get the predictions
sl_pre3 <- sl_fit3$predict(task = pre_task3)
csl_pre3 <- csl_fit3$predict(task = pre_task3)
# true_ratio3 <- I(test$a3 < 1) + I(test$a3 < 5) *
#     dbinom(test$a3 + 1, 5, expit(-2 + 1/(1 + 2*test$l3 + test$a2))) / 
#     dbinom(test$a3, 5, expit(-2 + 1/(1 + 2*test$l3 + test$a2)))
sl_risk3 <- mean(log(bound(sl_pre3)))
csl_risk3 <- mean(log(bound(csl_pre3)))



# t = 4
train4 <- augment_data4(train)
# standardize a4
a4_sd4 <- sd(train4$a4)
train4$a4_sdd <- train4$a4 / a4_sd4
test$a4_sdd <- test$a4 / a4_sd4
# re-standardize a1, a2, a3
train4$a1_sdd <- train4$a1 / a1_sd
train4$a2_sdd <- train4$a2 / a2_sd
a3_sd <- sd(train4$a3)
train4$a3_sdd <- train4$a3 / a3_sd
test$a3_sdd <- test$a3_sdd * a3_sd3 / a3_sd
covs <- c('l1_sdd', 'l2_sdd', 'l3_sdd', 'l4_sdd', 'a1_sdd', 'a2_sdd', 'a3_sdd', 'a4_sdd')
# train the learners
train_task4 <- sl3_Task$new(data = train4, covariates = covs, outcome = 'indicator')
pre_task4 <- sl3_Task$new(data = test, covariates = covs)
sl_fit4 <- sl$train(task = train_task4)
csl_fit4 <- csl$train(task = train_task4)
# get the predictions
sl_pre4 <- sl_fit4$predict(task = pre_task4)
csl_pre4 <- csl_fit4$predict(task = pre_task4)
# true_ratio4 <- I(test$a4 < 1) + I(test$a4 < 5) *
#     dbinom(test$a4 + 1, 5, expit(1 + test$l4 - 3*test$a3)) / 
#     dbinom(test$a4, 5, expit(1 + test$l4 - 3*test$a3))
sl_risk4 <- mean(log(bound(sl_pre4)))
csl_risk4 <- mean(log(bound(csl_pre4)))


# calculate estimate
sl_ratio_est <- sl_fit1$predict()[1:n] * sl_fit2$predict()[1:n] * 
    sl_fit3$predict()[1:n] * sl_fit4$predict()[1:n]
sl_est <- mean(sl_ratio_est * train$y)
csl_ratio_est <- csl_fit1$predict()[1:n] * csl_fit2$predict()[1:n] * 
    csl_fit3$predict()[1:n] * csl_fit4$predict()[1:n]
csl_est <- mean(csl_ratio_est * train$y)


# store the results
result <- c(sl_risk1, sl_risk2, sl_risk3, sl_risk4, sl_est,
            csl_risk1, csl_risk2, csl_risk3, csl_risk4, csl_est)
setwd('/projects/dbenkes/winn/lmtp')
filename = paste0("lmtp_seed", seed, "_n", n, "_", ".rds")
saveRDS(result, file = filename)


