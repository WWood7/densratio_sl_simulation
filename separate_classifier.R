setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()
setwd("/Users/winnwu/Documents/GitHub/densratio")
devtools::load_all()

# define a function to set the data
# n as sample size
# x = |x1| + x2 where x1 ~ N(0,4), x2 ~ Gamma(7.5, 1)
# w ~ bernoulli(0.5)
# a|x,w ~ bernoulli(0.4 + 0.15I(x>7.5) + 0.05I(x<6) + 0.1I(x<3) + 0.1I(w==1))
# m|x,w=1,a ~ |N(0.2x + 2a, 4)|
# m|x,w=0,a ~ Gamma(0.3x, a + 1)
# estimate p(m|x,w,a=1) / p(m|x,w,a=0)
setdata <- function(n){
    x <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
    w <- rbinom(n, 1, 0.5)
    a <- rbinom(n, 1, 0.4 + 0.15*I(x>7.5) + 0.05*I(x<6) + 0.1*I(x<3) + 0.1*I(w==1))
    m <- I(w==1) * abs(rnorm(n, 0.2 * x + 2 * a, 2)) + 
        I(w==0) * rgamma(n, shape = 0.3 * x, scale = a + 1)
    df <- data.frame(x = x, w = w, a = a, m = m)
}


# define classification super learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_dbarts$new()
cl3 <- Lrnr_glm$new()
cl4 <- Lrnr_hal9001$new()

# define learners
lr1 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.5, name = 'lr1'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.5, name = '', stage2 = TRUE))
lr2 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = '', stage2 = TRUE))
lr3 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = '', stage2 = TRUE))
lr4 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = 100, fold_num = 4, name = 'lr4'), 
                    Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = 100, fold_num = 4, name = '', stage2 = TRUE))
lr5 <- Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = 200, 
                                 fold_num = 8, name = 'lr5')
lr6 <- Pipeline$new(Lrnr_densratio_classification$new(name = 'lr6'), 
                    Lrnr_densratio_classification$new(stage2 = TRUE, name = ''))

# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2, cl3, cl4)
csl <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))


# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))


n = 1000

# generate data
data <- setdata(n)
data$indicator <- data$a
# normalize the data
data$m <- data$m / sd(data$m)
data$x <- data$x / sd(data$x)
# define a task
task1 <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 3)
# pass the folds argument from task1 so the two tasks have the same cv folds
task2 <- sl3_Task$new(data = data, covariates = c('x', 'w'), outcome = 'indicator', folds = 3)
# train the super learners
csl1_fit <- csl$train(task1)
csl2_fit <- csl$train(task2)
sl_fit <- sl$train(task1)


# test set
n = 200
data <- setdata(n)

# 从这里开始
pre_task <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'))
pre_task2 <- sl3_Task$new(data = data, covariates = c('x', 'w'))
# get the predictions
# get double - classifier predictions
cl_pre1 <- csl1_fit$predict(task = pre_task)
cl_pre2 <- csl2_fit$predict(task = pre_task2)
csl_results <- (cl_pre1 / (1 - cl_pre1)) / (cl_pre2 / (1 - cl_pre2))

sl_results <- sl_fit$predict(task = pre_task)





