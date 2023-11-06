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
    return(df)
}


# define a grid of parameters for the simulation
# 10 replicates at each sample size in each setting
params = expand.grid(seed = 1:10,
                    n = c(100, 500, 1000, 2000))


# define learners
lr1 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.5, name = 'lr1')
lr2 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.2, name = 'lr2')
lr3 <- Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = 'lr3')
lr4 <- Lrnr_densratio_kernel$new(method = 'uLSIF', kernel_num = 200, name = 'lr4')
lr5 <- Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = 200, 
                                 fold_num = 8, name = 'lr5')
lr6 <- Pipeline$new(Lrnr_densratio_classification$new(name = 'lr6'), 
                    Lrnr_densratio_classification$new(stage2 = TRUE, name = ''))
lr7 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_dbarts), name = 'lr7'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_bayesglm), stage2 = TRUE, name = ''))
lr8 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_hal9001), name = 'lr8'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_glm), stage2 = TRUE, name = ''))


# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))

# import environment parameter
# get iter from array
# iter = Sys.getenv("SLURM_ARRAY_TASK_ID")
# iter = as.numeric(iter)
iter = 1
# get nloop
# args = commandArgs(trailingOnly = TRUE)
# nloop = as.numeric(args[1])
nloop = 1
# get task id
max_jobs = 2
task_id = max_jobs*(nloop-1) + iter
seed = as.numeric(params[task_id,][1])
n = as.numeric(params[task_id,][2])
set.seed(seed)



# generate a data
data <- setdata(n)
data$indicator <- data$a
# normalize the data
data$m <- data$m / sd(data$m)
data$x <- data$x / sd(data$x)
# define a task
task <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 5)
# train the super learner
risk_table <- CV_lrnr_sl(sl, task, loss_weighted_loglik_densratio)

# store the results
setwd('/projects/dbenkes/winn/oracle')
filename = paste0("result_seed", seed, "_n", n, "_", ".rds")
saveRDS(risk_table, file = filename)


