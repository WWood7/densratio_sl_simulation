setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
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

# stack the learners into a super learner
stack <- Stack$new(cl1, cl2, cl3, cl4)
csl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))

n = 100
for (i in 1:5){
    # generate data
    data <- setdata(n)
    data$indicator <- data$a
    # normalize the data
    data$m <- data$m / sd(data$m)
    data$x <- data$x / sd(data$x)
    # define a task
    task1 <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 3)
    task2 <- sl3_Task$new(data = data, covariates = c('x', 'w'), outcome = 'indicator', folds = 3)
    # train the super learner
    risk_table <- CV_lrnr_sl(sl, task, loss_weighted_loglik_densratio)
    temp_risk <- as.matrix(risk_table[1:8, 3])
    risk_matrix2 <- cbind(risk_matrix2, temp_risk)
}








