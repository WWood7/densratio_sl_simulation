setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()

setdata <- function(n){
    x <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
    w <- rbinom(n, 1, 0.5)
    a <- rbinom(n, 1, 0.4 + 0.15*I(x>7.5) + 0.05*I(x<6) + 0.1*I(x<3) + 0.1*I(w==1))
    m <- I(w==1) * abs(rnorm(n, 0.2 * x + 2 * a, 2)) + 
        I(w==0) * rgamma(n, shape = 0.3 * x, scale = a + 1)
    df <- data.frame(x = x, w = w, a = a, m = m)
}
lr <- Lrnr_densratio_classification$new(name = 'lr', ub = 5)
n = 100
data <- setdata(n)
data$indicator <- data$a
# define a task
task <- sl3_Task$new(data = data, covariates = c('x', 'm', 'w'), outcome = 'indicator')

lr_fit <- lr$train(task)
