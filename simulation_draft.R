# define a function to set the data
# n as sample size
setdata <- function(n){
    x <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
    w <- rbinom(n, 1, 0.5)
    a <- rbinom(n, 1, 0.4 + 0.15*I(x>7.5) + 0.05*I(x<6) + 0.1*I(x<3) + 0.1*I(w==1))
    m <- I(w==1) * abs(rnorm(n, 0.3 * x + 2 * a, 1)) + 
        I(w==0) * rgamma(n, 0.3 * x, a + 1)
    df <- data.frame(x = x, w = w, a = a, m = m)
}

# define super learners
setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()
# define learner 1
# classification based logistic regression