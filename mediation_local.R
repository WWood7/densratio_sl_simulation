setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()
setwd("/Users/winnwu/Documents/GitHub/densratio")
devtools::load_all()


# define a function to set the data
# w = |w1| + w2 where w1 ~ N(0,4), w2 ~ Gamma(7.5, 1)
# a|w ~ Bernoulli(0.4 - 0.15I(w>7.5) + 0.15I(w>9) + 0.25I(w<6) - 0.1I(w<3))
# m|w,a ~ Beta(0.5w + a, 0.8w)
# y|a, m, w ~ N(5 + 1.5A + 2M + 0.3W, 4)
setdata <- function(n){
    w <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
    a <- rbinom(n, 1, 0.4 - 0.15*I(w>7.5) + 0.15*I(w>9) + 0.25*I(w<6) - 0.1*I(w<3))
    m <- rbeta(n, 0.5 * w + a, 0.8 * w)
    y <- rnorm(n, 5 + 1.5 * a + 2 * m + 0.3 * w, sd = 2)
    df <- data.frame(w = w, a = a, m = m, y = y)
}

# calculate the true value of the target parameter
true_val_data <- setdata(1000000)
# E[E(Y|A=0, M, W)|A=1, W]]
# 5 + 2 * E(M|A=1, W) + 0.3*W
intermediate <- 5 + 
    2 * (0.5 * true_val_data$w + 1) / (0.5 * true_val_data$w + 1 + 0.8 * true_val_data$w) + 
    0.3 * true_val_data$w
# E[E[E(Y|A=0, M, W)|A=1, W]]]
true_value <- mean(intermediate)




# define classification super learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_glm$new()
cl3 <- Lrnr_hal9001$new()
cl4 <- Lrnr_gam$new()
cl5 <- Lrnr_xgboost$new()

# define learners
lr1 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.5, name = 'lr1'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.5, name = '', stage2 = TRUE))
lr2 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = 100, alpha = 0.8, name = '', stage2 = TRUE))
lr3 <- Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = 200, 
                                 fold_num = 8, name = 'lr3')
lr4 <- Pipeline$new(Lrnr_densratio_classification$new(name = 'lr4'), 
                    Lrnr_densratio_classification$new(stage2 = TRUE, name = ''))
lr5 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_bayesglm), name = 'lr5'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_bayesglm), stage2 = TRUE, name = ''))
lr6 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_hal9001), name = 'lr6'), 
                    Lrnr_densratio_classification$new(classifier = make_learner(Lrnr_glm), stage2 = TRUE, name = ''))

# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2, cl3, cl4, cl5)
csl <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))


# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))



# define a function to calculate the two on-step estimations
# based on two ways of density ratio estimation
onestep_estimator <- function(df){
    
    # sequential regression
    
    # estimate E[Y|A, M, W] and get E[Y|A = 0, M, W]
    model1 <- lm(y ~ a + m + w, data = df)
    new_df1 <- df
    new_df1$a <- 0
    df$mu_hat <- predict(model1, newdata = new_df1)
    
    # estimate E[E[Y|A = 0, M, W]|A=1, W]] on A=1 subset
    # then predict on the whole data set
    sub_df <- df[df$a == 1, ]
    model2 <- lm(mu_hat ~ w, data = sub_df)
    df$theta_hat <- predict(model2, newdata = df)
    
    # get theta_bar
    # E[E[E[Y|A = 0, M, W]|A=1, W]]]
    df$theta_bar <- mean(df$theta_hat)
    
    # estimate propensity scores p(A|W)
    model3 <- glm(a ~ w, family = binomial, data = df)
    df$ps1 <- predict(model3, newdata = df, type = 'response')
    df$ps0 <- 1 - df$ps1
    
    # estimate the density ratios
    df$indicator <- df$a
    # standardize the data
    sd_m <- sd(df$m)
    sd_w <- sd(df$w)
    df$m_sdd <- df$m / sd_m
    df$w_sdd <- df$w / sd_w
    # define the tasks
    task1 <- sl3_Task$new(data = df, covariates = c('m_sdd', 'w_sdd'), outcome = 'indicator', folds = 3)
    task2 <- sl3_Task$new(data = df, covariates = c('w_sdd'), outcome = 'indicator', folds = 3)
    
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
    
    # get the bias correction terms
    df$bias_sl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_sl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    df$bias_csl <- as.numeric(df$a == 0) / df$ps0 * df$ratio_csl * (df$y - df$mu_hat) +
        as.numeric(df$a == 1) / df$ps1 * (df$mu_hat - df$theta_hat) + df$theta_hat - df$theta_bar
    
    est_sl <- mean(df$bias_sl) + mean(df$theta_hat)
    est_csl <- mean(df$bias_csl) + mean(df$theta_hat)
    return(c(est_sl, est_csl))
}



# comparison
est_sl <- NULL
est_csl <- NULL
for (i in c(1:10)){
    data <- setdata(500)
    results <- onestep_estimator(data)
    est_sl <- c(est_sl, results[1])
    est_csl <- c(est_csl, results[2])
}







