setwd('/Users/winnwu/projects/Benkeser_Lab/simulation_results/oracle')

# define a function to set the data
# n as sample size
setdata <- function(n){
    x <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
    w <- rbinom(n, 1, 0.5)
    a <- rbinom(n, 1, 0.4 + 0.15*I(x>7.5) + 0.05*I(x<6) + 0.1*I(x<3) + 0.1*I(w==1))
    m <- I(w==1) * abs(rnorm(n, 0.2 * x + 2 * a, 2)) + 
        I(w==0) * rgamma(n, shape = 0.3 * x, scale = a + 1)
    df <- data.frame(x = x, w = w, a = a, m = m)
}





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



# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr7) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
                            eval_function = loss_weighted_loglik_densratio))
n = 400
for (i in 1){
    # generate a data
    data <- setdata(n)
    data$indicator <- data$a
    # define a task
    task <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 3)
    # train the super learner
    sl_fit <- sl$train(task = task)
}

n = 100
pre_x <- abs(rnorm(n, 0, 2)) + rgamma(n, shape = 7.5, scale = 1)
pre_w <- rbinom(n, 1, 0.5)
pre_a <- rep(0, n)
pre_m <- I(pre_w==1) * abs(rnorm(n, 0.2 * pre_x + 2 * pre_a, 2)) + 
    I(pre_w==0) * rgamma(n, 0.3 * pre_x, scale = pre_a + 1)
density_a_1 <- I(pre_w==1) * 2 * dnorm(pre_m, 0.2 * pre_x + 2 *1, 2) + 
    I(pre_w==0) * dgamma(pre_m, shape = 0.3 * pre_x, scale = 1 + 1)
density_a_0 <- I(pre_w==1) * 2 * dnorm(pre_m, 0.2 * pre_x + 2 * pre_a, 2) + 
    I(pre_w==0) * dgamma(pre_m, shape = 0.3 * pre_x, scale = pre_a + 1)

true_ratio <- density_a_1 / density_a_0
# get the predictions
pred_data <- data.frame(x = pre_x, m = pre_m, w = pre_m)
pred_task <- sl3_Task$new(data = pred_data, covariates = c('x', 'm', 'w'), folds = 3)
sl_preds <- sl_fit$predict(task = pred_task)

sl_preds <- sl_fit$predict(pred_task)
lr1_preds <- sl_fit$learner_fits$lr1$predict(pred_task)
lr2_preds <- sl_fit$learner_fits$lr2$predict(pred_task)
lr3_preds <- sl_fit$learner_fits$lr3$predict(pred_task)
lr4_preds <- sl_fit$learner_fits$lr4$predict(pred_task)
lr5_preds <- sl_fit$learner_fits$lr5$predict(pred_task)
# lr6_preds <- sl_fit$learner_fits$`Pipeline(lr6->)`$predict(pred_task)
lr7_preds <- sl_fit$learner_fits$`Pipeline(lr7->)`$predict(pred_task)

# Create the scatter plot
plot(true_ratio, sl_preds, col = "red", pch = 16, xlab = "true_ratio", 
     ylab = "sl_preds", main = "Scatter Plot")
points(true_ratio, lr1_preds, col = "blue", pch = 16)
points(true_ratio, lr2_preds, col = "green", pch = 16)
points(true_ratio, lr3_preds, col = "orange", pch = 16)
points(true_ratio, lr4_preds, col = "purple", pch = 16)
points(true_ratio, lr7_preds, col = 'cyan', pch = 16)
lines(true_ratio, true_ratio)
legend("topright", 
       legend = c(
           "sl_preds", "lr1_preds", "lr2_preds", "lr3_preds", "lr4_preds", "lr5_preds"
       ), 
       col = c("red", "blue", "green", "orange", "purple","cyan"), pch = 16)
par(mar = c(5, 5, 4, 2) + 0.1)


