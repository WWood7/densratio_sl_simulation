setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()
setwd("/Users/winnwu/Documents/GitHub/densratio")
devtools::load_all()
library(reshape2)


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
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5, lr6, lr7) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))


n = 400
risk_matrix1 <- NULL
for (i in 1:5){
    # generate a data
    data <- setdata(n)
    data$indicator <- data$a
    # define a task
    task <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 3)
    # train the super learner
    risk_table <- CV_lrnr_sl(sl, task, loss_weighted_loglik_densratio)
    temp_risk <- as.matrix(risk_table[1:8, 3])
    risk_matrix1 <- cbind(risk_matrix1, temp_risk)
}


risk_means_400 <- apply(risk_matrix1, 1, mean)
data_long_400 <- as.data.frame(as.table(risk_matrix1))
data_long_400$Var1 <- c('lr1', 'lr2', 'lr3', 'lr4', 'lr5', 'lr6', 'lr7', 'sl')

ggplot(data_long_400, aes(x=Var1, y=Freq)) +
    geom_point(aes(group=Var1), position=position_jitter(width=0.1, height=0)) +
    geom_point(aes(y=risk_means_400[Var1], x=Var1), shape=3, size=3, color="red") +
    theme_minimal() +
    labs(y="Relative MSE", x="Method") +
    coord_flip()


data_long_400 <- melt(risk_matrix1)
data_long_400$RowMean <- apply(risk_matrix1, 1, mean)[data_long_400$Var1]
data_long_400$Var1 <- c('lr1', 'lr2', 'lr3', 'lr4', 'lr5', 'lr6', 'lr7', 'sl')
data_long_400 <- data_long_400[order(-data_long_400$RowMean), ]
ggplot(data_long_400, aes(x=factor(Var1, levels=unique(Var1)), y=value)) +
     geom_point(aes(group=Var1), position=position_jitter(width=0.1, height=0)) +
     geom_point(aes(y=RowMean, x=factor(Var1, levels=unique(Var1))), shape=3, size=3, color="red") +
     theme_minimal() +
     labs(y="cv_risk(n=400)", x="Learners") +
     coord_flip()



risk_matrix2 <- NULL
n = 1000
for (i in 1:5){
    # generate a data
    data <- setdata(n)
    data$indicator <- data$a
    # define a task
    task <- sl3_Task$new(data = data, covariates = c('m', 'x', 'w'), outcome = 'indicator', folds = 3)
    # train the super learner
    risk_table <- CV_lrnr_sl(sl, task, loss_weighted_loglik_densratio)
    temp_risk <- as.matrix(risk_table[1:8, 3])
    risk_matrix2 <- cbind(risk_matrix2, temp_risk)
}



data_long_1000 <- melt(risk_matrix2)
data_long_1000$RowMean <- apply(risk_matrix2, 1, mean)[data_long_1000$Var1]
data_long_1000$Var1 <- c('lr1', 'lr2', 'lr3', 'lr4', 'lr5', 'lr6', 'lr7', 'sl')
data_long_1000 <- data_long_1000[order(-data_long_1000$RowMean), ]
ggplot(data_long_1000, aes(x=factor(Var1, levels=unique(Var1)), y=value)) +
    geom_point(aes(group=Var1), position=position_jitter(width=0.1, height=0)) +
    geom_point(aes(y=RowMean, x=factor(Var1, levels=unique(Var1))), shape=3, size=3, color="red") +
    theme_minimal() +
    labs(y="cv_risk(n=1000)", x="Learners") +
    coord_flip()



