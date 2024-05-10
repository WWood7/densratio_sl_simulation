setwd("/Users/winnwu/Documents/GitHub/sl3_densratio")
devtools::load_all()
setwd("/Users/winnwu/Documents/GitHub/densratio")
devtools::load_all()
library(msm)
library(patchwork)

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

# # generate test data (for computing hold-out risk)
# set.seed(777)
# test <- setdata(10000)
# 
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

# # define a grid of parameters for the simulation
# # 10 replicates at each sample size in each setting
# params = expand.grid(seed = 1:100,
#                     n = c(100, 500, 1000, 2000, 5000))
# 
# # import environment parameter
# # get iter from array
# iter = Sys.getenv("SLURM_ARRAY_TASK_ID")
# iter = as.numeric(iter)
# # get nloop
# args = commandArgs(trailingOnly = TRUE)
# nloop = as.numeric(args[1])
# # get task id
# max_jobs = 100
# task_id = max_jobs*(nloop-1) + iter
# seed = as.numeric(params[task_id,][1])
# n = as.numeric(params[task_id,][2])
# set.seed(seed)




# generate a data
n <- 700
train <- setdata(n)
train$indicator <- train$a

# define classification learners
cl1 <- Lrnr_bayesglm$new()
cl2 <- Lrnr_glm$new()
cl3 <- Lrnr_hal9001$new()
# cl4 <- Lrnr_gam$new()
# cl5 <- Lrnr_xgboost$new()
# stack the learners into a super learner
stack_cl <- Stack$new(cl1, cl2)
csl <- Lrnr_sl$new(stack_cl, metalearner = Lrnr_solnp$new(
    eval_function = loss_loglik_binomial))

# define ratio learners
lr1 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.8, name = 'lr1'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.8, name = '', conditional_set = 'm_sdd'))
lr2 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = 'lr2'), 
                    Lrnr_densratio_kernel$new(method = 'RuLSIF', kernel_num = n, alpha = 0.5, name = '', conditional_set = 'm_sdd'))
lr3 <- Pipeline$new(Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = 'lr3'), 
                    Lrnr_densratio_kernel$new(method = 'KLIEP', kernel_num = n, name = '', conditional_set = 'm_sdd'))
# use Lrnr_glm for the second stage classification
lr4 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl2, name = 'cl2'),
                   Lrnr_densratio_classification$new(classifier = cl2, conditional_set = 'm_sdd', name = ''))
lr5 <- Pipeline$new(Lrnr_densratio_classification$new(classifier = cl3, name = 'cl3'),
                    Lrnr_densratio_classification$new(classifier = cl3, conditional_set = 'm_sdd', name = ''))



# stack the learners into a super learner
stack <- Stack$new(lr1, lr2, lr3, lr4, lr5) 
sl <- Lrnr_sl$new(stack, metalearner = Lrnr_solnp$new(
    eval_function = loss_weighted_loglik_densratio ))

# normalize the data
sd_m <- sd(train$m)
sd_w <- sd(train$w)
train$m_sdd <- train$m / sd_m
train$w_sdd <- train$w / sd_w
# define a task
task <- sl3_Task$new(data = train, covariates = c('m_sdd', 'w_sdd'), outcome = 'a', folds = 5)
# train the super learner
sl_fit <- sl$train(task)



# define a function to get the true ratios
get_tr <- function(df){
    new_df <- df
    new_df$tr <- dbeta(df$m, 0.6 * df$w + 1, 0.7 * df$w) /
        dtnorm(df$m, 0.1 * df$w, 1, lower = 0, upper = 1)
    return(new_df)
}

# construct a data frame to plot the ratio curve
test1 <- data.frame(
    w = 5,
    m = seq(0.01, 0.99, length.out = 1000)
)
test1 <- get_tr(test1)

test2 <- data.frame(
    w = 3,
    m = seq(0.01, 0.99, length.out = 1000)
)
test2 <- get_tr(test2)
test3 <- data.frame(
    w = 7,
    m = seq(0.01, 0.99, length.out = 1000)
)
test3 <- get_tr(test3)
# standardize test data using sd's from training set
test1$m_sdd <- test1$m / sd_m
test1$w_sdd <- test1$w / sd_w
test2$m_sdd <- test2$m / sd_m
test2$w_sdd <- test2$w / sd_w
test3$m_sdd <- test3$m / sd_m
test3$w_sdd <- test3$w / sd_w
# calculate the predicted ratios
pre_task1 <- sl3_Task$new(data = test1, covariates = c('m_sdd', 'w_sdd'))
pre_task2 <- sl3_Task$new(data = test2, covariates = c('m_sdd', 'w_sdd'))
pre_task3 <- sl3_Task$new(data = test3, covariates = c('m_sdd', 'w_sdd'))
sl_predict1 <- sl_fit$predict(pre_task1)
sl_predict2 <- sl_fit$predict(pre_task2)
sl_predict3 <- sl_fit$predict(pre_task3)

plotdf <- data.frame(
    m = seq(0.01, 0.99, length.out = 1000),
    tr1 = test1$tr,
    sl_predict1 = sl_predict1,
    tr2 = test2$tr,
    tr3 = test3$tr,
    sl_predict2 = sl_predict2,
    sl_predict3 = sl_predict3
)



plot_list <- list() # Initialize an empty list to store plots
w_list <- c(5, 3, 7)
for (i in 1:3) {
    if (i == 3){
        # Generate the plot for tr_i
        p_tr <- ggplot(data = plotdf, aes(x = m)) +
            geom_line(aes(y = .data[[paste0("tr", i)]], color = "True Ratio"), linewidth = 2) +  # Map color inside aes for legend
            geom_line(aes(y = .data[[paste0("sl_predict", i)]], color = "Estimated Ratio"), linewidth = 2) +  # Map color inside aes for legend
            scale_color_manual(values = c("True Ratio" = rgb(101/255, 144/255, 157/255), 
                                          "Estimated Ratio" = rgb(156/255, 49/255, 41/255)),
                               labels = c("True Ratio" = "True Ratio", 
                                          "Estimated Ratio" = "Super Learner Estimated Ratio")) +
            labs(title = paste0("W = ", w_list[i]), x = "m", y = NULL) +
            theme_minimal() +
            ylim(0, 3) + 
            scale_x_continuous(breaks = c(0, 1),  # Set the breaks for the x-axis ticks to 0 and 1
                               labels = c("0", "1")) + # Set custom labels for the ticks
            theme(
                # Main title
                plot.title = element_text(size = 22, face = "bold"),
                # Subtitle
                plot.subtitle = element_text(size = 20),
                # Caption
                plot.caption = element_text(size = 20),
                # Axis titles
                axis.title.x = element_text(size = 20, face = "bold"),
                axis.title.y = element_text(size = 20, face = "bold"),
                # Axis text (tick labels)
                axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 18),
                # Legend title (if you have one, remove legend.title = element_blank() if used)
                legend.title = element_text(size = 20),
                # Legend text
                legend.text = element_text(size = 20)
            ) + 
            theme(legend.title = element_blank(),  # Remove legend title
                  legend.position = c(0.45, 0.95)) +  # Adjust legend position
            guides(color = guide_legend(override.aes = list(size = 4)))  # Adjust legend line size
    }
    else if (i == 1){
        p_tr <- ggplot(data = plotdf, aes(x = m)) +
            geom_line(aes(y = .data[[paste0("tr", i)]], color = "True Ratio"), linewidth = 2) +  # Map color inside aes for legend
            geom_line(aes(y = .data[[paste0("sl_predict", i)]], color = "Estimated Ratio"), linewidth = 2) +  # Map color inside aes for legend
            scale_color_manual(values = c("True Ratio" = rgb(101/255, 144/255, 157/255), 
                                          "Estimated Ratio" = rgb(156/255, 49/255, 41/255)),
                               labels = c("True Ratio" = "True Ratio", 
                                          "Estimated Ratio" = "Super Learner Estimated Ratio")) +
            ylim(0, 3) + 
            scale_x_continuous(breaks = c(0, 1),  # Set the breaks for the x-axis ticks to 0 and 1
                               labels = c("0", "1")) + # Set custom labels for the ticks
            labs(title = paste0("W = ", w_list[i]), x = "m", y = "Density Ratio") +
            theme_minimal() +
            theme(
                # Main title
                plot.title = element_text(size = 22, face = "bold"),
                # Subtitle
                plot.subtitle = element_text(size = 20),
                # Caption
                plot.caption = element_text(size = 20),
                # Axis titles
                axis.title.x = element_text(size = 20, face = "bold"),
                axis.title.y = element_text(size = 20, face = "bold"),
                # Axis text (tick labels)
                axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 18),
                # Legend title (if you have one, remove legend.title = element_blank() if used)
                legend.title = element_text(size = 20),
                # Legend text
                legend.text = element_text(size = 20)
            ) + 
            theme(legend.title = element_blank(),  # Remove legend title
                  legend.position = 'none') +  # Adjust legend position
            guides(color = guide_legend(override.aes = list(size = 4)))  # Adjust legend line size
    }
    else{
        p_tr <- ggplot(data = plotdf, aes(x = m)) +
            geom_line(aes(y = .data[[paste0("tr", i)]], color = "True Ratio"), linewidth = 2) +  # Map color inside aes for legend
            geom_line(aes(y = .data[[paste0("sl_predict", i)]], color = "Estimated Ratio"), linewidth = 2) +  # Map color inside aes for legend
            scale_color_manual(values = c("True Ratio" = rgb(101/255, 144/255, 157/255), 
                                          "Estimated Ratio" = rgb(156/255, 49/255, 41/255)),
                               labels = c("True Ratio" = "True Ratio", 
                                          "Estimated Ratio" = "Super Learner Estimated Ratio")) +
            ylim(0, 3) + 
            scale_x_continuous(breaks = c(0, 1),  # Set the breaks for the x-axis ticks to 0 and 1
                               labels = c("0", "1")) + # Set custom labels for the ticks
            labs(title = paste0("W = ", w_list[i]), x = "m", y = NULL) +
            theme_minimal() +
            theme(
                # Main title
                plot.title = element_text(size = 22, face = "bold"),
                # Subtitle
                plot.subtitle = element_text(size = 20),
                # Caption
                plot.caption = element_text(size = 20),
                # Axis titles
                axis.title.x = element_text(size = 20, face = "bold"),
                axis.title.y = element_text(size = 20, face = "bold"),
                # Axis text (tick labels)
                axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 18),
                # Legend title (if you have one, remove legend.title = element_blank() if used)
                legend.title = element_text(size = 20),
                # Legend text
                legend.text = element_text(size = 20)
            ) + 
            theme(legend.title = element_blank(),  # Remove legend title
                  legend.position = 'none') +  # Adjust legend position
            guides(color = guide_legend(override.aes = list(size = 4)))  # Adjust legend line size
    }
   
    
    
    
    # Store the combined plot in the list
    plot_list[[i]] <- p_tr
}

combined_plot <- wrap_plots(plot_list, nrow = 1)
print(combined_plot)


