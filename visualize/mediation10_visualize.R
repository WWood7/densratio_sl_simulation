library(ggplot2)
library(reshape2)

# get the true value of the parameter
setdata <- function(n){
    w1 <- rbinom(n, 1, 0.5)
    w2 <- rnorm(n, 0.1 * w1 + 1.65, sd = 0.2)
    a <- rbinom(n, 1, sigmoid::sigmoid((2.5 - w2) * 1.2 * w1))
    m1 <- rnorm(n, a * w1 + 0.5 * a + 0.5 * w1 + 2 * w2, sd = 0.5)
    m2 <- rbinom(n, 1, sigmoid::sigmoid(w2 - 1.65 - a))
    y <- rnorm(n, w2 + 0.1 * m2 * a + 0.05 * m1 + 0.05 * a - 0.3 * w1 * m2, sd = 0.3)
    df <- data.frame(w1 = w1, w2 = w2, a = a, m1 = m1, m2 = m2, y = y)
}
# get a large dataset
df0 <- setdata(1000000)
# E[E(Y|A=0, M, W)|A=1, W]]
# w2 + 0.05(w1+0.5+0.5w1+2w2) - 0.3w1(sigmoid(w2-1.65-1))
df0$theta <- df0$w2 + 0.05 * (1.5 * d0f$w1 + 2 * df0$w2 + 0.5) - 0.3 * df0$w1 * (sigmoid::sigmoid(df0$w2 - 2.65))
true_value <- mean(df0$theta)




# Define the directory containing the .rds files
directory <- '/Users/winnwu/Emory_Projects/Benkeser_Lab/SL_for_Densratio/simulation_results/mediation10'
ssize_list <- c(100, 500, 1000, 2000)

results <- list(NULL)
for (i in 1:4){
    # Create a list of .rds files
    pattern <- paste0(ssize_list[i], "_.rds$")
    file_list <- list.files(directory, pattern = pattern, full.names = TRUE)
    
    # Read each .rds file and create a list of data frames
    data_list <- lapply(file_list, readRDS)
    
    # Combine the data frames into one
    combined_data <- do.call(rbind, data_list)
    
    results[[i]] <- combined_data
}

# get the means and sds
means <- NULL
sds <- NULL
mean_risk <- NULL
sd_risk <- NULL
extreme_value_indicators <- NULL
for (i in 1:4){
    sl <- results[[i]][, 1]
    csl <- results[[i]][, 2]
    sl_risk <- results[[i]][, 3]
    csl_risk <- results[[i]][, 4]
    # get the means
    means <- rbind(means, c(mean(sl), mean(csl)))
    sds <- rbind(sds, c(sd(sl), sd(csl)))
    mean_risk <- rbind(mean_risk, c(mean(sl_risk), mean(csl_risk)))
    sd_risk <- rbind(sd_risk, c(sd(sl_risk), sd(csl_risk)))
}

# create a data frame for ggplot
df <- data.frame(
    est_means = c(means[, 1], means[, 2]),
    est_sds = c(sds[, 1], sds[, 2]),
    mean_risk = c(mean_risk[, 1], mean_risk[, 2]),
    sd_risk = c(sd_risk[, 1], sd_risk[, 2]),
    sample_size = rep(c('n=100', 'n=500', 'n=1000', 'n=2000'), 2),
    group = rep(c('one-shot sl', 'separate csl'), each = 4)
)
df$sample_size <- factor(df$sample_size, levels = c('n=100', 'n=500', 'n=1000', 'n=2000'))

# create the plot
ggplot(df, aes(x = sample_size, y = est_means, color = group, group = group)) +
    geom_ribbon(aes(ymin = est_means - est_sds, ymax = est_means + est_sds, fill = group), alpha = 0.2) +
    geom_line() +
    geom_point() + 
    geom_hline(yintercept = true_value, linetype = 'dashed', color = 'black', size = 1) +
    labs(title = "Means of estimates with SD Bands", x = "Sample Size", y = "Means of estimates") +
    theme_minimal() +
    scale_x_discrete(limits = c('n=100', 'n=500', 'n=1000', 'n=2000'), expand = c(0, 0))
    
# another plot for cv-risks
ggplot(df, aes(x = sample_size, y = mean_risk, color = group, group = group)) +
    geom_ribbon(aes(ymin = mean_risk - sd_risk, ymax = mean_risk + sd_risk, fill = group), alpha = 0.2) +
    geom_line() +
    geom_point() + 
    labs(title = "Means of CV-Risks with SD Bands", x = "Sample Size", y = "Means of CV-Risks") +
    theme_minimal() +
    scale_x_discrete(limits = c('n=100', 'n=500', 'n=1000', 'n=2000'), expand = c(0, 0))


