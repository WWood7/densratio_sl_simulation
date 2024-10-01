library(ggplot2)
library(reshape2)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/Emory_Projects/Benkeser_Lab/SL_for_Densratio/simulation_results/mediation5'
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
extreme_value_indicators <- NULL
for (i in 1:4){
    # get the indicators
    extreme_value_indicators <- 
        rbind(extreme_value_indicators, apply(abs(results[[i]]) > 12, 2, sum))
    # get the vectors with extreme values excluded
    sl <- results[[i]][, 1][(abs(results[[i]]) <= 12)[, 1]]
    csl <- results[[i]][, 2][(abs(results[[i]]) <= 12)[, 2]]
    # get the means
    means <- rbind(means, c(mean(sl), mean(csl)))
    sds <- rbind(sds, c(sd(sl), sd(csl)))
}

# create a data frame for ggplot
df <- data.frame(
    est_means = c(means[, 1], means[, 2]),
    est_sds = c(sds[, 1], sds[, 2]),
    sample_size = rep(c('n=100', 'n=500', 'n=1000', 'n=2000'), 2),
    group = rep(c('one-shot sl', 'separate csl'), each = 4)
)
df$sample_size <- factor(df$sample_size, levels = c('n=100', 'n=500', 'n=1000', 'n=2000'))

# create the plot
ggplot(df, aes(x = sample_size, y = est_means, color = group, group = group)) +
    geom_ribbon(aes(ymin = est_means - est_sds, ymax = est_means + est_sds, fill = group), alpha = 0.2) +
    geom_line() +
    geom_point() + 
    geom_hline(yintercept = 9.1866, linetype = 'dashed', color = 'black', size = 1) +
    labs(title = "Means of estimates with SD Bands", x = "Sample Size", y = "Means of estimates") +
    theme_minimal() +
    scale_x_discrete(limits = c('n=100', 'n=500', 'n=1000', 'n=2000'), expand = c(0, 0))
    



