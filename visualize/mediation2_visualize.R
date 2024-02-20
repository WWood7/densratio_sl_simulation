library(ggplot2)
library(reshape2)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/projects/Benkeser_Lab/simulation_results/mini_result/2'
ssize_list <- c(500)

results <- list(NULL)
for (i in 1:1){
    # Create a list of .rds files
    pattern <- paste0(ssize_list[i], "_.rds$")
    file_list <- list.files(directory, pattern = pattern, full.names = TRUE)
    
    # Read each .rds file and create a list of data frames
    data_list <- lapply(file_list, readRDS)
    
    # Combine the data frames into one
    combined_data <- do.call(rbind, data_list)
    
    results[[i]] <- combined_data
}
mean1 <- mean(results[[1]][, 1])
sd1 <- sd(results[[1]][, 1])
mean2 <- mean(results[[1]][, 1])
sd2 <- sd(results[[1]][, 1])






