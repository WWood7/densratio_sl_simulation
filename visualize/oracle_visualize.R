library(ggplot2)
library(reshape2)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/projects/Benkeser_Lab/simulation_results/oracle'
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


# draw the plots
for (i in 1:4){
    data <- results[[i]][, c(1, 3)]
    # Calculate the mean for each learner
    mean_data <- aggregate(WNLL ~ learner, data = data, FUN = mean)
    
    # Rename the mean column for clarity
    names(mean_data)[which(names(mean_data) == "WNLL")] <- "mean"
    
    # Merge the mean scores back into the original data
    data <- merge(data, mean_data, by = "learner")
    data <- data[order(-data$mean), ]
    plot <- ggplot(data, aes(x=factor(learner, levels=unique(learner)), y=WNLL)) +
        geom_point(aes(group=learner), position=position_jitter(width=0.1, height=0)) +
        geom_point(aes(y=mean, x=factor(learner, levels=unique(learner))), shape=3, size=3, color="red") +
        theme_minimal() +
        labs(y=paste0("cv_risk(n=", ssize_list[i],")"), x="Learners") +
        coord_flip()
    print(plot)
}






