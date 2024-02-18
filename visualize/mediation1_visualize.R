library(ggplot2)
library(reshape2)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/projects/Benkeser_Lab/simulation_results/mediation1'
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
    data <- data[order(data$learner), ]
    data$learner <- c(rep('lr1', 100), rep('lr2', 100), rep('lr3', 100), rep('lr4', 100), 
                      rep('lr5', 100), rep('lr6', 100), rep('lr7', 100), rep('lr8', 100),
                      rep('sl', 100))
    # Calculate the mean for each learner
    mean_data <- aggregate(WNLL ~ learner, data = data, FUN = mean)
    mean_data <- mean_data[order(mean_data$learner), ]
    mean_data$learner <- c('lr1', 'lr2', 'lr3', 'lr4', 'lr5', 'lr6', 'lr7', 'lr8', 'sl')

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






