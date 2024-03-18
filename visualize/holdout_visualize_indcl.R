library(ggplot2)
library(reshape2)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/projects/Benkeser_Lab/simulation_results/holdout_indcl'
ssize_list <- c(100, 500, 1000, 2000, 5000)

results <- list(NULL)
for (i in 1:5){
    # Create a list of .rds files
    pattern <- paste0(ssize_list[i], "_.rds$")
    file_list <- list.files(directory, pattern = pattern, full.names = TRUE)
    
    # Read each .rds file and create a list of data frames
    data_list <- lapply(file_list, readRDS)
    
    # Combine the data frames into one
    combined_data <- do.call(rbind, data_list)
    
    results[[i]] <- combined_data
}


for (i in 1:10){
    df <- data.frame()
    for (j in 1:ncol(results[[i]])){
        risk_mean <- rep(mean(results[[i]][, j]), nrow(results[[i]]))
        new_df <- data.frame(risk = results[[i]][, j], mean_risk = risk_mean)
        df <- rbind(df, new_df)
    }
    df$learner <- rep(c('super_learner', 'kernel_lr1', 'kernel_lr2', 'kernel_lr3', 'classification_lr1',
                        'classification_lr2', 'classification_lr3', 'classification_lr4', 'classification_lr5',
                        'classification_sl'), 
                           each = 100)
    
    # Add a helper column for sorting 'super_learner' first
    df$learner_order <- ifelse(df$learner == 'super_learner', 1, 0)
    # Now, order 'df' by mean_risk descending and then by learner_order
    df <- df[order(-df$mean_risk, df$learner_order), ]
    
    df <- df[order(-df$mean_risk), ]
    plot <- ggplot(df, aes(x=factor(learner, levels=unique(learner)), y=risk)) +
        geom_point(aes(group=learner), position=position_jitter(width=0.1, height=0)) +
        geom_point(aes(y=mean_risk, x=factor(learner, levels=unique(learner))), shape=3, size=3, color="red") +
        theme_minimal() +
        labs(y=paste0("hold out risk (n=", ssize_list[i],")"), x="Learners") +
        coord_flip()
    print(plot)
}






