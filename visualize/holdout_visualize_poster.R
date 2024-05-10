library(ggplot2)
library(reshape2)
library(patchwork)

# Define the directory containing the .rds files
directory <- '/Users/winnwu/projects/Benkeser_Lab/simulation_results/holdout'
ssize_list <- c(500, 1000, 2000, 5000)

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

plot_list <- list()
for (i in 1:4){
    df <- data.frame()
    for (j in 1:ncol(results[[i]])){
        risk_mean <- rep(mean(results[[i]][, j]), nrow(results[[i]]))
        new_df <- data.frame(risk = results[[i]][, j], mean_risk = risk_mean)
        df <- rbind(df, new_df)
    }
    df$learner <- rep(c('Density Ratio Super Learner', 'Learner1', 'Learner2', 'Learner3', 'Learner4'), 
                           each = 100)
    
    # Add a helper column for sorting 'super_learner' first
    df$learner_order <- ifelse(df$learner == 'Density Ratio Super Learner', 1, 0)
    # Now, order 'df' by mean_risk descending and then by learner_order
    df <- df[order(-df$mean_risk, df$learner_order), ]
    
    df <- df[order(-df$mean_risk), ]
    
    # Define your RGB colors for each learner
    learner_colors <- c('Density Ratio Super Learner' = rgb(25/255, 49/255, 52/255), # Red
                        'Learner1' = rgb(76/255, 164/255, 162/255), # Blue
                        'Learner2' = rgb(76/255, 164/255, 162/255), # Green
                        'Learner3' = rgb(76/255, 164/255, 162/255), # Yellow
                        'Learner4' = rgb(76/255, 164/255, 162/255))# Purple
    
    # 'Learner4' = rgb(136/255, 116/255, 87/255))
    
    df$color <- learner_colors[df$learner]
    
    plot <- ggplot(df, aes(x=factor(learner, levels=unique(learner)), y=risk)) +
        geom_point(aes(group=learner), position=position_jitter(width=0.1, height=0), color=df$color) +
        geom_point(aes(y=mean_risk, x=factor(learner, levels=unique(learner))), shape=3, size=8, stroke = 2, color='red') +
        scale_color_identity() +
        ylim(-1.05, 0.05) +
        theme_minimal() +
        theme(
            # Main title
            plot.title = element_text(size = 24, face = "bold"),
            # Subtitle
            plot.subtitle = element_text(size = 22),
            # Caption
            plot.caption = element_text(size = 22),
            # Axis titles
            axis.title.x = element_text(size = 22, face = "bold"),
            axis.title.y = element_text(size = 22, face = "bold"),
            # Axis text (tick labels)
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            # Legend title (if you have one, remove legend.title = element_blank() if used)
            legend.title = element_text(size = 22),
            # Legend text
            legend.text = element_text(size = 22)
        ) +
        labs(y=paste0("Hold-Out Risk (n=", ssize_list[i],")"), x="Learners") +
        coord_flip()
    plot_list[[i]] <- plot
}

combined_plot <- wrap_plots(plot_list, ncol = 1) # Adjust ncol as needed
print(combined_plot)




