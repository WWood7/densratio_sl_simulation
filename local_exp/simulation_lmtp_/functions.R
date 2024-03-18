generate_data <- function(n){
    l1 <- max.col(t(rmultinom(n, 1, c(0.5, 0.25, 0.25))))
    a1 <- rbinom(n, 5, 0.5 * I(l1 > 1) + 0.1 * I(l1 > 2))
    l2 <- rbinom(n, 1, expit(-0.3 * l1 + 0.5 * a1))
    a2 <- rbinom(n, 5, expit(-2 + 1/(1 + 2*l2 + a1)))
    l3 <- rbinom(n, 1, expit(-0.3 * l2 + 0.5 * a2))
    a3 <- rbinom(n, 5, expit(-2 + 1/(1 + 2*l3 + a2)))
    l4 <- rbinom(n, 1, expit(-0.3 * l3 + 0.5 * a3))
    a4 <- rbinom(n, 5, expit(1 + l4 - 3*a3))
    y <- rbinom(n, 1, expit(-2 + 1/(1 - 1.2*a4 - 0.3*l4)))
    data <- data.frame(l1=l1, a1=a1, l2=l2, a2=a2, l3=l3, a3=a3, l4=l4, a4=a4, y=y)
}


expit <- function(x){
    exp(x) / (1 + exp(x))
}

# functions for augmentation
augment_data1 <- function(df){
    n <- nrow(df)
    df$id <- seq_len(n)
    aug_df <- df
    aug_df$indicator <- 1
    df$indicator <- 0
    aug_df$a1 <- df$a1 - I(df$a1 >= 1)
    merged_data <- rbind(df, aug_df)
}
augment_data2 <- function(df){
    n <- nrow(df)
    df$id <- seq_len(n)
    aug_df <- df
    aug_df$indicator <- 1
    df$indicator <- 0
    aug_df$a2 <- df$a2 - I(df$a2 >= 1)
    merged_data <- rbind(df, aug_df)
}
augment_data3 <- function(df){
    n <- nrow(df)
    df$id <- seq_len(n)
    aug_df <- df
    aug_df$indicator <- 1
    df$indicator <- 0
    aug_df$a3 <- df$a3 - I(df$a3 >= 1)
    merged_data <- rbind(df, aug_df)
}
augment_data4 <- function(df){
    n <- nrow(df)
    df$id <- seq_len(n)
    aug_df <- df
    aug_df$indicator <- 1
    df$indicator <- 0
    aug_df$a4 <- df$a4 - I(df$a4 >= 1)
    merged_data <- rbind(df, aug_df)
}



# define a function for calculating hold-out risk
holdout_risk <- function(lr, df, covs){
    # create a task
    test_task <- sl3_Task$new(data = df, covariates = covs)
    # get the prediction on the hold-out set
    pred <- lr$predict(test_task)
    # calculate risk
    out <- -log(bound(pred)) * (df$a - 0.5) * 2
    mean(out)
}





