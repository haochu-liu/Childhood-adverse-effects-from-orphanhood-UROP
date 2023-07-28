library(dplyr)
library(Hmisc)
library(labelled)
library(haven)


# functions for confidence interval
CI_upper <- function(x, n){
  upper <- x + 1.96*sqrt(x*(1-x)/n)
  return(upper)
}

CI_lower <- function(x, n){
  lower <- x - 1.96*sqrt(x*(1-x)/n)
  return(lower)
}


# function to count percentage and confidence interval
count_for_barplot <- function(df) {
  n <- length(df)
  p <- sum(df) / n
  upper <- CI_upper(p, n)
  lower <- CI_lower(p, n)
  c(p, upper, lower)
}


# function to return a dataframe for barplot
df_barplot <- function(df, col_names, col_orphan) {
  #' df: input dataframe
  #' col_names: column names for binary variables
  #' col_orphan: column name for orphanhood
  
  df <- df[!is.na(df[, col_orphan]), ]
  df_orphan <- subset(df, df[, col_orphan]=="orphan")
  df_not_orphan <- subset(df, df[, col_orphan]=="non-orphan")
  
  column_names <- rep(col_names, 2)
  label_list <- label(df)
  column_labels <- rep(as.character(label_list[col_names]), 2)
  orphan <- c(rep("orphan", length(col_names)),
              rep("non-orphan", length(col_names)))
  barplot_data <- data.frame(column_names, column_labels, orphan)
  barplot_data$percentage <- NA
  barplot_data$CI_upper <- NA
  barplot_data$CI_lower <- NA
  
  for (i in 1:length(col_names)) {
    val_labels(df_orphan[, col_names[i]]) <- NULL
    val_labels(df_not_orphan[, col_names[i]]) <- NULL
    barplot_data[i, 4:6] <- count_for_barplot(na.omit(df_orphan[, col_names[i]]==1))
    barplot_data[length(col_names) + i, 4:6] <- count_for_barplot(na.omit(df_not_orphan[, col_names[i]]==1))
  }
  
  barplot_data
}


df_boxplot <- function(df, col_names, col_orphan, col_age) {
  #' df: input dataframe
  #' col_names: column name of required data
  #' col_orphan: column name for orphanhood
  #' col_age: column name for age
  
  df <- df[!is.na(df[, col_orphan]), ]
  df <- df[!is.na(df[, col_age]), ]
  boxplot_data <- df[c(col_names, col_orphan, col_age)]
  boxplot_data[, col_age] <- as.character(boxplot_data[, col_age])
  boxplot_data
}


df_isna <- function(df_list, col_names, col_labels, years) {
  #' df_list: list of dataframes, key is year, value is dataframe
  #' col_names: column names for all dataframes
  #' years: years in string
  
  year <- c()
  for (i in 1:length(years)) {
    year <- c(year, rep(years[i], length(col_names)))
  }
  column <- rep(col_names, length(years))
  label <- rep(col_labels, length(years))
  isna_data <- data.frame(year, column, label)
  isna_data$na_percentage <- NA
  
  index <- 1
  for (i in 1:length(years)) {
    df <- df_list[[years[i]]]
    n <- nrow(df)
    cols_in_df <- colnames(df)
    for (j in 1:length(col_names)) {
      if (col_names[j] %in% cols_in_df) {
        isna_data[index, "na_percentage"] <- sum(is.na(df[, col_names[j]])) / n
      } else {
        # isna_data[index, "na_percentage"] <- 1.0
      }
      index <- index + 1
    }
  }
  isna_data
}


df_odd_ratio <- function(df, col_names, col_orphan){
  df <- df[!is.na(df[, col_orphan]), ]
  
  label_list <- label(df)
  column_labels <- as.character(label_list[col_names])
  odd_data <- data.frame(col_names, column_labels)
  odd_data$odd_ratio <- NA
  odd_data$CI_upper <- NA
  odd_data$CI_lower <- NA
  
  for (i in 1:length(col_names)) {
    val_labels(df[, col_names[i]]) <- NULL
    # count a b c d
    a <- sum(na.omit(df[,col_orphan] == "orphan" & df[,col_names[i]] == 1))
    b <- sum(na.omit(df[,col_orphan] == "orphan" & df[,col_names[i]] == 0))
    c <- sum(na.omit(df[,col_orphan] == "non-orphan" & df[,col_names[i]] == 1))
    d <- sum(na.omit(df[,col_orphan] == "non-orphan" & df[,col_names[i]] == 0))
    
    # odd ratio
    od <- (a*d) / (b*c)
      
    # CI lower
    lower <- exp(log(od) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
    
    # CI upper
    upper <- exp(log(od) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d))
    
    # updata df
    odd_data[i, "odd_ratio"] <- od
    odd_data[i, "CI_upper"] <- upper
    odd_data[i, "CI_lower"] <- lower
  }
  
  odd_data
}


df_forester_country <- function(odd_df, number_of_countries) {
  #' odd_df: dataframe for odds
  #' number_of_countries: numeric number
  df <- odd_df[order(odd_df$col_names, odd_df$country), ]
  forester_data <- data.frame()
  n <- nrow(df) / number_of_countries
  
  for (i in 1:n) {
    Outcome <- c(df$column_labels[1+3*(i-1)], df$country[1:3])
    slice_df <- data.frame(Outcome)
    slice_df$odd_ratio <- c(NA, df$odd_ratio[(3*i-2):(3*i)])
    slice_df$CI_lower <- c(NA, df$CI_lower[(3*i-2):(3*i)])
    slice_df$CI_upper <- c(NA, df$CI_upper[(3*i-2):(3*i)])
    forester_data <- rbind(forester_data, slice_df)
  }
  
  forester_data
}


df_forester_year <- function(df, years) {
  #' df: dataframe for odds
  #' years: list of years
  
  df <- 
}






