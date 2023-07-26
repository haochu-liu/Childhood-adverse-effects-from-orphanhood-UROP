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


