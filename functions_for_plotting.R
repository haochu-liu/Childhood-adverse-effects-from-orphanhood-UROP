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
  
  a <- number_of_countries
  b <- a - 1
  
  for (i in 1:n) {
    Outcome <- c(df$column_labels[1+a*(i-1)], df$country[1:a])
    slice_df <- data.frame(Outcome)
    slice_df$odd_ratio <- c(NA, df$odd_ratio[(a*i-b):(a*i)])
    slice_df$CI_lower <- c(NA, df$CI_lower[(a*i-b):(a*i)])
    slice_df$CI_upper <- c(NA, df$CI_upper[(a*i-b):(a*i)])
    forester_data <- rbind(forester_data, slice_df)
  }
  
  forester_data
}


df_forester_year <- function(odd_df) {
  #' odd_df: dataframe for odds
  
  df <- odd_df[order(odd_df$col_names, odd_df$year), ]
  forester_data <- data.frame()
  unique_col <- unique(df$col_names)
  
  for (i in 1:length(unique_col)) {
    new_df <- df[df$col_names == unique_col[i], ]
    Outcome <- c(new_df$column_labels[1], new_df$year)
    slice_df <- data.frame(Outcome)
    slice_df$odd_ratio <- c(NA, new_df$odd_ratio)
    slice_df$CI_lower <- c(NA, new_df$CI_lower)
    slice_df$CI_upper <- c(NA, new_df$CI_upper)
    forester_data <- rbind(forester_data, slice_df)
  }
  
  forester_data
}

edu_bar_df<-function(df,year,country){
  #' df: input dataframe
  #' column_name: school attendence, highest educational level
  edu_df<-df[,c("hv121","hv106","hv105","Orphanhood")]
  edu_df<-remove_var_label(edu_df)
  #edu_df$hv105<-as.integer(edu_df$hv105)
  edu_df<-na.omit(edu_df)
  edu_df<-edu_df[,hv105>5]
  
  edu_orphan_df<-subset(edu_df,Orphanhood=="orphan")
  edu_nonorphan_df<-subset(edu_df,Orphanhood=="non-orphan")
  
  #attendence bar dataframe
  attendence_df<-df_barplot(df,"hv121","Orphanhood")
  attendence_df$percentage<-as.numeric(attendence_df$percentage)
  
  #highest educational level bar dataframe
  edu_orphan_df<-subset(edu_orphan_df,hv105==15)
  edu_nonorphan_df<-subset(edu_nonorphan_df,hv105==15)
  hl_or_p0<-nrow(edu_orphan_df[edu_orphan_df$hv106==0,])/nrow(edu_orphan_df)
  hl_or_p1<-nrow(edu_orphan_df[edu_orphan_df$hv106==1,])/nrow(edu_orphan_df)
  hl_or_p2<-nrow(edu_orphan_df[edu_orphan_df$hv106==2,])/nrow(edu_orphan_df)
  
  hl_nor_p0<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==0,])/nrow(edu_nonorphan_df)
  hl_nor_p1<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==1,])/nrow(edu_nonorphan_df)
  hl_nor_p2<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==2,])/nrow(edu_nonorphan_df)
  
  
  percentage<-c(hl_or_p0,hl_nor_p0,hl_or_p1,hl_nor_p1,hl_or_p2,hl_nor_p2)
  orphan<-c(rep(c("orphan","non-orphan"),3))
  samplesize<-c(rep(c(nrow(edu_orphan_df),nrow(edu_nonorphan_df)),3))
  level<-c("no education","no education","primary","primary","secondary","secondary")
  
  mat2<-matrix(c(percentage,orphan,level,samplesize),ncol=4)
  highest_level_df<-data.frame(mat2)
  colnames(highest_level_df)<-c("percentage","orphanhood","level","samplesize")
  highest_level_df["CI_lower"]<-CI_lower(percentage,samplesize)
  highest_level_df["CI_upper"]<-CI_upper(percentage,samplesize)
  highest_level_df$percentage<-as.numeric(highest_level_df$percentage)
  
  attendence_df["country"]<-country
  attendence_df["year"]<-as.character(year)
  highest_level_df["country"]<-country
  highest_level_df["year"]<-as.character(year)
  list(attendence_df,highest_level_df)
}



