library(rdhs)
library(dplyr)
library(labelled)
library(ggplot2)


# choose one dataframe
# 2019
year <- "2019"
download <- get_datasets("RWPR81DT.ZIP")
df <- readRDS(download$RWPR81DT)
df <- data.frame(df)


# choose columns
df <- df[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105",
             "hv104", "ha3", "ha2", "ha40", "hv025", "hv201", "hv205",
             "hv206", "hv207", "hv208", "hv209", "hv210", "hv211", "hv212",
             "hv227", "hv221", "hv243a", "hv243b", "hv243e", "hv270",
             "hv106", "hv107", "hv121", "ha53", "ha57", "hml32")]


# get household dataframe
df_h <- df %>% distinct(hv001, hv002, .keep_all = TRUE)


# under 18
under_18 <- c(sum(df$hv105 < 18), sum(df$hv105 >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df <- df[df$hv105 < 18, ]


# set age as categorical
df$age_group <- NA
for (i in 1:nrow(df)) {
  if (df$hv105[i] < 5) {df$age_group[i] <- 1} # age 0-4
  if (df$hv105[i] <= 9 & df$hv105[i] >= 5) {df$age_group[i] <- 2} # age 5-9
  if (df$hv105[i] <= 14 & df$hv105[i] >= 10) {df$age_group[i] <- 3} # age 10-14
  if (df$hv105[i] <= 17 & df$hv105[i] >= 15) {df$age_group[i] <- 4} # age 15-17
}


count_sex <- c(sum(df$hv104==1), sum(df$hv104==2))
barplot(count_sex, names.arg=c("Male", "Female"), xlab="Sex",
        ylab="Number of individuals", col="blue", main=paste("Rwanda", year))


# split dataframes with orphan and not-orphan
df_orphan <- subset(df, (df$hv111==0 | df$hv113==0))
df_not_orphan <- subset(df, (df$hv111==1 & df$hv113==1))
df_h_orphan <- subset(df_h, (df_h$hv111==0 | df_h$hv113==0))
df_h_not_orphan <- subset(df_h, (df_h$hv111==1 & df_h$hv113==1))


# barplots
# functions for confidence interval
CI_upper <- function(x, n){
  upper <- x + 1.96*sqrt(x*(1-x)/n)
  return(upper)
}

CI_lower <- function(x, n){
  lower <- x - 1.96*sqrt(x*(1-x)/n)
  return(lower)
}

# table for barplots
barplot_data <- data.frame(col_name = c("hv025", "hv201", "hv205", "hv206",
                                        "hv207", "hv208", "hv209", "hv210",
                                        "hv211", "hv212", "hv227", "hv221",
                                        "hv243a", "hv243b", "hv243e", "hv270",
                                        "hv121", "hv121", "ha57", "hml32"))
barplot_data$col_label <- c("Lives in urban area", # 1: urban, 2: rural
                            "Has piped or tube water", # <=21: piped/tube, >21: not have
                            "Has flush or pit toilet", # <=23: flush/pit, >23: not have
                            "Has electricity", # 0: no, 1: yes
                            "Has radio", "Has television", "Has refrigerator",
                            "Has bicycle", "Has motorcycle/scooter",
                            "Has car/truck", "Has mosquito bed net",
                            "Has telephone (land-line)", "Has mobile telephone",
                            "Has watch", "Has a computer",
                            "Poor household wealth", # <=2: poor
                            "School attendance for age 7-12 (compulsory)", # 0: no, >=1: attend
                            "School attendance for age 13-17",
                            "Has anemia", # <=3: yes 4: no
                            "Has malaria" # 0: negative, 1: positive
                            )
barplot_data$percentage_orphan <- NA
barplot_data$CI_upper_orphan <- NA
barplot_data$CI_lower_orphan <- NA
barplot_data$percentage_not_orphan <- NA
barplot_data$CI_upper_not_orphan <- NA
barplot_data$CI_lower_not_orphan <- NA


count_for_barplot <- function(data_orphan, data_not_orphan) {
  n_orphan <- nrow(data_orphan)
  p_orphan <- sum(data_orphan) / n_orphan
  upper_orphan <- CI_upper(p_orphan, n_orphan)
  lower_orphan <- CI_lower(p_orphan, n_orphan)
  n_not <- nrow(data_not_orphan)
  p_not <- sum(data_not_orphan) / n_not
  upper_not <- CI_upper(p_not, n_not)
  lower_not <- CI_lower(p_not, n_not)
  c(p_orphan, upper_orphan, lower_orphan, p_not, upper_not, lower_not)
}
















