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


# under 18
under_18 <- c(sum(df$hv105 < 18), sum(df$hv105 >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df <- df[df$hv105 < 18, ]


# set age as categorical
# df$age_group <- NA
# for (i in 1:nrow(df)) {
#   if (df$hv105[i] < 5) {df$age_group[i] <- 1} # age 0-4
#   if (df$hv105[i] <= 9 & df$hv105[i] >= 5) {df$age_group[i] <- 2} # age 5-9
#   if (df$hv105[i] <= 14 & df$hv105[i] >= 10) {df$age_group[i] <- 3} # age 10-14
#   if (df$hv105[i] <= 17 & df$hv105[i] >= 15) {df$age_group[i] <- 4} # age 15-17
# }


count_sex <- c(sum(df$hv104==1), sum(df$hv104==2))
barplot(count_sex, names.arg=c("Male", "Female"), xlab="Sex",
        ylab="Number of individuals", col="blue", main=paste("Rwanda", year))


# get household dataframe
df_h <- df %>% distinct(hv001, hv002, .keep_all = TRUE)


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

# creat table for barplots
col_name <- c("hv025", "hv201", "hv205", "hv206",
              "hv207", "hv208", "hv209", "hv210",
              "hv211", "hv212", "hv227", "hv221",
              "hv243a", "hv243b", "hv243e", "hv270",
              "hv121", "hv121", "ha57", "hml32")
col_name <- rep(col_name, 2)
col_label <- c("Lives in urban area", # 1: urban, 2: rural
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
col_label <- rep(col_label, 2)
orphan <- c(rep("Orphan", length(col_name)/2), rep("Not orphan", length(col_name)/2))
barplot_data <- data.frame(col_name, col_label, orphan)
barplot_data$percentage <- NA
barplot_data$CI_upper <- NA
barplot_data$CI_lower <- NA


# creat function to fill in table
count_for_barplot <- function(df) {
  n <- length(df)
  p <- sum(df) / n
  upper <- CI_upper(p, n)
  lower <- CI_lower(p, n)
  c(p, upper, lower)
}


# filling the table for barplots
barplot_data[1, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv025==1))
barplot_data[2, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv201<=21))
barplot_data[3, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv205<=23))
barplot_data[4, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv206==1))
barplot_data[5, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv207==1))
barplot_data[6, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv208==1))
barplot_data[7, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv209==1))
barplot_data[8, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv210==1))
barplot_data[9, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv211==1))
barplot_data[10, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv212==1))
barplot_data[11, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv227==1))
barplot_data[12, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv221==1))
barplot_data[13, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv243a==1))
barplot_data[14, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv243b==1))
barplot_data[15, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv243e==1))
barplot_data[16, 4:6] <- count_for_barplot(na.omit(df_h_orphan$hv270<=2))
barplot_data[17, 4:6] <- count_for_barplot(na.omit(df_orphan$hv121[df_orphan$hv105 >= 7 & df_orphan$hv105 <= 12]>=1))
barplot_data[18, 4:6] <- count_for_barplot(na.omit(df_orphan$hv121[df_orphan$hv105 >= 13 & df_orphan$hv105 <= 17]>=1))
barplot_data[19, 4:6] <- count_for_barplot(na.omit(df_orphan$ha57<=3))
barplot_data[20, 4:6] <- count_for_barplot(na.omit(df_orphan$hml32==1))

barplot_data[length(col_name)/2 + 1, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv025==1))
barplot_data[length(col_name)/2 + 2, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv201<=21))
barplot_data[length(col_name)/2 + 3, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv205<=23))
barplot_data[length(col_name)/2 + 4, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv206==1))
barplot_data[length(col_name)/2 + 5, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv207==1))
barplot_data[length(col_name)/2 + 6, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv208==1))
barplot_data[length(col_name)/2 + 7, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv209==1))
barplot_data[length(col_name)/2 + 8, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv210==1))
barplot_data[length(col_name)/2 + 9, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv211==1))
barplot_data[length(col_name)/2 + 10, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv212==1))
barplot_data[length(col_name)/2 + 11, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv227==1))
barplot_data[length(col_name)/2 + 12, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv221==1))
barplot_data[length(col_name)/2 + 13, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv243a==1))
barplot_data[length(col_name)/2 + 14, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv243b==1))
barplot_data[length(col_name)/2 + 15, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv243e==1))
barplot_data[length(col_name)/2 + 16, 4:6] <- count_for_barplot(na.omit(df_h_not_orphan$hv270<=2))
barplot_data[length(col_name)/2 + 17, 4:6] <- count_for_barplot(na.omit(df_not_orphan$hv121[df_not_orphan$hv105 >= 7 & df_not_orphan$hv105 <= 12]>=1))
barplot_data[length(col_name)/2 + 18, 4:6] <- count_for_barplot(na.omit(df_not_orphan$hv121[df_not_orphan$hv105 >= 13 & df_not_orphan$hv105 <= 17]>=1))
barplot_data[length(col_name)/2 + 19, 4:6] <- count_for_barplot(na.omit(df_not_orphan$ha57<=3))
barplot_data[length(col_name)/2 + 20, 4:6] <- count_for_barplot(na.omit(df_not_orphan$hml32==1))


# plot barplot
ggplot(barplot_data, aes(fill=orphan, x=col_label, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


# boxplots
# creat table for boxplots
orphan <- c(rep("orphan", nrow(df_orphan)),
            rep("not orphan", nrow(df_not_orphan)))
boxplot_data <- data.frame(orphan)
boxplot_data$ha3 <- c(df_orphan$ha3, df_not_orphan$ha3)
boxplot_data$ha3[boxplot_data$ha3 > 2200] <- NA
boxplot_data$ha2 <- c(df_orphan$ha2, df_not_orphan$ha2)
boxplot_data$ha2[boxplot_data$ha2 > 2000] <- NA
boxplot_data$ha40 <- c(df_orphan$ha40, df_not_orphan$ha40)
boxplot_data$ha40[boxplot_data$ha40 > 6000] <- NA
boxplot_data$ha53 <- c(df_orphan$ha53, df_not_orphan$ha53)
boxplot_data$ha53[boxplot_data$ha53 > 900] <- NA


# plot boxplots
ggplot(boxplot_data, aes(x=orphan, y=ha3)) +
  geom_violin(aes(fill=orphan), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = "Woman's height in centimeters (1 decimal)") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(boxplot_data, aes(x=orphan, y=ha2)) +
  geom_violin(aes(fill=orphan), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = "Woman's weight in kilograms (1 decimal)") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(boxplot_data, aes(x=orphan, y=ha40)) +
  geom_violin(aes(fill=orphan), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = "Woman's Body Mass Index") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(boxplot_data, aes(x=orphan, y=ha53)) +
  geom_violin(aes(fill=orphan), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = "Woman's Hemoglobin level (g/dl - 1 decimal)") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()












