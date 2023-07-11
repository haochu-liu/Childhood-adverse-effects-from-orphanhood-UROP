library(rdhs)


# 1. Quantify orphanhood - looking for survey questions about whether mother/father is alive
# 2. Looking for factors (likely to be an immediate consequence)

# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs

# get 2019 datasets
datasets <- dhs_datasets(surveyIds = "RW2019DHS")
datasets


# quantify orphanhood
downloads <- get_datasets("RWPR81DT.ZIP")
downloads$RWPR81DT # household data
df_p <- readRDS(downloads$RWPR81DT)
df_p <- data.frame(df_p)


# find and combine factors
df <- df_p[, c("hvidx", "hv001", "hv002", # id
               "hv111", "hv113", # parents alive
               "hv014", "hv025", "hv026", "hv201", "hv205", "hv206", "hv208",
               "hv211", "hv212", "hv217", "hv227", "hv236", "hv243e", "hv270",
               "chl0", "hv101", "hv104", "hv105", "hv106", "hv107", "hv109",
               "hv121", "sb332", "ha40", "ha41", "ha50", "ha53", "ha57", "hc1",
               "hc57", "hb50", "hml35", "sb240", "sb121a", "sb121b", "sb121c",
               "sb121d", "sb121e", "sb121f", "sb121g", "sb121h", "sb124",
               "sb129", "mha40", "mha41", "mha50", "mha57", "sb533", "sb421",
               "sb422a", "sb422b", "sb422c", "sb422d", "sb422e", "sb422f",
               "sb422g", "sb422h", "sb425", "sb430")]


# cleaning
df <- df[, colSums(is.na(df)) < nrow(df)]
# under 18
under_18 <- c(sum(df$hv105 < 18), sum(df$hv105 >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main="Adults in Rwanda")
df <- df[df$hv105 < 18, ]


# count missing data
colname_list <- colnames(df)
na_num <- list()
for (i in 1:ncol(df)) {
  na_num[i] <- sum(is.na(df[, i]))
}
names(na_num) <- colname_list
# drop columns with a high proportion of NA
df <- df[, 1:24]
# drop rows with NA in mother/father alive
df <- df[complete.cases(df[, 4:5]), ]


# set age as categorical
df$age_group <- NA
for (i in 1:nrow(df)) {
  if (df$hv105[i] < 5) {df$age_group[i] <- 1} # age 0-4
  if (df$hv105[i] <= 9 & df$hv105[i] >= 5) {df$age_group[i] <- 2} # age 5-9
  if (df$hv105[i] <= 14 & df$hv105[i] >= 10) {df$age_group[i] <- 3} # age 10-14
  if (df$hv105[i] <= 17 & df$hv105[i] >= 15) {df$age_group[i] <- 4} # age 15-17
}












