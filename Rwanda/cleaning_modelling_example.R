library(rdhs)
library(dplyr)
library(labelled)


# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs

# get one survey
datasets <- dhs_datasets(surveyIds = "RW2019DHS")
datasets

# download household datasets
downloads <- get_datasets("RWHR81DT.ZIP")
downloads$RWHR81DT
df1 <- readRDS(downloads$RWHR81DT)
df1 <- data.frame(df1)

# download children datasets
downloads <- get_datasets("RWKR81DT.ZIP")
df2 <- readRDS(downloads$RWKR81DT)
df2 <- data.frame(df2)


# create new dataframe
df <- df2[, c("v001", "v002", "v116")]
df <- df %>% distinct(v001, v002, .keep_all = TRUE)
# unlabelled(df)
df$hv111 <- NA
df$hv113 <- NA
for (i in 1:nrow(df)) {
  cluster_index <- df[i, 1]
  household_index <- df[i, 2]
  df1_111 <- df1[df1$hv001 == cluster_index & df1$hv002 == household_index,
                 c("hv111_01", "hv111_02", "hv111_03", "hv111_04", "hv111_05",
                   "hv111_06", "hv111_07", "hv111_08", "hv111_09", "hv111_10",
                   "hv111_11", "hv111_12", "hv111_13", "hv111_14", "hv111_15",
                   "hv111_16")]
  df1_113 <- df1[df1$hv001 == cluster_index & df1$hv002 == household_index,
                 c("hv113_01", "hv113_02", "hv113_03", "hv113_04", "hv113_05",
                   "hv113_06", "hv113_07", "hv113_08", "hv113_09", "hv113_10",
                   "hv113_11", "hv113_12", "hv113_13", "hv113_14", "hv113_15",
                   "hv113_16")]
  if (1 %in% df1_111) {df$hv111[i] <- 1}
  if (0 %in% df1_111) {df$hv111[i] <- 0}
  if (1 %in% df1_113) {df$hv113[i] <- 1}
  if (0 %in% df1_113) {df$hv113[i] <- 0}
  # have a flush or pit toilet or not
  if (df$v116[i] <= 23) {
    df$v116[i] <- 1 # yes
  } else {
    df$v116[i] <- 0 # no
  }
}
df <- na.omit(df)


# data visualisation
hist(df$v116)
hist(df$hv111)
hist(df$hv113)





