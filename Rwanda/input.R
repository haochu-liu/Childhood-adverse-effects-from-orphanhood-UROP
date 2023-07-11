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
downloads <- get_datasets("RWHR81DT.ZIP")
downloads$RWHR81DT # household data
df_h <- readRDS(downloads$RWHR81DT)
df_h <- data.frame(df_h)

df <- df_h[, c("hv001", "hv002")]
df$hv111 <- NA
df$hv113 <- NA

for (i in 1:nrow(df)) {
  df_h_111 <- df_h[i,
                   c("hv111_01", "hv111_02", "hv111_03", "hv111_04", "hv111_05",
                     "hv111_06", "hv111_07", "hv111_08", "hv111_09", "hv111_10",
                     "hv111_11", "hv111_12", "hv111_13", "hv111_14", "hv111_15",
                     "hv111_16")]
  df_h_113 <- df_h[i,
                   c("hv113_01", "hv113_02", "hv113_03", "hv113_04", "hv113_05",
                     "hv113_06", "hv113_07", "hv113_08", "hv113_09", "hv113_10",
                     "hv113_11", "hv113_12", "hv113_13", "hv113_14", "hv113_15",
                     "hv113_16")]
  if (1 %in% df_h_111) {df$hv111[i] <- 1}
  if (0 %in% df_h_111) {df$hv111[i] <- 0}
  if (1 %in% df_h_113) {df$hv113[i] <- 1}
  if (0 %in% df_h_113) {df$hv113[i] <- 0}
}


# find and combine factors







