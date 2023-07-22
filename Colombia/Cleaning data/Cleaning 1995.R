# below 18
# other -> NA

df1995_new <- df1995[, c("hvidx","hv001","hv002",
                          "hv111","hv113", 
                          "hv217",
                          "hv105",
                          "hv104",
                          )]

# orphan
df1995_new["Orphanhood"] <- ifelse(df1995$hv111 == 0 | df1995$hv113 == 0, 
                                   "orphan", "non-orphan")

