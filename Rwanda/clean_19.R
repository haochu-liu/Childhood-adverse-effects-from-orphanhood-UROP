library(rdhs)
library(dplyr)
library(labelled)
library(Hmisc)
library(tidyr)


# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs


# get dataframes
# 2019
download_2019 <- get_datasets("RWPR81DT.ZIP")
df_2019 <- readRDS(download_2019$RWPR81DT)
df_2019 <- data.frame(df_2019)


# Year 2019
# choose columns
year <- "2019"
df_2019 <- df_2019[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217",
                       "hv105", "hv104",
                       "ha3", "hc3", "hb3", "hc1",
                       "ha2", "hc2", "hb2",
                       "ha40", "hb40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv243e", "hv106",
                       "hv270", "hv121",
                       "ha53", "hc53", "hb53",
                       "ha57", "hc57", "hb57",
                       "ha61", "ha63", "hml32", "hml18", "ha56", "hv115")]

# under 18
under_18 <- c(sum(na.omit(df_2019$hv105) < 18),
              sum(na.omit(df_2019$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main="Rwanda 2019")
df_2019 <- df_2019[df_2019$hv105 < 18, ]

# orphan
df_2019["Orphanhood"] <- ifelse(df_2019$hv111==0 | df_2019$hv113==0,
                                "orphan", "non-orphan")
lost_parents <- c(sum(df_2019$hv111==0), sum(df_2019$hv113==0))
barplot(lost_parents, names.arg=c("lost mother", "lost father"),
        ylab="Number of individuals", col="blue", main="Rwanda 2019")

# clean columns
df_2019$hv104[df_2019$hv104 > 2] <- NA
df_2019$ha3[df_2019$ha3 > 2200] <- NA
df_2019$hc3[df_2019$hc3 > 2000] <- NA
df_2019$hb3[df_2019$hb3 > 2500] <- NA
df_2019$ha2[df_2019$ha2 > 2000] <- NA
df_2019$hc2[df_2019$hc2 > 9990] <- NA
df_2019$hb2[df_2019$hb2 > 2500] <- NA
df_2019$ha40[df_2019$ha40 > 6000] <- NA
df_2019$hb40[df_2019$hb40 > 6000] <- NA
df_2019$hv025 <- ifelse(df_2019$hv025==1, 1, 0)
df_2019$hv201[df_2019$hv201 > 96] <- NA
df_2019$hv201 <- ifelse(df_2019$hv201<=21, 1, 0)
df_2019$hv205[df_2019$hv205 > 96] <- NA
df_2019$hv205 <- ifelse(df_2019$hv205<=23, 1, 0)
df_2019$hv206[df_2019$hv206 > 1] <- NA
df_2019$hv207[df_2019$hv207 > 1] <- NA
df_2019$hv208[df_2019$hv208 > 1] <- NA
df_2019$hv209[df_2019$hv209 > 1] <- NA
df_2019$hv210[df_2019$hv210 > 1] <- NA
df_2019$hv211[df_2019$hv211 > 1] <- NA
df_2019$hv212[df_2019$hv212 > 1] <- NA
df_2019$hv227[df_2019$hv227 > 1] <- NA
df_2019$hv221[df_2019$hv221 > 1] <- NA
df_2019$hv243a[df_2019$hv243a > 1] <- NA
df_2019$hv243b[df_2019$hv243b > 1] <- NA
df_2019$hv243e[df_2019$hv243e > 1] <- NA
df_2019$hv270 <- ifelse(df_2019$hv270<=2, 1, 0)
df_2019$hv121[df_2019$hv121 == 9] <- NA
df_2019$hv121 <- ifelse(df_2019$hv121>=1, 1, 0)
df_2019$ha53[df_2019$ha53 > 900] <- NA
df_2019$hc53[df_2019$hc53 > 990] <- NA
df_2019$hb53[df_2019$hb53 > 990] <- NA
df_2019$ha57[df_2019$ha57 > 4] <- NA
df_2019$ha57 <- ifelse(df_2019$ha57<=3, 1, 0)
df_2019$hc57[df_2019$hc57 > 4] <- NA
df_2019$hc57 <- ifelse(df_2019$hc57<=3, 1, 0)
df_2019$hb57[df_2019$hb57 > 4] <- NA
df_2019$hb57 <- ifelse(df_2019$hb57<=3, 1, 0)
df_2019$hml32[df_2019$hml32>1] <- NA
df_2019$hv106[df_2019$hv106 >= 8] <- NA
df_2019$hv111[df_2019$hv111 > 1] <- NA
df_2019$hv113[df_2019$hv113 > 1] <- NA
df_2019$ha61[df_2019$ha61 >= 9] <- NA
df_2019$ha61 <- ifelse(df_2019$ha61==1, 1, 0)
df_2019$ha63[df_2019$ha63 >= 4] <- NA
df_2019$ha63 <- ifelse(df_2019$ha63==1, 1, 0)
df_2019$hml32[df_2019$hml32 > 1] <- NA
df_2019$hml18[df_2019$hml18 > 1] <- NA
df_2019$ha56[df_2019$ha56 >= 997] <- NA
df_2019$hv115[df_2019$hv115 == 9] <- NA


# labels
label <- as.list(label(df_2019))
label$ha40 <- "woman's body mass index"
label$hb40 <- "man's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=21: piped/tube, >21: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv270 <- "Poor household wealth" # <=2: poor
label$hv121 <- "School attendance" # 0: no, >=1: attend
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$hb53 <- "man's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label$hb57 <- "Has anemia (man)"
label$hml32 <- "Has malaria" # 0: negative, 1: positive
label$ha61 <- "HIV consent (woman)"
label$ha63 <- "HIV result measurement (woman)"
label$hml18 <- "Pregnancy status"
label$ha56 <- "woman's adjusted hemoglobin level (g/dl - 1 decimal)"
label$hv115 <- "Ever married"
label(df_2019) <- label

# save file
df_2019 <- df_2019 %>% drop_na(c("hv105", "Orphanhood", "hv001", "hv002", "hvidx"))
save(df_2019, file="Rwanda/df_2019.Rda")
