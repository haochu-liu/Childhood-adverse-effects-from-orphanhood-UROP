library(rdhs)
library(dplyr)
library(labelled)
library(Hmisc)


# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs


# get dataframes
# 2019
download_2019 <- get_datasets("RWPR81DT.ZIP")
df_2019 <- readRDS(download_2019$RWPR81DT)
df_2019 <- data.frame(df_2019)
# 2014
download_2014 <- get_datasets("RWPR70DT.ZIP")
df_2014 <- readRDS(download_2014$RWPR70DT)
df_2014 <- data.frame(df_2014)
# 2010
download_2010 <- get_datasets("RWPR61DT.ZIP")
df_2010 <- readRDS(download_2010$RWPR61DT)
df_2010 <- data.frame(df_2010)
# 2005
download_2005 <- get_datasets("RWPR53DT.ZIP")
df_2005 <- readRDS(download_2005$RWPR53DT)
df_2005 <- data.frame(df_2005)
# 2000
download_2000 <- get_datasets("RWPR41DT.ZIP")
df_2000 <- readRDS(download_2000$RWPR41DT)
df_2000 <- data.frame(df_2000)
# 1992
download_1992 <- get_datasets("RWPR21DT.ZIP")
df_1992 <- readRDS(download_1992$RWPR21DT)
df_1992 <- data.frame(df_1992)


# Year 2019
# choose columns
year <- "2019"
df_2019 <- df_2019[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hb3", "hc1",
                       "ha2", "hc2", "hb2",
                       "ha40", "hb40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv243e", "hv252", "hv106",
                       "hv270", "hv121", "hv121",
                       "ha53", "hc53", "hb53",
                       "ha57", "hc57", "hb57",
                       "hml32")]

# under 18
under_18 <- c(sum(na.omit(df_2019$hv105) < 18), sum(na.omit(df_2019$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_2019 <- df_2019[df_2019$hv105 < 18, ]

# orphan
df_2019["Orphanhood"] <- ifelse(df_2019$hv111==0 | df_2019$hv113==0,
                                "orphan", "non-orphan")

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
df_2019$hv121[df_2019$hv105 < 7 | df_2019$hv105 > 12] <- NA
df_2019$hv121 <- ifelse(df_2019$hv121>=1, 1, 0)
df_2019$hv121.1[df_2019$hv121.1 == 9] <- NA
df_2019$hv121.1[df_2019$hv105 < 13 | df_2019$hv105 > 17] <- NA
df_2019$hv121.1 <- ifelse(df_2019$hv121.1>=1, 1, 0)
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
df_2019$hv252[df_2019$hv252 == 9] <- NA
df_2019$hv106[df_2019$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_2019))
label$ha40 <- "woman's body mass index"
label$hb40 <- "man's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=21: piped/tube, >21: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv270 <- "Poor household wealth" # <=2: poor
label$hv121 <- "School attendance for age 7-12 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "School attendance for age 13-17"
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$hb53 <- "man's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label$hb57 <- "Has anemia (man)"
label$hml32 <- "Has malaria" # 0: negative, 1: positive
label(df_2019) <- label

# save file
save(df_2019, file="Rwanda/df_2019.Rda")


# Year 2014
# choose columns
year <- "2014"
df_2014 <- df_2014[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hb3", "hc1",
                       "ha2", "hc2", "hb2",
                       "ha40", "hb40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv252", "hv106",
                       "hv270", "hv121", "hv121",
                       "ha53", "hc53", "hb53",
                       "ha57", "hc57", "hb57",
                       "hml32")]

# under 18
under_18 <- c(sum(na.omit(df_2014$hv105) < 18), sum(na.omit(df_2014$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_2014 <- df_2014[df_2014$hv105 < 18, ]

# orphan
df_2014["Orphanhood"] <- ifelse(df_2014$hv111==0 | df_2014$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df_2014$hv104[df_2014$hv104 > 2] <- NA
df_2014$ha3[df_2014$ha3 > 2200] <- NA
df_2014$hc3[df_2014$hc3 > 2000] <- NA
df_2014$hb3[df_2014$hb3 > 2500] <- NA
df_2014$ha2[df_2014$ha2 > 2000] <- NA
df_2014$hc2[df_2014$hc2 > 9990] <- NA
df_2014$hb2[df_2014$hb2 > 2500] <- NA
df_2014$ha40[df_2014$ha40 > 6000] <- NA
df_2014$hb40[df_2014$hb40 > 6000] <- NA
df_2014$hv025 <- ifelse(df_2014$hv025==1, 1, 0)
df_2014$hv201[df_2014$hv201 > 96] <- NA
df_2014$hv201 <- ifelse(df_2014$hv201<=21, 1, 0)
df_2014$hv205[df_2014$hv205 > 96] <- NA
df_2014$hv205 <- ifelse(df_2014$hv205<=23, 1, 0)
df_2014$hv206[df_2014$hv206 > 1] <- NA
df_2014$hv207[df_2014$hv207 > 1] <- NA
df_2014$hv208[df_2014$hv208 > 1] <- NA
df_2014$hv209[df_2014$hv209 > 1] <- NA
df_2014$hv210[df_2014$hv210 > 1] <- NA
df_2014$hv211[df_2014$hv211 > 1] <- NA
df_2014$hv212[df_2014$hv212 > 1] <- NA
df_2014$hv227[df_2014$hv227 > 1] <- NA
df_2014$hv221[df_2014$hv221 > 1] <- NA
df_2014$hv243a[df_2014$hv243a > 1] <- NA
df_2014$hv243b[df_2014$hv243b > 1] <- NA
df_2014$hv270 <- ifelse(df_2014$hv270<=2, 1, 0)
df_2014$hv121[df_2014$hv121 == 9] <- NA
df_2014$hv121[df_2014$hv105 < 7 | df_2014$hv105 > 12] <- NA
df_2014$hv121 <- ifelse(df_2014$hv121>=1, 1, 0)
df_2014$hv121.1[df_2014$hv121.1 == 9] <- NA
df_2014$hv121.1[df_2014$hv105 < 13 | df_2014$hv105 > 17] <- NA
df_2014$hv121.1 <- ifelse(df_2014$hv121.1>=1, 1, 0)
df_2014$ha53[df_2014$ha53 > 900] <- NA
df_2014$hc53[df_2014$hc53 > 990] <- NA
df_2014$hb53[df_2014$hb53 > 990] <- NA
df_2014$ha57[df_2014$ha57 > 4] <- NA
df_2014$ha57 <- ifelse(df_2014$ha57<=3, 1, 0)
df_2014$hc57[df_2014$hc57 > 4] <- NA
df_2014$hc57 <- ifelse(df_2014$hc57<=3, 1, 0)
df_2014$hb57[df_2014$hb57 > 4] <- NA
df_2014$hb57 <- ifelse(df_2014$hb57<=3, 1, 0)
df_2014$hml32[df_2014$hml32>1] <- NA
df_2014$hv252[df_2014$hv252 == 9] <- NA
df_2014$hv106[df_2014$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_2014))
label$ha40 <- "woman's body mass index"
label$hb40 <- "man's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=21: piped/tube, >21: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv270 <- "Poor household wealth" # <=2: poor
label$hv121 <- "School attendance for age 7-12 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "School attendance for age 13-17"
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$hb53 <- "man's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label$hb57 <- "Has anemia (man)"
label$hml32 <- "Has malaria" # 0: negative, 1: positive
label(df_2014) <- label

# save file
save(df_2014, file="Rwanda/df_2014.Rda")


# Year 2010
# choose columns
year <- "2010"
df_2010 <- df_2010[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hb3", "hc1",
                       "ha2", "hc2", "hb2",
                       "ha40", "hb40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv252", "hv106",
                       "hv270", "hv121", "hv121",
                       "ha53", "hc53", "hb53",
                       "ha57", "hc57", "hb57",
                       "hml32")]

# under 18
under_18 <- c(sum(na.omit(df_2010$hv105) < 18), sum(na.omit(df_2010$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_2010 <- df_2010[df_2010$hv105 < 18, ]

# orphan
df_2010["Orphanhood"] <- ifelse(df_2010$hv111==0 | df_2010$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df_2010$hv104[df_2010$hv104 > 2] <- NA
df_2010$ha3[df_2010$ha3 > 2200] <- NA
df_2010$hc3[df_2010$hc3 > 2000] <- NA
df_2010$hb3[df_2010$hb3 > 2500] <- NA
df_2010$ha2[df_2010$ha2 > 2000] <- NA
df_2010$hc2[df_2010$hc2 > 9990] <- NA
df_2010$hb2[df_2010$hb2 > 2500] <- NA
df_2010$ha40[df_2010$ha40 > 6000] <- NA
df_2010$hb40[df_2010$hb40 > 6000] <- NA
df_2010$hv025 <- ifelse(df_2010$hv025==1, 1, 0)
df_2010$hv201[df_2010$hv201 > 96] <- NA
df_2010$hv201 <- ifelse(df_2010$hv201<=21, 1, 0)
df_2010$hv205[df_2010$hv205 > 96] <- NA
df_2010$hv205 <- ifelse(df_2010$hv205<=23, 1, 0)
df_2010$hv206[df_2010$hv206 > 1] <- NA
df_2010$hv207[df_2010$hv207 > 1] <- NA
df_2010$hv208[df_2010$hv208 > 1] <- NA
df_2010$hv209[df_2010$hv209 > 1] <- NA
df_2010$hv210[df_2010$hv210 > 1] <- NA
df_2010$hv211[df_2010$hv211 > 1] <- NA
df_2010$hv212[df_2010$hv212 > 1] <- NA
df_2010$hv227[df_2010$hv227 > 1] <- NA
df_2010$hv221[df_2010$hv221 > 1] <- NA
df_2010$hv243a[df_2010$hv243a > 1] <- NA
df_2010$hv243b[df_2010$hv243b > 1] <- NA
df_2010$hv270 <- ifelse(df_2010$hv270<=2, 1, 0)
df_2010$hv121[df_2010$hv121 == 9] <- NA
df_2010$hv121[df_2010$hv105 < 7 | df_2010$hv105 > 12] <- NA
df_2010$hv121 <- ifelse(df_2010$hv121>=1, 1, 0)
df_2010$hv121.1[df_2010$hv121.1 == 9] <- NA
df_2010$hv121.1[df_2010$hv105 < 13 | df_2010$hv105 > 17] <- NA
df_2010$hv121.1 <- ifelse(df_2010$hv121.1>=1, 1, 0)
df_2010$ha53[df_2010$ha53 > 900] <- NA
df_2010$hc53[df_2010$hc53 > 990] <- NA
df_2010$hb53[df_2010$hb53 > 990] <- NA
df_2010$ha57[df_2010$ha57 > 4] <- NA
df_2010$ha57 <- ifelse(df_2010$ha57<=3, 1, 0)
df_2010$hc57[df_2010$hc57 > 4] <- NA
df_2010$hc57 <- ifelse(df_2010$hc57<=3, 1, 0)
df_2010$hb57[df_2010$hb57 > 4] <- NA
df_2010$hb57 <- ifelse(df_2010$hb57<=3, 1, 0)
df_2010$hml32[df_2010$hml32>1] <- NA
df_2010$hv252[df_2010$hv252 == 9] <- NA
df_2010$hv106[df_2010$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_2010))
label$ha40 <- "woman's body mass index"
label$hb40 <- "man's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=21: piped/tube, >21: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv270 <- "Poor household wealth" # <=2: poor
label$hv121 <- "School attendance for age 7-12 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "School attendance for age 13-17"
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$hb53 <- "man's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label$hb57 <- "Has anemia (man)"
label$hml32 <- "Has malaria" # 0: negative, 1: positive
label(df_2010) <- label

# save file
save(df_2010, file="Rwanda/df_2010.Rda")


# Year 2005
# choose columns
year <- "2005"
df_2005 <- df_2005[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hc1",
                       "ha2", "hc2",
                       "ha40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221", "hv106",
                       "hv270", "hv121", "hv121",
                       "ha53", "hc53",
                       "ha57", "hc57")]

# under 18
under_18 <- c(sum(na.omit(df_2005$hv105) < 18), sum(na.omit(df_2005$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_2005 <- df_2005[df_2005$hv105 < 18, ]

# orphan
df_2005["Orphanhood"] <- ifelse(df_2005$hv111==0 | df_2005$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df_2005$hv104[df_2005$hv104 > 2] <- NA
df_2005$ha3[df_2005$ha3 > 2200] <- NA
df_2005$hc3[df_2005$hc3 > 2000] <- NA
df_2005$ha2[df_2005$ha2 > 2000] <- NA
df_2005$hc2[df_2005$hc2 > 9990] <- NA
df_2005$ha40[df_2005$ha40 > 9990] <- NA
df_2005$hv025 <- ifelse(df_2005$hv025==1, 1, 0)
df_2005$hv201[df_2005$hv201 > 96] <- NA
df_2005$hv201 <- ifelse(df_2005$hv201<=13, 1, 0)
df_2005$hv205[df_2005$hv205 > 96] <- NA
df_2005$hv205 <- ifelse(df_2005$hv205<=22, 1, 0)
df_2005$hv206[df_2005$hv206 > 1] <- NA
df_2005$hv207[df_2005$hv207 > 1] <- NA
df_2005$hv208[df_2005$hv208 > 1] <- NA
df_2005$hv209[df_2005$hv209 > 1] <- NA
df_2005$hv210[df_2005$hv210 > 1] <- NA
df_2005$hv211[df_2005$hv211 > 1] <- NA
df_2005$hv212[df_2005$hv212 > 1] <- NA
df_2005$hv227[df_2005$hv227 > 1] <- NA
df_2005$hv221[df_2005$hv221 > 1] <- NA
df_2005$hv270 <- ifelse(df_2005$hv270<=2, 1, 0)
df_2005$hv121[df_2005$hv121 == 9] <- NA
df_2005$hv121[df_2005$hv105 < 7 | df_2005$hv105 > 12] <- NA
df_2005$hv121 <- ifelse(df_2005$hv121>=1, 1, 0)
df_2005$hv121.1[df_2005$hv121.1 == 9] <- NA
df_2005$hv121.1[df_2005$hv105 < 13 | df_2005$hv105 > 17] <- NA
df_2005$hv121.1 <- ifelse(df_2005$hv121.1>=1, 1, 0)
df_2005$ha53[df_2005$ha53 > 900] <- NA
df_2005$hc53[df_2005$hc53 > 990] <- NA
df_2005$ha57[df_2005$ha57 > 4] <- NA
df_2005$ha57 <- ifelse(df_2005$ha57<=3, 1, 0)
df_2005$hc57[df_2005$hc57 > 4] <- NA
df_2005$hc57 <- ifelse(df_2005$hc57<=3, 1, 0)
df_2005$hv106[df_2005$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_2005))
label$ha40 <- "woman's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=13: piped/tube, >13: not have
label$hv205 <- "Has flush or pit toilet" # <=22: flush/pit, >22: not have
label$hv270 <- "Poor household wealth" # <=2: poor
label$hv121 <- "School attendance for age 7-12 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "School attendance for age 13-17"
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label(df_2005) <- label

# save file
save(df_2005, file="Rwanda/df_2005.Rda")


# Year 2000
# choose columns
year <- "2000"
df_2000 <- df_2000[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hc1",
                       "ha2", "hc2",
                       "ha40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221", "hv106",
                       "hv121", "hv121",
                       "ha53", "hc53",
                       "ha57", "hc57")]

# under 18
under_18 <- c(sum(na.omit(df_2000$hv105) < 18), sum(na.omit(df_2000$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_2000 <- df_2000[df_2000$hv105 < 18, ]

# orphan
df_2000["Orphanhood"] <- ifelse(df_2000$hv111==0 | df_2000$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df_2000$hv104[df_2000$hv104 > 2] <- NA
df_2000$ha3[df_2000$ha3 > 2200] <- NA
df_2000$hc3[df_2000$hc3 > 2000] <- NA
df_2000$ha2[df_2000$ha2 > 2000] <- NA
df_2000$hc2[df_2000$hc2 > 9990] <- NA
df_2000$ha40[df_2000$ha40 > 9990] <- NA
df_2000$hv025 <- ifelse(df_2000$hv025==1, 1, 0)
df_2000$hv201[df_2000$hv201 > 96] <- NA
df_2000$hv201 <- ifelse(df_2000$hv201<=13, 1, 0)
df_2000$hv205[df_2000$hv205 > 96] <- NA
df_2000$hv205 <- ifelse(df_2000$hv205<=22, 1, 0)
df_2000$hv206[df_2000$hv206 > 1] <- NA
df_2000$hv207[df_2000$hv207 > 1] <- NA
df_2000$hv208[df_2000$hv208 > 1] <- NA
df_2000$hv209[df_2000$hv209 > 1] <- NA
df_2000$hv210[df_2000$hv210 > 1] <- NA
df_2000$hv211[df_2000$hv211 > 1] <- NA
df_2000$hv212[df_2000$hv212 > 1] <- NA
df_2000$hv227[df_2000$hv227 > 1] <- NA
df_2000$hv221[df_2000$hv221 > 1] <- NA
df_2000$hv121[df_2000$hv121 == 9] <- NA
df_2000$hv121[df_2000$hv105 < 7 | df_2000$hv105 > 12] <- NA
df_2000$hv121 <- ifelse(df_2000$hv121>=1, 1, 0)
df_2000$hv121.1[df_2000$hv121.1 == 9] <- NA
df_2000$hv121.1[df_2000$hv105 < 13 | df_2000$hv105 > 17] <- NA
df_2000$hv121.1 <- ifelse(df_2000$hv121.1>=1, 1, 0)
df_2000$ha53[df_2000$ha53 > 900] <- NA
df_2000$hc53[df_2000$hc53 > 990] <- NA
df_2000$ha57[df_2000$ha57 > 4] <- NA
df_2000$ha57 <- ifelse(df_2000$ha57<=3, 1, 0)
df_2000$hc57[df_2000$hc57 > 4] <- NA
df_2000$hc57 <- ifelse(df_2000$hc57<=3, 1, 0)
df_2000$hv106[df_2000$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_2000))
label$ha40 <- "woman's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=13: piped/tube, >13: not have
label$hv205 <- "Has flush or pit toilet" # <=22: flush/pit, >22: not have
label$hv121 <- "School attendance for age 7-12 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "School attendance for age 13-17"
label$ha53 <- "woman's hemoglobin level (g/dl - 1 decimal)"
label$hc53 <- "child's hemoglobin level (g/dl - 1 decimal)"
label$ha57 <- "Has anemia (woman)" # <=3: yes 4: no
label$hc57 <- "Has anemia (child)"
label(df_2000) <- label

# save file
save(df_2000, file="Rwanda/df_2000.Rda")


# Year 1992
# choose columns
year <- "1992"
df_1992 <- df_1992[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv106",
                       "hv110", "hv110")]

# under 18
under_18 <- c(sum(na.omit(df_1992$hv105) < 18), sum(na.omit(df_1992$hv105) >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main=paste("Rwanda", year))
df_1992 <- df_1992[df_1992$hv105 < 18, ]

# orphan
df_1992["Orphanhood"] <- ifelse(df_1992$hv111==0 | df_1992$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df_1992$hv104[df_1992$hv104 > 2] <- NA
df_1992$hv025 <- ifelse(df_1992$hv025==1, 1, 0)
df_1992$hv201[df_1992$hv201 > 71] <- NA
df_1992$hv201 <- ifelse(df_1992$hv201<=13, 1, 0)
df_1992$hv205[df_1992$hv205 > 41] <- NA
df_1992$hv205 <- ifelse(df_1992$hv205<=23, 1, 0)
df_1992$hv206[df_1992$hv206 > 1] <- NA
df_1992$hv207[df_1992$hv207 > 1] <- NA
df_1992$hv208[df_1992$hv208 > 1] <- NA
df_1992$hv209[df_1992$hv209 > 1] <- NA
df_1992$hv210[df_1992$hv210 > 1] <- NA
df_1992$hv211[df_1992$hv211 > 1] <- NA
df_1992$hv212[df_1992$hv212 > 1] <- NA
df_1992$hv110[df_1992$hv110 == 9] <- NA
df_1992$hv110[df_1992$hv105 < 7 | df_1992$hv105 > 12] <- NA
df_1992$hv110.1[df_1992$hv110.1 == 9] <- NA
df_1992$hv110.1[df_1992$hv105 < 13 | df_1992$hv105 > 17] <- NA
df_1992$hv106[df_1992$hv106 >= 8] <- NA

# labels
label <- as.list(label(df_1992))
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=13: piped/tube, >13: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv110 <- "School attendance for age 7-12 (compulsory)" # 0: no, 1: attend
label$hv110.1 <- "School attendance for age 13-17"
label(df_1992) <- label

# save file
save(df_1992, file="Rwanda/df_1992.Rda")


