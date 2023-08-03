source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
library(haven)

# 2015
df2015_new <- df2015[, c("hvidx","hv001","hv002",
                         "hv111","hv113", 
                         "hv217","hv105","hv104","hc1",
                         "hv025",
                         "hv201","hv205","hv206","hv207","hv208","hv209","hv221",
                         "hv210","hv211","hv212","hv243a","hv243e","hv270",
                         "hv106","hv107","hv121","hv121",
                         "hc70","hc71","hc72",
                         "ha3","hc3","ha2","hc2","ha40"
)]
# orphanhood
df2015_new["Orphanhood"] <- ifelse(df2015$hv111 == 0 | df2015$hv113 == 0, 
                                   "orphan", "non-orphan")

# under 18
under_18_2015 <- c(sum(df2015_new$hv105 < 18,na.rm=TRUE), sum(df2015_new$hv105 >= 18,na.rm=TRUE))
# barplot(under_18_2015, names.arg= c("< 18", ">= 18"), xlab="Age", ylab="Number of individuals",
#         col="blue", main= "Colombia2015")

df2015_new <- df2015_new[df2015_new$hv105 < 18, ]

# clean columns
df2015_new$hv104[df2015_new$hv104 > 2] <- NA

df2015_new$hv025 <- ifelse(df2015_new$hv025 == 1, 1, 0)

df2015_new$hv201[df2015_new$hv201 == 99] <- NA
df2015_new$hv201 <- ifelse(df2015_new$hv201 <= 13, 1, 0)

df2015_new$hv205[df2015_new$hv205 == 99] <- NA
df2015_new$hv205 <- ifelse(df2015_new$hv205 <= 21, 1, 0) 

df2015_new$hv206[df2015_new$hv206 == 9] <- NA
df2015_new$hv207[df2015_new$hv207 == 9] <- NA
df2015_new$hv208[df2015_new$hv208 == 9] <- NA
df2015_new$hv209[df2015_new$hv209 == 9] <- NA
df2015_new$hv221[df2015_new$hv221 == 9] <- NA

df2015_new$hv211[df2015_new$hv211 == 9] <- NA
df2015_new$hv212[df2015_new$hv212 == 9] <- NA
df2015_new$hv270 <- ifelse(df2015_new$hv270 <= 2, 1, 0)

df2015_new$hv210[df2015_new$hv210 == 9] <- NA
df2015_new$hv243a[df2015_new$hv243a == 9] <- NA
df2015_new$hv243e[df2015_new$hv243e == 9] <- NA

df2015_new$hv121[df2015_new$hv121 == 9] <- NA
df2015_new$hv121[df2015_new$hv105 < 5 | df2015_new$hv105 > 16] <- NA
df2015_new$hv121 <- ifelse(df2015_new$hv121 >= 1, 1, 0)

df2015_new$hv121.1[df2015_new$hv121.1 == 9] <- NA
df2015_new$hv121.1[df2015_new$hv105 == 17] <- NA
df2015_new$hv121.1 <- ifelse(df2015_new$hv121.1 >= 1, 1, 0)

# labels
library(Hmisc)
label <- as.list(label(df2015_new))
label$hv025 <- "lives in urban area" # 1: urban, 2: rural
label$hv201 <- "has piped or tube water" # <=13
label$hv205 <- "has flush or pit toilet" # <=21: flush/pit, >21: not have
label$hv121 <- "school attendance for age 5-16 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "school attendance for age 17"
label$hv270 <- "poor household wealth" # <=2: poor

label(df2015_new) <- label

save(df2000_new, file = "Colombia/Cleaning data/ df2015_new.Rda")
