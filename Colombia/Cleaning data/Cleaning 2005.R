source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")

# 2005
df2005_new <- df2005[, c("hvidx","hv001","hv002",
                         "hv111","hv113", 
                         "hv217","hv105","hv104",
                         "ha3","hc3","ha2","hc2","sh105","sh106","ha40",
                         "hv025","hv026",
                         "hv201","hv205","hv206","hv207","hv208","hv209","hv221",
                         "hv211","hv212","shh12i","hv270",
                         "hv106","hv107","hv121","hv121"
)]
# orphanhood
df2005_new["Orphanhood"] <- ifelse(df2005$hv111 == 0 | df2005$hv113 == 0, 
                                   "orphan", "non-orphan")

# under 18
under_18_2005 <- c(sum(df2005_new$hv105 < 18,na.rm=TRUE), sum(df2005_new$hv105 >= 18,na.rm=TRUE))
barplot(under_18_2005, names.arg= c("< 18", ">= 18"), xlab="Age", ylab="Number of individuals",
        col="blue", main= "Colombia2005")

df2005_new <- df2005_new[df2005_new$hv105 < 18, ]

# clean columns
df2005_new$ha3[df2005_new$ha3 > 9000] <- NA
df2005_new$hc3[df2005_new$hc3 > 9000] <- NA

df2005_new$ha2[df2005_new$ha2 > 9000] <- NA
df2005_new$hc2[df2005_new$hc2 > 9000] <- NA

df2005_new$ha40[df2005_new$ha40 > 9000] <- NA

df2005_new$hv104[df2005_new$hv104 > 2] <- NA

df2005_new$hv025 <- ifelse(df2005_new$hv025 == 1, 1, 0)

df2005_new$hv201[df2005_new$hv201 == 99] <- NA
df2005_new$hv201 <- ifelse(df2005_new$hv201 <= 71, 1, 0)

df2005_new$hv205[df2005_new$hv205 > 96] <- NA
df2005_new$hv205 <- ifelse(df2005_new$hv205 < 40 | df2005_new$hv205 == 71, 1, 0) 

df2005_new$hv206[df2005_new$hv206 == 9] <- NA
df2005_new$hv207[df2005_new$hv207 == 9] <- NA
df2005_new$hv208[df2005_new$hv208 == 9] <- NA
df2005_new$hv209[df2005_new$hv209 == 9] <- NA
df2005_new$hv221[df2005_new$hv221 == 9] <- NA

df2005_new$hvshh12i[df2005_new$shh12i == 9] <- NA
df2005_new$hv211[df2005_new$hv211 == 9] <- NA
df2005_new$hv212[df2005_new$hv212 == 9] <- NA
df2005_new$hv270 <- ifelse(df2005_new$hv270 <= 2, 1, 0)

df2005_new$hv121[df2005_new$hv121 == 9] <- NA
df2005_new$hv121[df2005_new$hv105 < 5 | df2005_new$hv105 > 16] <- NA
df2005_new$hv121 <- ifelse(df2005_new$hv121 >= 1, 1, 0)

df2005_new$hv121.1[df2005_new$hv121.1 == 9] <- NA
df2005_new$hv121.1[df2005_new$hv105 == 17] <- NA
df2005_new$hv121.1 <- ifelse(df2005_new$hv121.1 >= 1, 1, 0)

# labels
library(Hmisc)
label <- as.list(label(df2005_new))
label$ha40 <- "woman's body mass index"
label$hv025 <- "lives in urban area" # 1: urban, 2: rural
label$hv201 <- "has piped or tube water" # <=40: pipe/well 71: bottled
label$hv205 <- "has flush or pit toilet" # <=21: flush/pit, >21: not have
label$hv121 <- "school attendance for age 5-16 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "school attendance for age 17"
label$hv270 <- "poor household wealth" # <=2: poor
label$ha2 <- "woman's weight"
label$ha3 <- "woman's height"
label$hc2 <- "child's weight"
label$hc3 <- "child's height"
label$hv105 <- "age"

label(df2005_new) <- label
