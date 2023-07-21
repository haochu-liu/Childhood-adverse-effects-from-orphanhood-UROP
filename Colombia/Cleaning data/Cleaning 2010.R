source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")

# 2010
df2010_new <- df2010[, c("hvidx","hv001","hv002",
                         "hv111","hv113", 
                         "hv217","hv105","hv104",
                         "ha3","hc3","ha2","hc2","sh105","sh106","ha40",
                         "hv025",
                         "hv201","hv205","hv207","hv208","hv209","hv221",
                         "hv210","hv211","hv212","hv270","hv243a",
                         "hv106","hv107","hv121","hv121"
)]
# orphanhood
df2010_new["Orphanhood"] <- ifelse(df2010$hv111 == 0 | df2010$hv113 == 0, 
                                   "orphan", "non-orphan")

# under 18
under_18_2010 <- c(sum(df2010_new$hv105 < 18,na.rm=TRUE), sum(df2010_new$hv105 >= 18,na.rm=TRUE))
barplot(under_18_2010, names.arg= c("< 18", ">= 18"), xlab="Age", ylab="Number of individuals",
        col="blue", main= "Colombia2010")

df2010_new <- df2010_new[df2010_new$hv105 < 18, ]

# clean columns
df2010_new$ha3[df2010_new$ha3 > 9000] <- NA
df2010_new$hc3[df2010_new$hc3 > 9000] <- NA

df2010_new$ha2[df2010_new$ha2 > 9000] <- NA
df2010_new$hc2[df2010_new$hc2 > 9000] <- NA

df2010_new$ha40[df2010_new$ha40 > 9000] <- NA

df2010_new$hv104[df2010_new$hv104 > 2] <- NA

df2010_new$hv025 <- ifelse(df2010_new$hv025 == 1, 1, 0)

df2010_new$hv201[df2010_new$hv201 == 99] <- NA
df2010_new$hv201 <- ifelse(df2010_new$hv201 <= 71, 1, 0)

df2010_new$hv205[df2010_new$hv205 > 96] <- NA
df2010_new$hv205 <- ifelse(df2010_new$hv205 < 40 | df2010_new$hv205 == 71, 1, 0) 

df2010_new$hv207[df2010_new$hv207 == 9] <- NA
df2010_new$hv208[df2010_new$hv208 == 9] <- NA
df2010_new$hv209[df2010_new$hv209 == 9] <- NA
df2010_new$hv221[df2010_new$hv221 == 9] <- NA

df2010_new$hv243a[df2010_new$hv243a == 9] <- NA

df2010_new$hv210[df2010_new$hv210 == 9] <- NA
df2010_new$hv211[df2010_new$hv211 == 9] <- NA
df2010_new$hv212[df2010_new$hv212 == 9] <- NA
df2010_new$hv270 <- ifelse(df2010_new$hv270 <= 2, 1, 0)

df2010_new$hv121[df2010_new$hv121 == 9] <- NA
df2010_new$hv121[df2010_new$hv105 < 5 | df2010_new$hv105 > 16] <- NA
df2010_new$hv121 <- ifelse(df2010_new$hv121 >= 1, 1, 0)

df2010_new$hv121.1[df2010_new$hv121.1 == 9] <- NA
df2010_new$hv121.1[df2010_new$hv105 == 17] <- NA
df2010_new$hv121.1 <- ifelse(df2010_new$hv121.1 >= 1, 1, 0)

# labels
library(Hmisc)
label <- as.list(label(df2010_new))
label$ha40 <- "woman's body mass index"
label$hv025 <- "lives in urban area" # 1: urban, 2: rural
label$hv201 <- "has piped or tube water" # <=40: pipe/well 71: bottled
label$hv205 <- "has flush or pit toilet" # <=21: flush/pit, >21: not have
label$hv121 <- "school attendance for age 5-16 (compulsory)" # 0: no, >=1: attend
label$hv121.1 <- "school attendance for age 17"
label$hv270 <- "poor household wealth" # <=2: poor

label(df2010_new) <- label
