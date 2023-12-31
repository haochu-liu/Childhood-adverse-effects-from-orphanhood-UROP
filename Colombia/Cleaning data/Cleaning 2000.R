source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
library(haven)

# 2000
df2000_new <- df2000[, c("hvidx","hv001","hv002",
                         "hv111","hv113", 
                         "hv217","hv105","hv104","hc1","ha40",
                         "ha3","hc3","ha2","hc2",
                         "hv025","hv026",
                         "hv201","hv205","hv206","hv207","hv208","hv209",
                         "hv210","hv211","hv212","hv221",
                         "hv106","hv107","hv121")]

# orphanhood
df2000_new["Orphanhood"] <- ifelse(df2000$hv111 == 0 | df2000$hv113 == 0, 
                                   "orphan", "non-orphan")

# under 18
under_18_2000 <- c(sum(df2000_new$hv105 < 18,na.rm=TRUE), sum(df2000_new$hv105 >= 18,na.rm=TRUE))
# barplot(under_18_2000, names.arg= c("< 18", ">= 18"), xlab="Age", ylab="Number of individuals",
#        col="blue", main= "Colombia2000")

df2000_new <- df2000_new[df2000_new$hv105 < 18, ]

# clean columns
df2000_new$hv104[df2000_new$hv104 > 2] <- NA

df2000_new$hv025 <- ifelse(df2000_new$hv025 == 1, 1, 0)
df2000_new$hv026 <- ifelse(df2000_new$hv026 == 0 | df2000_new$hv026 == 1, 1, 0)

df2000_new$hv201[df2000_new$hv201 == 99] <- NA
df2000_new$hv201 <- ifelse(df2000_new$hv201 <= 13, 1, 0)

df2000_new$hv205[df2000_new$hv205 == 99] <- NA
df2000_new$hv205 <- ifelse(df2000_new$hv205 <= 21, 1, 0) 

df2000_new$hv206[df2000_new$hv206 == 9] <- NA
df2000_new$hv207[df2000_new$hv207 == 9] <- NA
df2000_new$hv208[df2000_new$hv208 == 9] <- NA
df2000_new$hv209[df2000_new$hv209 == 9] <- NA
df2000_new$hv221[df2000_new$hv221 == 9] <- NA

df2000_new$hv121[df2000_new$hv121 == 9] <- NA
df2000_new$hv121 <- ifelse(df2000_new$hv121 >= 1, 1, 0)

# labels
library(Hmisc)
label <- as.list(tolower(label(df2000_new)))
label$hv025 <- "lives in urban area" # 1: urban, 2: rural
label$hv026 <- "place of residence" # 1: city 2: other
label$hv201 <- "has piped or tube water" # <=13: pipe/tube
label$hv205 <- "has flush or pit toilet" # <=21: flush/pit, >21: not have
label$hv121 <- "school attendance" # 0: no, >=1: attend

label(df2000_new) <- label

save(df2000_new, file = "Colombia/Cleaning data/ df2000_new.Rda")
# load("Colombia/df2000_new.Rda")

# hv001, hv002, hvidx, hv105, Orphanhood
library(tidyr)
df2000_new <- df2000_new %>% drop_na(c("hv001", "hv002", "hvidx", "hv105", "Orphanhood"))
save(df2000_new, file = "Colombia/Cleaning data/ df2000_new.Rda")

