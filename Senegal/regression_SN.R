library(rdhs)
library(dplyr)
library(labelled)
library(Hmisc)
library(tidyr)
library(MASS)

source("functions_for_plotting.R")


# 2019

df2019 <- data.frame(SN19HM)


# choose columns
year <- "2019"
df2019 <- df2019[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "hc3", "hc1",
                       "hc2", 
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv243e", "hv106",
                       "hv270", "hv121"
                       )]

# under 18
df2019 <- df2019[df2019$hv105 < 18, ]

# orphan
df2019["Orphanhood"] <- ifelse(df2019$hv111==0 | df2019$hv113==0,
                                "orphan", "non-orphan")

# clean columns
df2019$hv104[df2019$hv104 > 2] <- NA
df2019$ha3[df2019$ha3 > 2200] <- NA
df2019$hc3[df2019$hc3 > 2000] <- NA
df2019$hb3[df2019$hb3 > 2500] <- NA
df2019$ha2[df2019$ha2 > 2000] <- NA
df2019$hc2[df2019$hc2 > 9990] <- NA
df2019$hb2[df2019$hb2 > 2500] <- NA
df2019$ha40[df2019$ha40 > 6000] <- NA
df2019$hb40[df2019$hb40 > 6000] <- NA
df2019$hv025 <- ifelse(df2019$hv025==1, 1, 0)
df2019$hv201[df2019$hv201 > 96] <- NA
df2019$hv201 <- ifelse(df2019$hv201<=21, 1, 0)
df2019$hv205[df2019$hv205 > 96] <- NA
df2019$hv205 <- ifelse(df2019$hv205<=23, 1, 0)
df2019$hv206[df2019$hv206 > 1] <- NA
df2019$hv207[df2019$hv207 > 1] <- NA
df2019$hv208[df2019$hv208 > 1] <- NA
df2019$hv209[df2019$hv209 > 1] <- NA
df2019$hv210[df2019$hv210 > 1] <- NA
df2019$hv211[df2019$hv211 > 1] <- NA
df2019$hv212[df2019$hv212 > 1] <- NA
df2019$hv227[df2019$hv227 > 1] <- NA
df2019$hv221[df2019$hv221 > 1] <- NA
df2019$hv243a[df2019$hv243a > 1] <- NA
df2019$hv243b[df2019$hv243b > 1] <- NA
df2019$hv243e[df2019$hv243e > 1] <- NA
df2019$hv121[df2019$hv121 == 9] <- NA
df2019$hv121 <- ifelse(df2019$hv121>=1, 1, 0)
df2019$ha53[df2019$ha53 > 900] <- NA
df2019$hc53[df2019$hc53 > 990] <- NA
df2019$hb53[df2019$hb53 > 990] <- NA
df2019$ha57[df2019$ha57 > 4] <- NA
df2019$ha57 <- ifelse(df2019$ha57<=3, 1, 0)
df2019$hc57[df2019$hc57 > 4] <- NA
df2019$hc57 <- ifelse(df2019$hc57<=3, 1, 0)
df2019$hb57[df2019$hb57 > 4] <- NA
df2019$hb57 <- ifelse(df2019$hb57<=3, 1, 0)
df2019$hml32[df2019$hml32>1] <- NA
df2019$hv106[df2019$hv106 >= 3] <- NA

# labels
label <- as.list(label(df2019))
label$ha40 <- "woman's body mass index"
label$hb40 <- "man's body mass index"
label$hv025 <- "Lives in urban area" # 1: urban, 2: rural
label$hv201 <- "Has piped or tube water" # <=21: piped/tube, >21: not have
label$hv205 <- "Has flush or pit toilet" # <=23: flush/pit, >23: not have
label$hv270 <- "household wealth index" # <=2: poor
label$hv121 <- "School attendance" # 0: no, >=1: attend
label$hc53 <- "child's hemoglobin level (g/dl)"
label$hc3 <- "child's height (cm)"
label$hc2 <- "child's weight (kg)"
label$hc57 <- "Has anemia (child)"
label$hml32 <- "Has malaria" # 0: negative, 1: positive
label$hv227 <- "Has mosquito bednet for sleeping"
label(df2019) <- label

# save file
df2019 <- df2019 %>% drop_na(c("hv105", "Orphanhood", "hv001", "hv002", "hvidx"))


# choose columns
explain_names <- c("Orphanhood", "hv105", "hv104", "hc1")

bin_names <- c("hv025", "hv206", "hv207", "hv208", "hv209", "hv210", "hv211",
               "hv212", "hv227", "hv221", "hv243a", "hv243e",
               "hv121", "hv201", "hv205")

con_names <- c("hc3", "hc2")

ord_names <- c("hv106", "hv270")

col_names <- c(explain_names, bin_names, con_names, ord_names)

df2019 <- df2019[, col_names]


# fit models
data <- df2019[, c(explain_names, bin_names)]
val_labels(data) <- NULL
bin_table <- df_binary(bin_names, data)

data <- df2019[, c(explain_names, con_names)]
val_labels(data) <- NULL
con_table <- df_cont(con_names, data)

data <- df2019[, c(explain_names, ord_names)]
ord_table <- df_ordered(ord_names, data)
bin_table$fill <- "Senegal 2019"
con_table$fill <- "Senegal 2019"

regression_table_SN <- rbind(bin_table, con_table,regression_table_SN)
regression_table_SN$Labels <- tolower(regression_table_SN$Labels)
regression_table_SN$fill <- "Senegal 2019"
save(regression_table_SN, file = "Senegal/regression_table_SN.Rda")
write.csv(regression_table_SN, file = "Senegal/regression_table_SN.csv")


coeff_SN <- data.frame(matrix(nrow = 0, ncol = 6))

data <- df2019[, c(explain_names, bin_names)]
val_labels(data) <- NULL
for (i in 1:length(bin_names)) {
  coeff_rows <- df_bin_coeff(bin_names[i], data)
  coeff_rows$Names <- bin_names[i]
  coeff_rows$Labels <- label(data)[[bin_names[i]]]
  coeff_SN <- rbind(coeff_SN, coeff_rows)
}

data <- df2019[, c(explain_names, con_names)]
val_labels(data) <- NULL
for (i in 1:length(con_names)) {
  coeff_rows <- df_cont_coeff(con_names[i], data)
  coeff_rows$Names <- con_names[i]
  coeff_rows$Labels <- label(data)[[con_names[i]]]
  coeff_SN <- rbind(coeff_SN, coeff_rows)
}

data <- df2019[, c(explain_names, ord_names)]
for (i in 1:length(ord_names)) {
  coeff_rows <- df_ordered_coeff(ord_names[i], data)
  coeff_rows$Names <- ord_names[i]
  coeff_rows$Labels <- label(data)[[ord_names[i]]]
  coeff_SN <- rbind(coeff_SN, coeff_rows)
}
coeff_SN$data <- "SN 2019"
write.csv(coeff_SN,"Senegal/coeff_SN.csv")
save(coeff_SN, file = "Senegal/coeff_SN.Rda")

coeff_SN_allfactors<-coeff_SN
save(coeff_SN_allfactors,file="Senegal/coeff_SN_allfactors.Rda")


coeff_SN<-subset(coeff_SN,coeff_SN$Predictors=="Orphanhood")
coeff_plot_SN<-read.csv("coeff_SN")
