library(rdhs)
library(dplyr)
library(labelled)
library(Hmisc)
library(tidyr)

source("functions_for_plotting.R")


# 2019
download_2019 <- get_datasets("RWPR81DT.ZIP")
df_2019 <- readRDS(download_2019$RWPR81DT)
df_2019 <- data.frame(df_2019)


# choose columns
year <- "2019"
df_2019 <- df_2019[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104",
                       "ha3", "hc3", "hb3", "hc1",
                       "ha2", "hc2", "hb2",
                       "ha40", "hb40",
                       "hv025", "hv201", "hv205", "hv206", "hv207", "hv208",
                       "hv209", "hv210", "hv211", "hv212", "hv227", "hv221",
                       "hv243a", "hv243b", "hv243e", "hv106",
                       "hv270", "hv121",
                       "ha53", "hc53", "hb53",
                       "ha57", "hc57", "hb57",
                       "hml32")]

# under 18
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

# labels
label <- as.list(label(df_2019))
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
label(df_2019) <- label

# save file
df_2019 <- df_2019 %>% drop_na(c("hv105", "Orphanhood", "hv001", "hv002", "hvidx"))


# choose columns
explain_names <- c("Orphanhood", "hv105", "hv104", "hc1")

bin_names <- c("hv025", "hv206", "hv207", "hv208", "hv209", "hv210", "hv211",
               "hv212", "hv227", "hv221", "hv243a", "hv243b", "hv243e",
               "hv121", "hv201", "hv205", "hc57", "hml32")

con_names <- c("hc3", "hc2", "hc53")

ord_names <- c("hv106", "hv270")

col_names <- c(explain_names, bin_names, con_names, ord_names)

df_2019 <- df_2019[, col_names]


# fit models
data <- df_2019[, c(explain_names, bin_names)]
val_labels(data) <- NULL
bin_table <- df_binary(bin_names, data)

data <- df_2019[, c(explain_names, con_names)]
val_labels(data) <- NULL
con_table <- df_cont(con_names, data)

data <- df_2019[, c(explain_names, ord_names)]
ord_table <- df_ordered(ord_names, data)

regression_table_RW <- rbind(bin_table, con_table, ord_table)
regression_table_RW$Labels <- tolower(regression_table_RW$Labels)
regression_table_RW$fill <- "Rwanda 2019"
save(regression_table_RW, file = "Rwanda/regression_table_RW.Rda")
write.csv(regression_table_RW, file = "Rwanda/regression_table_RW.csv")


coeff_RW <- data.frame(matrix(nrow = 0, ncol = 6))

data <- df_2019[, c(explain_names, bin_names)]
val_labels(data) <- NULL
for (i in 1:length(bin_names)) {
  coeff_rows <- df_bin_coeff(bin_names[i], data)
  coeff_rows$Names <- bin_names[i]
  coeff_rows$Labels <- label(data)[[bin_names[i]]]
  coeff_RW <- rbind(coeff_RW, coeff_rows)
}

data <- df_2019[, c(explain_names, con_names)]
val_labels(data) <- NULL
for (i in 1:length(con_names)) {
  coeff_rows <- df_cont_coeff(con_names[i], data)
  coeff_rows$Names <- con_names[i]
  coeff_rows$Labels <- label(data)[[con_names[i]]]
  coeff_RW <- rbind(coeff_RW, coeff_rows)
}

data <- df_2019[, c(explain_names, ord_names)]
for (i in 1:length(ord_names)) {
  coeff_rows <- df_ordered_coeff(ord_names[i], data)
  coeff_rows$Names <- ord_names[i]
  coeff_rows$Labels <- label(data)[[ord_names[i]]]
  coeff_RW <- rbind(coeff_RW, coeff_rows)
}
coeff_RW$data <- "Rwanda 2019"
save(coeff_RW, file = "Rwanda/coeff_RW.Rda")


coeff_plot_RW <- coeff_RW
coeff_plot_RW <- coeff_plot_RW[seq(1, 92, by=4), ]
coeff_plot_RW$Labels <- tolower(coeff_plot_RW$Labels)
label_order <- c("has mobile telephone", "household wealth index", "has radio",
                 "has mosquito bed net for sleeping", "highest educational level attained",
                 "has car/truck", "has bicycle", "school attendance", "has electricity",
                 "has a computer", "has motorcycle/scooter", "has piped or tube water",
                 "has refrigerator", "has television", "child's height (cm)", "lives in urban area",
                 "child's hemoglobin level (g/dl)", "has flush or pit toilet",
                 "has telephone (land-line)", "child's weight (kg)",
                 "has malaria", "has watch", "has anemia (child)")
coeff_plot_RW <- coeff_plot_RW[match(label_order, coeff_plot_RW$Labels), ]
save(coeff_plot_RW, file = "Rwanda/coeff_plot_RW.Rda")
write.csv(coeff_plot_RW, file = "Rwanda/coeff_plot_RW.csv")

