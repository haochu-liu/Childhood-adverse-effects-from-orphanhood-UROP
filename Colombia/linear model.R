library(dplyr)
library(labelled)
library(rdhs)
library(Hmisc)
library(tidyr)
library(MASS)

source("~/Desktop/Childhood advserse effects from orphanhood/functions_for_plotting.R")

# re-cleaning most recent year data
set_rdhs_config(email = "zhengxialu@gmail.com",
                project = "Childhood Adverse Effects From Orphanhood",
                config_path = "rdhs.json",
                global = FALSE)
data2015 <- get_datasets(dataset_filenames = "COPR72DT.zip")
df2015 <- readRDS(data2015$"COPR72DT")
df2015 <- data.frame(df2015)

df2015_CO <- df2015[, c("hvidx","hv001","hv002",
                         "hv111","hv113", 
                         "hv217","hv105","hv104","hc1",
                         "hv025",
                         "hv201","hv205","hv206","hv207","hv208","hv209","hv221",
                         "hv210","hv211","hv212","hv243a","hv243e","hv270",
                         "hv106","hv107","hv121",
                         "hc70","hc71","hc72",
                         "ha3","hc3","ha2","hc2","ha40"
)]
# orphanhood
df2015_CO["Orphanhood"] <- ifelse(df2015$hv111 == 0 | df2015$hv113 == 0, 
                                   "orphan", "non-orphan")

df2015_CO <- df2015_CO[df2015_CO$hv105 < 18, ]

# clean columns
df2015_CO$hv104[df2015_CO$hv104 > 2] <- NA

df2015_CO$hv025 <- ifelse(df2015_CO$hv025 == 1, 1, 0)

df2015_CO$hv201[df2015_CO$hv201 == 99] <- NA
df2015_CO$hv201 <- ifelse(df2015_CO$hv201 <= 13, 1, 0)

df2015_CO$hv205[df2015_CO$hv205 == 99] <- NA
df2015_CO$hv205 <- ifelse(df2015_CO$hv205 <= 21, 1, 0) 

df2015_CO$hv206[df2015_CO$hv206 == 9] <- NA
df2015_CO$hv207[df2015_CO$hv207 == 9] <- NA
df2015_CO$hv208[df2015_CO$hv208 == 9] <- NA
df2015_CO$hv209[df2015_CO$hv209 == 9] <- NA
df2015_CO$hv221[df2015_CO$hv221 == 9] <- NA

df2015_CO$hv211[df2015_CO$hv211 == 9] <- NA
df2015_CO$hv212[df2015_CO$hv212 == 9] <- NA

df2015_CO$hv210[df2015_CO$hv210 == 9] <- NA
df2015_CO$hv243a[df2015_CO$hv243a == 9] <- NA
df2015_CO$hv243e[df2015_CO$hv243e == 9] <- NA

df2015_CO$hv121[df2015_CO$hv121 == 9] <- NA
df2015_CO$hv121 <- ifelse(df2015_CO$hv121 >= 1, 1, 0)

# labels
library(Hmisc)
label <- as.list(label(df2015_CO))
label$hv025 <- "lives in urban area" # 1: urban, 2: rural
label$hv201 <- "has piped or tube water" # <=13
label$hv205 <- "has flush or pit toilet" # <=21: flush/pit, >21: not have
label$hv121 <- "school attendance" # 0: no, >=1: attend
label$hv270 <- "household wealth index"

label(df2015_CO) <- label

df2015_CO <- df2015_CO %>% drop_na(c("hv001", "hv002", "hvidx", "hv105", "Orphanhood"))
save(df2015_CO, file = "Colombia/Cleaning data/ df2015_CO.Rda")

# vec_response
bin_vec <- c("hv025", "hv201", "hv205", "hv206", "hv207", "hv208", "hv209", "hv210", "hv211",
            "hv212", "hv221", "hv243a", "hv243e", "hv121")
cont_vec <- c("hc2", "hc3")
ord_vec <- c("hv106", "hv270")

col_2015 <- c(c("Orphanhood", "hv105", "hv104", "hc1"), bin_vec, ord_vec)
col_2010 <- c(c("Orphanhood", "hv105", "hv104", "hc1"), cont_vec)

df_2015 <- df2015_CO[, col_2015]
df_2010 <- df2010_new[, col_2010]
label <- as.list(label(df_2010))
label$hc2 <- "child's weight in kilograms"
label$hc3 <- "child's height in centimeters"
label(df_2010) <- label

val_labels(df_2015) <- NULL
val_labels(df_2010) <- NULL

# linear model
bin_table <- df_binary(bin_vec,df_2015)
bin_table$data <- "Colombia 2015"
cont_table <- df_cont(cont_vec, df_2010)
cont_table$data <- "Colombia 2010"
ord_table <- df_ordered(ord_vec, df2015_CO[,col_2015])
ord_table$data <- "Colombia 2015"

regression_table_CO <- rbind(bin_table, cont_table, ord_table)
save(regression_table_CO, file = "Colombia/regression_table_CO.Rda")
write.csv(regression_table_CO, file = "Colombia/regression_table_CO.csv")

# coeff table
coeff_hv207_CO <- df_bin_coeff("hv207", df_2015)
coeff_hv207_CO$data <- "Colombia 2015"
save(coeff_hv207_CO, file = "Colombia/coeff_hv207_CO.Rda")

# fit models
bin_table <- df_binary(bin_vec, df_2015)

cont_table <- df_cont(cont_vec, df_2010)
cont_table$Labels <- tolower(cont_table$Labels)
cont_table$fill <- "Colombia 2010"

ord_table <- df_ordered(ord_vec, df2015_CO[,col_2015])

regression_table_CO <- rbind(bin_table, ord_table)
regression_table_CO$Labels <- tolower(regression_table_CO$Labels)
regression_table_CO$fill <- "Colombia 2015"

regression_table_CO <- rbind(regression_table_CO, cont_table)
save(regression_table_CO, file = "Colombia/regression_table_CO.Rda")
write.csv(regression_table_CO, file = "Colombia/regression_table_CO.csv")

coeff_CO <- data.frame(matrix(nrow = 0, ncol = 7))

data <- df_2015
for (i in 1:length(bin_vec)) {
  coeff_rows <- df_bin_coeff(bin_vec[i], data)
  coeff_rows$Names <- bin_vec[i]
  coeff_rows$Labels <- label(data)[[bin_vec[i]]]
  coeff_rows$data <- "Colombia 2015"
  coeff_CO <- rbind(coeff_CO, coeff_rows)
}

data <- df_2010
for (i in 1:length(cont_vec)) {
  coeff_rows <- df_cont_coeff(cont_vec[i], data)
  coeff_rows$Names <- cont_vec[i]
  coeff_rows$Labels <- label(data)[[cont_vec[i]]]
  coeff_rows$data <- "Colombia 2010"
  coeff_CO <- rbind(coeff_CO, coeff_rows)
}

data <- df2015_CO[,col_2015]
for (i in 1:length(ord_vec)) {
  coeff_rows <- df_ordered_coeff(ord_vec[i], data)
  coeff_rows$Names <- ord_vec[i]
  coeff_rows$Labels <- label(data)[[ord_vec[i]]]
  coeff_rows$data <- "Colombia 2015"
  coeff_CO <- rbind(coeff_CO, coeff_rows)
}

save(coeff_CO, file = "Colombia/coeff_CO.Rda")