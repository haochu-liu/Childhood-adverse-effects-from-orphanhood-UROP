library(MASS)
library(labelled)
library(mratios)
library(ggplot2)


source("functions_for_plotting.R")


load("Rwanda/df_2019.Rda")
val_labels(df_2019) <- NULL

load("Rwanda/df_2014.Rda")
val_labels(df_2014) <- NULL

load("Rwanda/df_2010.Rda")
val_labels(df_2010) <- NULL

load("Rwanda/df_2005.Rda")
val_labels(df_2005) <- NULL

load("Rwanda/df_2000.Rda")
val_labels(df_2000) <- NULL

# weight (woman): lambda = 0
# height (woman): lambda = 2

# weight (age 0-2): lambda = 1
# weight (age 2-5): lambda = 0
# height (age 0-2): lambda = 2
# height (age 2-5): : lambda = 1

lambda_w = 1
lambda_h = 1

lambda_w1 = 1
lambda_w2 = 0
lambda_h1 = 2
lambda_h2 = 1

df_woman <- rbind(df_woman_lfix(df_2019, "Rwanda", "2019", lambda_h, lambda_w),
                  df_woman_lfix(df_2014, "Rwanda", "2014", lambda_h, lambda_w),
                  df_woman_lfix(df_2010, "Rwanda", "2010", lambda_h, lambda_w),
                  df_woman_lfix(df_2005, "Rwanda", "2005", lambda_h, lambda_w))

df_child <- rbind(df_child_lfix(df_2019, "Rwanda", "2019", lambda_h1, lambda_h2, lambda_w1, lambda_w2),
                  df_child_lfix(df_2014, "Rwanda", "2014", lambda_h1, lambda_h2, lambda_w1, lambda_w2),
                  df_child_lfix(df_2010, "Rwanda", "2010", lambda_h1, lambda_h2, lambda_w1, lambda_w2),
                  df_child_lfix(df_2005, "Rwanda", "2005", lambda_h1, lambda_h2, lambda_w1, lambda_w2),
                  df_child_lfix(df_2000, "Rwanda", "2000", lambda_h1, lambda_h2, lambda_w1, lambda_w2))

fieller_fix_RW <- rbind(df_woman, df_child)

save(fieller_fix_RW, file="Rwanda/fieller_fix_RW.Rda")















