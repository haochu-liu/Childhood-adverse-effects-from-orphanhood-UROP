library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)


load("Rwanda/bar_RW_2019.Rda")
load("Rwanda/odd_RW_2019.Rda")
load("Rwanda/bar_RW_2014.Rda")
load("Rwanda/odd_RW_2014.Rda")
load("Rwanda/bar_RW_2010.Rda")
load("Rwanda/odd_RW_2010.Rda")
load("Rwanda/bar_RW_2005.Rda")
load("Rwanda/odd_RW_2005.Rda")
load("Rwanda/bar_RW_2000.Rda")
load("Rwanda/odd_RW_2000.Rda")
load("Rwanda/bar_RW_1992.Rda")
load("Rwanda/odd_RW_1992.Rda")


bar_df <- rbind(bar_RW_2019, bar_RW_2014, bar_RW_2010, bar_RW_2005, bar_RW_2000, bar_RW_1992)
odd_df <- rbind(odd_RW_2019, odd_RW_2014, odd_RW_2010, odd_RW_2005, odd_RW_2000, odd_RW_1992)


source("functions_for_plotting.R")


# get odd dataframe
odd_RW <- df_forester_year(odd_df)
save(odd_RW, file="Rwanda/odd_RW.Rda")


# create barplot






















