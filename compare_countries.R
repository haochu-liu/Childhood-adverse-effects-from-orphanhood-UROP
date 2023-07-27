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
load("Colombia/bar_CO_2015.Rda")
load("Colombia/odd_CO_2015.Rda")
load("Senegal/bar_SN_2019.Rda")
load("Senegal/odd_SN_2019.Rda")


bar_df <- rbind(bar_CO_2015, bar_RW_2019, bar_SN_2019)
odd_df <- rbind(odd_CO_2015, odd_RW_2019, odd_SN_2019)


