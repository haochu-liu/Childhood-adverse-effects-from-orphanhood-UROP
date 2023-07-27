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


co_names <- as.character(odd_CO_2015$col_names)
rw_names <- as.character(odd_RW_2019$col_names)
sn_names <- as.character(odd_SN_2019$col_names)
in_names <- intersect(co_names, rw_names)
in_names <- intersect(sn_names, in_names)

bar_CO_2015 <- bar_CO_2015[is.element(bar_CO_2015$column_names, in_names), ]
odd_CO_2015 <- odd_CO_2015[is.element(odd_CO_2015$col_names, in_names), ]
bar_RW_2019 <- bar_RW_2019[is.element(bar_RW_2019$column_names, in_names), ]
odd_RW_2019 <- odd_RW_2019[is.element(odd_RW_2019$col_names, in_names), ]
bar_SN_2019 <- bar_SN_2019[is.element(bar_SN_2019$column_names, in_names), ]
odd_SN_2019 <- odd_SN_2019[is.element(odd_SN_2019$col_names, in_names), ]


bar_df <- rbind(bar_CO_2015, bar_RW_2019, bar_SN_2019)
odd_df <- rbind(odd_CO_2015, odd_RW_2019, odd_SN_2019)








