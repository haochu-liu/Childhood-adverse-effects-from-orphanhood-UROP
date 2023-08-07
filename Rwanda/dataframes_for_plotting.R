library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)


# import functions
source("functions_for_plotting.R")


# 2019
year <- "2019"
load("Rwanda/df_2019.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv243e", "hv270")
bar_RW_2019 <- df_barplot(df_2019, col_names, "Orphanhood")
odd_RW_2019 <- df_odd_ratio(df_2019, col_names, "Orphanhood")
# barplot
bar_RW_2019$year <- year
bar_RW_2019$country <- "Rwanda"
bar_RW_2019$column_labels <- tolower(bar_RW_2019$column_labels)
# oddplot
odd_RW_2019$year <- year
odd_RW_2019$country <- "Rwanda"
odd_RW_2019$column_labels <- tolower(odd_RW_2019$column_labels)

save(bar_RW_2019, file="Rwanda/bar_RW_2019.Rda")
save(odd_RW_2019, file="Rwanda/odd_RW_2019.Rda")


# 2014
year <- "2014"
load("Rwanda/df_2014.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
               "hv207", "hv208", "hv209", "hv210",
               "hv211", "hv212", "hv227", "hv221",
               "hv243a", "hv243b", "hv270")
bar_RW_2014 <- df_barplot(df_2014, col_names, "Orphanhood")
odd_RW_2014 <- df_odd_ratio(df_2014, col_names, "Orphanhood")
# barplot
bar_RW_2014$year <- year
bar_RW_2014$country <- "Rwanda"
bar_RW_2014$column_labels <- tolower(bar_RW_2014$column_labels)
# oddplot
odd_RW_2014$year <- year
odd_RW_2014$country <- "Rwanda"
odd_RW_2014$column_labels <- tolower(odd_RW_2014$column_labels)

save(bar_RW_2014, file="Rwanda/bar_RW_2014.Rda")
save(odd_RW_2014, file="Rwanda/odd_RW_2014.Rda")


# 2010
year <- "2010"
load("Rwanda/df_2010.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
               "hv207", "hv208", "hv209", "hv210",
               "hv211", "hv212", "hv227", "hv221",
               "hv243a", "hv243b", "hv270")
bar_RW_2010 <- df_barplot(df_2010, col_names, "Orphanhood")
odd_RW_2010 <- df_odd_ratio(df_2010, col_names, "Orphanhood")
# barplot
bar_RW_2010$year <- year
bar_RW_2010$country <- "Rwanda"
bar_RW_2010$column_labels <- tolower(bar_RW_2010$column_labels)
# oddplot
odd_RW_2010$year <- year
odd_RW_2010$country <- "Rwanda"
odd_RW_2010$column_labels <- tolower(odd_RW_2010$column_labels)

save(bar_RW_2010, file="Rwanda/bar_RW_2010.Rda")
save(odd_RW_2010, file="Rwanda/odd_RW_2010.Rda")


# 2005
year <- "2005"
load("Rwanda/df_2005.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
               "hv207", "hv208", "hv209", "hv210",
               "hv211", "hv212", "hv227", "hv221",
               "hv270")
bar_RW_2005 <- df_barplot(df_2005, col_names, "Orphanhood")
odd_RW_2005 <- df_odd_ratio(df_2005, col_names, "Orphanhood")
# barplot
bar_RW_2005$year <- year
bar_RW_2005$country <- "Rwanda"
bar_RW_2005$column_labels <- tolower(bar_RW_2005$column_labels)
# oddplot
odd_RW_2005$year <- year
odd_RW_2005$country <- "Rwanda"
odd_RW_2005$column_labels <- tolower(odd_RW_2005$column_labels)

save(bar_RW_2005, file="Rwanda/bar_RW_2005.Rda")
save(odd_RW_2005, file="Rwanda/odd_RW_2005.Rda")



# 2019
year <- "2019"
load("Rwanda/df_2019.Rda")
col_names <- c("hv121")
odd_RW_2019 <- df_odd_ratio(df_2019, col_names, "Orphanhood")
# oddplot
odd_RW_2019$year <- year
odd_RW_2019$country <- "Rwanda"
odd_RW_2019$column_labels <- "school attendance"

# 2014
year <- "2014"
load("Rwanda/df_2014.Rda")
odd_RW_2014 <- df_odd_ratio(df_2014, col_names, "Orphanhood")
# oddplot
odd_RW_2014$year <- year
odd_RW_2014$country <- "Rwanda"
odd_RW_2014$column_labels <- "school attendance"

# 2010
year <- "2010"
load("Rwanda/df_2010.Rda")
odd_RW_2010 <- df_odd_ratio(df_2010, col_names, "Orphanhood")
# oddplot
odd_RW_2010$year <- year
odd_RW_2010$country <- "Rwanda"
odd_RW_2010$column_labels <- "school attendance"

# 2005
year <- "2005"
load("Rwanda/df_2005.Rda")
odd_RW_2005 <- df_odd_ratio(df_2005, col_names, "Orphanhood")
# oddplot
odd_RW_2005$year <- year
odd_RW_2005$country <- "Rwanda"
odd_RW_2005$column_labels <- "school attendance"

odd_edu_RW <- rbind(odd_RW_2019, odd_RW_2014, odd_RW_2010, odd_RW_2005)

save(odd_edu_RW, file="Rwanda/odd_edu_RW.Rda")




