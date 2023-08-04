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


# 2000
year <- "2000"
load("Rwanda/df_2000.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
               "hv207", "hv208", "hv209", "hv210",
               "hv211", "hv212", "hv227", "hv221")
bar_RW_2000 <- df_barplot(df_2000, col_names, "Orphanhood")
odd_RW_2000 <- df_odd_ratio(df_2000, col_names, "Orphanhood")
# barplot
bar_RW_2000$year <- year
bar_RW_2000$country <- "Rwanda"
bar_RW_2000$column_labels <- tolower(bar_RW_2000$column_labels)
# oddplot
odd_RW_2000$year <- year
odd_RW_2000$country <- "Rwanda"
odd_RW_2000$column_labels <- tolower(odd_RW_2000$column_labels)

save(bar_RW_2000, file="Rwanda/bar_RW_2000.Rda")
save(odd_RW_2000, file="Rwanda/odd_RW_2000.Rda")


# 1992
year <- "1992"
load("Rwanda/df_1992.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
               "hv207", "hv209", "hv210",
               "hv211", "hv212")
bar_RW_1992 <- df_barplot(df_1992, col_names, "Orphanhood")
odd_RW_1992 <- df_odd_ratio(df_1992, col_names, "Orphanhood")
# barplot
bar_RW_1992$year <- year
bar_RW_1992$country <- "Rwanda"
bar_RW_1992$column_labels <- tolower(bar_RW_1992$column_labels)
# oddplot
odd_RW_1992$year <- year
odd_RW_1992$country <- "Rwanda"
odd_RW_1992$column_labels <- tolower(odd_RW_1992$column_labels)

save(bar_RW_1992, file="Rwanda/bar_RW_1992.Rda")
save(odd_RW_1992, file="Rwanda/odd_RW_1992.Rda")



source("Box-cox.R")
# 2019
year <- "2019"
load("Rwanda/df_2019.Rda")
val_labels(df_2019) <- NULL
fielle_child_2019 <- fieller_child_df(df_2019, "Rwanda", year)
fielle_woman_2019 <- fieller_woman_df(df_2019, "Rwanda", year)

# 2014
year <- "2014"
load("Rwanda/df_2014.Rda")
val_labels(df_2014) <- NULL
fielle_child_2014 <- fieller_child_df(df_2014, "Rwanda", year)
fielle_woman_2014 <- fieller_woman_df(df_2014, "Rwanda", year)

# 2010
year <- "2010"
load("Rwanda/df_2010.Rda")
val_labels(df_2010) <- NULL
fielle_child_2010 <- fieller_child_df(df_2010, "Rwanda", year)
fielle_woman_2010 <- fieller_woman_df(df_2010, "Rwanda", year)

# 2005
year <- "2005"
load("Rwanda/df_2005.Rda")
val_labels(df_2005) <- NULL
fielle_child_2005 <- fieller_child_df(df_2005, "Rwanda", year)
fielle_woman_2005 <- fieller_woman_df(df_2005, "Rwanda", year)

# 2000
year <- "2000"
load("Rwanda/df_2000.Rda")
val_labels(df_2000) <- NULL
fielle_child_2000 <- fieller_child_df(df_2000, "Rwanda", year)

fieller_RW <- rbind(fielle_child_2019, fielle_woman_2019,
                    fielle_child_2014, fielle_woman_2014,
                    fielle_child_2010, fielle_woman_2010,
                    fielle_child_2005, fielle_woman_2005,
                    fielle_child_2000)
save(fieller_RW, file="Rwanda/fieller_RW.Rda")



# 2019
year <- "2019"
load("Rwanda/df_2019.Rda")
col_names <- c("hv121")
odd_RW_2019 <- df_odd_ratio(df_2019, col_names, "Orphanhood")
# oddplot
odd_RW_2019$year <- year
odd_RW_2019$country <- "Rwanda"
odd_RW_2019$column_labels <- tolower(odd_RW_2019$column_labels)

# 2014
year <- "2014"
load("Rwanda/df_2014.Rda")
odd_RW_2014 <- df_odd_ratio(df_2014, col_names, "Orphanhood")
# oddplot
odd_RW_2014$year <- year
odd_RW_2014$country <- "Rwanda"
odd_RW_2014$column_labels <- tolower(odd_RW_2014$column_labels)

# 2010
year <- "2010"
load("Rwanda/df_2010.Rda")
odd_RW_2010 <- df_odd_ratio(df_2010, col_names, "Orphanhood")
# oddplot
odd_RW_2010$year <- year
odd_RW_2010$country <- "Rwanda"
odd_RW_2010$column_labels <- tolower(odd_RW_2010$column_labels)

# 2005
year <- "2005"
load("Rwanda/df_2005.Rda")
odd_RW_2005 <- df_odd_ratio(df_2005, col_names, "Orphanhood")
# oddplot
odd_RW_2005$year <- year
odd_RW_2005$country <- "Rwanda"
odd_RW_2005$column_labels <- tolower(odd_RW_2005$column_labels)

odd_edu_RW <- rbind(odd_RW_2019, odd_RW_2014, odd_RW_2010, odd_RW_2005)

save(odd_edu_RW, file="Rwanda/odd_edu_RW.Rda")




