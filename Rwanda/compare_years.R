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
bar_df$column_labels[bar_df$column_names=="hv211"] <- "has motorcycle/scooter"
bar_df$column_labels[bar_df$column_names=="hv212"] <- "has car/truck"
bar_df$column_labels[bar_df$column_names=="hv227"] <- "have bednet for sleeping"
bar_df$column_labels[bar_df$column_names=="hv221"] <- "has telephone"
bar_df$column_labels <- tolower(bar_df$column_labels)
vehicle_df <- bar_df[bar_df$column_names %in% c('hv210', 'hv211', 'hv212'),]
basic_df <- bar_df[bar_df$column_names %in% c('hv025', 'hv270', 'hv201', 'hv205', 'hv206'),]
communication_df <- bar_df[bar_df$column_names %in% c('hv221', 'hv243a', 'hv243e'),]
appliance_df <- bar_df[bar_df$column_names %in% c('hv207', 'hv208', 'hv209', 'hv227', 'hv243b'),]

ggplot(vehicle_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household data in Rwanda (Vehicle)")

ggplot(basic_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household data in Rwanda (Basic level)")

ggplot(communication_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household data in Rwanda (Communication)")

ggplot(appliance_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household data in Rwanda (Appliance)")



















