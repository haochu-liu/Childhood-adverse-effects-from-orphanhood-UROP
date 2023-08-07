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


bar_df <- rbind(bar_RW_2019, bar_RW_2014, bar_RW_2010, bar_RW_2005)
odd_df <- rbind(odd_RW_2019, odd_RW_2014, odd_RW_2010, odd_RW_2005)


source("functions_for_plotting.R")


# get odd dataframe
odd_RW <- df_forester_year(odd_df)
save(odd_RW, file="Rwanda/odd_RW.Rda")


# create barplot
bar_df$column_labels[bar_df$column_names=="hv211"] <- "has motorcycle/scooter"
bar_df$column_labels[bar_df$column_names=="hv212"] <- "has car/truck"
bar_df$column_labels[bar_df$column_names=="hv227"] <- "have bednet for sleeping"
bar_df$column_labels[bar_df$column_names=="hv221"] <- "has telephone (land-line)"
bar_df$column_labels <- tolower(bar_df$column_labels)
vehicle_df <- bar_df[bar_df$column_names %in% c('hv210', 'hv211', 'hv212'),]
wealth_df <- bar_df[bar_df$column_names %in% c('hv025', 'hv270'),]
basic_df <- bar_df[bar_df$column_names %in% c('hv201', 'hv205', 'hv206'),]
communication_df <- bar_df[bar_df$column_names %in% c('hv221', 'hv243a', 'hv207'),]
appliance_df <- bar_df[bar_df$column_names %in% c('hv243e', 'hv208', 'hv209', 'hv227', 'hv243b'),]

ggplot(vehicle_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Rwanda (Vehicle)")
ggsave("bar_RW_vehicle.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

ggplot(wealth_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Rwanda (Wealth index and residence)")
ggsave("bar_RW_wealth.png.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

ggplot(basic_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Rwanda (Basic Household Amenities)")
ggsave("bar_RW_basic.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

ggplot(communication_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Rwanda (Communication devices)")
ggsave("bar_RW_communication.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

ggplot(appliance_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Rwanda (Additional household items)")
ggsave("bar_RW_items.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)



















