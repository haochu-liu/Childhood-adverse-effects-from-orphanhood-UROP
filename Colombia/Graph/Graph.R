source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2015.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2010.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2005.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2000.R")

library(ggplot2)

# barplot
bar_CO <- rbind(bar_CO_2015, bar_CO_2010, bar_CO_2005, bar_CO_2000)
save(bar_CO, file = "Colombia/bar_CO.Rda")


# library(ggpubr)
# ggarrange(g1, g2, g3, g4, 
          #labels = c("2015", "2010", "2005", "2000"),
          #ncol = 2, nrow = 2)

allyear_bar<-ggplot(bar_CO, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Senegal") +
  facet_wrap(~year) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


# heat map for NA
df2000_edu <- df2000_new
df2005_edu <- df2005_new
df2010_edu <- df2010_new
df2015_edu <- df2015_new

df2000_edu$hv121[!is.na(df2000_new$hv121.1)] = df2000_new$hv121.1[!is.na(df2000_new$hv121.1)]
df2005_edu$hv121[!is.na(df2005_new$hv121.1)] = df2005_new$hv121.1[!is.na(df2005_new$hv121.1)]
df2010_edu$hv121[!is.na(df2010_new$hv121.1)] = df2010_new$hv121.1[!is.na(df2010_new$hv121.1)]
df2015_edu$hv121[!is.na(df2015_new$hv121.1)] = df2015_new$hv121.1[!is.na(df2015_new$hv121.1)]


# odd ratio

odd_CO <- rbind(odd_CO_2015, odd_CO_2010, odd_CO_2005, odd_CO_2000)
odd_CO <- odd_CO[odd_CO$col_names %in% c("hv025", "hv201", "hv205", "hv206", "hv207", "hv208", "hv209", "hv210", "hv211", 
                                       "hv212", "hv227", "hv221", "hv243a", "hv243b", "hv243e", "hv270"),]

save(odd_CO, file = "Colombia/odd_CO.Rda")


## plotting heatmap
year <- c("2000", "2005", "2010", "2015")
data <- list("2000" = df2000_edu,
             "2005" = df2005_edu,
             "2010" = df2010_edu,
             "2015" = df2015_edu)
col_name <- c("hv025", "hv201", "hv205", "hv206",
              "hv207", "hv208", "hv209", "hv210",
              "hv211", "hv212", "hv221",
              "hv243a", "hv243e", "hv270",
              "hv121","hv106",
              "ha2", "ha3", "ha40", "hc2", "hc3", "hc1")

col_label <- c(
  "Lives in urban area",
  "Has piped or tube water",
  "Has flush or pit toilet",
  "Has electricity",
  "Has radio",
  "Has television",
  "Has refrigerator",
  "Has bicycle",
  "Has motorcycle/scooter",
  "Has car/truck",
  "Has telephone (land-line)",
  "Has mobile telephone",
  "Has a computer",
  "Poor household wealth",
  "School attendance",
  "Highest education level attained",
  "Woman's weight in kilograms",
  "Woman's height in centimeters",
  "Woman's body mass index",
  "Child's weight in kilograms",
  "Child's height in centimeters",
  "Child's age in months")

df_heatmap <- df_isna(data, col_name, col_label, year)

ggplot(df_heatmap, aes(label, year, fill=na_percentage)) + 
  geom_tile(aes(fill=na_percentage), colour="white") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlOrBr"),
                       na.value="grey") +
  geom_point(data=df_heatmap, aes(size="Question missing"), shape=NA, colour="grey") +
  guides(size=guide_legend("Not applicable",
                           override.aes=list(shape=15, size=7)),
         fill=guide_legend(title="Proportion of\nchildren with\nmissing outcomes")) +
  theme(axis.title.y=element_blank()) +
  scale_x_discrete(limits=col_label) +
  ggtitle("Colombia") +
  coord_flip()

ggsave("heatmap_CO.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

# compare years -- bar

vehicle_df_CO <- bar_CO[bar_CO$column_names %in% c('hv210','hv211','hv212'),]
wealth_df_CO <- bar_CO[bar_CO$column_names %in% c('hv270','hv025'),]
communication_df_CO <- bar_CO[bar_CO$column_names %in% c('hv207','hv221','hv243a'),]
appliance_df_CO <- bar_CO[bar_CO$column_names %in% c('hv243e','hv208','hv209'),]
basic_df_CO <- bar_CO[bar_CO$column_names %in% c('hv201','hv205','hv206'),]

ggplot(vehicle_df_CO, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data of in Colombia (Vehicle)")

ggsave("bar_CO_vehicle.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

ggplot(wealth_df_CO, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data of in Colombia (Wealth Index and Residence)")

ggsave("bar_CO_wealth.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

communication_df_CO$column_labels[communication_df_CO$column_names=="hv221"] <- "has telephone (land-line)"
communication_df_CO$column_labels[communication_df_CO$column_names=="hv243a"] <- "has mobile telephone"
ggplot(communication_df_CO, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data of in Colombia (Communication Devices)")

ggsave("bar_CO_communication.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

ggplot(appliance_df_CO, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data of in Colombia (Additional Household Items)")

ggsave("bar_CO_items.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

ggplot(basic_df_CO, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data of in Colombia (Basic Household Amenities)")

ggsave("bar_CO_items.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

# fieller
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Box-cox.R")
library(forester)

fieller_CO1 <- fieller_child_df(df2005_new, "Colombia", "2005")
fieller_CO2 <- fieller_child_df(df2010_new, "Colombia", "2010")
fieller_CO3 <- fieller_woman_df(df2005_new, "Colombia", "2005")
fieller_CO4 <- fieller_woman_df(df2010_new, "Colombia", "2010")
fieller_CO <- rbind(fieller_CO1, fieller_CO2, fieller_CO3, fieller_CO4)

save(fieller_CO, file = "Colombia/fieller_CO.Rda")


# indent outcome if there is a number in ratio column
fieller_CO <- df_yearsort(fieller_CO, 2)
fieller_CO$Outcome <- ifelse(is.na(fieller_CO$ratio), 
                         fieller_CO$Outcome,
                         paste0("   ", fieller_CO$Outcome))

# use forester to create the table with forest plot
forester(left_side_data = fieller_CO[,c("Outcome","p_value","parameter"), drop=FALSE],
         estimate = fieller_CO$ratio,
         ci_low = fieller_CO$CI_lower,
         ci_high = fieller_CO$CI_upper,
         estimate_precision = 4,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         estimate_col_name = "Ratio with 95% CI",
         file_path = here::here("Colombia/fieller_plot_CO.png"), 
         render_as = "png")


# education odd ratio plot
df2000_edu <- df2000_new
df2005_edu <- df2005_new
df2010_edu <- df2010_new
df2015_edu <- df2015_new

df2000_edu$hv121[!is.na(df2000_new$hv121.1)] = df2000_new$hv121.1[!is.na(df2000_new$hv121.1)]
df2005_edu$hv121[!is.na(df2005_new$hv121.1)] = df2005_new$hv121.1[!is.na(df2005_new$hv121.1)]
df2010_edu$hv121[!is.na(df2010_new$hv121.1)] = df2010_new$hv121.1[!is.na(df2010_new$hv121.1)]
df2015_edu$hv121[!is.na(df2015_new$hv121.1)] = df2015_new$hv121.1[!is.na(df2015_new$hv121.1)]

col_edu <- c("hv121")
odd_edu_CO1 <- df_odd_ratio(df2000_edu, col_edu, "Orphanhood")
odd_edu_CO1$year <- "2000"

odd_edu_CO2 <- df_odd_ratio(df2005_edu, col_edu, "Orphanhood")
odd_edu_CO2$year <- "2005"

odd_edu_CO3 <- df_odd_ratio(df2010_edu, col_edu, "Orphanhood")
odd_edu_CO3$year <- "2010"

odd_edu_CO4 <- df_odd_ratio(df2015_edu, col_edu, "Orphanhood")
odd_edu_CO4$year <- "2015"

odd_edu_CO <- rbind(odd_edu_CO1, odd_edu_CO2, odd_edu_CO3, odd_edu_CO4)
odd_edu_CO$country <- "Colombia"

save(odd_CO_2005, file = "Colombia/odd_edu_CO.Rda")
