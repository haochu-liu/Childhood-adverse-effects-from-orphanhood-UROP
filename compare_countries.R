library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)
library(forester)

source("functions_for_plotting.R")

load("Rwanda/bar_RW_2019.Rda")
load("Rwanda/odd_RW_2019.Rda")
load("Colombia/Cleaning data/bar_CO_2015.Rda")
load("Colombia/Cleaning data/odd_CO_2015.Rda")
load("Senegal/cleaned_data_SN/bar_SN_2019.Rda")
load("Senegal/cleaned_data_SN/odd_SN_2019.Rda")
load("Colombia/odd_edu_CO.Rda")
load("Rwanda/odd_edu_RW.Rda")
load("Senegal/cleaned_data_SN/odd_edu_SN.Rda")

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
bar_df$column_labels[bar_df$column_names=="hv221"] <- "has telephone (land-line)"
bar_df$column_labels[bar_df$column_names=="hv205"] <- "has flush or pit toilet"

#5 categories

amenities_df<-bar_df[bar_df$column_names %in% c('hv201','hv205','hv207'),]
vehicle_df<-bar_df[bar_df$column_names %in% c('hv210','hv211','hv212'),]
wealth_df<-bar_df[bar_df$column_names %in% c('hv025','hv270'),]
communication_df<-bar_df[bar_df$column_names %in% c('hv221','hv243a','hv207'),]
appliance_df<-bar_df[bar_df$column_names %in% c('hv243e','hv227','hv208','hv209','hv243b'),]

ggplot(vehicle_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Most Recent Year(Vehicle)")

ggplot(wealth_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Most Recent Year(Wealth Index and Residence)")

ggplot(communication_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Most Recent Year(Communication Devices)")

ggplot(appliance_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Most Recent Year(Additional Household Items)")

ggplot(amenities_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Household Data in Most Recent Year(Basic Household Amenities)")


# odd plot for three countries
dotCOLS = c("#a6d8f0", "#f9b282", "#adf0a6")
barCOLS = c("#008fd5", "#de6b35", "#4ede35")
ggplot(odd_df, aes(x=column_labels, y=odd_ratio, ymin=CI_lower, ymax=CI_upper,
                   col=country, fill=country)) + 
  geom_linerange(size=1, position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=1, lty=2) +
  geom_point(size=2.5, shape=21, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS) +
  scale_color_manual(values=dotCOLS) +
  scale_x_discrete(name=element_blank()) +
  scale_y_continuous(name="Odds ratio", trans='log10') +
  coord_flip() +
  theme_minimal() +
  annotation_logticks(sides="b") +
  ggtitle("Odds Ratio in most recent years")


forester_df <- df_forester_country(odd_df, 3)

# indent outcome if there is a number in odd_ratio column
forester_df$Outcomes <- ifelse(is.na(forester_df$odd_ratio), 
                         forester_df$Outcomes,
                         paste0("   ", forester_df$Outcomes))

# use forester to create the table with forest plot
forester(left_side_data = forester_df[,"Outcomes", drop=FALSE],
         estimate = forester_df$odd_ratio,
         ci_low = forester_df$CI_lower,
         ci_high = forester_df$CI_upper,
         estimate_precision = 2,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         xlim = c(0.2,2.6),
         estimate_col_name = "Odds Ratio (95% CI)",
         arrows = TRUE,
         arrow_labels = c("Non-orphan Better", "Orphan Better"),
         file_path = here::here("figures/forest_hd.png"))


# comparison for education (hv121)
edu_odd <- rbind(odd_edu_CO, odd_edu_RW, odd_edu_SN)
edu_df <- df_edu_sort(edu_odd)

# indent outcome if there is a number in odd_ratio column
edu_df$Outcomes <- ifelse(is.na(edu_df$odd_ratio), 
                               edu_df$Outcome,
                               paste0("   ", edu_df$Outcome))

# use forester to create the table with forest plot
forester(left_side_data = edu_df[,"Outcomes", drop=FALSE],
         estimate = edu_df$odd_ratio,
         ci_low = edu_df$CI_lower,
         ci_high = edu_df$CI_upper,
         estimate_precision = 2,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         xlim = c(0.3,2.9),
         estimate_col_name = "Odds Ratio (95% CI)",
         arrows = TRUE,
         arrow_labels = c("Non-orphan Better", "Orphan Better"),
         point_sizes = 2.5,
         file_path = here::here("figures/forest_edu.png"))

# coeff plot
load("Rwanda/coeff_hv207_RW.Rda")
load("Colombia/coeff_hv207_CO.Rda")
load("Senegal/coeff_hv207_SN.Rda")
dotCOLS = c("#a6d8f0", "#f9b282", "#adf0a6")
barCOLS = c("#008fd5", "#de6b35", "#4ede35")
coeff_hv207 <- rbind(coeff_hv207_CO, coeff_hv207_RW, coeff_hv207_SN)
ggplot(coeff_hv207, aes(x=Predictors, y=Coeff, ymin=CI_lower, ymax=CI_upper,
                   col=fill, fill=fill)) + 
  geom_linerange(size=1, position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=1, lty=2) +
  geom_point(size=2.5, shape=21, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS) +
  scale_color_manual(values=dotCOLS) +
  scale_x_discrete(name=element_blank()) +
  theme_minimal() +
  ggtitle("Coefficient")
