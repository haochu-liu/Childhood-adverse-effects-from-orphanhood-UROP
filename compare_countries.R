library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)
library(forester)


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

#4 categories
vehicle_df<-bar_df[bar_df$column_names %in% c('hv210','hv211','hv212'),]
wealth_df<-bar_df[bar_df$column_names %in% c('hv025','hv270'),]
communication_df<-bar_df[bar_df$column_names %in% c('hv221','hv243a','hv243e'),]
appliance_df<-bar_df[bar_df$column_names %in% c('hv206','hv207','hv208','hv209'),]

ggplot(vehicle_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of Most Recent Year(Vehicle)")

ggplot(wealth_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of Most Recent Year(Wealth)")

ggplot(communication_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of Most Recent Year(Communication)")

ggplot(appliance_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of Most Recent Year(Appliance)")


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
  ggtitle("Odd Ratio in most recent years")



