#Senegal Compare among years of data
#barchart
save(bar_years,file="Senegal/bar_years")
bar_SN_years<-bar_years
save(bar_SN_years,file="Senegal/bar_SN_years")


bar_SN_years<-bar_SN_years[bar_SN_years$year %in% c('2016','2017','2018','2019'),]
  
vehicle_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv210','hv211','hv212'),]
wealth_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv025','hv270'),]
communication_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv221','hv243a','hv243e'),]
appliance_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv206','hv207','hv208','hv209'),]

ggplot(vehicle_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of 4 years in Senegal(Vehicle)")

ggplot(wealth_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of 4 years in Senegal(Wealth)")

ggplot(communication_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of 4 years in Senegal(Communication)")

ggplot(appliance_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data of 4 years in Senegal(Appliance)")


#odd ratio plot
