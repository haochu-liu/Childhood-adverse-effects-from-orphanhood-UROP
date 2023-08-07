#Senegal Compare among years of data
#barchart
save(bar_years,file="Senegal/bar_years.Rda")
bar_SN_years<-bar_years
save(bar_SN_years,file="Senegal/bar_SN_years.Rda")


bar_SN_years<-bar_SN_years[bar_SN_years$year %in% c('2016','2017','2018','2019'),]

amenities_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv201','hv205','hv207'),]
vehicle_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv210','hv211','hv212'),]
wealth_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv025','hv270'),]
communication_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv221','hv243a','hv207'),]
appliance_df_SN<-bar_SN_years[bar_SN_years$column_names %in% c('hv243e','hv227','hv208','hv209'),]

ggplot(vehicle_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data in Senegal(Vehicle)")

ggplot(wealth_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data in Senegal(Wealth Index and Residence)")

ggplot(communication_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data in Senegal(Communication Devices)")

ggplot(appliance_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data in Senegal(Additional Household Items)")

ggplot(amenities_df_SN, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("Orphanhood Data in Senegal(Basic Household Amenities)")

#odd ratio plot
odd2016$year<-"2016"
odd2017$year<-"2017"
odd2018$year<-"2018"
odd2019$year<-"2019"
odd_SN<-rbind(odd2016,odd2017,odd2018,odd2019)
odd_SN$country<-"Senegal"

odd_SN$column_labels[odd_SN$col_names=="hv221"] <- "has telephone (land-line)"
odd_SN$column_labels[odd_SN$col_names=="hv205"] <- "has flush or pit toilet"

odd_SN<-odd_SN[is.na(odd_SN$odd_ratio)==FALSE,]

odd_SN$column_labels<-tolower(odd_SN$column_labels)

odd_SN<-odd_SN[odd_SN$col_names!="hv121",]
odd_SN<-odd_SN[odd_SN$col_names!="hml35",]
save(odd_SN,file="Senegal/cleaned_data_SN/odd_SN.Rda")



bar_SN_2019<-dfbar2019
odd_SN_2019<-dfodd2019

