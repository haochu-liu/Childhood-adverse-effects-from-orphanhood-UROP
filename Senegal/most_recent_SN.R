#Most recent year data
#2019
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221","hv243a","hv243b","hv243e",
            "hv270")
dfbar2019<-df_barplot(chdf2019,col_name,"Orphanhood")
dfbar2019$year <- "2019"
dfbar2019$country <- "Senegal"
bar_SN_2019<-dfbar2019
bar_SN_2019$column_labels<-tolower(bar_SN_2019$column_labels)
save(bar_SN_2019,file="bar_SN_2019.Rda")

dfodd2019<-df_odd_ratio(chdf2019,col_name,"Orphanhood")
dfodd2019$year <- "2019"
dfodd2019$country <- "Senegal"
odd_SN_2019<-dfodd2019
odd_SN_2019$column_labels<-tolower(odd_SN_2019$column_labels)
save(odd_SN_2019,file="odd_SN_2019.Rda")


bar2019countries<-ggplot(bar_df,aes(x=column_names,y=percentage,fill=orphan))+
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) 
