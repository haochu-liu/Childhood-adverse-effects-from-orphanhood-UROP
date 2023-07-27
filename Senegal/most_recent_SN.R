#Most recent year data
#2019
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv270")
dfbar2019<-df_barplot(chdf2019,col_name,"Orphanhood")
dfbar2019$year <- "2019"
dfbar2019$country <- "Senegal"
save(dfbar2019,file="bar_SN_2019.Rda")
