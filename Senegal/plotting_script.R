#plotting

#further clean the dataframe
chdf2005 <- chdf2005[,colSums(is.na(chdf2005))<nrow(chdf2005)]

#run the dataframe

col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212","hv227","hv221","hv110")
dfbar2005<-df_barplot(chdf2005,col_name,"Orphanhood")