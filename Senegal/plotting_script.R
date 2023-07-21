#plotting


#run the dataframe

col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212","hv227","hv221","hv121")
dfbar2019<-df_barplot(chdf2019,col_name,"Orphanhood")
dfbar2019<-subset(dfbar2019,dfbar2019$percentage!=0)


# barplot
bar2019<-ggplot(dfbar2019, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2019)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

# boxplot
BMI05<-ggplot(data=,mapping=aes(x=orphanhood,y=BMI))+geom_boxplot()+ylim(c(5,35))+geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  coord_flip()+
  ggtitle("Senegal 2010(Children under 5)")+theme_bw()
print(BMI05)