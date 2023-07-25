#plotting
#2005

#run the dataframe

col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2005<-df_barplot(chdf2005,col_name,"Orphanhood")



# barplot
bar2005<-ggplot(dfbar2005, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2005)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

# boxplot
dfbox2010<-df_boxplot(chdf2010,"BMI","Orphanhood","hv105")
BMI05<-ggplot(data=dfbox2010,mapping=aes(x=Orphanhood,y=BMI))+
  geom_boxplot(width=0.2, position = position_dodge(0.9))+
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9))+
  ylim(c(5,35))+geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  coord_flip()+
  ggtitle("Senegal 2010(Children under 5)")+theme_bw()
print(BMI05)

#2010
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2010<-df_barplot(chdf2010,col_name,"Orphanhood")


# barplot
bar2010<-ggplot(dfbar2010, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2010)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2012
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2012<-df_barplot(chdf2012,col_name,"Orphanhood")


# barplot
bar2012<-ggplot(dfbar2012, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2012)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2014
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2014<-df_barplot(chdf2014,col_name,"Orphanhood")


# barplot
bar2014<-ggplot(dfbar2014, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2014)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2015
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2015<-df_barplot(chdf2015,col_name,"Orphanhood")


# barplot
bar2015<-ggplot(dfbar2015, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2015)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2016
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2016<-df_barplot(chdf2016,col_name,"Orphanhood")


# barplot
bar2016<-ggplot(dfbar2016, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2016)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2017
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2017<-df_barplot(chdf2017,col_name,"Orphanhood")


# barplot
bar2017<-ggplot(dfbar2017, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2017)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2018
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2018<-df_barplot(chdf2018,col_name,"Orphanhood")


# barplot
bar2018<-ggplot(dfbar2018, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Senegal", 2018)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

#2019
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2019<-df_barplot(chdf2019,col_name,"Orphanhood")


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

