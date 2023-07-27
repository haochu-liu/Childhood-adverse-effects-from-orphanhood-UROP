#plotting
#2005

#run the dataframe
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121",
            "hv270")
dfbar2005<-df_barplot(chdf2005,col_name,"Orphanhood")
dfbar2005$year <- 2005
bar_years <- dfbar2005


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

#odd ratio plot

odd2005<-df_odd_ratio(chdf2005,col_name,"Orphanhood")

ggplot(odd2005, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2005") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())
  

#2010
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2010<-df_barplot(chdf2010,col_name,"Orphanhood")
dfbar2010$year <- 2010
bar_years <- rbind(bar_years,dfbar2010)


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

#odd ratio plot

odd2010<-df_odd_ratio(chdf2010,col_name,"Orphanhood")

ggplot(odd2010, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2010") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2012
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2012<-df_barplot(chdf2012,col_name,"Orphanhood")
dfbar2012$year <- 2012
bar_years <- rbind(bar_years,dfbar2012)

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

#odd ratio plot

odd2012<-df_odd_ratio(chdf2012,col_name,"Orphanhood")

ggplot(odd2012, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2012") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2014
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2014<-df_barplot(chdf2014,col_name,"Orphanhood")
dfbar2014$year <- 2014
bar_years <- rbind(bar_years,dfbar2014)

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

#odd ratio plot

odd2014<-df_odd_ratio(chdf2014,col_name,"Orphanhood")

ggplot(odd2014, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2014") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2015
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2015<-df_barplot(chdf2015,col_name,"Orphanhood")
dfbar2015$year <- 2015
bar_years <- rbind(bar_years,dfbar2015)

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

odd2015<-df_odd_ratio(chdf2015,col_name,"Orphanhood")

ggplot(odd2015, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2015") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2016
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2016<-df_barplot(chdf2016,col_name,"Orphanhood")
dfbar2016$year <- 2016
bar_years <- rbind(bar_years,dfbar2016)

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

odd2016<-df_odd_ratio(chdf2016,col_name,"Orphanhood")

ggplot(odd2016, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2016") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2017
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2017<-df_barplot(chdf2017,col_name,"Orphanhood")
dfbar2017$year <- 2017
bar_years <- rbind(bar_years,dfbar2017)

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

#odds
odd2017<-df_odd_ratio(chdf2017,col_name,"Orphanhood")

ggplot(odd2017, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2017") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2018
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2018<-df_barplot(chdf2018,col_name,"Orphanhood")
dfbar2018$year <- 2018
bar_years <- rbind(bar_years,dfbar2018)

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

#odds
odd2018<-df_odd_ratio(chdf2018,col_name,"Orphanhood")

ggplot(odd2018, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2018") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  scale_x_continuous(trans='log2')+
  theme(axis.title.y=element_blank())

#2019
col_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221",
            "hv121","hml35",
            "hv270")
dfbar2019<-df_barplot(chdf2019,col_name,"Orphanhood")
dfbar2019$year <- 2019
dfbar2019$country <- Senegal
bar_years <- rbind(bar_years,dfbar2019)

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

#odds
odd2019<-df_odd_ratio(chdf2019,col_name,"Orphanhood")

ggplot(odd2019, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Senegal 2019") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))+
  theme(axis.title.y=element_blank())
  +scale_x_continuous(trans='log2')


# side by side barplots
allyear_bar <- ggplot(bar_years, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Senegal") +
  facet_wrap(~year) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

year_list <- c("2005", "2010", "2012", "2014", "2015", "2016","2017","2018","2019")
df_list <- list("2005" = chdf2005,
                "2010" = chdf2010,
                "2012" = chdf2012,
                "2014" = chdf2014,
                "2015" = chdf2015,
                "2016" = chdf2016,
                "2017" = chdf2017,
                "2018" = chdf2018,
                "2019" = chdf2019)
col_name_list <- c("hv025", "hv206",
                   "hv207", "hv208", "hv209", "hv210",
                   "hv211", "hv212", "hv227", "hv221",
                    "hv270",
                   "hv121",
                   "hml35", "ha3", "hc3","ha2", "hc2","ha1","hc1")
col_label_list <- c(
  "Lives in urban area",
  "Has electricity",
  "Has radio",
  "Has television",
  "Has refrigerator",
  "Has bicycle",
  "Has motorcycle/scooter",
  "Has car/truck",
  "Has mosquito bed net for sleeping",
  "Has telephone",
  "Poor household wealth",
  "School attendance",
  "Has malaria",
  "Woman's height in centimeters (1 decimal)",
  "Child's height in centimeters (1 decimal)",
  "Woman's weight in kilograms (1 decimal)",
  "Child's weight in kilograms (1 decimal)",
  "Woman's age in years ",
  "Child's age in months"
)
heatmap_df <- df_isna(df_list, col_name_list, col_label_list, year_list)

ggplot(heatmap_df, aes(label, year, fill=na_percentage)) + 
  geom_tile(aes(fill=na_percentage), colour="white") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlOrBr"),
                       na.value="grey") +
  geom_point(data=heatmap_df, aes(size="Questions missing"), shape=NA, colour="grey") +
  guides(size=guide_legend("Not applicable",
                           override.aes=list(shape=15, size=7)),
         fill=guide_legend(title="Proportion of\nchildren with\nmissing outcomes")) +
  ggtitle("Senegal") +
  theme(axis.title.y=element_blank())+
  scale_x_discrete(limits=col_label_list) +
  coord_flip()


