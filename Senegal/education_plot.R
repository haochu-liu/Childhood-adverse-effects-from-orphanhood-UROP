library(haven)
library(stringr)
library(dplyr)
library(Hmisc)
library(naniar)
library(labelled)
library(ggplot2)

recent_df_RW<-df_2019
recent_df_CO<-df2015_new
recent_df_SN<-chdf2019

edu_bar_df<-function(df,year,country){
  #' df: input dataframe
  #' column_name: school attendence, highest educational level
  
  edu_df<-subset(df,select=c("hv121","hv106","hv105","Orphanhood"))
  edu_df<-na.omit(edu_df)
  edu_df<-subset(edu_df,hv105>5)
  
  edu_orphan_df<-subset(edu_df,Orphanhood=="orphan")
  edu_nonorphan_df<-subset(edu_df,Orphanhood=="non-orphan")
  
  #attendence bar dataframe
  attendence_df<-df_barplot(df,"hv121","Orphanhood")
  attendence_df$percentage<-as.numeric(attendence_df$percentage)
  
  #highest educational level bar dataframe
  edu_orphan_df<-subset(edu_orphan_df,hv105==15)
  edu_nonorphan_df<-subset(edu_nonorphan_df,hv105==15)
  hl_or_p0<-nrow(edu_orphan_df[edu_orphan_df$hv106==0,])/nrow(edu_orphan_df)
  hl_or_p1<-nrow(edu_orphan_df[edu_orphan_df$hv106==1,])/nrow(edu_orphan_df)
  hl_or_p2<-nrow(edu_orphan_df[edu_orphan_df$hv106==2,])/nrow(edu_orphan_df)
  hl_or_p3<-nrow(edu_orphan_df[edu_orphan_df$hv106==3,])/nrow(edu_orphan_df)
  hl_nor_p0<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==0,])/nrow(edu_nonorphan_df)
  hl_nor_p1<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==1,])/nrow(edu_nonorphan_df)
  hl_nor_p2<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==2,])/nrow(edu_nonorphan_df)
  hl_nor_p3<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==3,])/nrow(edu_nonorphan_df)
  
  percentage<-c(hl_or_p0,hl_nor_p0,hl_or_p1,hl_nor_p1,hl_or_p2,hl_nor_p2,hl_or_p3,hl_nor_p3)
  orphan<-c(rep(c("orphan","non-orphan"),4))
  samplesize<-c(rep(c(nrow(edu_orphan_df),nrow(edu_nonorphan_df)),4))
  level<-c("no education","no education","primary","primary","secondary","secondary","higher","higher")
  
  mat2<-matrix(c(percentage,orphan,level,samplesize),ncol=4)
  highest_level_df<-data.frame(mat2)
  colnames(highest_level_df)<-c("percentage","orphanhood","level","samplesize")
  highest_level_df["CI_lower"]<-CIlower(percentage,samplesize)
  highest_level_df["CI_upper"]<-CIupper(percentage,samplesize)
  highest_level_df$percentage<-as.numeric(highest_level_df$percentage)
  
  attendence_df["country"]<-country
  attendence_df["year"]<-as.character(year)
  highest_level_df["country"]<-country
  highest_level_df["year"]<-as.character(year)
  list(attendence_df,highest_level_df)
}

#
att_country_df<-rbind(edu_bar_df(recent_df_RW,2019,"Rwanda"),edu_bar_df(recent_df_CO,2015,"Colombia"),edu_bar_df(recent_df_SN,2019,"Senegal"))

#Senegal plot
att_year_df<-rbind(edu_bar_df(chdf2016,2016,"Senegal")[[1]],edu_bar_df(chdf2017,2017,"Senegal")[[1]],edu_bar_df(chdf2018,2018,"Senegal")[[1]],edu_bar_df(chdf2019,2019,"Senegal")[[1]])
highest_level_bar_df<-edu_bar_df(chdf2019,2019,"Senegal")[[2]]
#highest_level_bar_df$percentage<-as.numeric(highest_level_bar_df$percentage)

#single year bar plot of school attendence
att_bar<-ggplot(attendence_bar_df, aes(x=orphan, y=percentage)) +
  geom_bar(stat="identity",width=0.5)+
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),width=0.4, colour="black", position = position_dodge(.5))+
  labs(x ="Orphanhood") +
  ggtitle(paste("Senegal", 2019)) +
  coord_flip(ylim=c(0, 1))+
  theme_classic()

#single year bar plot of highest level of education
hl_bar<-ggplot(highest_level_bar_df, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Highest level of education") +
  ggtitle(paste("Senegal", 2019)) +
  ylim(0, 1)+
  theme_classic()

#multiple years barplots of school attendence
att_year<-ggplot(att_year_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "School attendence") +
  ggtitle(paste("Senegal", " 2016-2019")) +
  ylim(0, 1)+
  theme_classic()

#Comparison plot by countries
#multiple countries barplots of school attendence
att_year<-ggplot(att_year_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "School attendence") +
  ggtitle(paste("Senegal", " 2016-2019")) +
  ylim(0, 1)+
  theme_classic()

#multiple countries bar plot of highest level of education
hl_bar<-ggplot(highest_level_bar_df, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Highest level of education") +
  ggtitle(paste("Senegal", 2019)) +
  ylim(0, 1)+
  theme_classic()

