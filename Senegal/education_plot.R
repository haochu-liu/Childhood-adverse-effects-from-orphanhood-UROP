library(haven)
library(stringr)
library(dplyr)
library(Hmisc)
library(naniar)
library(labelled)
library(ggplot2)
library(ggpubr)


df_2019$hv121[!is.na(df_2019$hv121.1)] = df_2019$hv121.1[!is.na(df_2019$hv121.1)]
df2015_new$hv121[!is.na(df2015_new$hv121.1)] = df2015_new$hv121.1[!is.na(df2015_new$hv121.1)]
recent_df_RW<-df_2019
recent_df_CO<-df2015_new
recent_df_SN<-chdf2019



edu_bar_df<-function(df,year,country){
  #' df: input dataframe
  #' column_name: school attendence, highest educational level
  #df<-recent_df_RW
  #year<-2019
  #country<-"Rwanda"
  edu_df<-subset(df,select=c("hv121","hv106","hv105","Orphanhood"))
  edu_df<-df[,c("hv121","hv106","hv105","Orphanhood")]
  edu_df<-remove_var_label(edu_df)
  #edu_df$hv105<-as.integer(edu_df$hv105)
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

  hl_nor_p0<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==0,])/nrow(edu_nonorphan_df)
  hl_nor_p1<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==1,])/nrow(edu_nonorphan_df)
  hl_nor_p2<-nrow(edu_nonorphan_df[edu_nonorphan_df$hv106==2,])/nrow(edu_nonorphan_df)

  
  percentage<-c(hl_or_p0,hl_nor_p0,hl_or_p1,hl_nor_p1,hl_or_p2,hl_nor_p2)
  orphan<-c(rep(c("orphan","non-orphan"),3))
  samplesize<-c(rep(c(nrow(edu_orphan_df),nrow(edu_nonorphan_df)),3))
  level<-c("no education","no education","primary","primary","secondary","secondary")
  
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
att_country_df<-rbind(edu_bar_df(recent_df_RW,2019,"Rwanda")[[1]],edu_bar_df(recent_df_CO,2015,"Colombia")[[1]],edu_bar_df(recent_df_SN,2019,"Senegal")[[1]])
hl_RW<-edu_bar_df(recent_df_RW,2019,"Rwanda")[[2]]
hl_CO<-edu_bar_df(recent_df_CO,2015,"Colombia")[[2]]
hl_SN<-edu_bar_df(recent_df_SN,2019,"Senegal")[[2]]
hl_country_df<-rbind(hl_RW,hl_CO,hl_SN)

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
hl_SN_bar<-ggplot(hl_SN, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Highest level of education at the age 15") +
  ggtitle(paste("Senegal", 2019)) +
  ylim(0, 1)+
  theme_classic()
print(hl_SN_bar)

#single year bar plot of highest level of education
hl_RW_bar<-ggplot(hl_RW, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Highest level of education at the age 15") +
  ggtitle(paste("Rwanda", 2019)) +
  ylim(0, 1)+
  theme_classic()
print(hl_RW_bar)

#single year bar plot of highest level of education
hl_CO_bar<-ggplot(hl_CO, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Highest level of education at the age 15") +
  ggtitle(paste("Colombia", 2015)) +
  ylim(0, 1)+
  theme_classic()#+ theme(legend.position = "none")
print(hl_CO_bar)

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
att_country<-ggplot(att_country_df, aes(fill=orphan, x=country, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "School attendence") +
  ggtitle("Rwanda,Colombia and Senegal\nSchool Attendence of Most Recent Year") +
  ylim(0, 1)+
  theme_classic()
print(att_country)

#multiple countries bar plot of highest level of education
hl_country<-ggarrange(hl_SN_bar,hl_RW_bar,hl_CO_bar,nrow=1,ncol=3,common.legend = TRUE,legend="bottom")
print(hl_country)
