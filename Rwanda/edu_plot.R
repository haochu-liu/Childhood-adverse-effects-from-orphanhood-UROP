library(haven)
library(stringr)
library(dplyr)
library(Hmisc)
library(naniar)
library(labelled)
library(ggplot2)


source("functions_for_plotting.R")

edu_bar_df <- function(df, year, country){
  #' df: input dataframe
  #' column_name: school attendence, highest educational level
  #' df<-recent_df_RW
  #' year<-2019
  #' country<-"Rwanda"
  edu_df<-df[,c("hv121","hv106","hv105","Orphanhood")]
  edu_df<-remove_var_label(edu_df)
  # edu_df$hv105<-as.integer(edu_df$hv105)
  edu_df<-na.omit(edu_df)
  edu_df<-subset(edu_df,hv105>5)
  
  edu_orphan_df<-subset(edu_df,Orphanhood=="orphan")
  edu_nonorphan_df<-subset(edu_df,Orphanhood=="non-orphan")
  
  # attendence bar dataframe
  attendence_df<-df_barplot(df,"hv121","Orphanhood")
  attendence_df$percentage<-as.numeric(attendence_df$percentage)
  
  # highest educational level bar dataframe
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
  highest_level_df["CI_lower"]<-CI_lower(percentage,samplesize)
  highest_level_df["CI_upper"]<-CI_upper(percentage,samplesize)
  highest_level_df$percentage<-as.numeric(highest_level_df$percentage)
  
  attendence_df["country"]<-country
  attendence_df["year"]<-as.character(year)
  highest_level_df["country"]<-country
  highest_level_df["year"]<-as.character(year)
  list(attendence_df,highest_level_df)
}


edu_bar_df_17 <- function(df, year, country){
  #' df: input dataframe
  #' column_name: school attendence, highest educational level
  #' df<-recent_df_RW
  #' year<-2019
  #' country<-"Rwanda"
  edu_df<-df[,c("hv121","hv106","hv105","Orphanhood")]
  edu_df<-remove_var_label(edu_df)
  # edu_df$hv105<-as.integer(edu_df$hv105)
  edu_df<-na.omit(edu_df)
  edu_df<-subset(edu_df,hv105>5)
  
  edu_orphan_df<-subset(edu_df,Orphanhood=="orphan")
  edu_nonorphan_df<-subset(edu_df,Orphanhood=="non-orphan")
  
  # attendence bar dataframe
  attendence_df<-df_barplot(df,"hv121","Orphanhood")
  attendence_df$percentage<-as.numeric(attendence_df$percentage)
  
  # highest educational level bar dataframe
  edu_orphan_df<-subset(edu_orphan_df,hv105==17)
  edu_nonorphan_df<-subset(edu_nonorphan_df,hv105==17)
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
  highest_level_df["CI_lower"]<-CI_lower(percentage,samplesize)
  highest_level_df["CI_upper"]<-CI_upper(percentage,samplesize)
  highest_level_df$percentage<-as.numeric(highest_level_df$percentage)
  
  attendence_df["country"]<-country
  attendence_df["year"]<-as.character(year)
  highest_level_df["country"]<-country
  highest_level_df["year"]<-as.character(year)
  list(attendence_df,highest_level_df)
}


load("Rwanda/df_2019.Rda")
load("Rwanda/df_2014.Rda")
load("Rwanda/df_2010.Rda")
load("Rwanda/df_2005.Rda")


# Rwanda plot
att_year_df <- rbind(edu_bar_df(df_2019,2019,"Rwanda")[[1]],
                     edu_bar_df(df_2014,2014,"Rwanda")[[1]],
                     edu_bar_df(df_2010,2010,"Rwanda")[[1]],
                     edu_bar_df(df_2005,2005,"Rwanda")[[1]])
hl_year_df <- rbind(edu_bar_df(df_2019,2019,"Rwanda")[[2]],
                    edu_bar_df(df_2014,2014,"Rwanda")[[2]],
                    edu_bar_df(df_2010,2010,"Rwanda")[[2]],
                    edu_bar_df(df_2005,2005,"Rwanda")[[2]])
hl_year_df_17 <- rbind(edu_bar_df_17(df_2019,2019,"Rwanda")[[2]],
                    edu_bar_df_17(df_2014,2014,"Rwanda")[[2]],
                    edu_bar_df_17(df_2010,2010,"Rwanda")[[2]],
                    edu_bar_df_17(df_2005,2005,"Rwanda")[[2]])


# multiple years barplots of school attendence
att_year_RW <- ggplot(att_year_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "School attendance") +
  ggtitle("Rwanda School Attendance (2005-2019)") +
  ylim(0, 1)+
  theme_classic()
print(att_year_RW)
ggsave("bar_RW_attendance.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

# multiple years barplots of highest level of education
hl_year_RW <- ggplot(hl_year_df, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "highest level of education at the age of 15") +
  ggtitle("Rwanda Highest Level of Education (2005-2019)") +
  facet_wrap(~year) +
  #scale_y_continuous(expand = c(0, 1)) +
  ylim(0,1)+
  theme_classic()
print(hl_year_RW)
ggsave("bar_RW_level15.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)

# multiple years barplots of highest level of education
hl_year_RW_17 <- ggplot(hl_year_df_17, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "highest level of education at the age of 17") +
  ggtitle("Rwanda Highest Level of Education (2005-2019)") +
  facet_wrap(~year) +
  #scale_y_continuous(expand = c(0, 1)) +
  ylim(0,1)+
  theme_classic()
print(hl_year_RW_17)







