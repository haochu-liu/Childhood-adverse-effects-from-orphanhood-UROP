# analysing 2010 data which is more complete

# getting data
data2010 <- get_datasets(dataset_filenames = "COPR61DT.zip")
df2010 <- readRDS(data2010$"COPR61DT")
df2010 <- data.frame(df2010)

# gathering useful infor in one dataframe
df_new <- df2010[, c("hhid","hvidx","hv001","hv002","hv003")] #identify respondent
df_new["hv111"] <- df2010$hv111 #whether mother alive
df_new["hv113"] <- df2010$hv113 #whether father alive

df_new["age"] <- df2010$hv105
df_new["age_in_month"] <- df2010$hc1
df_new["gender"] <- df2010$hv104

df_new["stunting"] <- ifelse(df2010$hc70 < -300, "severely stunted", 
                             ifelse(df2010$hc70 < -200 & df2010$hc72 >= -300, "moderately stunted", 
                                    "out of plausible limit or flagged"))

df_new["wasting"] <- ifelse(df2010$hc72 < -300, "severely wasted",
                            ifelse(df2010$hc72 < -200 & df2010$hc72 >= -300, "moderately wasted",
                                   ifelse(df2010$hc72 > 200 & df2010$hc72 < 9996, "overweight", "other")))

# df_new["underweight"] <- ifelse(df2010$shbm < 1850, 1, ifelse(df2010$shbm >= 1850 & df2010$shbm < 9998, 0, "flagged or missing"))
  
df_new["underweight"] <- ifelse(df2010$hc71 < -300, "severely underweight",
                                ifelse(df2010$hc71 < -200 & df2010$hc71 >= -300, "moderately underweight",
                                       ifelse(df2010$hc71 > 200 & df2010$hc71 < 9996, "overweight", "other")))

na.omit(df_new)
# View(df_new)

library(haven)

# focusing on children below 5 (0-59 months)
chdf <- subset(df_new,!is.na(age_in_month))

chdf["stunted"] <- ifelse(chdf$stunting == "severely stunted" | chdf$stunting == "moderately stunted", 1, 0)
chdf["wasted"] <- ifelse(chdf$wasting == "severely wasted" | chdf$wasting == "moderately wasted", 1, 0)
chdf["weight"] <- ifelse(chdf$underweight == "severely underweight" | chdf$underweight == "moderately underweight", 1, 0)

opdf <- subset(chdf, chdf$hv111 == 0 | chdf$hv113 == 0)
nopdf <- subset(chdf, chdf$hv111 != 0 & chdf$hv113 != 0)
lmdf <- subset(chdf, chdf$hv111 == 0)
lfdf <- subset(chdf, chdf$hv113 == 0)

fdf <- subset(chdf,chdf$gender==2)
mdf <- subset(chdf,chdf$gender==1)
fopdf <- subset(opdf,opdf$gender==2)
mopdf <- subset(opdf,opdf$gender==1)

# compare nutritional status
ch_s<-table(chdf$stunted)[2]/nrow(chdf)
op_s<-table(opdf$stunted)[2]/nrow(opdf)
nop_s<-table(nopdf$stunted)[2]/nrow(nopdf)

lm_s<-table(lmdf$stunted)[2]/nrow(lmdf)
lf_s<-table(lfdf$stunted)[2]/nrow(lfdf)

f_s<-table(fdf$stunted)[2]/nrow(fdf)
m_s<-table(mdf$stunted)[2]/nrow(mdf)

fop_s<-table(fopdf$stunted)[2]/nrow(fopdf)
mop_s<-table(mopdf$stunted)[2]/nrow(mopdf)

stunted_vec<-c(op_s,nop_s,lm_s,lf_s)
# barplot
par(mar=c(3,3,3,3))
barplot(stunted_vec,names.arg=c("orphan","non-orphan","loss of mother","loss of father"),
        main="Proportion of orphan children stunted",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))

stunted_vec<-c(ch_s,f_s,m_s)
# barplot 
par(mar=c(3,3,3,3))
barplot(stunted_vec,names.arg=c("children","female","male"),
        main="Proportion of children stunted",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))

stunted_vec<-c(fop1,mop1,op1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv121vec,names.arg=c("fop1","mop1","op1"),main="Proportion of children stunted",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))


temp <- subset(df2010,!is.na(hc1))
temp_op <- subset(temp, hv111 == 0 | hv113 == 0)
table(temp_op$hc70 < -200)
