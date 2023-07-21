library(rdhs)

set_rdhs_config(email = "zhengxialu@gmail.com",
                project = "Childhood Adverse Effects From Orphanhood",
                config_path = "rdhs.json",
                global = FALSE)


# getting data
data2015 <- get_datasets(dataset_filenames = "COPR72DT.zip")
df2015 <- readRDS(data2015$"COPR72DT")
df2015 <- data.frame(df2015)

# gathering useful infor in one dataframe
df <- df2015[, c("hhid","hvidx","hv001","hv002","hv003")] #identify respondent
df["hv111"] <- df2015$hv111 #whether mother alive
df["hv113"] <- df2015$hv113 #whether father alive

df["age"] <- df2015$hv105
df["hv121"] <- df2015$hv121
df["hv104"] <- df2015$hv104
df["hv110"] <- df2015$hv110

na.omit(df)

View(df)

library(haven)
opdf <- subset(df, df$hv111 == 0 | df$hv113 == 0)
nopdf <- subset(df, df$hv111 != 0 & df$hv113 != 0)
lmdf <- subset(df, df$hv111 == 0)
lfdf <- subset(df, df$hv113 == 0)

# children
chdf <- subset(df, df$age > 6 & df$age < 15)

# gender hv104
fdf<-subset(chdf,chdf$hv104==2)
mdf<-subset(chdf,chdf$hv104==1)
fopdf<-subset(opdf,opdf$hv104==2)
mopdf<-subset(opdf,opdf$hv104==1)

# HV121 Member attended school during current school year
ch1<-table(chdf$hv121)[2]/nrow(chdf)
op1<-table(opdf$hv121)[2]/nrow(opdf)
nop1<-table(nopdf$hv121)[2]/nrow(nopdf)

lm1<-table(lmdf$hv121)[2]/nrow(lmdf)
lf1<-table(lfdf$hv121)[2]/nrow(lfdf)

f1<-table(fdf$hv121)[2]/nrow(fdf)
m1<-table(mdf$hv121)[2]/nrow(mdf)

fop1<-table(fopdf$hv121)[2]/nrow(fopdf)
mop1<-table(mopdf$hv121)[2]/nrow(mopdf)

hv121vec<-c(op1,nop1,lm1,lf1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv121vec,names.arg=c("op1","nop1","lm1","lf1"),main="hv121 chart",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))

hv121vec<-c(ch1,f1,m1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv121vec,names.arg=c("ch1","f1","m1"),main="hv121 chart",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))

hv121vec<-c(fop1,mop1,op1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv121vec,names.arg=c("fop1","mop1","op1"),main="hv121 chart",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))
