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

# HV110 member still in school
ch1<-table(chdf$hv110)[2]/nrow(chdf)
op1<-table(opdf$hv110)[2]/nrow(opdf)
nop1<-table(nopdf$hv110)[2]/nrow(nopdf)

lm1<-table(lmdf$hv110)[2]/nrow(lmdf)
lf1<-table(lfdf$hv110)[2]/nrow(lfdf)

f1<-table(fdf$hv110)[2]/nrow(fdf)
m1<-table(mdf$hv110)[2]/nrow(mdf)

fop1<-table(fopdf$hv110)[2]/nrow(fopdf)
mop1<-table(mopdf$hv110)[2]/nrow(mopdf)

hv110vec<-c(op1,nop1,lm1,lf1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv110vec,names.arg=c("op1","nop1","lm1","lf1"),main="hv110 chart",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))

hv110vec<-c(ch1,f1,m1)
#draw a barplot of hv121 statistics
par(mar=c(3,3,3,3))
barplot(hv110vec,names.arg=c("ch1","f1","m1"),main="hv110 chart",xlab="class",ylab="proportion",xlim=c(0,4),ylim=c(0,1))