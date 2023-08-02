library(MASS)
chdf<-subset(chdf2019,select=c("hc2","hc3","hv105","Orphanhood"))
chdf<-na.omit(chdf)
lmodel_h<-lm(hc3~1,data=chdf)
lmodel_w<-lm(hc2~1,data=chdf)
bc_h<-boxcox(lmodel_h,lambda=seq(0,4,by=0.5))
#find lambda
lambda_h<-round(bc_h$x[which.max(bc_h$y)])

#transform the indicator data
chdf["bc_height"]<-(chdf$hc3^lambda_h-1)/lambda_h
omean<-mean(chdf[chdf$Orphanhood=="orphan","bc_height"])
nomean<-mean(chdf[chdf$Orphanhood=="non-orphan","bc_height"])

#
ordf<-subset(chdf,chdf$Orphanhood=="orphan")
nordf<-subset(chdf,chdf$Orphanhood=="non-orphan")
#cont_summary<-data.frame(matrix(ncol=5,nrow=0,dimnames=list(NULL,c("col_labels","mean","sd","country","year"))))


# assume normal and same variance











