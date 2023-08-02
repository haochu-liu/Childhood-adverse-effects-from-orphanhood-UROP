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


fieller_child_df<-function(df,country,year){
  cont_df <- subset(df, select=c("hc3", "hc2", "hv105", "Orphanhood"))
  cont_df <- na.omit(cont_df)
  
  # height
  hmodel <- lm(hc3~1,data=cont_df)
  bc_h <- boxcox(hmodel, plotit = FALSE)
  
  #find lambda
  lambda_h <- round(bc_h$x[which.max(bc_h$y)])
  
  #transform
  cont_df["bc_height"] <- (cont_df$hc3^lambda_h - 1) / lambda_h
  
  #orphan/nonorphan datasets in 0-2 and 2-5 year groups
  ordf1<-subset(cont_df,chdf$Orphanhood=="orphan"&chdf$hv105<2)
  nordf1<-subset(chdf,chdf$Orphanhood=="non-orphan"&chdf$hv105<2)
  ordf2<-subset(cont_df,chdf$Orphanhood=="orphan"&chdf$hv105>=2)
  nordf2<-subset(chdf,chdf$Orphanhood=="non-orphan"&chdf$hv105>=2)
  
  #fieller
  ttr<-ttestratio(ordf$bc_height,nordf$bc_height,var.equal=TRUE,conf.level=0.95)
  cont<-data.frame(matrix(ncol=7,nrow=0))
  row <- c("height", ttr$estimate[3], ttr$conf.int[1], ttr$conf.int[2], ttr$p.value, country, year)
  
  cont <- rbind(cont, row)
  
  colnames(cont)<-c("col_labels","ratio","upper","lower","p-value","country","year")
  
  # weight
  
  
  cont
}











