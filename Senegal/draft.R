# confidence intervals
df05features["lower"]<-integer(44)
for (x in 1:11){
  df05features$lower[x]<-CIlower(df05features$proportion[x],nrow(opdf))
}
for (x in 12:22){
  df05features$lower[x]<-CIlower(df05features$proportion[x],nrow(nopdf))
}
for (x in 23:33){
  df05features$lower[x]<-CIlower(df05features$proportion[x],nrow(lmdf))
}
for (x in 34:44){
  df05features$lower[x]<-CIlower(df05features$proportion[x],nrow(lfdf))
}