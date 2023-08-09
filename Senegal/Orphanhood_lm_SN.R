col_name<-c("hv105","hv104","hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221","hv243a","hv243e","hv106",
            "hv121","hv201","hv205","hc1","hc2","hc3","Orphanhood")

con_name<-c("hc2","hc3")

log_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv221","hv243a","hv243e",
            "hv121","hv201","hv205")

df_SN<-chdf2019[,col_name]


lm_weight2_RW <- lm("Weight ~ Orphanhood + Age + I(Age^2) + Sex",
                    data=df_RW)
summary(lm_weight2_RW)


df_binary <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv104) + Age (hv105)
  
  str <- "~ Orphanhood + hv104 + hv105"
  bin_df <- data.frame(matrix(nrow = 0, ncol = 3))
  
  
  for (i in 1:length(vec_response)){
    
    f <- paste(vec_response[i], str)
    
    model <- glm(f, data=data)
    coeff <- coef(summary(model))["Orphanhoodorphan","Estimate"]
    p_val <- coef(summary(model))["Orphanhoodorphan","Pr(>|t|)"]
    
    bin_df <- rbind(bin_df,c(vec_response[i], coeff, p_val))
  }
  
  colnames(bin_df) <- c("Outcomes","Coeff","P_val")
  bin_df
}

df_ordered <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv105) + Age (hv105)
  
  vec_reponse<-"hv106"
  data<-df_SN
  str <- "~ Orphanhood + hv104 + hv105"
  order_df <- data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in 1:length(vec_response)){
    
    i<-1
    f <- paste(vec_response[i], str)
    data<-as.factor(data$hv106)
    model <- polr(f, data=data, Hess = TRUE,method="logistic")
    ctable <- coef(summary(model))
    coeff <- ctable["Orphanhoodorphan","Value"]
    p_val <- pnorm(abs(ctable["Orphanhoodorphan", "t value"]), lower.tail = FALSE) * 2
    
    order_df <- rbind(order_df,c(vec_response[i], coeff, p_val))
  }
  
  colnames(order_df) <- c("Outcomes","Coeff","P_val")
  order_df
}


bin_df_SN<-df_binary(log_name,df_SN)
cont_df_SN<-df_cont(con_name,df_SN)
library(labelled)
df_SN <- unlabelled(df_SN)
ordered_df_SN<-df_ordered("hv106",df_SN)

df_SN<-chdf2019[,col_name]

g = sample(1:11000, 500, replace=F)
df_SN<-df_SN[df_SN$hv106!="higher",]
df_SN<-df_SN[is.na(df_SN$hv104)!=TRUE,]
df_SN<-df_SN[is.na(df_SN$hv105)!=TRUE,]
df_SN<-df_SN[g,]
model1 <- polr(as.factor(hv106)~ Orphanhood + hv104 + hv105, data=df_SN,
               Hess=TRUE, method = c("logistic"))

startval<-c(model1$coefficients,model1$zeta)
model2 <- polr(as.factor(hv106)~ Orphanhood + hv104 + hv105, data=df_SN,
               Hess=TRUE, method = c("logistic"),start=startval)


