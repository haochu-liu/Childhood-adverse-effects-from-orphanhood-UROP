col_name<-c("hv105","hv104","hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv221","hv243a","hv243e","hv106",
            "hv121","hv201","hv205","hc1","hc2","hc3","Orphanhood","hv270")

con_name<-c("hc2","hc3")

log_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv221","hv243a","hv243e",
            "hv121","hv201","hv205")

cat_name<-c("hv106","hv270")

df_SN1<-df_SN
df_SN<-df_SN[,col_name]


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

df_cont <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv104) + Age (hc1) + Age^2
  
  data<-df_SN
  str <- "~ Orphanhood + hv104 + hc1 + I(hc1^2)"
  cont_df <- data.frame(matrix(nrow = 0, ncol = 6))
  vec_response<-con_name
  
  for (i in 1:length(vec_response)){
    
    i<-1
    f <- paste(vec_response[i], str)
    
    model <- lm(f, data=data)
    ci <- confint(model)
    coeff <- coef(summary(model))["Orphanhoodorphan","Estimate"]
    p_val <- coef(summary(model))["Orphanhoodorphan","Pr(>|t|)"]
    ci_lower <- ci[2, 1]
    ci_upper <- ci[2, 2]
    label <- label(data[,vec_response[i]])
    
    cont_df <- rbind(cont_df, c(vec_response[i], label, coeff, p_val, ci_lower, ci_upper))
  }
  
  colnames(cont_df) <- c("Outcomes", "Labels", "Coeff", "P_val", "CI_lower", "CI_upper")
  cont_df
}


df_ordered <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv105) + Age (hv105)
  
  vec_reponse<-"hv106"
  data<-df_SN
  str <- "~ Orphanhood + hv104 + hv105"
  order_df <- data.frame(matrix(nrow = 0, ncol = 3))
  
  for (i in 1:length(cat_name)){
    
    i<-1
    f <- paste(vec_response[i], str)
    model <- polr(f, data=data, Hess = TRUE,method="logistic")
    ctable <- coef(summary(model))
    coeff<-coef(summary(model))["Orphanhoodorphan","Estimate"]
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
ordered_df_SN<-df_ordered(cat_name,df_SN)

df_SN<-chdf2019[,col_name]

df_ordered <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv105) + Age (hv105)
  vec_response<-cat_name
  data<-df_SN
  str <- "~ Orphanhood + hv104 + hv105"
  order_df <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in 1:length(vec_response)){
    
    i<-2
    f <- paste(vec_response[i], str)
    df <- data[, c(vec_response[i], "Orphanhood", "hv104", "hv105")]
    df <- unlabelled(df)
    
    model1 <- polr(as.factor(hv270)~ Orphanhood + hv104 + hv105, data=df_SN,
                   Hess=TRUE, method = c("logistic"))
    
    startval<-c(model1$coefficients,model1$zeta)
    model2 <- polr(as.factor(hv270)~ Orphanhood + hv104 + hv105, data=df_SN1,
                   Hess=TRUE, method = c("logistic"),start=startval)
    
    model<-model2
    
    ctable <- coef(summary(model))
    ci <- confint.default(model)
    coeff <- ctable["Orphanhoodorphan", "Value"]
    p_val <- pnorm(abs(ctable["Orphanhoodorphan", "t value"]), lower.tail = FALSE) * 2
    ci_lower <- ci[1, 1]
    ci_upper <- ci[1, 2]
    label <- label(data[,vec_response[i]])
    
    order_df <- rbind(order_df,c(vec_response[i], label, coeff, p_val, ci_lower, ci_upper))
  }
  
  colnames(order_df) <- c("Outcomes", "Labels", "Coeff", "P_val", "CI_lower", "CI_upper")
  order_df
}

order_df_SN<-order_df
df_SN<-df_SN[df_SN$hv106!="higher",]
df_SN<-df_SN[is.na(df_SN$hv104)!=TRUE,]
df_SN<-df_SN[is.na(df_SN$hv105)!=TRUE,]
df_SN<-df_SN[g,]
model1 <- polr(as.factor(hv106)~ Orphanhood + hv104 + hv105, data=df_SN,
               Hess=TRUE, method = c("logistic"))

startval<-c(model1$coefficients,model1$zeta)
model2 <- polr(as.factor(hv106)~ Orphanhood + hv104 + hv105, data=df_SN1,
               Hess=TRUE, method = c("logistic"),start=startval)


regression_table_SN<-rbind(bin_df_SN,cont_df_SN,order_df_SN)
regression_table_SN["fill"]<-"Senegal 2019"

regression_table_SN$Labels<-tolower(regression_table_SN$Labels)

regression_table_SN[9,"Labels"]<-"has telephone (land-line)"
regression_table_SN[14,"Labels"]<-"has flush or pit toilet"
regression_table_SN[15,"Labels"]<-"child's weight in kilograms (1 decimal)"
regression_table_SN[16,"Labels"]<-"child's height in centimeters (1 decimal)"
regression_table_SN[18,"Labels"]<-"household wealth index"



save(regression_table_SN,file="regression_table_SN.Rda")
write.csv(regression_table_SN,file="regression_table_SN.csv")

bin_coeff_SN<-df_bin_coeff(log_name,df_SN)
con_coeff_SN<-df_cont_coeff(con_name,df_SN)

coeff_hv207_SN <- df_bin_coeff("hv207", df_SN)
coeff_hv207_SN$fill <- "Senegal 2019"
save(coeff_hv207_SN, file = "coeff_hv207_SN.Rda")

regression_table<-read.csv("regression_table.csv")

regression_table$average_coeff<-NULL
for(x in 1:24){
  if (is.na(regression_table$average.coeff[x])==TRUE){
    regression_table$average.coeff[x]<-regression_table$Coeff.3[x]
  }
  else{
    regression_table$average.coeff[x]<-as.numeric(regression_table$average.coeff[x])
  }
}

regression_table$average_p<-NULL
for(x in 1:24){
  regression_table$average_p[x]<-mean(c(as.numeric(regression_table$Coeff.3[x]),as.numeric(regression_table$Coeff.4[x]),as.numeric(regression_table$Coeff.5[x])))
}

for(x in 1:24){
  if (is.na(regression_table$average.p.value[x])==TRUE){
    regression_table$average.p.value[x]<-regression_table$P_val.3[x]
  }
  else{
    regression_table$average.p.value[x]<-as.numeric(regression_table$average.p.value[x])
  }
}
write.csv(regression_table,file="regression_table.csv")
