col_name<-c("hv105","hv104","hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv227","hv221","hv243a","hv243e","hv106",
            "hv121","hml35","hv201","hv205","hc1","hc2","hc3","Orphanhood")

con_name<-c("hc2","hc3")

log_name<-c("hv025","hv206","hv207","hv208","hv209","hv210","hv211","hv212",
            "hv221","hv243a","hv243e","hv106",
            "hv121","hv201","hv205")

df_SN<-chdf2019[,col_name]


lm_weight2_RW <- lm("Weight ~ Orphanhood + Age + I(Age^2) + Sex",
                    data=df_RW)
summary(lm_weight2_RW)


df_binary <- function(vec_response, data){
  # glm : response ~ Orphanhood + Sex (hv104) + Age (hv105)
  
  str <- "~ Orphanhood + hv104 + hv105"
  bin_df <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(bin_df) <- c("Outcomes","Coeff","P_val")
  
  for (i in 1:length(vec_response)){
    
    f <- paste(vec_response[i], str)
    
    model <<- glm(f, data=data)
    coeff <- coef(summary(model))["Orphanhoodorphan","Estimate"]
    p_val <- coef(summary(model))["Orphanhoodorphan","Pr(>|t|)"]
    
    bin_df <- rbind(bin_df,c(vec_response[i], coeff, p_val))
  }
  bin_df
}

val_labels(df2015_new) <- NULL
bin_df_SN<-df_binary(log_name,df_SN)
cont_df_SN<-df_cont(con_name,df_SN)
