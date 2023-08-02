library(MASS)
# assume normal and same variance

fieller_child_df<-function(df,country,year){
  cont_df <- subset(df, select = c("hc3", "hc2", "hv105", "Orphanhood"))
  cont_df <- na.omit(cont_df)
  
  fieller_df <- data.frame(matrix(ncol=7,nrow=0))
  colnames(fieller_df)<-c("col_labels","ratio","upper","lower","p-value","country","year")
  
  # height
  hmodel <- lm(hc3~1, data = cont_df)
  bc_h <- boxcox(hmodel, lambda = seq(-5, 5, 0.2))
  # weight
  wmodel <- lm(hc2~1, data = cont_df)
  bc_w <- boxcox(wmodel, lambda = seq(-5, 5, 0.2))
  
  # find lambda
  lambda_h <- round(bc_h$x[which.max(bc_h$y)])
  lambda_w <- round(bc_w$x[which.max(bc_w$y)])
  
  # transform
  cont_df["bc_height"] <- (cont_df$hc3^lambda_h - 1) / lambda_h
  cont_df["bc_weight"] <- (cont_df$hc2^lambda_w - 1) / lambda_w
  
  #orphan/nonorphan datasets in 0-2 and 2-5 year groups
  ordf1 <- subset(cont_df, cont_df$Orphanhood=="orphan" & cont_df$hv105 < 2)
  nordf1 <- subset(cont_df, cont_df$Orphanhood=="non-orphan" & cont_df$hv105 < 2)
  ordf2 <- subset(cont_df, cont_df$Orphanhood=="orphan" & cont_df$hv105 >= 2)
  nordf2 <- subset(cont_df, cont_df$Orphanhood=="non-orphan" & cont_df$hv105 >= 2)
  
  # Fieller
  ttr1_h <- ttestratio(ordf1$bc_height, nordf1$bc_height, var.equal=TRUE, conf.level=0.95)
  ttr2_h <- ttestratio(ordf2$bc_height, nordf2$bc_height, var.equal=TRUE, conf.level=0.95)
  row1_h <- c("height 0-2 years", ttr1_h$estimate[3], ttr1_h$conf.int[1], ttr1_h$conf.int[2], ttr1_h$p.value, country, year)
  row2_h <- c("height 2-5 years", ttr2_h$estimate[3], ttr2_h$conf.int[1], ttr2_h$conf.int[2], ttr2_h$p.value, country, year)
  
  ttr1_w <- ttestratio(ordf1$bc_weight, nordf1$bc_weight, var.equal=TRUE, conf.level=0.95)
  ttr2_w <- ttestratio(ordf2$bc_weight, nordf2$bc_weight, var.equal=TRUE, conf.level=0.95)
  row1_w <- c("weight 0-2 years", ttr1_w$estimate[3], ttr1_w$conf.int[1], ttr1_w$conf.int[2], ttr1_w$p.value, country, year)
  row2_w <- c("weight 2-5 years", ttr2_w$estimate[3], ttr2_w$conf.int[1], ttr2_w$conf.int[2], ttr2_w$p.value, country, year)
  
  fieller_df <- rbind(fieller_df, row1_h, row2_h, row1_w, row2_w)
  
  fieller_df
}











