library(MASS)
library(labelled)

# Fieller theorem assumes independent normal and same variance

boxcox_trans <- function(vector, lambda){
  if (lambda == 0) {
    vector <- log(vector)
  } else {
    vector <- (vector^lambda - 1) / lambda
  }
  
  vector
}

fieller_child_df <- function(df,country,year){
  cont_df <- subset(df, select = c("hc3", "hc2", "hv105", "Orphanhood"))
  cont_df <- na.omit(cont_df)
  
  val_labels(cont_df) <- NULL  
  
  fieller_df <- data.frame(matrix(ncol=7,nrow=0))
  colnames(fieller_df)<-c("col_labels","ratio","upper","lower","p-value","country","year")
  
  # 0-2 and 2-5 year groups 
  yrdf1 <- subset(cont_df, cont_df$hv105 < 2)
  yrdf2 <- subset(cont_df, cont_df$hv105 >= 2)

  # boxcox to find 4 lambdas
  hmodel1 <- lm(hc3 ~ 1, data = yrdf1)
  bc_h1 <- boxcox(hmodel1, lambda = seq(-5, 5, 0.2))
  print(head(yrdf1))
  
  hmodel2 <- lm(hc3 ~ 1, data = yrdf2)
  bc_h2 <- boxcox(hmodel2, lambda = seq(-5, 5, 0.2))
  
  wmodel1 <- lm(hc2 ~ 1, data = yrdf1)
  bc_w1 <- boxcox(wmodel1, lambda = seq(-5, 5, 0.2))
  
  wmodel2 <- lm(hc2 ~ 1, data = yrdf2)
  bc_w2 <- boxcox(wmodel2, lambda = seq(-5, 5, 0.2))
  
  # find lambda
  lambda_h1 <- round(bc_h1$x[which.max(bc_h1$y)])
  lambda_h2 <- round(bc_h2$x[which.max(bc_h2$y)])
  lambda_w1 <- round(bc_w1$x[which.max(bc_w1$y)])
  lambda_w2 <- round(bc_w2$x[which.max(bc_w2$y)])
  
  # transform
  yrdf1["bc_height"] <- (yrdf1$hc3^lambda_h1 - 1) / lambda_h1
  yrdf1["bc_weight"] <- (yrdf1$hc2^lambda_w1 - 1) / lambda_w1
  
  yrdf2["bc_height"] <- (yrdf2$hc3^lambda_h2 - 1) / lambda_h2
  yrdf2["bc_weight"] <- (yrdf2$hc2^lambda_w2 - 1) / lambda_w2
  
  #orphan/nonorphan datasets in 0-2 and 2-5 year groups
  ordf1 <- subset(yrdf1, cont_df$Orphanhood=="orphan")
  nordf1 <- subset(yrdf1, cont_df$Orphanhood=="non-orphan")
  
  ordf2 <- subset(yrdf2, cont_df$Orphanhood=="orphan")
  nordf2 <- subset(yrdf2, cont_df$Orphanhood=="non-orphan")

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











