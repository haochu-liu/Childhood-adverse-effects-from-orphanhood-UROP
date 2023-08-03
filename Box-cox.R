library(MASS)
library(labelled)
library(mratios)

# Fieller theorem assumes independent normal and same variance

boxcox_trans <- function(vector, lambda){
  if (lambda == 0) {
    vector <- log(vector)
  } else {
    vector <- (vector^lambda - 1) / lambda
  }
  
  vector
}


fieller_child_df <- function(df, country, year){
  cont_df <- subset(df, select = c("hc3", "hc2", "hv105", "Orphanhood"))
  cont_df <- na.omit(cont_df)
  
  val_labels(cont_df) <- NULL  
  
  fieller_df <- data.frame(matrix(ncol=8,nrow=0))
  
  # 0-2 and 2-5 year groups 
  yrdf1 <<- subset(cont_df, cont_df$hv105 < 2)
  yrdf2 <<- subset(cont_df, cont_df$hv105 >= 2)

  # boxcox to find 4 lambdas
  hmodel1 <- lm(hc3 ~ 1, data=yrdf1)
  bc_h1 <- boxcox(hmodel1, lambda = seq(-5, 5, 0.2))
  
  hmodel2 <- lm(hc3 ~ 1, data=yrdf2)
  bc_h2 <- boxcox(hmodel2, lambda = seq(-5, 5, 0.2))
  
  wmodel1 <- lm(hc2 ~ 1, data=yrdf1)
  bc_w1 <- boxcox(wmodel1, lambda = seq(-5, 5, 0.2))
  
  wmodel2 <- lm(hc2 ~ 1, data=yrdf2)
  bc_w2 <- boxcox(wmodel2, lambda = seq(-5, 5, 0.2))
  
  # find lambda
  lambda_h1 <- round(bc_h1$x[which.max(bc_h1$y)])
  lambda_h2 <- round(bc_h2$x[which.max(bc_h2$y)])
  lambda_w1 <- round(bc_w1$x[which.max(bc_w1$y)])
  lambda_w2 <- round(bc_w2$x[which.max(bc_w2$y)])
  
  # transform
  yrdf1["bc_height"] <- boxcox_trans(yrdf1$hc3, lambda_h1)
  yrdf1["bc_weight"] <- boxcox_trans(yrdf1$hc2, lambda_w1)
  
  yrdf2["bc_height"] <- boxcox_trans(yrdf2$hc3, lambda_h2)
  yrdf2["bc_weight"] <- boxcox_trans(yrdf2$hc2, lambda_w2)
  
  #orphan/nonorphan datasets in 0-2 and 2-5 year groups
  ordf1 <- subset(yrdf1, yrdf1$Orphanhood=="orphan")
  nordf1 <- subset(yrdf1, yrdf1$Orphanhood=="non-orphan")
  
  ordf2 <- subset(yrdf2, yrdf2$Orphanhood=="orphan")
  nordf2 <- subset(yrdf2, yrdf2$Orphanhood=="non-orphan")

  # Fieller
  ttr1_h <- ttestratio(ordf1$bc_height, nordf1$bc_height, var.equal=TRUE, conf.level=0.95)
  ttr2_h <- ttestratio(ordf2$bc_height, nordf2$bc_height, var.equal=TRUE, conf.level=0.95)
  row1_h <- c("hc3.1","Height (age 0-2 years)", lambda_h1, ttr1_h$estimate[3], ttr1_h$conf.int[1], ttr1_h$conf.int[2], ttr1_h$p.value, country, year)
  row2_h <- c("hc3.2","Height (age 2-5 years)", lambda_h2, ttr2_h$estimate[3], ttr2_h$conf.int[1], ttr2_h$conf.int[2], ttr2_h$p.value, country, year)
  
  ttr1_w <- ttestratio(ordf1$bc_weight, nordf1$bc_weight, var.equal=TRUE, conf.level=0.95)
  ttr2_w <- ttestratio(ordf2$bc_weight, nordf2$bc_weight, var.equal=TRUE, conf.level=0.95)
  row1_w <- c("hc2.1","Weight (age 0-2 years)", lambda_w1, ttr1_w$estimate[3], ttr1_w$conf.int[1], ttr1_w$conf.int[2], ttr1_w$p.value, country, year)
  row2_w <- c("hc2.2","Weight (age 2-5 years)", lambda_w2, ttr2_w$estimate[3], ttr2_w$conf.int[1], ttr2_w$conf.int[2], ttr2_w$p.value, country, year)
  
  fieller_df <- rbind(fieller_df, row1_h, row2_h, row1_w, row2_w)
  colnames(fieller_df)<-c("col_names","col_labels","parameter","ratio","CI_lower","CI_upper","p_value","country","year")
  
  # changing data type
  fieller_df$parameter = as.numeric(fieller_df$parameter)
  fieller_df$ratio = as.numeric(fieller_df$ratio)
  fieller_df$CI_lower = as.numeric(fieller_df$CI_lower)
  fieller_df$CI_upper = as.numeric(fieller_df$CI_upper)
  
  fieller_df
}

fieller_woman_df <- function(df, country, year){
  cont_df <<- subset(df, select = c("ha3", "ha2", "hv105", "Orphanhood"))
  cont_df <- na.omit(cont_df)
  
  val_labels(cont_df) <- NULL  
  
  fieller_df <- data.frame(matrix(ncol=8,nrow=0))
  
  # boxcox to find 2 lambdas
  hmodel <- lm(ha3 ~ 1, data = cont_df)
  bc_h <- boxcox(hmodel, lambda = seq(-5, 5, 0.2))
  
  wmodel <- lm(ha2 ~ 1, data = cont_df)
  bc_w <- boxcox(wmodel, lambda = seq(-5, 5, 0.2))
 
  # find lambda
  lambda_h <- round(bc_h$x[which.max(bc_h$y)])
  lambda_w <- round(bc_w$x[which.max(bc_w$y)])
  
  # transform
  cont_df["bc_height"] <- boxcox_trans(cont_df$ha3, lambda_h)
  cont_df["bc_weight"] <- boxcox_trans(cont_df$ha2, lambda_w)
  
  #orphan/nonorphan datasets
  ordf <- subset(cont_df, cont_df$Orphanhood=="orphan")
  nordf <- subset(cont_df, cont_df$Orphanhood=="non-orphan")
  
  # Fieller
  ttr_h <- ttestratio(ordf$bc_height, nordf$bc_height, var.equal=TRUE, conf.level=0.95)
  row_h <- c("ha3","Height (woman)", lambda_h, ttr_h$estimate[3], ttr_h$conf.int[1], ttr_h$conf.int[2], ttr_h$p.value, country, year)
 
  ttr_w <- ttestratio(ordf$bc_weight, nordf$bc_weight, var.equal=TRUE, conf.level=0.95)
  row_w <- c("ha2","Weight (woman)", lambda_w, ttr_w$estimate[3], ttr_w$conf.int[1], ttr_w$conf.int[2], ttr_w$p.value, country, year)
  
  fieller_df <- rbind(fieller_df, row_h, row_w)
  colnames(fieller_df)<-c("col_names","col_labels","parameter","ratio","CI_lower","CI_upper","p_value","country","year")
  
  # changing data type
  fieller_df$parameter = as.numeric(fieller_df$parameter)
  fieller_df$ratio = as.numeric(fieller_df$ratio)
  fieller_df$CI_lower = as.numeric(fieller_df$CI_lower)
  fieller_df$CI_upper = as.numeric(fieller_df$CI_upper)
  
  fieller_df
}

df_yearsort <- function(fieller_df, number_of_years) {
    df <- fieller_df[order(fieller_df$col_names, fieller_df$year), ]
    forester_data <- data.frame()
    n <- nrow(df) / number_of_years
    
    a <- number_of_years
    b <- a - 1
    
    for (i in 1:n) {
      Outcome <- c(df$col_labels[1+a*(i-1)], df$year[1:a])
      slice_df <- data.frame(Outcome)
      slice_df$ratio <- c(NA, df$ratio[(a*i-b):(a*i)])
      slice_df$CI_lower <- c(NA, df$CI_lower[(a*i-b):(a*i)])
      slice_df$CI_upper <- c(NA, df$CI_upper[(a*i-b):(a*i)])
      slice_df$p_value <- c(NA, df$p_value[(a*i-b):(a*i)])
      slice_df$parameter <- c(NA, df$parameter[(a*i-b):(a*i)])
      forester_data <- rbind(forester_data, slice_df)
    }
    
    forester_data
}
  
df_sort <- function(fieller_list){
    # fieller_list: list of dataframe indexed by country name
    country_name <- names(fieller_list)
    
    # year sort for every country 
    for (i in 1:3){
      fieller_df <- fieller_list[i]
      yr <- unique(fieller_df$year)
      fieller_data <- df_yearsort(fieller_df, length(yr))
      
      fieller_list[i] <- fieller_data
    }
    
    forester_data <- data.frame()
    df <- rbind(fieller_list[1], fieller_list[2], fieller_list[3])
    unique_col <- c("ha2", "ha3", "hc2.1", "hc2.2", "hc3.1", "hc3.2")
    
    for (i in 1:length(unique_col)) {
      new_df <- df[df$col_names == unique_col[i], ]
      Outcome <- c(new_df$column_labels[1], new_df$year)
      slice_df <- data.frame(Outcome)
      slice_df$ratio <- c(NA, new_df$ratio)
      slice_df$CI_lower <- c(NA, new_df$CI_lower)
      slice_df$CI_upper <- c(NA, new_df$CI_upper)
      slice_df$p_value <- c(NA, new_df$p_value)
      slice_df$parameter <- c(NA, new_df$parameter)
      forester_data <- rbind(forester_data, slice_df)
    }
}




