library(rdhs)
library(dplyr)
library(labelled)
library(ggplot2)


# choose one dataframe
# 2019
download <- get_datasets("RWPR81DT.ZIP")
df <- readRDS(download$RWPR81DT)
df <- data.frame(df)


# under 18
under_18 <- c(sum(df$hv105 < 18), sum(df$hv105 >= 18))
x_axis <- c("< 18", ">= 18")
barplot(under_18, names.arg=x_axis, xlab="Age", ylab="Number of individuals",
        col="blue", main="Ages in Rwanda")
df <- df[df$hv105 < 18, ]


# choose columns
df <- df[, c("hvidx", "hv001", "hv002", "hv111", "hv113", "hv217", "hv105", "hv104")



")]


