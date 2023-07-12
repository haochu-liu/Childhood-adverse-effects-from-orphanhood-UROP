library(dplyr)
library(ggplot2)


# get subsets for orphanhood
not_orphan_m <- subset(df, df$hv111==1 & df$hv113==1 & df$hv104==1)
not_orphan_f <- subset(df, df$hv111==1 & df$hv113==1 & df$hv104==2)
orphan_m <- subset(df, (df$hv111==0 | df$hv113==0) & df$hv104==1)
orphan_f <- subset(df, (df$hv111==0 | df$hv113==0) & df$hv104==2)


# define a function to count columns
count_by_orphan <- function(not_orphan_m, not_orphan_f, orphan_m, orphan_f) {
  sta_col <- data.frame(
    not_orphan_male = c(sum(not_orphan_m), length(not_orphan_m)-sum(not_orphan_m)),
    not_orphan_female = c(sum(not_orphan_f), length(not_orphan_f)-sum(not_orphan_f)),
    orphan_male = c(sum(orphan_m), length(orphan_m)-sum(orphan_m)),
    orphan_female = c(sum(orphan_f), length(orphan_f)-sum(orphan_f))
  )
  for (i in 1:4) {
    s <- sum(sta_col[, i])
    sta_col[1, i] <- sta_col[1, i] / s
    sta_col[2, i] <- sta_col[2, i] / s
  }
  sta_col
}


# age 10-17 children attending school
school_sta <- count_by_orphan(not_orphan_m$hv121[not_orphan_m$age_group>=3]!=0,
                              not_orphan_f$hv121[not_orphan_f$age_group>=3]!=0,
                              orphan_m$hv121[orphan_m$age_group>=3]!=0,
                              orphan_f$hv121[orphan_f$age_group>=3]!=0)
# plot barplot
x_axis <- c("not_op_m", "not_op_f",
            "op_m", "op_f")
par(mar=c(3,3,3,3))
barplot(as.numeric(school_sta[1,]), names.arg=x_axis, ylab="proportion",
        main="Attending school for age 10-17 children", ylim=c(0,1))


# having mosquito bed net for sleeping
bed_net_sta <- count_by_orphan(not_orphan_m$hv227==1,
                               not_orphan_f$hv227==1,
                               orphan_m$hv227==1,
                               orphan_f$hv227==1)
# plot barplot
barplot(as.numeric(bed_net_sta[1,]), names.arg=x_axis, ylab="proportion",
        main="Having mosquito bed net for sleeping", ylim=c(0,1))


# per household data
df_h <- df %>% distinct(hv001, hv002, .keep_all = TRUE)


# household wealth
x_axis <- c("not orphan", "orphan")
h1 <- hist(df_h$hv270[df$hv111==1 & df$hv113==1], density=TRUE, plot = FALSE)
h2 <- hist(df_h$hv270[df$hv111==0 | df$hv113==0], density=TRUE, plot = FALSE)
plot(h1, col = c1)
plot(h2, col = c2, add = TRUE)






