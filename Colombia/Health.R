col <- c("hv201","hv205")

df1 <- df_barplot(df2000_new, col, "Orphanhood")
df1$year <- "2000"
df2 <- df_barplot(df2005_new, col, "Orphanhood")
df2$year <- "2005"
df3 <- df_barplot(df2010_new, col, "Orphanhood")
df3$year <- "2010"
df4 <- df_barplot(df2015_new, col, "Orphanhood")
df4$year <- "2015"

temp <- rbind(df1, df2, df3, df4)
ggplot(temp, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("hv201,hv205")

