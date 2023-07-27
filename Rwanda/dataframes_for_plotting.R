library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)


# 2019
year <- "2019"
load("Rwanda/df_2019.Rda")
col_names <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv243e", "hv270")
bar_RW_2019 <- df_barplot(df_2019, col_names, "Orphanhood")
odd_RW_2019 <- df_odd_ratio(df_2019, col_names, "Orphanhood")
# barplot
ggplot(bar_df, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()
bar_RW_2019$year <- year
bar_RW_2019$country <- "Rwanda"
# oddplot
ggplot(odd_df, aes(x=odd_ratio, y=column_labels)) + 
  geom_vline(xintercept=1, color="red", linetype="dashed", cex=0.5, alpha=0.5) +
  geom_errorbarh(aes(xmax=CI_upper, xmin=CI_lower),
                 size=0.25, height=0.25, color="gray50") +
  geom_point(shape=18, size=3, color="orange") +
  theme(panel.grid.minor=element_blank()) +
  xlab("Odds ratio (with 95% CI)") +
  ylab(element_blank()) +
  ggtitle("Odd Ratio for Rwanda 2019") +
  theme_classic() +
  scale_x_continuous(trans='log10') +
  annotation_logticks(sides="b")
odd_RW_2019$year <- year
odd_RW_2019$country <- "Rwanda"

save(bar_RW_2019, file="Rwanda/bar_RW_2019.Rda")
save(odd_RW_2019, file="Rwanda/odd_RW_2019.Rda")


