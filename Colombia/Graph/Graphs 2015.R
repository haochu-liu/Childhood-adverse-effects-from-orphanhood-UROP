source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Cleaning data/Cleaning 2015.R")

library(ggplot2)
library(Hmisc)
library(labelled)
# barplot

# binary variables
col_name_2015b <- c("hv025","hv201","hv205", "hv206",
                    "hv207", "hv208", "hv209", "hv210",
                    "hv211", "hv212", "hv221",
                    "hv243a", "hv243e", "hv270",
                    "hv121", "hv121.1")

bar_CO_2015 <- df_barplot(df2015_new, col_name_2015b, "Orphanhood")

bar_CO_2015$year <- "2015"
bar_CO_2015$country <- "Colombia"
save(bar_CO_2015, file = "Colombia/bar_CO_2015.Rda")

ggplot(bar_CO_2015, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2015") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

# continuous variables 
# col_name_2015c <- c("")
# df_boxplot(df2015_new, col_name_c, col_orphan, "hv105")

# odd
odd_CO_2015 <- df_odd_ratio(df2015_new, col_name_2015b, "Orphanhood")

odd_CO_2015$year <- "2015"
odd_CO_2015$country <- "Colombia"
save(odd_CO_2015, file = "Colombia/odd_CO_2015.Rda")

or2015 <- ggplot(odd_CO_2015, aes(x = odd_ratio, y = column_labels)) + 
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
    geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                     0.25, color = "gray50") +
    geom_point(shape = 18, size = 3, color = "orange") +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    ylab("Outcomes") +
    xlab("Odds ratio (95% CI)") +
    ggtitle("Odd Ratio for Colombia 2015") +
    theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

or2015
