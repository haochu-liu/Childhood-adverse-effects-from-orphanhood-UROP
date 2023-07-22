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

barplot_data2015 <- df_barplot(df2015_new, col_name_2015b, "Orphanhood")

ggplot(barplot_data2015, aes(fill = orphan, x = column_labels, y=percentage)) +
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
