source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Cleaning data/Cleaning 2000.R")

library(ggplot2)
library(Hmisc)
library(labelled)
# bar plot
col_name_2000b <- c("hv025", "hv026","hv201","hv205", "hv206",
                    "hv207", "hv208", "hv209", "hv221",
                    "hv121", "hv121.1")

barplot_data2000 <- df_barplot(df2000_new, col_name_2000b, "Orphanhood")

ggplot(barplot_data2000, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2000") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

