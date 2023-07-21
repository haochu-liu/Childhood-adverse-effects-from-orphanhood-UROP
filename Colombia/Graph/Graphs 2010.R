source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Cleaning data/Cleaning 2010.R")

library(ggplot2)
# bar plot
col_name_2010b <- c("hv025","hv201","hv205", 
                    "hv207", "hv208", "hv209", "hv210",
                    "hv211", "hv212", "hv221",
                    "hv243a", "hv270",
                    "hv121", "hv121.1")

barplot_data2010 <- df_barplot(df2010_new, col_name_2010b, "Orphanhood")

ggplot(barplot_data2010, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2010") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

col_name_2010c <- c("ha3", "hc3", "ha2", "hc2", "ha40")
boxplot_data2010 <- df_boxplot(df2010_new, col_name_2010c, "Orphanhood", "hv105")

# boxplot
box_labels <- label(barplot_data2010)
val_labels(barplot_data2010) <- NULL

ggplot(boxplot_data2010, aes(x=hv105, y=ha3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha3"]), x = "Age") +
  ggtitle("Colombia2010") +
  coord_flip()

ggplot(boxplot_data2010, aes(x=hv105, y=hc3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc3"]), x = "Age") +
  ggtitle("Colombia2010") +
  coord_flip()

ggplot(boxplot_data2010, aes(x=hv105, y=ha2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha2"]), x = "Age") +
  ggtitle("Colombia2010") +
  coord_flip()

ggplot(boxplot_data2010, aes(x=hv105, y=hc2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc2"]), x = "Age") +
  ggtitle("Colombia2010") +
  coord_flip()

ggplot(boxplot_data2010, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle("Colombia2010") +
  coord_flip()
