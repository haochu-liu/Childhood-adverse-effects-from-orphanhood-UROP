source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Cleaning data/Cleaning 2005.R")

library(ggplot2)
library(Hmisc)
library(labelled)
# bar plot
col_name_2005b <- c("hv025","hv026","hv201","hv205", "hv206",
                    "hv207", "hv208", "hv209", 
                    "hv211", "hv212", "hv221",
                    "shh12i","hv270",
                    "hv121", "hv121.1")

barplot_data2005 <- df_barplot(df2005_new, col_name_2005b, "Orphanhood")

ggplot(barplot_data2005, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2005") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

col_name_2005c <- c("ha3", "hc3", "ha2", "hc2", "ha40")
boxplot_data2005 <- df_boxplot(df2005_new, col_name_2005c, "Orphanhood", "hv105")

# boxplot
box_labels <- label(boxplot_data2005)
val_labels(boxplot_data2005) <- NULL

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha3"]),]
ggplot(df, aes(x=hv105,y=ha3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha3"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

######
df <- boxplot_data2005 
df <- df[!is.na(df[,"hc3"]),]
ggplot(df, aes(x=hv105, y=hc3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc3"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha2"]),]
ggplot(df, aes(x=hv105, y=ha2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha2"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

######
df <- boxplot_data2005 
df <- df[!is.na(df[,"hc2"]),]
ggplot(df, aes(x=hv105, y=hc2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc2"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha40"]),]
ggplot(df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()
