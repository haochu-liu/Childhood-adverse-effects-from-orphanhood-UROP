source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Summary of DHS data.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Cleaning data/Cleaning 2005.R")

library(ggplot2)
library(Hmisc)
library(labelled)
library(haven)
# bar plot
col_name_2005b <- c("hv025","hv026","hv201","hv205", "hv206",
                    "hv207", "hv208", "hv209", 
                    "hv211", "hv212", "hv221",
                    "hv243e","hv270",
                    "hv121", "hv121.1")

bar_CO_2005 <- df_barplot(df2005_new, col_name_2005b, "Orphanhood")

ggplot(bar_CO_2005, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2005") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

bar_CO_2005$year <- "2005"
bar_CO_2005$country <- "Colombia"
save(bar_CO_2005, file = "Colombia/Cleaning data/bar_CO_2005.Rda")

# odd
odd_CO_2005 <- df_odd_ratio(df2005_new, col_name_2005b, "Orphanhood")
ggplot(odd_CO_2005, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Colombia 2005") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))

odd_CO_2005$year <- "2005"
odd_CO_2005$country <- "Colombia"
save(odd_CO_2005, file = "Colombia/Cleaning data/odd_CO_2005.Rda")


col_name_2005c <- c("ha3", "hc3", "ha2", "hc2", "ha40")
boxplot_data2005 <- df_boxplot(df2005_new, col_name_2005c, "Orphanhood", "hv105")

# boxplot
box_labels <- label(boxplot_data2005)
val_labels(boxplot_data2005) <- NULL

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha3"]),]
box_2005_ha3 <- ggplot(df, aes(x=hv105,y=ha3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha3"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

box_2005_ha3

######
df <- boxplot_data2005 
df <- df[!is.na(df[,"hc3"]),]
box_2005_hc3 <- ggplot(df, aes(x=hv105, y=hc3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc3"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

box_2005_hc3

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha2"]),]
box_2005_ha2 <- ggplot(df, aes(x=hv105, y=ha2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha2"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

box_2005_ha2

######
df <- boxplot_data2005 
df <- df[!is.na(df[,"hc2"]),]
box_2005_hc2 <- ggplot(df, aes(x=hv105, y=hc2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc2"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

box_2005_hc2

df <- boxplot_data2005 
df <- df[!is.na(df[,"ha40"]),]
box_2005_ha40 <- ggplot(df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle("Colombia2005") +
  coord_flip()

box_2005_ha40

