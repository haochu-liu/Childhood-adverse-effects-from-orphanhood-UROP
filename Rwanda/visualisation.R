library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)


# lineplot for orphanhood
load("Rwanda/survey_list.Rda")
survey_list <- as.data.frame(survey_list)
colnames(survey_list) <- c("Rwanda", "1992", "2000", "2005", "2007", "2010",
                           "2013", "2014", "2017", "2019")
year_list <- c("1992", "2000", "2005", "2010", "2014", "2019")
years <- as.Date(year_list, format = "%Y")
orphan_history <- data.frame(years)
orphan_history$under_18 <- as.numeric(survey_list[3, year_list])
orphan_history$orphan <- as.numeric(survey_list[4, year_list])
orphan_history$percentage <- mapply('/', as.numeric(orphan_history$orphan),
                                    as.numeric(orphan_history$under_18))

plot(orphan_history$years, orphan_history$percentage,
     type="b", lwd=2, lty = 2, col="#00AFBB", bty="l", pch=20, cex=2,
     xlab="Year", ylab="# of orphans / # of underages", main="Rwanda")
abline(v = as.Date(c("1994"), format = "%Y"), col="#FC4E07", lwd=3)
text(x=as.Date(c("1994"), format = "%Y"), y=0.2, 'Genocide')


# import functions
source("functions_for_plotting.R")


# 2019
year <- 2019
load("Rwanda/df_2019.Rda")
bar_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv243e", "hv270",
             "hv121", "hv121.1", "ha57", "hc57", "hml32")
box_col <- c("ha3", "hc3", "ha2", "hc2", "ha40", "ha53", "hc53")
bar_df <- df_barplot(df_2019, bar_col, "Orphanhood")
box_df <- df_boxplot(df_2019, box_col, "Orphanhood", "hv105")
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
# boxplot
box_labels <- label(box_df)
val_labels(box_df) <- NULL

ggplot(box_df, aes(x=hv105, y=ha3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha3"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=hv105, y=hc3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc3"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=hv105, y=ha2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["ha2"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=hv105, y=hc2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hc2"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=ha53)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha53"])) +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=hc53)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["hc53"])) +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()




























