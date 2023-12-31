library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)


# lineplot for orphanhood
# load("Rwanda/survey_list.Rda")
# survey_list <- as.data.frame(survey_list)
# colnames(survey_list) <- c("Rwanda", "1992", "2000", "2005", "2007", "2010",
#                            "2013", "2014", "2017", "2019")
# year_list <- c("1992", "2000", "2005", "2010", "2014", "2019")
# years <- as.Date(year_list, format = "%Y")
# orphan_history <- data.frame(years)
# orphan_history$under_18 <- as.numeric(survey_list[3, year_list])
# orphan_history$orphan <- as.numeric(survey_list[4, year_list])
# orphan_history$percentage <- mapply('/', as.numeric(orphan_history$orphan),
#                                     as.numeric(orphan_history$under_18))
# 
# plot(orphan_history$years, orphan_history$percentage,
#      type="b", lwd=2, lty = 2, col="#00AFBB", bty="l", pch=20, cex=2,
#      xlab="Year", ylab="# of orphans / # of underages", main="Rwanda")
# abline(v = as.Date(c("1994"), format = "%Y"), col="#FC4E07", lwd=3)
# text(x=as.Date(c("1994"), format = "%Y"), y=0.2, 'Genocide')


# import functions
source("functions_for_plotting.R")

# create dataframe bar_years
bar_years <- data.frame()


# 2019
year <- 2019
load("Rwanda/df_2019.Rda")
bar_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv243e", "hv270",
             "hv121", "ha57", "hc57", "hml32")
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
bar_df$year <- year
bar_years <- rbind(bar_years, bar_df)
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

ha40_2019 <- ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
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


# 2014
year <- 2014
load("Rwanda/df_2014.Rda")
bar_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv270",
             "hv121", "ha57", "hc57", "hml32")
box_col <- c("ha3", "hc3", "hb3", "ha2", "hc2", "hb2", "ha40", "hb40", "ha53", "hc53")
bar_df <- df_barplot(df_2014, bar_col, "Orphanhood")
box_df <- df_boxplot(df_2014, box_col, "Orphanhood", "hv105")
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
bar_df$year <- year
bar_years <- rbind(bar_years, bar_df)
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

ggplot(box_df, aes(x=hv105, y=hb3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hb3"]), x = "Age") +
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

ggplot(box_df, aes(x=hv105, y=hb2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hb2"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ha40_2014 <- ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=hb40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["hb40"]), x = "Age") +
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


# 2010
year <- 2010
load("Rwanda/df_2010.Rda")
bar_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv270",
             "hv121", "ha57", "hc57", "hml32")
box_col <- c("ha3", "hc3", "hb3", "ha2", "hc2", "hb2", "ha40", "hb40", "ha53", "hc53")
bar_df <- df_barplot(df_2010, bar_col, "Orphanhood")
box_df <- df_boxplot(df_2010, box_col, "Orphanhood", "hv105")
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
bar_df$year <- year
bar_years <- rbind(bar_years, bar_df)
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

ggplot(box_df, aes(x=hv105, y=hb3, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hb3"]), x = "Age") +
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

ggplot(box_df, aes(x=hv105, y=hb2, fill=Orphanhood)) +
  geom_violin(aes(fill=Orphanhood),
              width=0.8, alpha=0.5, position = position_dodge(0.9)) +
  geom_boxplot(width=0.2, position = position_dodge(0.9)) +
  labs(y = as.character(box_labels["hb2"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ha40_2010 <- ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["ha40"]), x = "Age") +
  ggtitle(paste("Rwanda", year)) +
  coord_flip()

ggplot(box_df, aes(x=Orphanhood, y=hb40)) +
  geom_violin(aes(fill=Orphanhood), width=0.8, alpha=0.5) +
  geom_boxplot(width=0.1) +
  labs(y = as.character(box_labels["hb40"]), x = "Age") +
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


# 2005
year <- 2005
load("Rwanda/df_2005.Rda")
bar_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv270",
             "hv121", "ha57", "hc57")
box_col <- c("ha3", "hc3", "ha2", "hc2", "ha40", "ha53", "hc53")
bar_df <- df_barplot(df_2005, bar_col, "Orphanhood")
box_df <- df_boxplot(df_2005, box_col, "Orphanhood", "hv105")
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
bar_df$year <- year
bar_years <- rbind(bar_years, bar_df)
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

ha40_2005 <- ggplot(box_df, aes(x=Orphanhood, y=ha40)) +
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


# side by side barplots
ggplot(bar_years, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Rwanda") +
  facet_wrap(~year) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


ha40_2019 + ha40_2014 + ha40_2010 + ha40_2005 + plot_layout(ncol = 2)


# heatmap for missing values
year_list <- c("2019", "2014", "2010", "2005")
df_list <- list("2019" = df_2019,
                "2014" = df_2014,
                "2010" = df_2010,
                "2005" = df_2005)
col_name_list <- c("hv025", "hv201", "hv205", "hv206",
                   "hv207", "hv208", "hv209", "hv210",
                   "hv211", "hv212", "hv227", "hv221",
                   "hv243a", "hv243b", "hv243e", "hv270",
                   "hv121", "hv106", "ha57", "hc57",
                   "hml32", "ha3", "hc3", "hb3", "ha2", "hc2", "hb2",
                   "hc1", "ha40", "hb40", "ha53", "hc53")
col_label_list <- c(
  "Lives in urban area",
  "Has piped or tube water",
  "Has flush or pit toilet",
  "Has electricity",
  "Has radio",
  "Has television",
  "Has refrigerator",
  "Has bicycle",
  "Has motorcycle/scooter",
  "Has car/truck",
  "Has mosquito bed net for sleeping",
  "Has telephone (land-line)",
  "Has mobile telephone",
  "Has watch",
  "Has a computer",
  "Poor household wealth",
  "School attendance",
  "Highest educational level attained",
  "Has anemia (woman)",
  "Has anemia (child)",
  "Has malaria",
  "Woman's height in centimeters",
  "Child's height in centimeters",
  "Man's height in centimeters",
  "Woman's weight in kilograms",
  "Child's weight in kilograms",
  "Man's weight in kilograms",
  "Child's age in months",
  "Woman's body mass index",
  "Man's body mass index",
  "Woman's hemoglobin level (g/dl)",
  "Child's hemoglobin level (g/dl)"
)
heatmap_df <- df_isna(df_list, col_name_list, col_label_list, year_list)

ggplot(heatmap_df, aes(label, year, fill=na_percentage)) + 
  geom_tile(aes(fill=na_percentage), colour="white") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlOrBr"),
                       na.value="grey") +
  geom_point(data=heatmap_df, aes(size="Question missing"), shape=NA, colour="grey") +
  guides(size=guide_legend("Not applicable",
                           override.aes=list(shape=15, size=7)),
         fill=guide_legend(title="Proportion of \nchildren with \nmissing outcomes")) +
  scale_x_discrete(limits=col_label_list) +
  theme(axis.title.y=element_blank()) +
  ggtitle("Rwanda") +
  coord_flip()
ggsave("heatmap_RW.png",
       path="figures", dpi=700, height = 5.6, width = 8.5)


# odd plots
odd_col <- c("hv025", "hv201", "hv205", "hv206",
             "hv207", "hv208", "hv209", "hv210",
             "hv211", "hv212", "hv227", "hv221",
             "hv243a", "hv243b", "hv243e", "hv270",
             "hv121", "ha57", "hc57", "hml32")
odd_df <- df_odd_ratio(df_2019, odd_col, "Orphanhood")

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
























