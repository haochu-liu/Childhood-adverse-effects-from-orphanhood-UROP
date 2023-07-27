source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2015.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2010.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2005.R")
source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/Colombia/Graph/Graphs 2000.R")

# barplot
allyear_CO_bar <- rbind(bar_CO_2015, bar_CO_2010, bar_CO_2005, bar_CO_2000)
allyear_CO_odd <- rbind(odd_CO_2015, odd_CO_2010, odd_CO_2005, odd_CO_2000)

allyear_CO_bar_plot <- ggplot(allyear_CO_bar, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Senegal") +
  facet_wrap(~year) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

allyear_CO_bar_plot

library(ggpubr)
# ggarrange(g1, g2, g3, g4, 
          #labels = c("2015", "2010", "2005", "2000"),
          #ncol = 2, nrow = 2)

bar_years <- rbind
allyear_bar<-ggplot(bar_years, aes(fill=orphan, x=column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Senegal") +
  facet_wrap(~year) +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

# box
ggarrange(box_2005_ha2, box_2010_ha2, 
          box_2005_ha3, box_2010_ha3,
          box_2005_hc2, box_2010_hc2,
          box_2005_hc3, box_2010_hc3,
          box_2005_ha40, box_2010_ha40,
          ncol = 2, nrow = 5)

# heat map for NA

col_name <- c("hv025","hv201","hv205", "hv206",
              "hv207", "hv208", "hv209", "hv210",
              "hv211", "hv212", "hv221",
              "hv243a", "hv243e", "hv270",
              "hv121", "hv121.1")

year <- c("2000", "2005", "2010", "2015")
data <- list("2000" = df2000_new,
             "2005" = df2005_new,
             "2010" = df2010_new,
             "2015" = df2015_new)
col_name <- c("hv025", "hv026","hv201", "hv205", "hv206",
              "hv207", "hv208", "hv209", "hv210",
              "hv211", "hv212", "hv221",
              "hv243a", "hv243e", "hv270",
              "hv121", "hv121.1", 
              "ha2", "ha3", "hc2","hc3", "ha40")

col_label <- c(
  "Lives in urban area",
  "Place of residence",
  "Has piped or tube water",
  "Has flush or pit toilet",
  "Has electricity",
  "Has radio",
  "Has television",
  "Has refrigerator",
  "Has bicycle",
  "Has motorcycle/scooter",
  "Has car/truck",
  "Has telephone (land-line)",
  "Has mobile telephone",
  "Has a computer",
  "Poor household wealth",
  "School attendance for age 5-16 (compulsory)",
  "School attendance for age 17",
  "Woman's weight in kilograms (1 decimal)",
  "Woman's height in centimeters (1 decimal)",
  "Child's weight in kilograms (1 decimal)",
  "Child's height in centimeters (1 decimal)",
  "Woman's body mass index")

df_heatmap <- df_isna(data, col_name, col_label, year)

ggplot(df_heatmap, aes(label, year, fill=na_percentage)) + 
  geom_tile(aes(fill=na_percentage), colour="white") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlOrBr"),
                       na.value="grey") +
  geom_point(data=df_heatmap, aes(size="Question missing"), shape=NA, colour="grey") +
  guides(size=guide_legend("Not applicable",
                           override.aes=list(shape=15, size=7)),
         fill=guide_legend(title="Proportion of children with missing outcomes")) +
  theme(axis.title.y=element_blank()) +
  scale_x_discrete(limits=col_label) +
  ggtitle("Colombia") +
  coord_flip()


# odd ratio
ggarrange(or2000,or2005,or2010,or2015,
          ncol = 2, nrow = 2)
