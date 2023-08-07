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

bar_CO_2000 <- df_barplot(df2000_new, col_name_2000b, "Orphanhood")

ggplot(bar_CO_2000, aes(fill = orphan, x = column_labels, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "Questions") +
  ggtitle("Colombia2000") +
  coord_flip(ylim=c(0, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

bar_CO_2000$year <- "2000"
bar_CO_2000$country <- "Colombia"
save(bar_CO_2000, file = "Colombia/Cleaning data/bar_CO_2000.Rda")

# odd
odd_CO_2000 <- df_odd_ratio(df2000_new, col_name_2000b, "Orphanhood")
ggplot(odd_CO_2000, aes(x = odd_ratio, y = column_labels)) + 
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower), size = 0.25, height = 
                   0.25, color = "gray50") +
  geom_point(shape = 18, size = 3, color = "orange") +
  theme_bw()+
  theme(panel.grid.minor = element_blank()) +
  ylab("Outcomes") +
  xlab("Odds ratio (95% CI)") +
  ggtitle("Odd Ratio for Colombia 2000") +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))


odd_CO_2000$year <- "2000"
odd_CO_2000$country <- "Colombia"
save(odd_CO_2000, file = "Colombia/Cleaning data/odd_CO_2000.Rda")

