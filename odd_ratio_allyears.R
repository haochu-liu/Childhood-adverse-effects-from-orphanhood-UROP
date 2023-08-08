library(rdhs)
library(labelled)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(haven)
library(patchwork)
library(scales)
library(forester)

load("Rwanda/odd_RW.Rda")
load("Colombia/odd_CO.Rda")
load("Senegal/cleaned_data_SN/odd_SN.Rda")

source("functions_for_plotting.R")
RW_df <- odd_RW
CO_df <- df_forester_year(odd_CO)
SN_df <- df_forester_year(odd_SN)

# indent outcome if there is a number in odd_ratio column
CO_df$Outcomes <- ifelse(is.na(CO_df$odd_ratio), 
                               CO_df$Outcomes,
                               paste0("   ", CO_df$Outcomes))

# use forester to create the table with forest plot
forester(left_side_data = CO_df[,1, drop=FALSE],
         estimate = CO_df$odd_ratio,
         ci_low = CO_df$CI_lower,
         ci_high = CO_df$CI_upper,
         estimate_precision = 2,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         xlim = c(0.2,3.6),
         estimate_col_name = "Odds Ratio (95% CI)",
         arrows = TRUE,
         arrow_labels = c("Non-orphan Better", "Orphan Better"),
         file_path = here::here("figures/forest_CO_hd.png"))


# indent outcome if there is a number in odd_ratio column
SN_df$Outcomes <- ifelse(is.na(SN_df$odd_ratio), 
                         SN_df$Outcomes,
                         paste0("   ", SN_df$Outcomes))

# use forester to create the table with forest plot
forester(left_side_data = SN_df[,1, drop=FALSE],
         estimate = SN_df$odd_ratio,
         ci_low = SN_df$CI_lower,
         ci_high = SN_df$CI_upper,
         estimate_precision = 2,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         xlim = c(0.2,3.6),
         estimate_col_name = "Odds Ratio (95% CI)",
         arrows = TRUE,
         arrow_labels = c("Non-orphan Better", "Orphan Better"),
         file_path = here::here("figures/forest_SN_hd.png"))

# indent outcome if there is a number in odd_ratio column
RW_df$Outcomes <- ifelse(is.na(RW_df$odd_ratio), 
                         RW_df$Outcomes,
                         paste0("   ", RW_df$Outcomes))

# use forester to create the table with forest plot
forester(left_side_data = RW_df[,1, drop=FALSE],
         estimate = RW_df$odd_ratio,
         ci_low = RW_df$CI_lower,
         ci_high = RW_df$CI_upper,
         estimate_precision = 2,
         null_line_at = 1,
         ggplot_width = 40,
         nudge_x = 0.5,
         xlim = c(0.2,3.6),
         estimate_col_name = "Odds Ratio (95% CI)",
         arrows = TRUE,
         arrow_labels = c("Non-orphan Better", "Orphan Better"),
         file_path = here::here("figures/forest_RW_hd.png"))
