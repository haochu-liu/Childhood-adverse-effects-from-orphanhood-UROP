library(MASS)
library(labelled)
library(mratios)
library(ggplot2)
library(forester)

source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")
load("Rwanda/fieller_fix_RW.Rda")
load("Colombia/fieller_fix_CO.Rda")
load("Senegal/fieller_fix_SN.Rda")

# CO
fieller_fix_CO <- df_sortyear(fieller_fix_CO)
fieller_fix_CO$Outcome <- ifelse(is.na(fieller_fix_CO$ratio), 
                                 fieller_fix_CO$Outcome,
                                 paste0("   ", fieller_fix_CO$Outcome))

fieller_fix_CO$`P value` <- round(as.numeric(fieller_fix_CO$p_value), digits = 4)

# plot
forester(left_side_data = fieller_fix_CO[,c("Outcome","P value"), drop=FALSE],
         estimate = fieller_fix_CO$ratio,
         ci_low = fieller_fix_CO$CI_lower,
         ci_high = fieller_fix_CO$CI_upper,
         estimate_precision = 4,
         null_line_at = 1,
         ggplot_width = 50,
         nudge_x = 0.5,
         estimate_col_name = "Ratio with 95% CI",
         file_path = here::here("Colombia/fieller_plot_CO_lfix.png"), 
         render_as = "png")

# RW
fieller_fix_RW <- df_sortyear(fieller_fix_RW)
fieller_fix_RW$Outcome <- ifelse(is.na(fieller_fix_RW$ratio), 
                                 fieller_fix_RW$Outcome,
                                 paste0("   ", fieller_fix_RW$Outcome))

fieller_fix_RW$`P value` <- round(as.numeric(fieller_fix_RW$p_value), digits = 4)

# plot
forester(left_side_data = fieller_fix_RW[,c("Outcome","P value"), drop=FALSE],
         estimate = fieller_fix_RW$ratio,
         ci_low = fieller_fix_RW$CI_lower,
         ci_high = fieller_fix_RW$CI_upper,
         estimate_precision = 4,
         null_line_at = 1,
         ggplot_width = 50,
         nudge_x = 0.5,
         estimate_col_name = "Ratio with 95% CI",
         file_path = here::here("Rwanda/fieller_plot_RW_lfix.png"), 
         render_as = "png")

# SN
fieller_fix_SN <- df_sortyear(fieller_fix_SN)
fieller_fix_SN$Outcome <- ifelse(is.na(fieller_fix_SN$ratio), 
                                 fieller_fix_SN$Outcome,
                                 paste0("   ", fieller_fix_SN$Outcome))

fieller_fix_SN$`P value` <- round(as.numeric(fieller_fix_SN$p_value), digits = 4)

# plot
forester(left_side_data = fieller_fix_SN[,c("Outcome","P value"), drop=FALSE],
         estimate = fieller_fix_SN$ratio,
         ci_low = fieller_fix_SN$CI_lower,
         ci_high = fieller_fix_SN$CI_upper,
         estimate_precision = 4,
         null_line_at = 1,
         ggplot_width = 50,
         nudge_x = 0.5,
         estimate_col_name = "Ratio with 95% CI",
         file_path = here::here("Senegal/fieller_plot_SN_lfix.png"), 
         render_as = "png")
