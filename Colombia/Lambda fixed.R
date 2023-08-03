library(MASS)
library(labelled)
library(mratios)
library(ggplot2)
library(forester)

source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")

# weight (woman): lambda = 0
# height (woman): lambda = 2

# weight (age 0-2): lambda = 1
# weight (age 2-5): lambda = 0
# height (age 0-2): lambda = 2
# height (age 2-5): : lambda = 1

lambda_w = 0
lambda_h = 2

lambda_w1 = 1
lambda_w2 = 0
lambda_h1 = 2
lambda_h2 = 1

df_woman <- rbind(df_woman_lfix(df2005_new, "Colombia", "2005", lambda_h, lambda_w),
                  df_woman_lfix(df2010_new, "Colombia", "2010", lambda_h, lambda_w))

df_child <- rbind(df_child_lfix(df2005_new, "Colombia", "2005", lambda_h1, lambda_h2, lambda_w1, lambda_w2),
                  df_child_lfix(df2010_new, "Colombia", "2010", lambda_h1, lambda_h2, lambda_w1, lambda_w2))

fieller_fix_CO <- rbind(df_woman, df_child)



# to use forester 
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
         ggplot_width = 40,
         nudge_x = 0.5,
         estimate_col_name = "Ratio with 95% CI",
         file_path = here::here("Colombia/fieller_plot_CO_lfix.png"), 
         render_as = "png")
