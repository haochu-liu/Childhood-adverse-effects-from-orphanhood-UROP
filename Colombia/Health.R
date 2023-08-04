col <- c("hv201","hv205")

df1 <- df_barplot(df2000_new, col, "Orphanhood")
df1$year <- "2000"
df2 <- df_barplot(df2005_new, col, "Orphanhood")
df2$year <- "2005"
df3 <- df_barplot(df2010_new, col, "Orphanhood")
df3$year <- "2010"
df4 <- df_barplot(df2015_new, col, "Orphanhood")
df4$year <- "2015"

temp <- rbind(df1, df2, df3, df4)
ggplot(temp, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  ylim(c(0,1))+
  labs(x = "Questions") +
  facet_wrap(~column_labels) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  ggtitle("hv201,hv205")


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
save(fieller_fix_CO, file = "Colombia/fieller_fix_CO.Rda")



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
