library(haven)
library(stringr)
library(dplyr)
library(Hmisc)
library(naniar)
library(labelled)
library(ggplot2)

source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")

att_year_df <- rbind(edu_bar_df(df2000_new, 2000, "Colombia")[[1]],
                     edu_bar_df(df2005_new, 2005, "Colombia")[[1]],
                     edu_bar_df(df2010_new, 2010, "Colombia")[[1]],
                     edu_bar_df(df2015_new, 2015, "Colombia")[[1]])

hl_year_df <- rbind(edu_bar_df(df2000_new, 2000, "Colombia")[[2]],
                    edu_bar_df(df2005_new, 2005, "Colombia")[[2]],
                    edu_bar_df(df2010_new, 2010, "Colombia")[[2]],
                    edu_bar_df(df2015_new, 2015, "Colombia")[[2]])


# multiple years barplots of school attendence
att_year_CO <- ggplot(att_year_df, aes(fill=orphan, x=year, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "School attendence") +
  ggtitle("Colombia School Attendence") +
  ylim(0, 1)+
  theme_classic()
print(att_year_CO)

ggsave("bar_CO_attendence.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

# multiple years barplots of highest level of education
hl_year_CO <- ggplot(hl_year_df, aes(fill=orphanhood, x=level, y=percentage)) +
  geom_col(width=0.5, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper),
                width=0.4, colour="black", position = position_dodge(.5)) +
  labs(x = "highest level of education at the age of 15") +
  ggtitle("Colombia highest level of education") +
  facet_wrap(~year) +
  #scale_y_continuous(expand = c(0, 1)) +
  ylim(0,1)+
  theme_classic()
print(hl_year_CO)

ggsave("bar_CO_level15.png",
       path = "figures", 
       height = 5.6, width = 8.5, dpi = 700)

