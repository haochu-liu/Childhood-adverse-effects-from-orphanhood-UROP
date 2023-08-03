library(haven)
library(stringr)
library(dplyr)
library(Hmisc)
library(naniar)
library(labelled)
library(ggplot2)

source("~/Desktop/Childhood-adverse-effects-from-orphanhood-UROP/functions_for_plotting.R")

df2000_edu <- df2000_new
df2005_edu <- df2005_new
df2010_edu <- df2010_new
df2015_edu <- df2015_new

df2000_edu$hv121[!is.na(df2000_new$hv121.1)] = df2000_new$hv121.1[!is.na(df2000_new$hv121.1)]
df2005_edu$hv121[!is.na(df2005_new$hv121.1)] = df2005_new$hv121.1[!is.na(df2005_new$hv121.1)]
df2010_edu$hv121[!is.na(df2010_new$hv121.1)] = df2010_new$hv121.1[!is.na(df2010_new$hv121.1)]
df2015_edu$hv121[!is.na(df2015_new$hv121.1)] = df2015_new$hv121.1[!is.na(df2015_new$hv121.1)]

att_year_df <- rbind(edu_bar_df(df2000_edu, 2000, "Colombia")[[1]],
                     edu_bar_df(df2005_edu, 2005, "Colombia")[[1]],
                     edu_bar_df(df2010_edu, 2010, "Colombia")[[1]],
                     edu_bar_df(df2015_edu, 2015, "Colombia")[[1]])

hl_year_df <- rbind(edu_bar_df(df2000_edu, 2000, "Colombia")[[2]],
                    edu_bar_df(df2005_edu, 2005, "Colombia")[[2]],
                    edu_bar_df(df2010_edu, 2010, "Colombia")[[2]],
                    edu_bar_df(df2015_edu, 2015, "Colombia")[[2]])


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


