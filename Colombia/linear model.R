library(dplyr)
library(labelled)

col_names <- c("hc1", "hc2", "hc3", "Orphanhood", "hv104", "hv270")

df_2005 <- df2005_new[, col_names]
df_2005$year <- 2005
df_2005 <- unlabelled(df_2005)

df_2010 <- df2010_new[, col_names]
df_2010$year <- 2010
df_2010 <- unlabelled(df_2010)

df <- rbind(df_2005, df_2010)
df <- na.omit(df)
df <- df %>% rename(
  "Age" = "hc1",
  "Weight" = "hc2",
  "Height" = "hc3",
  "Sex" = "hv104",
  "Poverty" = "hv270"
)

lm_weight1 <- lm(Weight ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year +
                   Orphanhood*Sex + Orphanhood*Poverty + Orphanhood*Age, data=df)
summary(lm_weight1)

lm_weight2 <- lm(Weight ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year,
                 data=df)
summary(lm_weight2)

lm_weight3 <- lm(Weight ~ Age + I(Age^2) + Sex + Poverty + year,
                 data=df)
summary(lm_weight3)


anova(lm_weight1, lm_weight2, lm_weight3)

drop1(lm_weight3, test = "F")


lm_height1 <- lm(Height ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year +
                   Orphanhood*Sex + Orphanhood*Poverty + Orphanhood*Age, data=df)
summary(lm_height1)

lm_height2 <- lm(Height ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year + Orphanhood*Sex,
                 data=df)
summary(lm_height2)

lm_height3 <- lm(Height ~ Age + I(Age^2) + Sex + Poverty + year,
                 data=df)
summary(lm_height3)


anova(lm_height1, lm_height2, lm_height3)

drop1(lm_height2, test = "F")