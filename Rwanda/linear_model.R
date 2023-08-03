library(dplyr)
library(labelled)


load("Rwanda/df_2019.Rda")
load("Rwanda/df_2014.Rda")
load("Rwanda/df_2010.Rda")
load("Rwanda/df_2005.Rda")


col_names <- c("hc1", "hc2", "hc3", "Orphanhood", "hv104", "hv270")

df_2019 <- df_2019[, col_names]
df_2019$year <- 2019
df_2019 <- unlabelled(df_2019)

df_2014 <- df_2014[, col_names]
df_2014$year <- 2014
df_2014 <- unlabelled(df_2014)

df_2010 <- df_2010[, col_names]
df_2010$year <- 2010
df_2010 <- unlabelled(df_2010)

df_2005 <- df_2005[, col_names]
df_2005$year <- 2005
df_2005 <- unlabelled(df_2005)

df_RW <- rbind(df_2019, df_2014, df_2010, df_2005)
df_RW <- na.omit(df_RW)
df_RW <- df_RW %>% rename(
  "Age" = "hc1",
  "Weight" = "hc2",
  "Height" = "hc3",
  "Sex" = "hv104",
  "Poverty" = "hv270"
)

lm_weight1_RW <- lm(Weight ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year +
                  Orphanhood*Sex + Orphanhood*Poverty + Orphanhood*Age, data=df_RW)
summary(lm_weight1_RW)

lm_weight2_RW <- lm(Weight ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year,
                 data=df_RW)
summary(lm_weight2_RW)

lm_weight3_RW <- lm(Weight ~ Age + I(Age^2) + Sex + Poverty + year,
                 data=df_RW)
summary(lm_weight3_RW)


anova(lm_weight1_RW, lm_weight2_RW, lm_weight3_RW)

drop1(lm_weight3_RW, test = "F")


lm_height1_RW <- lm(Height ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year +
                   Orphanhood*Sex + Orphanhood*Poverty + Orphanhood*Age, data=df_RW)
summary(lm_height1_RW)

lm_height2_RW <- lm(Height ~ Orphanhood + Age + I(Age^2) + Sex + Poverty + year + Orphanhood*Sex,
                 data=df_RW)
summary(lm_height2_RW)

lm_height3_RW <- lm(Height ~ Age + I(Age^2) + Sex + Poverty + year,
                 data=df_RW)
summary(lm_height3_RW)


anova(lm_height1_RW, lm_height2_RW, lm_height3_RW)

drop1(lm_height2_RW, test = "F")



load("Rwanda/df_2019.Rda")
load("Rwanda/df_2014.Rda")
load("Rwanda/df_2010.Rda")
load("Rwanda/df_2005.Rda")
# load("Rwanda/df_2000.Rda")

col_names <- c("hv121", "Orphanhood", "hv104", "hv270", "hv105")

df_2019 <- df_2019[, col_names]
df_2019$year <- 2019
df_2019 <- unlabelled(df_2019)

df_2014 <- df_2014[, col_names]
df_2014$year <- 2014
df_2014 <- unlabelled(df_2014)

df_2010 <- df_2010[, col_names]
df_2010$year <- 2010
df_2010 <- unlabelled(df_2010)

df_2005 <- df_2005[, col_names]
df_2005$year <- 2005
df_2005 <- unlabelled(df_2005)

df <- rbind(df_2019, df_2014, df_2010, df_2005)
df <- na.omit(df)
df <- df %>% rename(
  "Age" = "hv105",
  "school_attainment" = "hv121",
  "Sex" = "hv104",
  "Poverty" = "hv270"
)

glm_edu1 <- glm(school_attainment ~ Orphanhood + Age + Sex + Poverty + year +
                Orphanhood*Sex + Orphanhood*Poverty + Orphanhood*Age, data=df, family = "binomial")
summary(glm_edu1)

glm_edu2 <- glm(school_attainment ~ Orphanhood + Age + Sex + Poverty + year,
              data=df, family = "binomial")
summary(glm_edu2)

glm_edu3 <- glm(school_attainment ~ Age + Sex + Poverty + year,
              data=df, family = "binomial")
summary(glm_edu3)


anova(glm_edu1, glm_edu2, glm_edu3)

drop1(glm_edu1)






