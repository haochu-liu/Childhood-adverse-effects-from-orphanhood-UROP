library(labelled)
library(dplyr)



load("Rwanda/df_2019.Rda")
load("Rwanda/df_2014.Rda")
load("Rwanda/df_2010.Rda")
load("Rwanda/df_2005.Rda")


col_names <- c("hv105", "Orphanhood", "hv001", "hv002", "hvidx")
df_2019 <- df_2019[, col_names]
df_2014 <- df_2014[, col_names]
df_2010 <- df_2010[, col_names]
df_2005 <- df_2005[, col_names]
val_labels(df_2019) <- NULL
val_labels(df_2014) <- NULL
val_labels(df_2010) <- NULL
val_labels(df_2005) <- NULL


year <- c("Children under 18", "Children aged 0-4 years",
          "Children aged 5-15 years", "Children aged 16-18 years")
df_children <- data.frame(year)
df_children$"2019" <- c(sum(df_2019$hv105<18),
                        sum(df_2019$hv105<5),
                        sum(df_2019$hv105>=5 & df_2019$hv105<16),
                        sum(df_2019$hv105>=16 & df_2019$hv105<18))
df_children$"2014" <- c(sum(df_2014$hv105<18),
                        sum(df_2014$hv105<5),
                        sum(df_2014$hv105>=5 & df_2014$hv105<16),
                        sum(df_2014$hv105>=16 & df_2014$hv105<18))
df_children$"2010" <- c(sum(df_2010$hv105<18),
                        sum(df_2010$hv105<5),
                        sum(df_2010$hv105>=5 & df_2010$hv105<16),
                        sum(df_2010$hv105>=16 & df_2010$hv105<18))
df_children$"2005" <- c(sum(df_2005$hv105<18),
                        sum(df_2005$hv105<5),
                        sum(df_2005$hv105>=5 & df_2005$hv105<16),
                        sum(df_2005$hv105>=16 & df_2005$hv105<18))


df_orphan <- data.frame(year)
df_orphan$"2019" <- c(sum(df_2019$hv105<18 & df_2019$Orphanhood=="orphan"),
                      sum(df_2019$hv105<5 & df_2019$Orphanhood=="orphan"),
                      sum(df_2019$hv105>=5 & df_2019$hv105<16 & df_2019$Orphanhood=="orphan"),
                      sum(df_2019$hv105>=16 & df_2019$hv105<18 & df_2019$Orphanhood=="orphan"))
df_orphan$"2014" <- c(sum(df_2014$hv105<18 & df_2014$Orphanhood=="orphan"),
                      sum(df_2014$hv105<5 & df_2014$Orphanhood=="orphan"),
                      sum(df_2014$hv105>=5 & df_2014$hv105<16 & df_2014$Orphanhood=="orphan"),
                      sum(df_2014$hv105>=16 & df_2014$hv105<18 & df_2014$Orphanhood=="orphan"))
df_orphan$"2010" <- c(sum(df_2010$hv105<18 & df_2010$Orphanhood=="orphan"),
                      sum(df_2010$hv105<5 & df_2010$Orphanhood=="orphan"),
                      sum(df_2010$hv105>=5 & df_2010$hv105<16 & df_2010$Orphanhood=="orphan"),
                      sum(df_2010$hv105>=16 & df_2010$hv105<18 & df_2010$Orphanhood=="orphan"))
df_orphan$"2005" <- c(sum(df_2005$hv105<18 & df_2005$Orphanhood=="orphan"),
                      sum(df_2005$hv105<5 & df_2005$Orphanhood=="orphan"),
                      sum(df_2005$hv105>=5 & df_2005$hv105<16 & df_2005$Orphanhood=="orphan"),
                      sum(df_2005$hv105>=16 & df_2005$hv105<18 & df_2005$Orphanhood=="orphan"))


year <- c("household", "household with orphan")
df_household <- data.frame(year)
df_2019$Orphanhood[df_2019$Orphanhood == "non-orphan"] <- NA
df_household$"2019" <- c(nrow(df_2019 %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df_2019) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df_2014$Orphanhood[df_2014$Orphanhood == "non-orphan"] <- NA
df_household$"2014" <- c(nrow(df_2014 %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df_2014) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df_2010$Orphanhood[df_2010$Orphanhood == "non-orphan"] <- NA
df_household$"2010" <- c(nrow(df_2010 %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df_2010) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df_2005$Orphanhood[df_2005$Orphanhood == "non-orphan"] <- NA
df_household$"2005" <- c(nrow(df_2005 %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df_2005) %>% distinct(hv001, hv002, .keep_all = TRUE)))


df_household$"2019"[2] / df_household$"2019"[1]
df_orphan$"2019"[1] / df_children$"2019"[1]

