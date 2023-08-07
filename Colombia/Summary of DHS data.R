library(rdhs)
library(haven)

set_rdhs_config(email = "zhengxialu@gmail.com",
                project = "Childhood Adverse Effects From Orphanhood",
                config_path = "rdhs.json",
                global = FALSE)

# Colombia has DHS data on 1986, 1990, 1995, 2000, 2005, 2010, 2015

# factors to explore - recode
# orphanhood hv111, hv113
# education hv106, hv110, hv121
# stunted hc70, shsd4 (Ht/A)
# wasted hc72, shds6 (Wt/Ht)
# underweight hc71, shsd5 (Wt/A) 
# (height: sh106, sh107, weight: hc2, sh105, bmi: shsd7, shbm)
# health sh71

# per household data: facilities
# water hv202, hv204
# toilet hv205, hv225
# electricity hv206

# strata
# race
# gender hv104
# ethnicity

# 1986 (household raw)
data1986 <- get_datasets(dataset_filenames = "COHH02FL.zip")
df1986 <- readRDS(data1986$"COHH02FL")
df1986 <- data.frame(df1986)

# 1990 (household member)
data1990 <- get_datasets(dataset_filenames = "COKR22FL.zip")
df1990 <- readRDS(data1990$"COKR22FL")
df1990 <- data.frame(df1990)

# 1995 (household member)
data1995 <- get_datasets(dataset_filenames = "COPR31DT.zip")
df1995 <- readRDS(data1995$"COPR31DT")
df1995 <- data.frame(df1995)

# 2000 (household member)
data2000 <- get_datasets(dataset_filenames = "COPR41FL.zip")
df2000 <- readRDS(data2000$"COPR41FL")
df2000 <- data.frame(df2000)

# 2005 (household member)
data2005 <- get_datasets(dataset_filenames = "COPR53DT.zip")
df2005 <- readRDS(data2005$"COPR53DT")
df2005 <- data.frame(df2005)

# 2010 (household member)
data2010 <- get_datasets(dataset_filenames = "COPR61DT.zip")
df2010 <- readRDS(data2010$"COPR61DT")
df2010 <- data.frame(df2010)

# 2015 (household member)
data2015 <- get_datasets(dataset_filenames = "COPR72DT.zip")
df2015 <- readRDS(data2015$"COPR72DT")
df2015 <- data.frame(df2015)

# search for key words
# library(Hmisc)
# df_label <- label(df2010)
# "Height" %in% df_label
# sum(is.na(df2005$hv227))==nrow(df2005)

# summary

###
year <- c("household", "household with orphan")
df_household <- data.frame(year)
df2000_hd <- df2000_new[,c("hv001","hv002","hvidx","hv105","Orphanhood")] 
df2005_hd <- df2005_new[,c("hv001","hv002","hvidx","hv105","Orphanhood")] 
df2010_hd <- df2010_new[,c("hv001","hv002","hvidx","hv105","Orphanhood")] 
df2015_hd <- df2015_new[,c("hv001","hv002","hvidx","hv105","Orphanhood")] 

library(haven)
library(labelled)
val_labels(df2000_hd) <- NULL
val_labels(df2005_hd) <- NULL
val_labels(df2010_hd) <- NULL
val_labels(df2015_hd) <- NULL

col <- c("Children under 18", "Children aged 0-4 years",
          "Children aged 5-15 years", "Children aged 16-18 years",
         "Orphan under 18", "Orphan aged 0-4 years",
         "Orphan aged 5-15 years", "Orphan aged 16-18 years")
df_data <- data.frame(col)
df_data$"2000" <- c(sum(df2000_hd$hv105<18),
                    sum(df2000_hd$hv105<5),
                    sum(df2000_hd$hv105>=5 & df2000_hd$hv105<16),
                    sum(df2000_hd$hv105>=16 & df2000_hd$hv105<18),
                    sum(df2000_hd$hv105<18 & df2000_hd$Orphanhood=="orphan"),
                    sum(df2000_hd$hv105<5 & df2000_hd$Orphanhood=="orphan"),
                    sum(df2000_hd$hv105>=5 & df2000_hd$hv105<16 & df2000_hd$Orphanhood=="orphan"),
                    sum(df2000_hd$hv105>=16 & df2000_hd$hv105<18 & df2000_hd$Orphanhood=="orphan"))
df_data$"2005" <- c(sum(df2005_hd$hv105<18),
                    sum(df2005_hd$hv105<5),
                    sum(df2005_hd$hv105>=5 & df2005_hd$hv105<16),
                    sum(df2005_hd$hv105>=16 & df2005_hd$hv105<18),
                    sum(df2005_hd$hv105<18 & df2005_hd$Orphanhood=="orphan"),
                    sum(df2005_hd$hv105<5 & df2005_hd$Orphanhood=="orphan"),
                    sum(df2005_hd$hv105>=5 & df2005_hd$hv105<16 & df2005_hd$Orphanhood=="orphan"),
                    sum(df2005_hd$hv105>=16 & df2005_hd$hv105<18 & df2005_hd$Orphanhood=="orphan"))
df_data$"2010" <- c(sum(df2010_hd$hv105<18),
                    sum(df2010_hd$hv105<5),
                    sum(df2010_hd$hv105>=5 & df2010_hd$hv105<16),
                    sum(df2010_hd$hv105>=16 & df2010_hd$hv105<18),
                    sum(df2010_hd$hv105<18 & df2010_hd$Orphanhood=="orphan"),
                    sum(df2010_hd$hv105<5 & df2010_hd$Orphanhood=="orphan"),
                    sum(df2010_hd$hv105>=5 & df2010_hd$hv105<16 & df2010_hd$Orphanhood=="orphan"),
                    sum(df2010_hd$hv105>=16 & df2010_hd$hv105<18 & df2010_hd$Orphanhood=="orphan"))
df_data$"2015" <- c(sum(df2015_hd$hv105<18),
                    sum(df2015_hd$hv105<5),
                    sum(df2015_hd$hv105>=5 & df2015_hd$hv105<16),
                    sum(df2015_hd$hv105>=16 & df2015_hd$hv105<18),
                    sum(df2015_hd$hv105<18 & df2015_hd$Orphanhood=="orphan"),
                    sum(df2015_hd$hv105<5 & df2015_hd$Orphanhood=="orphan"),
                    sum(df2015_hd$hv105>=5 & df2015_hd$hv105<16 & df2015_hd$Orphanhood=="orphan"),
                    sum(df2015_hd$hv105>=16 & df2015_hd$hv105<18 & df2015_hd$Orphanhood=="orphan"))

View(df_data)


df2000_hd$Orphanhood[df2000_hd$Orphanhood == "non-orphan"] <- NA
df_household$"2000" <- c(nrow(df2000_hd %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df2000_hd) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df2005_hd$Orphanhood[df2005_hd$Orphanhood == "non-orphan"] <- NA
df_household$"2005" <- c(nrow(df2005_hd %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df2005_hd) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df2010_hd$Orphanhood[df2010_hd$Orphanhood == "non-orphan"] <- NA
df_household$"2010" <- c(nrow(df2010_hd %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df2010_hd) %>% distinct(hv001, hv002, .keep_all = TRUE)))
df2015_hd$Orphanhood[df2015_hd$Orphanhood == "non-orphan"] <- NA
df_household$"2015" <- c(nrow(df2015_hd %>% distinct(hv001, hv002, .keep_all = TRUE)),
                         nrow(na.omit(df2015_hd) %>% distinct(hv001, hv002, .keep_all = TRUE)))
View(df_household)

