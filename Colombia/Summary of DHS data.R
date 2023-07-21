library(rdhs)

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

# 1995 (household member)colnam
data1995 <- get_datasets(dataset_filenames = "COPR31DT.zip")
df1995 <- readRDS(data1995$"COPR31DT")
df1995 <- data.frame(df1995)



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

