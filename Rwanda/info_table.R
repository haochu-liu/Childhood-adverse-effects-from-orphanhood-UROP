library(rdhs)


# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs


# get dataframes
# 2019
download_2019 <- get_datasets("RWPR81DT.ZIP")
df_2019 <- readRDS(download_2019$RWPR81DT)
df_2019 <- data.frame(df_2019)
# 2017
download_2017 <- get_datasets("RWPR7ADT.ZIP")
df_2017 <- readRDS(download_2017$RWPR7ADT)
df_2017 <- data.frame(df_2017)
# 2014
download_2014 <- get_datasets("RWPR70DT.ZIP")
df_2014 <- readRDS(download_2014$RWPR70DT)
df_2014 <- data.frame(df_2014)
# 2013
download_2013 <- get_datasets("RWPR6IDT.ZIP")
df_2013 <- readRDS(download_2013$RWPR6IDT)
df_2013 <- data.frame(df_2013)
# 2010
download_2010 <- get_datasets("RWPR61DT.ZIP")
df_2010 <- readRDS(download_2010$RWPR61DT)
df_2010 <- data.frame(df_2010)
# 2007
download_2007 <- get_datasets("RWPR5ADT.ZIP")
df_2007 <- readRDS(download_2007$RWPR5ADT)
df_2007 <- data.frame(df_2007)
# 2005
download_2005 <- get_datasets("RWPR53DT.ZIP")
df_2005 <- readRDS(download_2005$RWPR53DT)
df_2005 <- data.frame(df_2005)
# 2000
download_2000 <- get_datasets("RWPR41DT.ZIP")
df_2000 <- readRDS(download_2000$RWPR41DT)
df_2000 <- data.frame(df_2000)
# 1992
download_1992 <- get_datasets("RWPR21DT.ZIP")
df_1992 <- readRDS(download_1992$RWPR21DT)
df_1992 <- data.frame(df_1992)










