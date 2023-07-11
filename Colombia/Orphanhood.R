library(rdhs)

set_rdhs_config(email = "zhengxialu@gmail.com",
                project = "Childhood Adverse Effects From Orphanhood",
                config_path = "rdhs.json",
                global = FALSE)


# getting data
data1 <- get_datasets(dataset_filenames = "COHR72DT.zip")
df1 <- readRDS(data1$"COHR72DT")
df1 <- data.frame(df1)

data2 <- get_datasets(dataset_filenames = "COKR72DT.zip")
df2 <- readRDS(data2$"COKR72DT")
df2 <- data.frame(df2)

# collecting data
df <- df1[, c("v001", "v002")]