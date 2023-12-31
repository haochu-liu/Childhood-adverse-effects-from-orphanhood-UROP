library(rdhs)


# setup
# set_rdhs_config(email = "hl3720@ic.ac.uk",
#                 project = "Childhood adverse effects from orphanhood",
#                 config_path = "rdhs.json",
#                 global = FALSE)


# examples
dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields=c("IndicatorId", "ShortName"))

dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
         returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))


# country list
dhs_countries(returnFields = c("CountryName", "DHS_CountryCode"))
# RW  Rwanda

# survey list
survs <- dhs_surveys(countryIds = c("RW"))
survs

# get one survey
datasets <- dhs_datasets(surveyIds = "RW2019DHS")
datasets

# download datasets
downloads <- get_datasets("RWKR81SV.ZIP")
downloads$RWKR81SV

# load datasets into R
df1 <- readRDS(downloads$RWKR81SV)
df1 <- data.frame(df1)

# some basic data cleaning
print(dim(df1))
colname_list <- colnames(df1)
na_num <- list()
col_label_list <- list()
for (i in 1:ncol(df1)) {
  na_num[i] <- sum(is.na(df1[, i]))
  col_label_list[i] <- attr(df1[[colname_list[i]]], "label")
}
names(na_num) <- colname_list
names(col_label_list) <- colname_list

# search and extract
vars <- search_variable_labels(datasets$FileName, search_terms = "Total children ever born")
vars

vars <- search_variables(datasets$FileName, variables = c("v201"))
vars

extract <- extract_dhs(vars, add_geo = FALSE)
extract


