library(rdhs)


# setup
set_rdhs_config(email = "hl3720@ic.ac.uk",
                project = "Childhood adverse effects from orphanhood",
                config_path = "rdhs.json",
                global = FALSE)


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
downloads <- get_datasets("RWMR81.ZIP")
downloads$RWKR21SV

# load datasets into R
df1 <- readRDS(downloads$RWKR21SV)



