library(rdhs)


# examples
dhs_indicators(indicatorIds = "ML_PMAL_C_RDT", returnFields=c("IndicatorId", "ShortName"))

dhs_data(countryIds = c("CD","TZ"), indicatorIds = "ML_PMAL_C_RDT", surveyYearStart = 2013,
         returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))



# country list
dhs_countries(returnFields = c("CountryName", "DHS_CountryCode"))
# RW  Rwanda


