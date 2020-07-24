# set mapping colour for each outbreak
covid_col = "#cd4652"
covid_other_col = "#d3553f"
covid_test_col = '#43abb6'

# TO BE ADAPTED
#setwd('/Users/aurelienmace/Documents/FIND/Projects/DM/R_scripts/script/nCoV_test_tracker')

# update data with automated script
#source("jhu_data_full.R")
library(geojsonio)

# Load the Dropbox access info
load(file = paste0("input_data/token_Covid19.rds"))

# import data
countries = read.csv("input_data/countries_codes_and_coordinates_income.csv", check.names = F)
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv", stringsAsFactors=FALSE)
country_geoms$countries_present[country_geoms$countries_present=="Russian Federation"] = "Russia"
UN_list <- read.csv("input_data/UN_list.csv",sep=";",stringsAsFactors = F)[,1]

cv_cases = read.csv("input_data/coronavirus.csv")
cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d")

cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$casesPer100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$deathsPer100k = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/100000),1),nsmall=1))

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1)

cv_tests <- read.csv("input_data/coronavirus_tests.csv", encoding = "UTF-8")
cv_tests$date = as.Date(cv_tests$date, format="%Y-%m-%d") #as.Date(cv_tests$date, format="%d.%m.%y")

cv_tests = merge(cv_tests, countries, by = "country")
cv_tests = cv_tests[order(cv_tests$date),]
cv_tests$testsPer100k = as.numeric(format(round(cv_tests$tests_cumulative/(cv_tests$population/100000),1),nsmall=1))


# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) {
    print("Error: inconsistent country names")
}

# extract dates from cv data
cv_min_date = min(c(cv_cases$date))#, cv_tests$date))
current_date = max(c(cv_cases$date))#, cv_tests$date))
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

selected_countries <- c('Italy', 'France', 'Republic of Korea', 'USA', 'Spain', 'Germany', 'Mainland China', 'UK', 'Switzerland')

cv_data <- join(cv_cases, cv_tests[,c('country', 'date', 'new_tests','tests_cumulative', 'testsPer100k', 'Continent', 'Income group')], by = c('country', 'date', 'Continent', 'Income group'))
cv_data$country <- as.character(cv_data$country)
cv_today <- subset(cv_data, date==current_date)

cv_cases_download = read.csv("input_data/cv_cases_download.csv")
cv_cases_download$date <- as.character(cv_cases_download$date)
cv_cases_download$country <- as.character(cv_cases_download$country)
cv_cases_download$alpha3 <- as.character(cv_cases_download$alpha3)

cv_tests_download = read.csv("input_data/cv_tests_download.csv")
cv_tests_download$date <- as.character(cv_tests_download$date)
cv_tests_download$country <- as.character(cv_tests_download$country)
cv_tests_download$alpha3 <- as.character(cv_tests_download$alpha3)

cv_data_download = read.csv("input_data/cv_data_download.csv")
