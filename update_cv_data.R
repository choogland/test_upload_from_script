#rm(list = ls())

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")


# Load the Dropbox access info
load(file = paste0("input_data/token_Covid19.rds"))

DB_dir <- 'FIND_Cov_19_Tracker/input_data'
#DB_dir <- 'FIND_Cov_19_Tracker/test_CH'
DB_filelist <- drop_dir(DB_dir, dtoken = token) %>% filter(., grepl("coronavirus_tests_[0-9]{8}_sources_SO.csv",`name`)) %>% arrange(., desc(`client_modified`))

last_updated_tests_filename <- DB_filelist$name[1]

last_upd_done <- as.POSIXlt(file.info('input_data/coronavirus_tests.csv')$mtime)
last_upd_DB <- as.POSIXlt(DB_filelist$server_modified[1],format="%Y-%m-%dT%H:%M")

is_local_or_test <- Sys.getenv('SHINY_PORT')=="" | str_detect(Sys.getenv('PWD'),"_TEST")

source("./cv_data.R")

prepare_downloadable_data = FALSE
an_error_occured <- FALSE

#if a newer file is found, do the update
if (last_upd_DB > last_upd_done) {
  message("Updating with ",last_updated_tests_filename,"...")
  while (TRUE){
    #get new file (either comma or semicolon should be OK)
    cv_tests <- drop_read_csv(file = paste(DB_dir,last_updated_tests_filename,sep="/"), dtoken = token, sep = ";", stringsAsFactors = F)
    if (ncol(cv_tests)<7) {
      cv_tests <- drop_read_csv(file = paste(DB_dir,last_updated_tests_filename,sep="/"), dtoken = token, sep = ",", stringsAsFactors = F)
      if (ncol(cv_tests)<7) {
        an_error_occured <- TRUE
        warning(paste0("Error: columns number in file ",last_updated_tests_filename," not as expected!"))
        break
      }
    }
    #tests file is ok, go on with the update
    cv_tests$date <- as.Date(cv_tests$date, tryFormats = c("%d.%m.%y", "%d/%m/%Y", "%Y/%m/%d", "%Y-%m-%d"))
  
    #be sure no NA in new_tests field
    cv_tests$new_tests <- ifelse(is.na(cv_tests$new_tests),0,cv_tests$new_tests)
    message("total:",sum(cv_tests$new_tests))
  
    cv_tests$tests_cumulative <- as.numeric(cv_tests$tests_cumulative)
    if (any(is.na(cv_tests$tests_cumulative))) {
      id <- which(is.na(cv_tests$tests_cumulative))
      if (length(id)==1 & cv_tests[id,"country"]=="Malta" & cv_tests[id,"date"] == "2020-07-05") {
        message("warning: replace Malta number...")
        cv_tests[id,"tests_cumulative"] <- 100000 #replace 1,00E+05-this error is back every day!!
      } else {
        an_error_occured <- TRUE
        warning(paste0("Error: incorrect number formats - ", unlist(id), collapse = "\n"))
        break
      }
    }
    
    #prevent issues in DT with non ascii characters in URL
    cv_tests$source <- iconv(cv_tests$source, from = 'ISO8859-1', to = 'UTF-8')
    break
  }

  if (!an_error_occured) {
    #update cases data
    source("jhu_data_full.R")
    
    # import data
    cv_cases = read.csv("input_data/coronavirus.csv")
    countries = read.csv("input_data/countries_codes_and_coordinates_income.csv")
    
    # extract time stamp from cv_cases
    update = tail(cv_cases$last_update,1) 
    
    # check consistency of country names across datasets
    countries_without_coordinates = setdiff(unique(cv_tests$country),unique(countries$country))
    
    if (length(countries_without_coordinates) > 0)
      print(paste0("Error: mapping data lacking for the following countries: ", countries_without_coordinates))
    
    # extract dates from cv data
    cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d")
    
    # idx_slash_4  <- grep('\\/\\d{4}$', cv_tests[,'date'])
    # idx_slash_2  <- grep('\\/\\d{2}', cv_tests[,'date'])
    # idx_dot_4 <- grep('\\.\\d{4}$', cv_tests[,'date'])
    # idx_dot_2 <- grep('\\.\\d{2}$', cv_tests[,'date'])
    # idx_num <- which(!is.na(as.numeric(cv_tests[,'date'])))
    # idx_dash <- grep('-', cv_tests[,'date'])
    # d_tp  <- as.Date(rep(NA, nrow(cv_tests)))
    # d_tp[idx_num]<- as.Date(as.numeric(cv_tests[idx_num,'date']), origin="1899-12-30")
    # d_tp[idx_slash_4] <- as.Date(cv_tests[idx_slash_4,'date'], format = "%d/%m/%Y")
    # d_tp[idx_slash_2] <- as.Date(cv_tests[idx_slash_2,'date'], format = "%d/%m/%y")
    # d_tp[idx_dot_4]  <- as.Date(cv_tests[idx_dot_4,'date'], format = "%d.%m.%Y")
    # d_tp[idx_dot_2]  <- as.Date(cv_tests[idx_dot_2,'date'], format = "%d.%m.%y")
    # d_tp[idx_dash] <- as.Date(cv_tests[idx_dash,'date'], format = "%Y-%m-%d")
    # cv_tests[,'date'] <- as.Date(as.Date(d_tp))
    
    cv_cases_min_date <- min(cv_cases$date)
    cv_tests_min_date <- min(cv_tests$date)
    cv_min_date = min(c(cv_cases_min_date, cv_tests_min_date))
    
    cv_cases_max_date <- max(cv_cases$date)
    cv_tests_max_date <- max(cv_tests$date)
    current_date = max(c(cv_cases_max_date, cv_tests_max_date))
    cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")
    
    
    cv_tests_summ <- plyr::ddply(cv_tests, .(country), summarise,
                                 max_date = max(date),
                                 last_tests_cum = max(tests_cumulative),
                                 jhu_ID = paste(unique(jhu_ID), collapse = ','))
    
    cv_tests_summ_missing <- subset(cv_tests_summ, max_date < current_date)
    
    cv_tests_added <- lapply(unique(cv_tests_summ_missing$country), function(x){
      message(x)
      df <- subset(cv_tests_summ, country == x)
      dates <- seq(ymd(df$max_date) + 1 ,ymd(current_date), by = 'day')
      df_add <- data.frame(ind = '', country = df$country, date = dates,
                           new_tests = rep(0, length(dates)), tests_cumulative = rep(df$last_tests_cum, length(dates)),
                           jhu_ID = df$jhu_ID)
    })
    
    cv_tests_added <- unique(do.call(rbind, cv_tests_added))
    
    if (!is.null(cv_tests_added)){
      cv_tests <- unique(rbind.fill(cv_tests, cv_tests_added))
    }
    write.table(cv_tests, "input_data/coronavirus_tests.csv", col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
    write.table(cv_tests, "www/downloads/coronavirus_tests.csv", col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
    
    prepare_downloadable_data = TRUE
    
  }
  
} else {
#in case tests data were ready before cases data
  if (max(cv_cases$date) < max(cv_tests$date)) {
    source("jhu_data_full.R")

    prepare_downloadable_data = TRUE
  }
}

if (prepare_downloadable_data) {
  source("./cv_data.R")
  
  cv_cases_download <- cv_cases[,c("alpha3","country","date","cases","new_cases","deaths","population","casesPer100k","deathsPer100k","Income group","Continent","Region")]
  filename <- "input_data/cv_cases_download.csv"
  write.table(cv_cases_download, filename, col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  write.table(cv_cases_download, "www/downloads/cv_cases_download.csv", col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  exportJSON <- jsonlite::toJSON(cv_cases_download, pretty = T)
  write(exportJSON, "www/downloads/cv_cases_download.json")
  
  cv_tests_download <- cv_tests[,c("alpha3","country","date","new_tests","tests_cumulative", "population","testsPer100k", "source","Income group","Continent","Region")]
  filename <- "input_data/cv_tests_download.csv"
  write.table(cv_tests_download, filename, col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  write.table(cv_tests_download, "www/downloads/cv_tests_download.csv", col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  exportJSON <- jsonlite::toJSON(cv_tests_download, pretty = T)
  write(exportJSON, "www/downloads/cv_tests_download.json")
  
  cols_all <- c("alpha3","country","date","new_tests","tests_cumulative", "testsPer100k", "source","cases","new_cases","deaths","casesPer100k","deathsPer100k", "population","Income group","Continent","Region")
  cv_data_download <- merge(cv_tests_download, cv_cases_download, by = c("country","date","alpha3","population","Income group","Continent","Region"), all = T) 
  cv_data_download <- cv_data_download[,cols_all] %>% arrange(date,country) 
  filename <- "input_data/cv_data_download.csv"
  write.table(cv_data_download, filename, col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  write.table(cv_data_download, "www/downloads/cv_data_download.csv", col.names = T, row.names = F, sep = ',',fileEncoding = "UTF-8")
  exportJSON <- jsonlite::toJSON(cv_data_download, pretty = T)
  write(exportJSON, "www/downloads/cv_data_download.json")
  
}

if (an_error_occured) {
  message("No update was done!")
} else {
  message("Up to date!")
}

