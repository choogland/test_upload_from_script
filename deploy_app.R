library(rsconnect)


rsconnect::setAccountInfo(name='finddx',
                          
                          token='998B92715CD4D3DC973E23E3C9B4BAB5',
                          
                          secret='PPAuOHlojmJoDIlZ1ohW/tqNBn87IcJHASVj01V2')

#SHINY_DIR             <- '/Users/Anna/FIND/BB_Projects/Shinyapps_projects/develop/'
SHINY_DIR             <- 'C:/Users/christine/Documents/FIND/FIND_Cov_19_Tracker/2020-07-08/cleaned_PROD'

#runApp(appDir = SHINY_DIR)



#SHINY_DIR <- getwd()

deployApp(appDir = SHINY_DIR, appName = 'FIND_Cov_19_Tracker')
