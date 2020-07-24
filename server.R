## Covid-2019  test interactive mapping tool
## Foundation for Innovative New Diagnostics

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
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
#if(!require(leaflet)) devtools::install_github('rstudio/leaflet')
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
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

#source("./update_cv_tests.R")
source("./cv_data.R")
source("./lib.R")

current_case_count = sum(cv_today$cases)
current_death_count = sum(cv_today$deaths)
# cv_cases_download <- cv_cases[,c("alpha3","country","date","cases","new_cases","deaths","population","casesPer100k","deathsPer100k","Income group","Continent","Region")]
# 
# cv_cases_download$date <- as.character(cv_cases_download$date)
# cv_cases_download$country <- as.character(cv_cases_download$country)
# cv_cases_download$alpha3 <- as.character(cv_cases_download$alpha3)

# merge cv test data with country data and extract key summary variables
# cv_tests = merge(cv_tests, countries, by = "country")
# cv_tests = cv_tests[order(cv_tests$date),]
# cv_tests$per100k = as.numeric(format(round(cv_tests$tests_cumulative/(cv_tests$population/100000),1),nsmall=1))
# cv_tests$testsPer100k = cv_tests$per100k
current_test_count = sum(cv_tests$new_tests)
# cv_tests_download <- cv_tests[,c("alpha3","country","date","new_tests","tests_cumulative", "population","testsPer100k", "source","Income group","Continent","Region")]
# 
# cv_tests_download$date <- as.character(cv_tests_download$date)
# cv_tests_download$source <- ifelse(is.na(cv_tests_download$source),NA,paste0("<a href='",cv_tests_download$source,"' target='_blank'>",cv_tests_download$source,"</a>"))
# cv_tests_download$country <- as.character(cv_tests_download$country)
# cv_tests_download$alpha3 <- as.character(cv_tests_download$alpha3)

# cols_all <- c("alpha3","country","date","new_tests","tests_cumulative", "testsPer100k", "source","cases","new_cases","deaths","casesPer100k","deathsPer100k", "population","Income group","Continent","Region")
# cv_data_download <- merge(cv_tests_download, cv_cases_download, by = c("country","date","alpha3","population","Income group","Continent","Region"), all = T) 
# cv_data_download <- cv_data_download[,cols_all] %>% arrange(date,country) 

cv_tests_large = cv_tests %>% filter(country %in% country_geoms$countries_present)

cv_tests_country = cv_tests %>% select("alpha3", "country", "date", "tests_cumulative")
cv_cases_country = cv_cases %>% select("alpha3", "date", "cases")
cv_cases_per_tests = join(cv_tests_country, cv_cases_country, by = c("alpha3", "date"), type="inner")
cv_cases_per_tests$cases_per_tests=cv_cases_per_tests$cases/cv_cases_per_tests$tests_cumulative
cv_cases_per_tests = cv_cases_per_tests[!is.na(cv_cases_per_tests$cases_per_tests), ]

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(country %in% country_geoms$countries_present)

# Remove countries without an id
cv_large_countries_withoutid = cv_large_countries$country[!cv_large_countries$alpha3 %in% worldcountry$id]
if (length(cv_large_countries_withoutid) != 0) {
    print("WARNING: countries not found in the list of world countries (these will be removed):")
    print(cv_large_countries_withoutid, max.levels = 0)
    cv_large_countries = subset(cv_large_countries, cv_large_countries$alpha3 %in% worldcountry$id)
}

cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

overlay_cases_label="Number of cases/100k population"
overlay_tests_label="Number of tests/100k population"
overlay_cases_per_tests_label="Number of cases per test"

cumul_label= "Cumulative tests and cases per 100k"
daily_label="Daily tests and cases per 100k"

circle_scaling=5

# creat cv base map
basemap = leaflet(plot_map) %>%
    addTiles() %>%
    addLayersControl(
        position = "bottomright",
        baseGroups = c(overlay_tests_label, overlay_cases_label, overlay_cases_per_tests_label),
        overlayGroups = c(cumul_label, daily_label),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c(overlay_cases_label, overlay_cases_per_tests_label, cumul_label, daily_label)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-50,~80,80)

cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
cv_aggregated_China = aggregate(subset(cv_cases, country=="Mainland China")$cases, by=list(Category=subset(cv_cases, country=="Mainland China")$date), FUN=sum)
cv_aggregated_other = aggregate(subset(cv_cases, country!="Mainland China")$cases, by=list(Category=subset(cv_cases, country!="Mainland China")$date), FUN=sum)
names(cv_aggregated) = names(cv_aggregated_China) = names(cv_aggregated_other) = c("date", "cases")

deaths_aggregated = aggregate(cv_cases$deaths, by=list(Category=cv_cases$date), FUN=sum)
names(deaths_aggregated) = c("date", "cases")

# sum cv test counts by date
cv_test_aggregated = aggregate(cv_tests$new_tests, by = list(Category = cv_tests$date), FUN = sum)
names(cv_test_aggregated) = c('date', 'tests')
cv_test_aggregated$cum_tests <- cumsum(cv_test_aggregated$tests)


cv_aggregated_tp <- cv_aggregated
cv_aggregated_China_tp <- cv_aggregated_China
cv_aggregated_other_tp <- cv_aggregated_other

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) {
    #  message(i)
    if (i == 1) {
        cv_aggregated$new[i] = cv_aggregated_China$new[i] = cv_aggregated_other$new[i] = NA
    } else {
        cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1]
        cv_aggregated_China$new[i] = cv_aggregated_China$cases[i] - cv_aggregated_China$cases[i-1]
        cv_aggregated_other$new[i] = cv_aggregated_other$cases[i] - cv_aggregated_other$cases[i-1]
    }
}

# add variable for tests in last 24 hours
for (i in 1:nrow(cv_test_aggregated)) {
	if (i==1) {
        cv_test_aggregated$new[i] = NA
    } else {
		cv_test_aggregated$new[i] = cv_test_aggregated$cum_tests[i] - cv_test_aggregated$cum_tests[i-1]

	}
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated_China$region = "Mainland China"
cv_aggregated_other$region = "Other"
deaths_aggregated$region = "Deaths"
cv_aggregated = rbind.fill(cv_aggregated, deaths_aggregated, cv_aggregated_China, cv_aggregated_other)
cv_aggregated$region = factor(cv_aggregated$region, levels=c("Global", "Mainland China", "Other", "Deaths"))

# add plotting region for tests
cv_test_aggregated$region = "Global"
cv_test_aggregated$region = factor(cv_test_aggregated$region, levels=c("Global"))

covid_col = "#cd4652"
covid_other_col = "#d3553f"
covid_test_col = '#43abb6'

# assign colours to countries to ensure consistency between plots
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
country_cols = cls[1:length(unique(cv_data$country))]
names(country_cols) = unique(cv_data$country)

# cv_cases <- join(cv_cases, cv_tests[,c('country', 'date', 'new_tests','tests_cumulative', 'testsPer100k')], by = c('country', 'date'))
# cv_today <- subset(cv_cases, date==current_date)

chosen_countries <- c('Italy', 'Mainland China', 'France')

# load epidemic comparison data
epi_comp = as.data.frame(data.table::fread("input_data/epi_comp.csv"))
epi_comp$outbreak = factor(epi_comp$outbreak, levels = epi_comp$outbreak)
epi_comp$cases[1] = current_case_count
epi_comp$deaths[1] = current_death_count
epi_comp$countries[1] = nrow(
    subset(
        cv_today,
        country!="International cruise ship (Japan)" &
        country!="Hong Kong" &
        country!="Taiwan" &
        country!="Macao"
    )
)

epi_comp$cfr[1] = round(epi_comp$deaths[1]/epi_comp$cases[1]*100,1)
epi_comp$cfr = round(epi_comp$cfr,2)

nonzero_min <- function(x) {
    min(x[x>0])
}

geomSeries <- function(base, minimum, maximum) {
    base^(floor(log(minimum, base)):ceiling(log(maximum, base)))
}


server = function(input, output, session) {
    # covid tab
    output$clean_date_reactive <- renderText({
        paste0('Last updated: ', format(as.POSIXct(input$plot_date),"%d %B %Y"))
    })

    reactive_db = reactive({
        cv_cases %>% filter(date == input$plot_date)
    })

    reactive_db_last24h = reactive({
        cv_cases %>% filter(date == input$plot_date)
    })

    reactive_db_test = reactive({
        cv_tests %>% filter(date == input$plot_date)
    })

    reactive_db_cases_per_tests = reactive({
        cv_cases_per_tests %>% filter(date == input$plot_date)
    })
    
    reactive_db_test_last24h = reactive({
        return_value = cv_tests %>% filter(date == input$plot_date & new_tests > 0)
        if (nrow(return_value) == 0) {
            print('WARNING: there seem to be no test results in the last 24 hours, adding an empty row')
            return_value[1,] <- NA
        }
        return_value
    })

    reactive_cv_cases_per_tests_large = reactive({
        large_countries = reactive_db_cases_per_tests() %>% filter(country %in% country_geoms$countries_present)
        large_countries = large_countries[order(large_countries$alpha3),]
        worldcountry_in_large = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
        rownames(large_countries) = as.character(large_countries$alpha3)
        large_countries	= large_countries[worldcountry_in_large$id, ]
        large_countries
    })
    
    reactive_cv_tests_large = reactive({
        large_countries = reactive_db_test() %>% filter(country %in% country_geoms$countries_present)
        large_countries = large_countries[order(large_countries$alpha3),]
        worldcountry_in_large = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
        rownames(large_countries) = as.character(large_countries$alpha3)
        large_countries	= large_countries[worldcountry_in_large$id, ]
        large_countries
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(country %in% country_geoms$countries_present)
        large_countries = large_countries[which(large_countries$alpha3 %in% worldcountry$id), ]
        worldcountry_in_large = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
        rownames(large_countries) = as.character(large_countries$alpha3)
        large_countries	= large_countries[worldcountry_in_large$id, ]
        large_countries
    })
    
    reactive_db_large_last24h = reactive({
        large_countries = reactive_db_last24h() %>% filter(country %in% country_geoms$countries_present)
        large_countries = large_countries[which(large_countries$alpha3 %in% worldcountry$id), ]
        worldcountry_in_large = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
        rownames(large_countries) = as.character(large_countries$alpha3)
        large_countries	= large_countries[worldcountry_in_large$id, ]
        large_countries
    })
    
    reactive_tests_polygons = reactive({
        worldcountry[worldcountry$id %in% reactive_cv_tests_large()$alpha3, ]
    })
    
    reactive_cases_per_tests_polygons = reactive({
        worldcountry[worldcountry$id %in% reactive_cv_cases_per_tests_large()$alpha3, ]
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
    })

    reactive_polygons_last24h = reactive({
        worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
    })

    output$reactive_case_count <- renderText({
        paste0(format(sum(reactive_db()$cases), big.mark=","), " COVID-19 positive tests")
    })

    output$reactive_death_count <- renderText({
        paste0(format(sum(reactive_db()$deaths), big.mark=","), " Deaths attributed to COVID-19")
    })

    output$reactive_test_count_simple <- renderText({
        paste0(format((cv_test_aggregated %>% filter(date == input$plot_date & region=="Global"))$cum_tests, big.mark=","), " SARS-CoV-2 tests performed")
    })

    output$reactive_test_count <- renderText({
        paste0(
            "Global # of tests performed: ",
            format(
                (cv_test_aggregated %>% filter(date == input$plot_date & region=="Global"))$cum_tests,
                " (",
                (cv_test_aggregated %>% filter(date == input$plot_date & region=="Global"))$new,
                big.mark=","
            ),
            " new)"
        )
    })

    output$reactive_case_count_China <- renderText({
        paste0(
            "Mainland China: ",
            sum(subset(reactive_db(), country=="Mainland China")$cases),
            " (",
            (cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new,
            " new)"
        )
    })

    output$reactive_case_count_Global <- renderText({
        paste0(
            "Global # of cases: ",
            sum(reactive_db()$cases),
            " (",
            (cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new,
            " new)"
        )
    })

    output$reactive_case_count_row <- renderText({
        paste0(
            "Other: ",
            sum(subset(reactive_db(), country!="Mainland China")$cases),
            " (",
            (cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new,
            " new)"
        )
    })

    output$reactive_country_count <- renderText({
        paste0(
            'Case data available for ',
            length(intersect(unique(reactive_db()$country),UN_list)),
            "/195 countries"
        )
    })

    output$reactive_test_country_count <- renderText({
        
        paste0(
            'Diagnostic data available for ',
            length(intersect(unique(reactive_db_test()$country),UN_list)),
            "/195 countries"
        )
    })

    output$reactive_new_cases_24h <- renderText({
        paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
    })

    output$reactive_new_tests_24h <- renderText({
        paste0((cv_test_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
    })

    output$mymap <- renderLeaflet({
        basemap
    })
    
    observeEvent(input$mymap_groups,{
        #TODO these 6 lines are duplicate with lines below, should be put in 1 place
        bins_tests <- unique(quantile(reactive_cv_tests_large()$testsPer100k, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_tests <- colorBin(palette=fc_tests(10), domain=reactive_cv_tests_large()$testsPer100k, bins=bins_tests, pretty = TRUE)
        bins_cases <- unique(quantile(reactive_db_large()$casesPer100k, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_cases <- colorBin(palette=fc_cases(10), domain=reactive_db_large()$casesPer100k, bins=bins_cases, pretty = TRUE)
        bins_cases_per_tests <- unique(quantile(reactive_cv_cases_per_tests_large()$cases_per_tests, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_cases_per_tests <- colorBin(palette=fc_tests(10), domain=reactive_cv_cases_per_tests_large()$cases_per_tests, bins=bins_cases_per_tests, pretty = TRUE)
        leafletProxy('mymap') %>% 
            removeControl(layerId = "tests_legend") %>% 
            removeControl(layerId = "cases_per_tests_legend") %>%
            removeControl(layerId = "cases_legend") 
        if (overlay_tests_label %in% isolate(input$mymap_groups)){
            leafletProxy('mymap') %>%
                addLegend("bottomright", 
                          pal = cv_pal_tests, 
                          values = reactive_cv_tests_large()$testsPer100k,
                          title="Tests per 100k population",
                          layerId="tests_legend"
                )
        }
        else if (overlay_cases_label %in% isolate(input$mymap_groups)) {
            leafletProxy('mymap') %>%
                addLegend("bottomright", 
                          pal = cv_pal_cases, 
                          values = reactive_db_large()$casesPer100k,
                          title="Cases per 100k population",
                          layerId="cases_legend"
                )
        }
        else if (overlay_cases_per_tests_label %in% isolate(input$mymap_groups)) {
            leafletProxy('mymap') %>%
                addLegend("bottomright", 
                          pal = cv_pal_cases_per_tests, 
                          values = reactive_cv_cases_per_tests_large()$cases_per_tests,
                          title="Cases per test",
                          layerId="cases_per_tests_legend"
                )
        }
    }) 
    
    observeEvent(input$plot_date , {
        bins_tests <- unique(quantile(reactive_cv_tests_large()$testsPer100k, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_tests <- colorBin(palette=fc_tests(10), domain=reactive_cv_tests_large()$testsPer100k, bins=bins_tests, pretty = TRUE)
        bins_cases <- unique(quantile(reactive_db_large()$casesPer100k, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_cases <- colorBin(palette=fc_cases(10), domain=reactive_db_large()$casesPer100k, bins=bins_cases, pretty = TRUE)
        bins_cases_per_tests <- unique(quantile(reactive_cv_cases_per_tests_large()$cases_per_tests, na.rm =T, probs = c(seq(0,1,0.1))))
        cv_pal_cases_per_tests <- colorBin(palette=fc_tests(10), domain=reactive_cv_cases_per_tests_large()$cases_per_tests, bins=bins_cases_per_tests, pretty = TRUE)
        leafletProxy("mymap") %>%
            clearShapes() %>%
            clearMarkers() %>%
            addPolygons(
                data = reactive_tests_polygons(),
                stroke = FALSE,
                smoothFactor = 0.8,
                fillOpacity = 0.5,
                color = cv_pal_tests(reactive_cv_tests_large()$testsPer100k),
                group = overlay_tests_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Tests per 100k population: %g",
                    reactive_cv_tests_large()$country,
                    reactive_cv_tests_large()$testsPer100k
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_test_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = reactive_cases_per_tests_polygons(),
                stroke = FALSE,
                smoothFactor = 0.8,
                fillOpacity = 0.5,
                color = cv_pal_cases_per_tests(reactive_cv_cases_per_tests_large()$cases_per_tests),
                group = overlay_cases_per_tests_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Cases per test: %g<br/>Cumulative cases: %s<br/>Cumulative tests: %s",
                    reactive_cv_cases_per_tests_large()$country,
                    reactive_cv_cases_per_tests_large()$cases_per_tests,
                    reactive_cv_cases_per_tests_large()$cases,
                    reactive_cv_cases_per_tests_large()$tests_cumulative
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_test_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = reactive_polygons(),
                stroke = FALSE,
                smoothFactor = 0.8,
                fillOpacity = 0.5,
                color = cv_pal_cases(reactive_db_large()$casesPer100k),
                group = overlay_cases_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Cases per 100k population: %g",
                    reactive_db_large()$country,
                    reactive_db_large()$casesPer100k
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addCircleMarkers(
                data = reactive_db_test(),
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = ~log10(1+tests_cumulative)*circle_scaling,
                fillOpacity = 0.4,
                color = covid_test_col,
                group = cumul_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Cumulative tests: %s<br/>Cumulative tests per 100k population: %g",
                    reactive_db_test()$country,
                    format(reactive_db_test()$tests_cumulative, big.mark=","),
                    reactive_db_test()$testsPer100k
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_test_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addCircleMarkers(
                data = reactive_db_test_last24h(),
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = ~log10(1+new_tests)*circle_scaling,
                fillOpacity = 0.4,
                color = covid_test_col,
                group = daily_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Daily tests: %s<br/>Daily tests per 100k population: %g",
                    reactive_db_test_last24h()$country,
                    format(reactive_db_test_last24h()$new_tests, big.mark=","),
                    reactive_db_test_last24h()$testsPer100k
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_test_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addCircleMarkers(
                data = reactive_db(),
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = ~log10(1+cases)*circle_scaling,
                fillOpacity = 0.4,
                color = covid_col,
                group = cumul_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Cumlative cases: %s<br/>Cumulative deaths: %s<br/>Cumulative cases per 100k population: %g",
                    reactive_db()$country,
                    format(reactive_db()$cases, big.mark=","),
                    format(reactive_db()$deaths, big.mark=","),
                    reactive_db()$casesPer100k
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_col
                    ),
                    textsize = "15px", direction = "auto"
                )
            ) %>%
            addCircleMarkers(
                data = reactive_db_last24h(),
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = ~log10(1+new_cases)*circle_scaling,
                fillOpacity = 0.4,
                color = covid_col,
                group = daily_label,
                label = sprintf(
                    "<strong>%s</strong><br/>Daily cases: %s<br/>Daily deaths: %s",
                    reactive_db_last24h()$country,
                    format(reactive_db_last24h()$new_cases, big.mark=","),
                    format(reactive_db_last24h()$new_deaths, big.mark=",")
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px",
                        "color" = covid_col
                    ),
                    textsize = "15px",
                    direction = "auto"
                )
            )
        
    })

    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, input$plot_date, current_case_count)
    })

    output$epi_curve <- renderPlot({
        new_cases_plot(cv_aggregated, input$plot_date)
    })

    output$cumulative_test_plot <- renderPlot({
        cumulative_test_plot(cv_test_aggregated, input$plot_date, current_test_count)
    })

    output$new_tests_plot <- renderPlot({
        new_tests_plot(cv_test_aggregated, input$plot_date)
    })

    country_select_react = reactive({
        get_country_select_options(
            input$country_select_order,
            countries,
            cv_today
        )
    })
    
    observe({
        selected_countries <- input$country_select
        updatePickerInput(
            session = session,
            inputId = "country_select",
            choices = country_select_react(),
            selected = selected_countries
        )
    })
    
    country_select_bubble_react = reactive({
        get_country_select_options(
            input$country_select_bubble_order,
            countries,
            cv_today
        )
    })
    
    observe({
        selected_countries <- input$country_select_bubble
        updatePickerInput(
            session = session,
            inputId = "country_select_bubble",
            choices = country_select_bubble_react(),
            selected = selected_countries
        )
    })
    
    # create dataframe with selected countries


    country_reactive_db = reactive({
        if (input$outcome_select=="Cases") {
            cv_cases$outcome = cv_data$cases
            cv_cases$new_outcome = cv_data$new_cases
            cv_cases$label_name = "cases"
        }
        if (input$outcome_select=="Deaths") {
            cv_cases$outcome = cv_data$deaths
            cv_cases$new_outcome = cv_data$new_deaths
            cv_cases$label_name = "deaths"
        }
        if (input$outcome_select=="Tests") {
            cv_cases$outcome = cv_data$tests_cumulative
            cv_cases$new_outcome = cv_data$new_tests
            cv_cases$label_name = "tests"
        }
        if (input$outcome_select=="Tests/100k") {
            cv_cases$outcome = cv_data$testsPer100k
            #cv_cases$new_outcome = cv_cases$testsPer100k
            cv_cases$label_name = "tests/100k"
        }
        if (input$outcome_select=="Cases/Tests") {
            cv_cases$outcome = log10((cv_data$cases/cv_data$tests_cumulative) + 10)
            cv_cases$new_outcome = log10((cv_data$new_cases/cv_data$new_tests) + 10)
            cv_cases$label_name = "cases/tests (log10 + 10)"
        }
        if(input$outcome_select=="Deaths/Tests"){
            cv_cases$outcome = log10((cv_data$deaths/cv_data$tests_cumulative) + 10)
            cv_cases$new_outcome = log10((cv_data$new_deaths/cv_data$new_tests) + 10)
            cv_cases$label_name = "deaths/tests (log10 + 10)"
        }
        cv_cases %>% filter(country %in% input$country_select)

    })

    # country-specific plots
    output$country_plot <- renderPlotly({
        data <- country_reactive_db()
        if (nrow(data)>0) {
            if (!input$outcome_select %in% c("Cases/Tests", "Deaths/Tests", "Tests/100k")) {
                country_cases_plot(data, country_cols)
            } else {
                country_cases_plot_empty('Not Available \n Only Cumulative available')
            }
        } else {
            country_cases_plot_empty('Please select a country.')
        }
    })

    output$country_plot_cumulative <- renderPlotly({
        data <- country_reactive_db()
        if (nrow(data)>0) {
            country_cases_cumulative(data, country_cols)
        } else {
            country_cases_plot_empty('Please select a country.')
        }
    })
    
    # bubble plot
    bubbleDataset <- reactive({
        #data = join(cv_cases, cv_tests[,c("country", "date")], by = c("country", "date"))

        cv_data %>% filter(country %in% input$country_select_bubble) %>% filter(date == input$bubble_plot_date)
    })

    bubbleDatasetLatest <- reactive({
        #data = join(cv_cases, cv_tests[,c("country", "date")], by = c("country", "date"))

        cv_data %>% filter(country %in% input$country_select_bubble) %>% filter(date == current_date)
    })

    output$bubble_plot <- renderPlotly({
        data <- bubbleDataset()
        dataLatest <- bubbleDatasetLatest()
        if (nrow(data)>0&nrow(dataLatest)>0) {
            bubble_plot(data, dataLatest, country_cols)
        } else {
            country_cases_plot_empty('Please select a country.')
        }
    })

    output$source_info <- renderText({
        switch(
            input$dataset,
            "Cases" = "<h4>Cases dataset is downloaded once a day from the <a href=\"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series\" target=\"_blank\">Johns Hopkins Center for Systems Science and Engineering github page</a>.</h4>",
            "Tests" = "<h4>Tests dataset is compiled once a day from reports published across health department websites, statistical reports and press releases, as stated in source column below.<br><i>Please note that depending on the country, the number of tests may refer to the number of people tested or the number of samples tested</i>.</h4>",
            "Tests+Cases" = "<h4>Tests dataset is compiled once a day from reports published across health department websites, statistical reports and press releases, as stated in source column below.<br><i>Please note that depending on the country, the number of tests may refer to the number of people tested or the number of samples tested</i>.</h4>\n
            <h4>Cases dataset is downloaded once a day from the <a href=\"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series\" target=\"_blank\">Johns Hopkins Center for Systems Science and Engineering github page</a>.</h4>"
        )
    })

    output$downloadFull <- renderText({
        #appName <- basename(Sys.getenv('PWD'))
        fileUrl <- paste0("downloads/",switch(
            input$dataset,
            "Cases" = "cv_cases_download.csv",
            "Tests" = "cv_tests_download.csv",
            "Tests+Cases" = "cv_data_download.csv"))
        jsonUrl <- gsub(".csv",".json",fileUrl)
        paste0("<h4>Download the full dataset as <a href=\"",fileUrl,"\">CSV</a> or <a href=\"",jsonUrl,"\">JSON</a></h4>")
    })
    
    observeEvent(input$link_to_src_data, {
        updateTabItems(session, "nav", "Source data")
    })
    
    # Download the data
    datasetInput <- reactive({
        switch(
            input$dataset,
            "Cases" = cv_cases_download,
            "Tests" = cv_tests_download,
            "Tests+Cases" = cv_data_download
        )
    })

    # datasetInputCols <- reactive({
    #     switch(
    #         input$dataset,
    #         "Cases" = c("country","date","cases","new_cases","deaths","population","casesPer100k","deathsPer100k"),
    #         "Tests" = c("country","date","new_tests","tests_cumulative", "population","testsPer100k", "source")
    #     )
    # })

    # Table of selected dataset ----
    output$table <- DT::renderDT({
        data <- datasetInput()
        if (any(grepl("source",names(data)))) {
            data$source <- ifelse(is.na(data$source),NA,paste0("<a href='",data$source,"' target='_blank'>",data$source,"</a>"))
        }
        datatable(
            data,#[,datasetInputCols()],
            filter="top", 
            rownames = FALSE,
            extensions = 'Buttons', 
            escape = FALSE,
            options = list(
                columnDefs = list(list(targets=c(0), visible=FALSE)),
                pageLength = 5, lengthMenu = list(c(5, 10, 25, 50, 100, -1),c(5, 10, 25, 50, 100, "All")), 
                searchHighlight = TRUE, 
                caseInsensitive = T, 
                scrollX = TRUE,
                dom = 'Bfltip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
    }, server = TRUE)


    # Downloadable csv of selected dataset ----
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         paste(input$dataset, ".csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(datasetInput(), file, row.names = FALSE)
    #     }
    # )
}
