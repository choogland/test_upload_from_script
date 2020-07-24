library(shiny)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinythemes)
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")

source("./update_cv_data.R")
source("./cv_data.R")

ui <- navbarPage(theme = shinytheme("spacelab"), collapsible = TRUE,
        windowTitle = "COVID-19 cases and tests tracker",
        title = tagList(tags$head(tags$link(rel="shortcut icon", href="favicon.jpg", type="image/png"),includeCSS("styles.css")),
                      tags$a(href='https://www.finddx.org/',tags$img(class='logo',src="FIND_Horizontal+signature_RGB.png",height='32',width='90'), target="_blank")),
        id="nav",
        tabPanel("World view",
                      div(class="outer",
                          leafletOutput("mymap", width="100%", height="100%"),
                          tags$style(".leaflet .legend {line-height: 10px;font-size: 10px;}"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 80, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h4(textOutput("reactive_test_count_simple"), align = "right"),
                                        h4(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
											                  h6(textOutput("reactive_test_country_count"), align = "right"),
											                  h6(textOutput("reactive_country_count"), align = "right"),
                  											sliderInput("plot_date",
                  													label = h5("Select mapping date"),
                  													min = cv_min_date+2,
                  													max = as.Date(current_date,"%Y-%m-%d"),
                  													value = as.Date(current_date),
                  													timeFormat = "%d %b",
                  													animate=animationOptions(interval = 2000, loop = FALSE)
                  													),
											                  withSpinner(plotOutput("cumulative_plot", height="130px", width="100%")),
											                  withSpinner(plotOutput("cumulative_test_plot", height="130px", width="100%")))
                      )
                 ),

                tabPanel("Country data",

                         sidebarLayout(
                           sidebarPanel(

                             strong("Select outcome and countries from drop-down menues to update plots:"),br(),br(),
                             pickerInput("outcome_select", "Outcome:",
                                         choices = c("Cases", "Tests", "Tests/100k", "Cases/Tests", "Deaths/Tests"),
                                         #options = list(`actions-box` = TRUE),
                                         selected = c("Tests/100k"),
                                         multiple = FALSE),

                             pickerInput("country_select", "Country:",
                                         choices = as.character(cv_today[order(-cv_today$cases),]$country),
                                         options = list(`actions-box` = TRUE),
                                         selected = selected_countries,
                                         multiple = TRUE),
                             
                             pickerInput("country_select_order", "Country Order:",
                                         choices = c(
                                           "Total Tests",
                                           "Tests/100k",
                                           "Total Cases",
                                           "Cases/100k",
                                           "Alphabetical",
                                           "Population"
                                         ),
                                         selected = c("Total Cases"),
                                         multiple = FALSE)
                           ),

                           mainPanel(
                             tabsetPanel(
                               tabPanel("Cumulative", withSpinner(plotlyOutput("country_plot_cumulative"))),
                               tabPanel("Daily", withSpinner(plotlyOutput("country_plot")))
                             )
                           )
                         )
                ),
                tabPanel("Testing",
                    sidebarLayout(
                        sidebarPanel(
                             pickerInput(
                                 "country_select_bubble",
                                 "Countries:",
                                 choices = as.character(cv_today[order(-cv_today$cases),]$country),
                                 options = list(`actions-box` = TRUE),
                                 selected = selected_countries,
                                 multiple = TRUE),
                            
                              pickerInput("country_select_bubble_order", "Country Order:",
                                         choices = c(
                                           "Total Tests",
                                           "Tests/100k",
                                           "Total Cases",
                                           "Cases/100k",
                                           "Alphabetical",
                                           "Population"
                                         ),
                                         selected = c("Total Cases"),
                                         multiple = FALSE),
                            
                            sliderInput("bubble_plot_date",
                                    label = h5("Select mapping date"),
                                    min = cv_min_date,
                                    max = as.Date(current_date,"%Y-%m-%d"),
                                    value = as.Date(current_date),
                                    timeFormat = "%d %b",
                                    animate=animationOptions(interval = 2000, loop = FALSE)),
                        ),
                        mainPanel(
                          withSpinner(plotlyOutput("bubble_plot", height="500px", width="100%"))
                        )
                    )
                ),
                tabPanel("Source data",

                         # App title ----
                         titlePanel(""),

                         # Sidebar layout with input and output definitions ----
                         # sidebarLayout(
                         # 
                         #   # Sidebar panel for inputs ----
                         #   sidebarPanel(
                         # 
                         #     # Input: Choose dataset ----
                         #     selectInput("dataset", "Choose a dataset:",
                         #                 choices = c("Tests", "Cases")),
                         # 
                         # 
                         #     # Button
                         #     downloadButton("downloadData", "Download")
                         # 
                         #   ),

                           # Main panel for displaying outputs ----
                           mainPanel(
                             selectInput("dataset", "Choose a dataset:", choices = c("Tests", "Cases","Tests+Cases")),#br(),br(),
                             htmlOutput("source_info"),
                             htmlOutput("downloadFull"),br(),br(),
                             withSpinner(DTOutput("table")),
                             width = 12

                           )

                         #)
                ),
              tabPanel("About",

                       tags$div(
"Our interactive map uses publicly available data to show the number of SARS-CoV-2 tests that have been performed in each country, and the number of
positive cases. The map was developed using ",
tags$a(href="https://github.com/eparker12/nCoV_tracker", "open-source code", target="_blank")," initially developed by the ",
tags$a(href="https://www.lshtm.ac.uk/","London School of Hygiene & Tropical Medicine", target="_blank"),".",
tags$br(),tags$br(),"Several countries and entities including the World Bank are publishing aggregate estimates on the total number of tests performed.
These reports are published across individual websites, statistical reports and press releases - often in multiple languages and updated with different
periodicity. Although there is currently no centralized database and many countries do not publish official reports on volumes of tests performed, we are
working to build a global picture of how many people are being tested for COVID-19.",
tags$br(),tags$br(),tags$b("Our map collates data from every reliable data source we can find and is being updated continually as new information becomes
                           available."),
tags$br(),tags$br(),tags$em("NOTE: We have only been able to track down numbers of tests performed for 1 Chinese province so far, thus test data for China
                            are not currently shown."),
tags$br(),tags$br(),"As well as continuing to hunt for data, we are also working to improve the format and functionality of the map to make it as
user-friendly as possible.",
#tags$br(),tags$br(),tags$a(href="https://github.com/dsbbfinddx/FIND_Cov_19_Tracker/tree/master/input_data", "Download the full datasets from our GitHub repository", target="_blank"),
tags$br(),tags$br(),actionLink("link_to_src_data","Download the full datasets from the source data page"),
tags$br(),tags$br(),tags$b("About FIND"),
tags$br(),tags$a(href="https://www.finddx.org/","FIND", target="_blank")," is a global non-profit organization that drives innovation in the development and delivery of diagnostics to combat major diseases affecting the world's poorest populations. Our work bridges R&D to access, overcoming scientific barriers to technology development; generating evidence for regulators and policy-makers; addressing market failures; and enabling accelerated uptake and access to diagnostics in low- and middle-income countries (LMICs). Since 2003, we have been instrumental in the development of 24 new diagnostic tools. Over 50 million FIND-supported products have been provided to 150 LMICs since the start of 2015. A WHO Collaborating Centre, we work with more than 200 academic, industry, governmental, and civil society partners worldwide, on over 70 active projects that cross six priority disease areas. FIND is committed to a future in which diagnostics underpin treatment decisions and provide the foundation for disease surveillance, control, and prevention."
                       )
              )
)
