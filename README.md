# FIND_Cov_19_Tracker

Our interactive map uses publicly available data to show the number of SARS-CoV-2 tests that have been performed in each country, and the number of positive cases. The map was developed using [open-source code](https://github.com/eparker12/nCoV_tracker) initially developed by the [London School of Hygiene & Tropical Medicine](https://www.lshtm.ac.uk/)

Several countries and entities including the World Bank are publishing aggregate estimates on the total number of tests performed. These reports are published across individual websites, statistical reports and press releases – often in multiple languages and updated with different periodicity. Although there is currently no centralized database and many countries do not publish official reports on volumes of tests performed, we are working to build a global picture of how many people are being tested for COVID-19.

**Our map collates data from every reliable data source we can find and is being updated continually as new information becomes available.**

*NOTE: We have only been able to track down numbers of tests performed for 1 Chinese province so far, thus test data for China are not currently shown.*

As well as continuing to hunt for data, we are also working to improve the format and functionality of the map to make it as user-friendly as possible.

[View full screen map](https://finddx.shinyapps.io/FIND_Cov_19_Tracker/)

Data can be filtered and results downloaded from the app from the "Source data" page.

**Full datasets are downloadable directly from the input_data folder**

### Data sources 
**Test data** : test data are collated everyday by the FIND team, from information found online.

**Case data** : case data are downloaded daily from the [COVID19 JHU repository](https://github.com/CSSEGISandData/COVID-19)

### Rscript pipeline for data update
**jhu_data_full.R** : updates the jhu cases data and saves them in coronavirus.csv file in the input_data folder

**update_cv_data.R** : sources __jhu_data_full.R__ if there is a need for an update of the Case data. Updates the test data if there is an updated Test Data file in the dropbox folder. Adds empty lines in the test data frame if there are no data available for that country for the specific date. It saves the test data in the coronovirus_tests.csv file in the input_data folder. It is sourced by the ui.R script.

**cv_data.R** : loads coronavirus.csv (cases) and coronavirus_tests.csv (tests) files and other essential files from input_data folder. It creates the data frame that is needed as an input for the application. It is sourced by update_cv_data.R, ui.R and server.R scripts.
