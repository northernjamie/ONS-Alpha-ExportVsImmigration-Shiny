# ONS-Alpha-ExportVsImmigration-Shiny
Sample visualisation showing data from ONS GSS Project on trade and immigration in one tool

## Intro

This very simple app demonstrates getting data from the ONS GSS datastore using SPARQL queries from two different government departments (HMRC and ONS), processing the data using scripts, and displaying the data in a Shiny web application.

The app can be viewed here https://swirrl.shinyapps.io/ONS_Alpha_Plus_Exports_Vs_Immigration/

## How to reproduce this

Cloning this repo and running the scripts locally would be the best way to do this.

The app.R file will run from a new R project, in Rstudio.


To prepare the data, ExportsVsImmigration.R is required. This will fetch data via api from the sparql endpoint at http://gss-data.org.uk convert it and merge it, ultimately loading in a geojson file of English regions (englandregions.geojson), and saving back out a geojson will of English regions with data (englandregions_trade_migration.geojson). This is then used by the app.R file


## Comments

This is only intended to be a demonstration of the benefits of this approach of harmonising data. This should not be considered rigorous analysis of the data.

The code here is provided with no guarantees, and is not a production ready app. Hopefully it will help people get started. Try changing some of the SPARQL queries to get different time periods, or datasets.

For background to the project see the GSS blog: https://gss.civilservice.gov.uk/guidance/the-gss-data-project/ 



