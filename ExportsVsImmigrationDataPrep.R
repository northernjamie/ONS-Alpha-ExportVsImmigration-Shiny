#### Script to prepare the data for the Exports Vs Trade Shiny App ####

#### Libraries ####

library(httr)
library(rgdal)
library(dplyr)
library(DT)
library(geojsonio)

#### Get the data from gss-data.org.uk for trade ####

endpoint <- "http://gss-data.org.uk/sparql?"

## First query to count the number of records to be returned. This is used to determine how many loops of the AI call to do  ## 

sparql_count <- "query=SELECT (count(distinct ?s) as ?counts)
  WHERE {
?s <http://purl.org/linked-data/cube#dataSet> <http://gss-data.org.uk/data/hmrc-regional-trade-statistics> ;
<http://gss-data.org.uk/def/dimension/flow> <http://gss-data.org.uk/def/concept/flow-directions/exports> ;
<http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/quarter/2016-Q4> ;
<http://purl.org/linked-data/cube#measureType> <http://gss-data.org.uk/def/measure/gbp-total> .
}"

results <- POST(url = endpoint,
                add_headers(Accept = "text/csv"),
                body = sparql_count)

count_results_df <- content(results,"parsed")

count_results_val <- as.numeric(count_results_df[1])


#### create the dataframe from a looping sparql query, getting 10,000 rows at a time ####
sparqlqry_offset <- 0

while (sparqlqry_offset < count_results_val) {
  
  sparqlqry <- paste0("query=PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                      
                      SELECT *
                      WHERE {
                      ?s <http://purl.org/linked-data/cube#dataSet> <http://gss-data.org.uk/data/hmrc-regional-trade-statistics> ;
                      <http://gss-data.org.uk/def/dimension/flow> <http://gss-data.org.uk/def/concept/flow-directions/exports> ;
                      <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/quarter/2016-Q4> ;
                      <http://purl.org/linked-data/cube#measureType> <http://gss-data.org.uk/def/measure/gbp-total> ;
                      <http://gss-data.org.uk/def/measure/gbp-total> ?value ;
                      <http://gss-data.org.uk/def/dimension/product> ?product ;
                      <http://gss-data.org.uk/def/dimension/trade-partner-geography> ?partner ;
                      <http://gss-data.org.uk/def/dimension/trade-reporter-geography> ?region .
                      ?partner rdfs:label ?partnerlabel .
                      ?region rdfs:label ?regionlabel .
                      ?product rdfs:label ?productlabel .
                      }
                      
                      LIMIT 10000
                      OFFSET ", sparqlqry_offset)
  
  results <- POST(url = endpoint,
                  add_headers(Accept = "text/csv"),
                  body = sparqlqry)
  
  results <- content(results,"parsed")
  
  if(sparqlqry_offset == 0) {
    all_results <- results
  } else {
    all_results <- rbind(results,all_results)
  }
  
  sparqlqry_offset <- sparqlqry_offset + 10000
  
}

#### Aggregate the data to get total exports by region ####

trade_region_summ <- all_results[,c(2,7)]

trade_region_summ <- trade_region_summ %>%
  group_by(regionlabel) %>%
  summarise(total_export = sum(value))

total_trade <- sum(trade_region_summ$total_export)  

trade_region_summ$perc_of_total <- (trade_region_summ$total_export/total_trade)

## Probably remove this - doesn't seem to work ##
# trade_region_summ$perc_of_total <- formatPercentage(trade_region_summ$perc_of_total,2)

# calculate totals excluding scotland, n ireland and wales to match immigration figs

trade_region_summ_noscotwalnire <- trade_region_summ[which(!(trade_region_summ$regionlabel %in% c('Scotland','Wales','Northern Ireland','Unallocated - Known','Unallocated - Unknown'))),]

total_trade_noSWNI <- sum(trade_region_summ_noscotwalnire$total_export) 

trade_region_summ_noscotwalnire$perc_of_total <- (trade_region_summ_noscotwalnire$total_export/total_trade_noSWNI)

#### Get the immigration data from gss-data.org.uk ####

endpoint <- "http://gss-data.org.uk/sparql?"

mig_sparqlqry <- "query=PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?refAreaCode ?refAreaName ?refAreaType ?refAreaParent1Name ?refAreaParent1Code ?refAreaParent2Name ?refAreaParent2Code ?count 
WHERE {
  ?s <http://purl.org/linked-data/cube#dataSet> <http://gss-data.org.uk/data/gss_data/migration/ons-local-area-migration-indicators> ;
  <http://gss-data.org.uk/def/measure/count> ?count ;
  <http://gss-data.org.uk/def/dimension/migration> <http://gss-data.org.uk/def/concept/migration-directions/inflow> ;
  <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?refArea ;
  <http://gss-data.org.uk/def/dimension/migration-type> <http://gss-data.org.uk/def/concept/migration-types/long-term-international-migration> ;
  <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/gregorian-interval/2016-06-30T00:00:00/P1Y> .
  ?refArea rdfs:label ?refAreaCode ;
  <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?refAreaName ;
  <http://statistics.data.gov.uk/def/statistical-entity#code> ?refAreaType ;
  <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?refAreaParent1 .
  
  ?refAreaParent1 rdfs:label ?refAreaParent1Code ;
  <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?refAreaParent1Name ;
  <http://statistics.data.gov.uk/def/statistical-geography#parentcode> ?refAreaParent2 .
  ?refAreaParent2 rdfs:label ?refAreaParent2Code ;
  <http://statistics.data.gov.uk/def/statistical-geography#officialname> ?refAreaParent2Name .
}"

mig_results <- POST(url = endpoint,
                    add_headers(Accept = "text/csv"),
                    body = mig_sparqlqry)

mig_results <- content(mig_results,"parsed")

## Add the region column to the dataframe derived from the parent fields ##

mig_results$region <- ifelse(mig_results$refAreaType =='http://statistics.data.gov.uk/id/statistical-entity/E07',mig_results$refAreaParent2Name,mig_results$refAreaParent1Name)

## Aggregate the dataframe by region ##

mig_results_min <- mig_results[,c(8,9)]

migration_region_summ <- mig_results %>%
  group_by(region) %>%
  summarise(sum_region = sum(count))

total_immigration <- sum(migration_region_summ$sum_region)

migration_region_summ$perc_of_total_imm <- migration_region_summ$sum_region/total_immigration

## Shudder - need to rename the regions in the migration file so it matches the trade file and geojson

migration_region_summ$region <- gsub('The','the',migration_region_summ$region)

migration_region_summ$region <- gsub('East of England','East',migration_region_summ$region)


## Merge the two dataframes to make one ##

trade_migration_df <- merge(migration_region_summ,trade_region_summ_noscotwalnire, by.x="region", by.y ="regionlabel",all = T)

trade_migration_df$perc_difference <- 100*(trade_migration_df$perc_of_total_imm - trade_migration_df$perc_of_total)

## Need to make the geojson now ##

engregions <- readOGR("englandregions.geojson", "OGRGeoJSON")

engregions@data <- data.frame(engregions@data, trade_migration_df[match(engregions@data[,'rgn15nm'], trade_migration_df[,'region']),])

geojson_write(engregions, file = "englandregions_trade_migration.geojson", overwrite = TRUE)






