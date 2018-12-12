library(shiny)
library(leaflet)
library(DT)
library(rgdal)
library(shinyjs)
library(scales)
library(tidyr)

# Choropleth showing trade vs immigration

ui <- navbarPage("GSS Trade and Migration", id="nav",
                 tabPanel("MAP",
                          useShinyjs(),
                          div(class="outer",
                              tags$head(
                                includeCSS("www/styles.css")
                              ),
                              leafletOutput("regionmap", width="100%", height="100%"),
                              
                              absolutePanel(id="controls",
                                            style="overflow-y: auto",
                                            class="panel panel-default",
                                            fixed=TRUE,
                                            draggable=TRUE,
                                            top=60,
                                            left="auto",
                                            right=30,
                                            bottom="auto",
                                            width=450,
                                            height="90%",
                                            h4("Trade and migration in English Regions"),
                                            selectInput('choosemap','Choose data to show on map',c('Total Exports', 'Total Immigration', 'Percentage Share of Exports', 'Percentage Share of Immigration', 'Difference Between Immigration and Exports')),
                                            h5("Click a region to see summary statistics"),
                                            htmlOutput('regionname'),
                                            htmlOutput('totalexport'),
                                            htmlOutput('percshareexport'),
                                            htmlOutput('totalimmigration'),
                                            htmlOutput('percshareimmigration'),
                                            htmlOutput('percpointdifference')
                                            
                                      
                              )
                              
                              
                          )))

server <- function(input, output) {
  
  engregions <-readOGR("englandregions_trade_migration.geojson", "OGRGeoJSON")
  
  pounds <- dollar_format(prefix='£',big.mark = ",")
  
  merged_trade_migration <- data.frame(engregions@data)
  
  col_pal_perc_diff <- colorNumeric("Spectral",domain = merged_trade_migration$perc_difference)
  
  
  output$regionmap <-  renderLeaflet({leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-2,53, zoom = 6) %>%
      addLegend(pal = col_pal_perc_diff,
                values = merged_trade_migration$perc_difference,
                opacity = 0.7,
                title = paste0("% point difference between national",br(),"share of export trade and immigration"),
                position = "bottomleft") %>%
      addPolygons(data = engregions, layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal_perc_diff(perc_difference))})
  
  
  observe({
    click <- input$regionmap_shape_click
    if(is.null(click))
      return()
    
    temp_region_dframe <- merged_trade_migration[which(merged_trade_migration$region == click$id),]
    txt_region_name <- temp_region_dframe$region
    txt_total_export <- temp_region_dframe$total_export
    txt_total_imm <- temp_region_dframe$sum_region
    txt_perc_export <- temp_region_dframe$perc_of_total
    txt_perc_imm <- temp_region_dframe$perc_of_total_imm
    txt_perc_difference <- temp_region_dframe$perc_difference
    output$regionname <- renderText(paste0('<h3>',txt_region_name,'</h3>'))
    output$totalexport <- renderText(paste0('<h5>Total value of international exports (Q4 2016):</h5><h4>',pounds(txt_total_export*1000),'</h4>'))
    output$totalimmigration <- renderText(paste0('<h5>Total number of immigrants (Mid 2016 - Mid 2017):</h5><h4>',format(txt_total_imm,big.mark=","),'</h4>'))
    output$percshareexport <- renderText(paste0('<h5>% Share of national export total (Q4 2016):</h5><h4>',sprintf("%.1f %%",100*txt_perc_export),'</h4>'))
    output$percshareimmigration <- renderText(paste0('<h5>% Share of national immigrants total (Mid 2016 - Mid 2017):</h5><h4>',sprintf("%.1f %%",100*txt_perc_imm),'</h4>'))
    output$percpointdifference <- renderText(paste0('<h5>Percentage point difference between share of immigration and exports</h5><h4>',sprintf("%.1f %%",txt_perc_difference),'</h4>'))
    
  })
  
  
  observeEvent(input$choosemap, {
    if(input$choosemap == 'Total Exports') {
      col_pal <- colorNumeric("Spectral",domain = merged_trade_migration$total_export);
      
      leafletProxy("regionmap") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = engregions,layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal(total_export)) %>%
        addLegend(pal = col_pal, values = merged_trade_migration$total_export, opacity = 0.7,
                  position = 'bottomleft',
                  labFormat = labelFormat(prefix="£", big.mark = ",", transform = function(x) 1000 * x),
                  title = paste0("Total Value of Exports",br(),"Q4 2016"))
    }
    if(input$choosemap == 'Total Immigration') {
      col_pal <- colorNumeric("Spectral",domain = merged_trade_migration$sum_region);
      
      leafletProxy("regionmap") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = engregions,layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal(sum_region)) %>%
        addLegend(pal = col_pal, values = merged_trade_migration$sum_region, opacity = 0.7,
                  position = 'bottomleft', 
                  title = paste0("Total Number of immigrants",br(),"Mid-2016 to Mid-2017"))
    }
    if(input$choosemap == 'Percentage Share of Exports') {
      col_pal <- colorNumeric("Spectral",domain = merged_trade_migration$perc_of_total);
      
      leafletProxy("regionmap") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = engregions,layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal(perc_of_total)) %>%
        addLegend(pal = col_pal, values = merged_trade_migration$perc_of_total, opacity = 0.7,
                  position = 'bottomleft',
                  labFormat = labelFormat(suffix="%", transform = function(x) 100 * x),
                  title = paste0("% Share of nationwide exports",br(),"Q4 2016"))
    }
    if(input$choosemap == 'Percentage Share of Immigration') {
      col_pal <- colorNumeric("Spectral",domain = merged_trade_migration$perc_of_total_imm);
      
      leafletProxy("regionmap") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = engregions,layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal(perc_of_total_imm)) %>%
        addLegend(pal = col_pal, values = merged_trade_migration$perc_of_total_imm, opacity = 0.7,
                  position = 'bottomleft',
                  labFormat = labelFormat(suffix="%", transform = function(x) 100 * x),
                  title = paste0("% Share of national immigration total",br(),"Mid-2016 to Mid-2017"))
    }
    if(input$choosemap == 'Difference Between Immigration and Exports') {
      col_pal <- colorNumeric("Spectral",domain = merged_trade_migration$perc_difference);
      
      leafletProxy("regionmap") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = engregions,layerId = ~region,weight = 2,color = "#111111", fillOpacity = 0.7, fillColor = ~col_pal(perc_difference)) %>%
        addLegend(pal = col_pal, values = merged_trade_migration$perc_difference, opacity = 0.7,
                  position = 'bottomleft', 
                  labFormat = labelFormat(suffix="%"),
                  title = paste0("% point difference between national",br(),"share of export trade and immigration"))
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
