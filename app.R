library(dplyr)
library(magrittr)
library(WHO)
library(tidyr)
library(DT)
library(plotly)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)

source("global.R")



myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download table as xlsx"),
                  
                  easyClose = TRUE, title = "Download Table")
  )
}


header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(HTML(
      ".tabbable ul li:nth-child(1) { float: left; }
      .tabbable ul li:nth-child(2) { float: left; }
      .tabbable > .nav > li > a  {background-color: white;  color:black}"
    ))
  ),
  
  
  fluidRow(
    
    column(width = 12,
           
           boxPlus(
             title = "Set criteria for data retrieval", 
             closable = FALSE,
             width = 12,
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 3,
                               
                               selectizeInput(inputId = "who_codes",
                                              label = "Select table",
                                              choices = sort(WHO::get_codes()$display),
                                              selected = "Cholera case fatality rate")
                        ),
                        
                        column(width = 3,
                              
                               radioButtons(inputId = "selectAllRegions",
                                            label = "",
                                            choices = c("Select all regions", "Choose specific region/-s"),
                                            selected = "Choose specific region/-s",
                                            inline = T)
                               ),
                        
                        column(width = 3,
                               
                               radioButtons(inputId = "selectAllCountries",
                                            label = "",
                                            choices = c("Select all countries", "Choose specific country/-s"),
                                            selected = "Choose specific country/-s",
                                            inline = T)
                        ),
                        
                        
                        column(width = 3,
                               
                               radioButtons(inputId = "selectAllYears",
                                            label = "",
                                            choices = c("Select entire period", "Set a specific time range"),
                                            selected = "Set a specific time range",
                                            inline = T)
                               )
                        
                        
                      )
               )
               
               
             ),
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 3),
                        
                        column(width = 3,
                               htmlOutput("who_regions_display") %>% withSpinner(color="#0dc5c1", type = 7, proxy.height = "30px", size = .7)),
                        
                        column(width = 3,
                               htmlOutput("who_countries_display")%>% withSpinner(color="#0dc5c1", type = 7, proxy.height = "30px", size = .7)),
                        
                        column(width = 3,
                               htmlOutput("who_years_display")%>% withSpinner(color="#0dc5c1", type = 7, proxy.height = "30px", size = .7))
                        
                        
                      )
               )
               
               
             )
             
             
           )
           
           
    )
    
  ),
  
  fluidRow(
    
    column(width = 6,
           
           gradientBox(
             title = "Full table",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "purple",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 3,
                               
                               radioButtons(inputId = "retrieve_clean_data",
                                            label = "Clean up original values",
                                            choices = c("Yes", "No"),
                                            selected = "Yes",
                                            inline = T)
                               
                        )  
                        
                      ))
               
               
             ),
             
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      uiOutput("who_tables"))
               
               
             )
             
             
           )
    ),
    
    
    column(width = 6,
           
           gradientBox(
             title = "Overview",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "maroon",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      
                      fluidRow(
                        
                        
                        column(width = 6,
                               
                               radioButtons(inputId = "who_boxes_level",
                                            label = "Group data by",
                                            choices = c("Region", "Country"),
                                            selected = "Region",
                                            inline = T)
                               
                               )
                        
                      )
               )
             ),
             
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      uiOutput("d1"),
                      
                      
                      downloadButton("report", "Generate report")
                      
               )
               
             )             
             
           )
    )
    
  )
)



shinyApp(
  
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output, session) { 
    
    options(shiny.usecairo=T)
    
    
    who_data_init <- reactive({
      
      ll <- WHO::get_codes() %>%
        dplyr::filter(display == input$who_codes) %>% 
        select(label)
      
      WHO::get_data(ll$label)
      
    })
    
    output$who_regions_display <- renderUI({
      
      if(input$selectAllRegions == "Choose specific region/-s") {
      
        htmlOutput("who_regions")
        
        }
      
    })

        
    output$who_regions <- renderUI({
      
      selectizeInput(inputId = "who_regions",
                     label = "Select regions",
                     choices = sort(unique(who_data_init()$region)),
                     selected = sample(unique(who_data_init()$region),3),
                     multiple = T
      )
      })
    
    
    output$who_countries_display <- renderUI({
      
      if(input$selectAllCountries == "Choose specific country/-s"){htmlOutput("who_countries_sel")}
      else if(input$selectAllCountries == "Choose specific country/-s" & input$selectAllRegions == "Select all regions") {htmlOutput("who_countries_all")}
      
    })
    
    output$who_countries_sel <- renderUI({
      
      who_data_init()[who_data_init()$region %in% input$who_regions, "country"] -> cc
      
      selectizeInput(inputId = "who_countries_sel",
                     label = "Select countries",
                     choices = sort(unique(cc$country)),
                     selected = sample(unique(cc$country),3),
                     multiple = T
      )
        
    })
    
    
    output$who_countries_all <- renderUI({  
      
      who_data_init()[ , "country"] -> cc_all
      
      
      selectizeInput(inputId = "who_countries_all",
                     label = "Select countries",
                     choices = sort(unique(cc_all$country)),
                     selected = sample(unique(cc_all$country),3),
                     multiple = T
      )
      
      })
      
      
    
    
    output$who_years_display <- renderUI({
      
      
      if(input$selectAllYears == "Set a specific time range"){htmlOutput("who_years")}
      
      
    })
      
    output$who_years <- renderUI({
      
      
      who_data_init()[ , "year"] -> yy
      yy$year -> yy
      
      sliderInput(inputId = "who_years",
                  label = "Select range of years",
                  min = min(yy, na.rm = T),
                  max = max(yy, na.rm = T),
                  value = c(quantile(yy)[3],quantile(yy)[5]),
                  step = 1,
                  sep = ""
                 )
      
    })
    
    
    
    ## ------------------ RAW DATA ------------------------------
    
    ## selected regions -- selected coutnries -- selected years -----
    
    who_data_raw_SSS <- reactive({
      
      who_data_init()%>% 
        dplyr::filter(region %in% input$who_regions & country %in% input$who_countries_sel & year %in% seq(input$who_years[1],
                                                                                                input$who_years[2],
                                                                                                1))
      
    })
    
    output$table_raw_SSS <- renderDataTable(
      
      
      datatable(who_data_raw_SSS(),
                                            extensions = 'Buttons',
                                            options = list(
                                              scrollX = TRUE,
                                              dom = 'Bfrtip',
                                              buttons = list(
                                                "copy",
                                                list(
                                                  extend = "collection",
                                                  text = 'Download entire dataset',
                                                  action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                                                )
                                              )
                                            )
    ))
  
    
    ## selected regions -- selected coutnries -- all years -----
    
    
    who_data_raw_SSA <- reactive({
      
      who_data_init()%>% 
        dplyr::filter(region %in% input$who_regions & country %in% input$who_countries_sel)
      
    })
    
     output$table_raw_SSA <- renderDataTable(
       
       datatable(who_data_raw_SSA(),
                 extensions = 'Buttons',
                 options = list(
                   scrollX = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(
                     "copy",
                     list(
                       extend = "collection",
                       text = 'Download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                     )
                   )
                 )
       )
       
       )
    
    
    ## selected regions -- all coutnries -- all years -----
    
    
    who_data_raw_SAA <- reactive({
      
      who_data_init()%>%
        dplyr::filter(region %in% input$who_regions)
      
    })
    
    output$table_raw_SAA <- renderDataTable(
      
      datatable(who_data_raw_SAA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      )
      )
    
    
    ## selected regions -- all coutnries -- selected years -----
    
    who_data_raw_SAS <- reactive({
      
      who_data_init() %>%
        dplyr::filter(region %in% input$who_regions  & year %in% seq(input$who_years[1],
                                                              input$who_years[2],
                                                              1))
      
    })
    
    output$table_raw_SAS <- renderDataTable(
      datatable(who_data_raw_SAS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## all regions -- selected coutnries -- selected years -----
    
    who_data_raw_ASS <- reactive({
      
      
      who_data_init()%>%
        dplyr::filter(country %in% input$who_countries_all  &
                                  year %in% seq(input$who_years[1],
                                                input$who_years[2],
                                                1))
      
    })
    
    output$table_raw_ASS <- renderDataTable(
      datatable(who_data_raw_ASS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    ## all regions -- all coutnries -- selected years -----
    
    who_data_raw_AAS <- reactive({
      
      who_data_init()%>% 
        dplyr::filter(year %in% seq(input$who_years[1],
                             input$who_years[2],
                             1))
      
    })
    
    output$table_raw_AAS <- renderDataTable(
      
      datatable(who_data_raw_AAS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    ## all regions -- all coutnries -- all years -----
    
    who_data_raw_AAA <- reactive({
      
      who_data_init()  })
    
    output$table_raw_AAA <- renderDataTable(
      datatable(who_data_raw_AAA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## all regions -- selected coutnries -- all years -----
    
    who_data_raw_ASA <- reactive({
      
      who_data_init()%>%
        dplyr::filter(country %in% input$who_countries_all)
      
    })
    
    output$table_raw_ASA <- renderDataTable(
      datatable(who_data_raw_ASA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## ------------------ CLEAN DATA ------------------------------
    
    
    ## selected regions -- selected coutnries -- selected years -----
    
    who_data_clean_SSS <- reactive({
      
      who_data_init()%>%
        dplyr::filter(region %in% input$who_regions & country %in% input$who_countries_sel & year %in% seq(input$who_years[1],
                                                                                                input$who_years[2],
                                                                                                1)) %>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_SSS <- renderDataTable(
      datatable(who_data_clean_SSS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## selected regions -- selected coutnries -- all years -----
    
    
    who_data_clean_SSA <- reactive({
      
      who_data_init()%>%
        dplyr::filter(region %in% input$who_regions & country %in% input$who_countries_sel)%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_SSA <- renderDataTable(
      datatable(who_data_clean_SSA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## selected regions -- all coutnries -- all years -----
    
    
    who_data_clean_SAA <- reactive({
      
      who_data_init()%>%
        dplyr::filter(region %in% input$who_regions)%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_SAA <- renderDataTable(
      datatable(who_data_clean_SAA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## selected regions -- all coutnries -- selected years -----
    
    who_data_clean_SAS <- reactive({
      
      who_data_init()%>%
        dplyr::filter(region %in% input$who_regions  & year %in% seq(input$who_years[1],
                                                              input$who_years[2],
                                                              1))%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_SAS <- renderDataTable(
      datatable(who_data_clean_SAS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    ## all regions -- all coutnries -- selected years -----
    
    who_data_clean_AAS <- reactive({
      
      who_data_init()%>%
        dplyr::filter(year %in% seq(input$who_years[1],
                             input$who_years[2],
                             1))%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_AAS <- renderDataTable(
      datatable(who_data_clean_AAS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    ## all regions -- all coutnries -- all years -----
    
    who_data_clean_AAA <- reactive({
      
      who_data_init()%>%
        mutate(value = clean_value(value)) })
    
    output$table_clean_AAA <- renderDataTable(
      datatable(who_data_clean_AAA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
    
    
    ## all regions -- selected coutnries -- selected years -----
    
    who_data_clean_ASS <- reactive({
      
      
      who_data_init()%>%
        dplyr::filter(country %in% input$who_countries_all  &
                        year %in% seq(input$who_years[1],
                                      input$who_years[2],
                                      1))%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_ASS <- renderDataTable(
      datatable(who_data_clean_ASS(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))

    
    ## all regions -- selected coutnries -- all years -----
    
    who_data_clean_ASA <- reactive({
      
      
      who_data_init()%>%
        dplyr::filter(country %in% input$who_countries_all)%>%
        mutate(value = clean_value(value))
      
    })
    
    output$table_clean_ASA <- renderDataTable(
      datatable(who_data_clean_ASA(),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      ))
        
      
    
    output$who_tables <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){
        
        if(input$selectAllRegions == "Select all regions" &
           input$selectAllCountries == "Select all countries" &
           input$selectAllYears == "Select entire period"){dataTableOutput("table_clean_AAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_clean_SAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_clean_AAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_clean_SAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_clean_SSA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_clean_SSS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_clean_ASS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_clean_ASA") %>% withSpinner(color="#0dc5c1")}
      }
      
      
      else if(input$retrieve_clean_data == "No"){
        
        if(input$selectAllRegions == "Select all regions" &
           input$selectAllCountries == "Select all countries" &
           input$selectAllYears == "Select entire period"){dataTableOutput("table_raw_AAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_raw_SAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_raw_AAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_raw_SAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_raw_SSA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_raw_SSS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){dataTableOutput("table_raw_ASS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){dataTableOutput("table_raw_ASA") %>% withSpinner(color="#0dc5c1")}
        
      }
      
    })
    
    
    ## plotly by region ------
    
    output$d1_region_AAA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_AAA(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_region_SAA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SAA(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_region_AAS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_AAS(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_region_SAS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SAS(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1_region_SSA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SSA(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
   
    
    output$d1_region_SSS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SSS(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_region_ASS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_ASS(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1_region_ASA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_ASA(), x = ~value, color = ~region, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    ## plotly by country -----
    
    
    output$d1_country_AAA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_AAA(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_country_SAA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SAA(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_country_AAS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_AAS(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_country_SAS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SAS(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1_country_SSA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SSA(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1_country_SSS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_SSS(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    output$d1_country_ASS <- renderPlotly({
      
      p <- plot_ly(who_data_clean_ASS(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1_country_ASA <- renderPlotly({
      
      p <- plot_ly(who_data_clean_ASA(), x = ~value, color = ~country, type = "box", showlegend=FALSE)
      
      p
      
      
    })
    
    
    output$d1 <- renderUI({
      
      if(input$retrieve_clean_data == "Yes" & input$who_boxes_level == "Region"){
        
        if(input$selectAllRegions == "Select all regions" &
           input$selectAllCountries == "Select all countries" &
           input$selectAllYears == "Select entire period"){plotlyOutput("d1_region_AAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_region_SAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_region_AAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_region_SAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_region_SSA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_region_SSS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_region_ASS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_region_ASA") %>% withSpinner(color="#0dc5c1")}
      }
      
      else if(input$retrieve_clean_data == "Yes" & input$who_boxes_level == "Country"){
        
        if(input$selectAllRegions == "Select all regions" &
           input$selectAllCountries == "Select all countries" &
           input$selectAllYears == "Select entire period"){plotlyOutput("d1_country_AAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_country_SAA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_country_AAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Select all countries" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_country_SAS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_country_SSA") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Choose specific region/-s" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_country_SSS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Set a specific time range"){plotlyOutput("d1_country_ASS") %>% withSpinner(color="#0dc5c1")}
        
        else if(input$selectAllRegions == "Select all regions" &
                input$selectAllCountries == "Choose specific country/-s" &
                input$selectAllYears == "Select entire period"){plotlyOutput("d1_country_ASA") %>% withSpinner(color="#0dc5c1")}
      }
      
      
    })
    
    
    
    observeEvent(input$test, {
      print("hello")
      showModal(myModal())
    })
    
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste(input$who_codes, ".csv", sep="")
      },
      content = function(file) {
        
        if(input$retrieve_clean_data == "Yes"){
          
          if(input$selectAllRegions == "Select all regions" &
             input$selectAllCountries == "Select all countries" &
             input$selectAllYears == "Select entire period"){write.csv(who_data_clean_AAA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_clean_SAA(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_clean_AAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_clean_SAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_clean_SSA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_clean_SSS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_clean_ASS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_clean_ASA(),file)}
        }
        
        
        else if(input$retrieve_clean_data == "No"){
          
          if(input$selectAllRegions == "Select all regions" &
             input$selectAllCountries == "Select all countries" &
             input$selectAllYears == "Select entire period"){write.csv(who_data_raw_AAA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_raw_SAA(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_raw_AAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_raw_SAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_raw_SSA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_raw_SSS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.csv(who_data_raw_ASS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.csv(who_data_raw_ASA(),file)}
          
        }
        
              }
    )
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste(input$who_codes, ".xlsx", sep="")
      },
      content = function(file) {
        
        if(input$retrieve_clean_data == "Yes"){
          
          if(input$selectAllRegions == "Select all regions" &
             input$selectAllCountries == "Select all countries" &
             input$selectAllYears == "Select entire period"){write.xlsx(who_data_clean_AAA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_clean_SAA(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_clean_AAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_clean_SAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_clean_SSA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_clean_SSS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_clean_ASS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_clean_ASA(),file)}
        }
        
        
        else if(input$retrieve_clean_data == "No"){
          
          if(input$selectAllRegions == "Select all regions" &
             input$selectAllCountries == "Select all countries" &
             input$selectAllYears == "Select entire period"){write.xlsx(who_data_raw_AAA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_raw_SAA(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_raw_AAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_raw_SAS(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_raw_SSA(),file)}
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_raw_SSS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){write.xlsx(who_data_raw_ASS(),file)}
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){write.xlsx(who_data_raw_ASA(),file)}
          
        }       
      }
    )
    
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        if(input$retrieve_clean_data == "Yes"){
          
          if(input$selectAllRegions == "Select all regions" &
             input$selectAllCountries == "Select all countries" &
             input$selectAllYears == "Select entire period"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_AAA())
            
          }
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Select entire period"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_SAA())
            
            
          }
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_AAS(),
                           y1 = input$who_years[1],
                           y2 = input$who_years[2])
            
          }
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Select all countries" &
                  input$selectAllYears == "Set a specific time range"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_SAS(),
                           y1 = input$who_years[1],
                           y2 = input$who_years[2])
            
          }
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_SSA())
            
          }
          
          else if(input$selectAllRegions == "Choose specific region/-s" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_SSS(),
                           y1 = input$who_years[1],
                           y2 = input$who_years[2])
            
          }
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Set a specific time range"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_ASS(),
                           y1 = input$who_years[1],
                           y2 = input$who_years[2])
            
          }
          
          else if(input$selectAllRegions == "Select all regions" &
                  input$selectAllCountries == "Choose specific country/-s" &
                  input$selectAllYears == "Select entire period"){
            
            # Set up parameters to pass to Rmd document
            params <- list(n = who_data_clean_ASA())
            
          }
        }
       
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    
  }
)

