
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny/v2/data" # when testing
# codedir <- "shiny/v2/code" # when testing
textdir <- "app_text"

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
nut_cntry_yr <- readRDS(file.path(datadir, "genus_nutrient_supplies_by_cntry_year.Rds"))
nut_cntry_age_sex_2011 <- readRDS(file.path(datadir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))

# User interface
################################################################################

# User interface
ui <- navbarPage("Climate change, fisheries, and nutrition web explorer",
                 
  # 1. Overview
  tabPanel("Overview", 
           mainPanel(includeHTML(file.path(textdir, "01overview.html")))),
  
  # 2. Fish nutrition data
  tabPanel("Fish nutrition data"),
  
  # 3. National nutrition data
  tabPanel("National nutrition data",
           
           # Select a country
           selectInput("country",
                       "Select country:",
                       choices=sort(unique(nut_cntry_yr$country)), multiple=F, selectize=F),
           
           # Plot daily per capita nutrient supply over time
           plotOutput(outputId = "nutrientTimeseriesPlot", width=1600, height=800),
           br(),
           
           # Plot daily per capita nutrient supply by age/sex in 2011
           plotOutput(outputId = "nutrientAgeSexPlot", width=1600, height=800),
           br()
           
           ),
  
  # 4. National fisheries and nutrition forecasts
  tabPanel("National fisheries and nutrition forecasts"),
  
  # 5. Fish nutrition data
  tabPanel("National report cards")

)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Plot daily per capita nutrient supply time series
  output$nutrientTimeseriesPlot <- renderPlot({
    
    # Subset data
    # cntry <- "United States"
    cntry <- input$country
    g <- plot_nutrient_ts(nut_cntry_yr, cntry=cntry)
    g
    
  })
  
  # Plot daily per capita nutrient supply time series
  output$nutrientAgeSexPlot <- renderPlot({
    
    # Subset data
    # cntry <- "United States"
    cntry <- input$country
    g <- plot_nutrient_age_sex(nut_cntry_age_sex_2011, cntry=cntry)
    g
    
  })
  

}

shinyApp(ui = ui, server = server)
