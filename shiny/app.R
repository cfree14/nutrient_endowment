
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

# Read data
datadir <- "data" # for actual app
# datadir <- "shiny/data" # when testing

# Load data
data_orig <- readRDS(file.path(datadir, "country_nutrient_time_series.Rds"))

# Countries
countries <- sort(unique(data_orig$sovereign))

# Nutrients
nutrients <- c("Protein", "Fat", "Omega-3 fatty acids", "Omega-6 fatty acids", "Iron", "Zinc",
               "Vitamin A", "Vitamin B12", "Vitamin D")

# Build data
################################################################################

# Format data
data <- data_orig %>% 
  ungroup() %>% 
  select(rcp:edible_mt, protein_mt:vitD_mt) %>% 
  mutate(rcp=recode(rcp, "RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5")) %>% 
  gather(key="type", value="weight_mt", 5:ncol(.)) %>% 
  mutate(type=recode(type, "edible_mt"="Edible meat",
                             "fat_mt"="Fat",
                             "iron_mt"="Iron",
                             "msy_mt"="MSY", 
                             "omega3_mt"="Omega-3 fatty acids", 
                             "omega6_mt"="Omega-6 fatty acids", 
                             "protein_mt"="Protein", 
                             "vitA_mt"="Vitamin A", 
                             "vitB12_mt"="Vitamin B12", 
                             "vitD_mt"="Vitamin D", 
                             "zinc_mt"="Zinc"))

# Nutient data
ndata <- data %>% 
  filter(type %in% nutrients) %>% 
  mutate(type=factor(type, levels=nutrients))

# Production data
pdata <- data %>% 
  filter(!type %in% nutrients) %>% 
  mutate(type=factor(type, levels=c("MSY", "Edible meat")))


# User interface
################################################################################

# User interface
ui <- fluidPage(
                 
  # Theme
  # CSS Stylesheet: https://bootswatch.com/flatly/
  # Making new stylesheet: http://shiny.rstudio.com/articles/css.html
  theme=shinytheme("flatly"),
  
  # Title
  titlePanel("Nutrient endowment of marine fisheries under climate change"),
  
  # Setup sidebar layout
  sidebarLayout(
    
    # Side panel: stock selection
    sidebarPanel(
      
      # Country selector
      selectInput("country",
                  "Select country:",
                  choices=countries, multiple=F, selectize=F)
      
    ),
    
    # Main panel
    mainPanel(
      
      # Plot production over time
      p("Climate-driven changes in maximum sustainable yield and the edible meat that can be derived from this catch."),
      plotOutput(outputId = "prodPlot"),
      
      # Plot production over time
      p("Climate-driven changes in the nominal nutrient composition of the edible catch produced at maximum sustainable yield."),
      plotOutput(outputId = "nutrPlot"),
      
      # Plot production over time
      p("Climate-driven changes in the proportional nutrient composition of the edible catch produced at maximum sustainable yield."),
      plotOutput(outputId = "nutrPropPlot"),
      
      # Plot production over time
      p("Climate-driven changes in the nominal nutrient composition of the edible catch produced at maximum sustainable yield."),
      plotOutput(outputId = "nutrIndivPlot"),
      
      p("Climate-driven changes in Principal Components Analysis (PCA) 1."),
      plotOutput(outputId = "pcaPlot")
      
    )
    
  )
        
)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Plot landings/edible
  output$prodPlot <- renderPlot({
    
    # Subset data
    country <- "United States"
    country <- input$country
    s_nutrients <- ndata %>% 
      filter(sovereign==country)
    s_production <- pdata %>% 
      filter(sovereign==country)
    
    g <- ggplot(s_production, aes(x=year, y=weight_mt/1e3, color=type)) +
      geom_line() + 
      labs(x="", y="Biomass (thousands of mt)", 
           title="Figure 1. Maximum sustainable yield under climate change.") +
      scale_color_discrete(name="") +
      facet_wrap(~ rcp, ncol=2) +
      theme_bw()
    g
    
  })
  
  # Plot nutrients over time
  output$nutrPlot <- renderPlot({
    
    # Subset data
    country <- "United States"
    country <- input$country
    s_nutrients <- ndata %>% 
      filter(sovereign==country)
    s_production <- pdata %>% 
      filter(sovereign==country)
    
    g <- ggplot(s_nutrients, aes(x=year, y=weight_mt/1e3, fill=type)) +
      geom_area() +
      labs(x="", y="Edible biomass (thousands of mt)", 
           title="Figure 2. Nutrient availability under climate change.") +
      scale_fill_discrete(name="Nutrient") +
      facet_wrap(~ rcp, ncol=2) +
      theme_bw()
    g
    
  })

  # Plot nutrient proportions over time
  output$nutrPropPlot <- renderPlot({
    
    # Subset data
    country <- "United States"
    country <- input$country
    s_nutrients <- ndata %>% 
      filter(sovereign==country)
    s_production <- pdata %>% 
      filter(sovereign==country)
    
    g <- ggplot(s_nutrients, aes(x=year, y=weight_mt/1e3, fill=type)) +
      geom_area(position="fill") +
      labs(x="", y="Proportion of edible biomass", 
           title="Figure 3. Nutrient composition under climate change.") +
      scale_fill_discrete(name="Nutrient") +
      facet_wrap(~ rcp, ncol=2) +
      theme_bw()
    g
    
  })

  # Plot nutrients individually over time
  output$nutrIndivPlot <- renderPlot({
    
    # Subset data
    country <- "United States"
    country <- input$country
    s_nutrients <- ndata %>% 
      filter(sovereign==country)
    s_production <- pdata %>% 
      filter(sovereign==country)
    
    g <- ggplot(s_nutrients, aes(x=year, y=weight_mt/1e3, color=rcp)) +
      geom_line() +
      labs(x="", y="Edible biomass (thousands of mt)", 
           title="Figure 4. Nutrient availability under climate change.") +
      scale_color_discrete(name="Emissions scenario") +
      facet_wrap(~ type, ncol=3, scales="free") +
      theme_bw()
    g
  
  })
  
  # Plot PCA results
  output$pcaPlot <- renderPlot({
    
    # Subset data
    country <- "United States"
    country <- input$country
    s_nutrients <- ndata %>% 
      filter(sovereign==country)
    
    # Rehape data for PCA
    sdata1 <- ndata %>% 
      filter(sovereign==country & rcp=="RCP 8.5") %>% 
      spread(key="type", value="weight_mt") %>% 
      select(-c(rcp:year))
    
    # Fit PCA
    pca_fit <- prcomp(sdata1, center = T, scale = T)

    # Build data
    pca1_ts <- tibble(year=2012:2100,
                      pca1=pca_fit$x[,1])

    # Plot PCA1 over time
    g <- ggplot(pca1_ts, aes(x=year, y=pca1)) +
      geom_line() +
      labs(x="", y="PCA 1",
           title="Figure 5. PCA1 under climate change.") +
      theme_bw()
    g
    
  })
  
  
}

shinyApp(ui = ui, server = server)
