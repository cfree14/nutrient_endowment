
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(rgeos)
library(sf)
library(countrycode)
library(rnaturalearth)
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(ggradar)

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
pdiet_seafood_cntry_yr <- readRDS(file.path(datadir, "genus_pdiet_seafood_by_cntry_year.Rds"))
pnutrient_seafood_cntry_2011 <- readRDS(file.path(datadir, "genus_pnutrient_seafood_by_cntry_2011.Rds"))
load(file.path(datadir, "vaitla_etal_2018_finfish_nutrient_data.Rdata"))

# GENuS countries with data
genus_countries <- nut_cntry_yr %>% 
  filter(!is.na(value_med)) %>% 
  pull(country) %>% unique() %>% sort()

# Vaitla species to show
vaitla_species <- vaitla_nutr_preds_long %>% 
  filter(!is.na(value_md)) %>% 
  pull(species_label) %>% unique() %>% sort()


# User interface
################################################################################

# User interface
ui <- navbarPage("Climate change, fisheries, and nutrition web explorer",
                 
  # 1. Overview
  tabPanel("Overview", 
           mainPanel(includeHTML(file.path(textdir, "01overview.html")))),
  
  # 2. Fish nutrition data
  tabPanel("Fish nutrition data",
           
           # Text
           h2("Overview"),
           p("On this page, you can compare the nutrient content of over 7,000 finfish species as estimated by Vaitla et al. (2018). Vaitla et al. (2018) used phylogenetic relatedness and life history traits to predict the protein, total fat, omega-3 fatty acid, omega-6 fatty acid, iron, zinc, vitamin A, vitamin B12, and vitamin D content of ray-finned fishes (Class Actinopterygii). These nine nutrients are critical for human nutrition and generally occur in high concentrations in marine finfish."),
           p("We provide two methods for visualizing and comparing the nutrient content of marine finfish species. The first figure shows the nutrient content of each species as a percentage of the maximum content for any finfish species. These percentages are arrayed in a radar plot for quick comparisons between species. The second figure shows the nutrient content of each species overlayed on histograms illustrating the distribution of nutrient contents for all species."),
           p("To begin, select a single species or a set of many species from the dropdown menu below."),
           
           # Select species
           selectInput("species",
                       "Select species:",
                       choices=vaitla_species, multiple=T, selectize=F, selected=vaitla_species[1]),
           
           # Plot nutrition radar plot
           h2("Figure 1. Nutrient content as a percentage of the maximum value predicted for any finfish species."),
           plotOutput(outputId = "finfishNutrRadarPlot", width=1000, height=600),
           br(),
           
           # Plot nutrition histogram plot
           h2("Figure 2. Nutrient content relative to the distribution of values predicted for all finfish species."),
           plotOutput(outputId = "finfishNutrHistPlot", width=1000, height=800),
           br(),
           
           # References
           h2("References"),
           p("Vaitla B, Collar D, Smith MR, Myers SS, Rice BL, Golden CD (2018) Predicting nutrient content of ray-finned fishes using phylogenetic information. Nature Communications 9(3742). https://doi.org/10.1038/s41467-018-06199-w")
           
           ),
  
  # 3. National nutrition data
  tabPanel("National nutrition data",
           
           # Overview
           h2("Overview"),
           p("The purpose of this page is to illustrate the current nutritional health of a nation’s population and the relative contribution of marine fisheries to its nutritional health. All of the presented data is from the open-source Global Expanded Nutrient Supply (GENuS) database (Smith et al. 2016). The GENuS database provides nutrient consumptuon estimates for 23 individual nutrients (see below) across 225 food categories for 34 age-sex groups in nearly all countries. Furthermore, it provides historical trends in national nutrition consumption using data from 1961–2011."),
           
           # Select a country
           selectInput("country",
                       "Select country:",
                       choices=genus_countries, multiple=F, selectize=F),
           
           # Importance of seafood
           ############################################
           
           # Importance of seafood
           h2("Importance of marine seafood to national nutrition"),
           p("Example text."),
           plotOutput(outputId = "natl_seafood_impt_map", width=1000, height=400),
           br(),
           p("The figure below shows the relative importance of marine seafood to national diets and nutrition. The first panel (A) shows median daily per capita marine seafood consumption from 1961 to 2011 (when data is available). The second panel (B) shows the percentage of daily food consumption coming from marine seafood over the same time period (when data is available). The final panel (C) shows the proportion of daily per capita nutrient intake supplied by marine seafood in 2011 (the most recent year with complete data). In this analysis, marine seafood was defined as coming from the following food groups: marine fish (other), pelagic fish, demersal fish, molluscs (other), crustaceans, fish body oil, and fish liver oil. The following sources of aquatic seafood were excluded from this calculation: freshwater fish, aquatic animals (other), and aquatic plants."),
           plotOutput(outputId = "natl_seafood_impt", width=1000, height=700),
           br(),
           
           # Importance of seafood
           ############################################
           
           # National nutritional health by age/sex
           h2("National nutritional health by age and sex"),
           p("The figure below illustrates national nutritional intake by age and sex relative to the levels recommended by the U.S. Dietary Guidelines (USDHHS 2015). Red shading indicates groups with median consumption below the recommended amounts and blue shading indicates groups with median consumption above the recommended amounts."),
           br(),
           
           # Macronutrients
           ############################################
           
           # Header
           h2("Macronutrients"),
           
           # Calories
           h3("Calories"),
           p("Calories are a unit of energy widely used in nutrition. They are measured in kilocalorie (kcal) which represents the amount of heat required to raise the temperature of 1 kilogram of water by 1°C. Fat in food contains 9 kilocalories per gram (kcal/g) while carbohydrates and proteins in food contain 4 kcal/g. Alcohol in food contains 7 kcal/g."),
           plotOutput(outputId = "natl_nutr_stats_calories", width=1000, height=400),
           br(),
           
           # Protein
           h3("Protein"),
           p("Proteins serve as the major structural component of all cells of the body, and functions as enzymes, in membranes, as transport carriers, and as some hormones. Protein is found in meat, dairy, legumes, nuts, seafood, and eggs."),
           plotOutput(outputId = "natl_nutr_stats_protein", width=1000, height=400),
           br(),
           
           # Carbohydrates
           h3("Carbohydrates"),
           p("Carbohydrates provide energy, spare protein consumption, and support the central nervous system. Carbohydrates are found in pasta, rice, cereals, breads, potatoes, milk, fruit, and sugar."),
           plotOutput(outputId = "natl_nutr_stats_carbs", width=1000, height=400),
           br(),
           
           # Total fat
           h3("Total fat"),
           p("Fat is an important energy source and can be a source of Omega-6 and Omega-6 polyunsaturated fatty acids. Fats are categorized according to the number and bonding of the carbon atoms in their aliphatic chains. Saturated fats have no double bonds between the carbons in the chain. Unsaturated fats have one (mono) or more (poly) double bonded carbons in the chain."),
           plotOutput(outputId = "natl_nutr_stats_fat", width=1000, height=400),
           br(),
           
           # Saturated fatty acids
           h3("Saturated fatty acids"),
           p("Saturated fatty acids are fatty acids without double bonds in their backbones. A diet low in saturated fatty acids is thought to lower the risk of cardiovascular disease, diabetes, and death. Most animal fats are saturated whereas the fats of plants and fish are generally unsaturated."),
           plotOutput(outputId = "natl_nutr_stats_sfacids", width=1000, height=400),
           br(),
           
           # Monounsaturated fatty acids
           h3("Monounsaturated fatty acids"),
           p("Monounsaturated fatty acids are fatty acids that have only one double bond in their backbone. They can improve insulin sensitivity and promote healthier serum lipid profiles in children. Monounsaturated fats are found in animal flesh such as red meat, whole milk products, nuts, high fat fruits such as olives and avocados, and vegetable oils."),
           plotOutput(outputId = "natl_nutr_stats_mfacids", width=1000, height=400),
           br(),
           
           # Polyunsaturated fatty acids
           h3("Polyunsaturated fatty acids"),
           p("Polyunsaturated fatty acids are fatty acids that contain more than one double bond in their backbone. Omega-6 polyunsaturated fatty acids (linoleic acid) are an essential component of structural membrane lipids, involved with cell signaling, and precursor of eicosanoids. They are also required for normal skin function. Omega-3 polyunsaturated fatty acids (α-linoleic acid) are Involved with neurological development and growth. They are also a precursor of eicosanoids."),
           plotOutput(outputId = "natl_nutr_stats_pfacids", width=1000, height=400),
           br(),
           
           # Dietary fiber
           h3("Dietary fiber"),
           p("Dietary fiber (roughage) is the portion of plant-derived food that cannot be completely broken down by human digestive enzymes. It normalizes bowel movements, maintains bowel health, lowers cholesterol levels, and helps control blood sugar levels. Dietary fiber is found in Whole grains, nuts and seeds, and fruit and vegetables."),
           plotOutput(outputId = "natl_nutr_stats_fiber", width=1000, height=400),
           br(),
           
           # Minerals
           ############################################
           
           # Header
           h2("Minerals"),
           
           # Calcium
           h3("Calcium"),
           p("Calcium is a macromineral that is important for healthy bones and teeth, muscle relaxation and contraction, nerve functioning, blood clotting, blood pressure regulation, and immune system health. Calcium is found in milk and milk products, canned fish, fortified tofu and fortified soy milk, greens (especially broccoli and mustard greens), and legumes."),
           plotOutput(outputId = "natl_nutr_stats_calcium", width=1000, height=400),
           br(),
           
           # Copper
           h3("Copper"),
           p("Copper is a trace mineral that is part of many enzymes and is needed for iron metabolism. Copper is found in legumes, nuts and seeds, whole grains, organ meats, and drinking water.  "),
           plotOutput(outputId = "natl_nutr_stats_copper", width=1000, height=400),
           br(),
           
           # Iron
           h3("Iron"),
           p("Iron is a trace mineral that is critical in hemoglobin and is needed for energy metabolism. Iron is found in organ meats, red meats, fish, poultry, shellfish (especially clams), egg yolks, legumes, dried fruits, dark, leafy greens, iron-enriched breads and cereals, and fortified cereals. "),
           plotOutput(outputId = "natl_nutr_stats_iron", width=1000, height=400),
           br(),
           
           # Magnesium
           h3("Magnesium"),
           p("Magnesium is a macromineral that is important for healthy bones and teeth, making protein, muscle contraction, nerve transmission, and immune system health. Magnesium is found in nuts and seeds, legumes, leafy, green vegetables, seafood, chocolate, artichokes, and “hard” drinking water."),
           plotOutput(outputId = "natl_nutr_stats_magnesium", width=1000, height=400),
           br(),
          
           # Phosphorus
           h3("Phosphorus"),
           p("Phosphorus is a macromineral that is important for healthy bones and teeth. It is found in every cell and is part of the system that maintains acid-base balance. Phosphorus is found in meat, fish, poultry, eggs, milk, and processed foods. "),
           plotOutput(outputId = "natl_nutr_stats_phos", width=1000, height=400),
           br(),
           
           # Potassium
           h3("Potassium"),
           p("Potassium is a macromineral needed for proper fluid balance, nerve transmission, and muscle contraction. Potassium is found in meats, milk, fresh fruits and vegetables, whole grains, and legumes."),
           plotOutput(outputId = "natl_nutr_stats_potassium", width=1000, height=400),
           br(),

           # Sodium
           h3("Sodium"),
           p("Sodium is a macromineral that is needed for proper fluid balance, nerve transmission, and muscle contraction. It is found in large amounts in table salt, soy sauce, and processed foods. It is found in small amounts in milk, breads, vegetables, and unprocessed meats. "),
           plotOutput(outputId = "natl_nutr_stats_sodium", width=1000, height=400),
           br(),
           
           # Zinc
           h3("Zinc"),
           p("Zinc is a trace mineral that is part of many enzymes, needed for making protein and genetic material, has a function in taste perception, wound healing, normal fetal development, production of sperm, normal growth and sexual maturation, and immune system health. Zinc is found in meats, fish, poultry, leavened whole grains, and vegetables."),
           plotOutput(outputId = "natl_nutr_stats_zinc", width=1000, height=400),
           br(),
           
           # Vitamins
           ############################################
           
           # Header
           h2("Vitamins"),
           
           # Folate
           h3("Folate"),
           p("Folate (folic acid) is part of an enzyme needed for making DNA and new cells, especially red blood cells. Folate is found in leafy green vegetables and legumes, seeds, orange juice, and liver. It is also now added to most refined grains."),
           plotOutput(outputId = "natl_nutr_stats_folate", width=1000, height=400),
           br(),
           
           # Niacin
           h3("Niacin"),
           p("Niacin (vitamin B3) is a water soluble vitamin that is part of an enzyme needed for energy metabolism and is important for nervous system, digestive system, and skin health. Niacin is found in meat, poultry, fish, whole-grain or enriched breads and cereals, vegetables (especially mushrooms, asparagus, and leafy green vegetables), and peanut butter."),
           plotOutput(outputId = "natl_nutr_stats_niacin", width=1000, height=400),
           br(),
           
           # Riboflavin
           h3("Riboflavin"),
           p("Riboflavin (vitamin B2) is a water soluble vitamin that is part of an enzyme needed for energy metabolism and is important for normal vision and skin health. Riboflavin is found in milk and milk products, leafy green vegetables, whole-grain, and enriched breads and cereals."),
           plotOutput(outputId = "natl_nutr_stats_ribo", width=1000, height=400),
           br(),

           # Thiamin
           h3("Thiamin"),
           p("Thiamine (vitamin B1) is a water soluble vitamin that is part of an enzyme needed for energy metabolism and is important to nerve function. Thiamine is found in all nutritious foods in moderate amounts, pork, whole-grain or enriched breads and cereals, legumes, and nuts and seeds."),
           plotOutput(outputId = "natl_nutr_stats_thiamin", width=1000, height=400),
           br(),
           
           # Vitamin A
           h3("Vitamin A"),
           p("Vitamin A is a fat soluble vitamin that is needed for vision, healthy skin and mucous membranes, bone and tooth growth, and immune system health. Vitamin A is found in animal sources (retinol): fortified milk, cheese, cream, butter, fortified margarine, eggs, and liver."),
           plotOutput(outputId = "natl_nutr_stats_vitA", width=1000, height=400),
           br(),
           
           # Vitamin B6
           h3("Vitamin B6"),
           p("Vitamin B6 (Pyridoxine) is a water soluble vitamin that is part of an enzyme needed for protein metabolism and it helps make red blood cells. Vitamin B6 is found in meat, fish, poultry, vegetables, and fruits."),
           plotOutput(outputId = "natl_nutr_stats_vitb6", width=1000, height=400),
           br(),
           
           # Vitamin C
           h3("Vitamin C"),
           p("Vitamin C (ascorbic acid) is a water soluble vitamin that is an antioxidant, part of an enzyme needed for protein metabolism, important for immune system health, and aids in iron absorption. Vitamin C is found only in fruits and vegetables, especially citrus fruits, vegetables in the cabbage family, cantaloupe, strawberries, peppers, tomatoes, potatoes, lettuce, papayas, mangoes, and kiwifruit."),
           plotOutput(outputId = "natl_nutr_stats_vitC", width=1000, height=400),
           br(),
           
           # References
           ############################################
           
           # References
           h2("References"),
           p("Vaitla B, Collar D, Smith MR, Myers SS, Rice BL, Golden CD (2018) Predicting nutrient content of ray-finned fishes using phylogenetic information. Nature Communications 9(3742). https://doi.org/10.1038/s41467-018-06199-w"), 
           p("U.S. Department of Health and Human Services and U.S. Department of Agriculture. 2015–2020 Dietary Guidelines for Americans. 8th Edition. December 2015. Available at http://health.gov/dietaryguidelines/2015/guidelines/")
           
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
  
  # Finfish nutrition
  ######################################################################
  
  output$finfishNutrRadarPlot <- renderPlot({
    g <- plot_vaitla_finfish_nutr_radar(vaitla_nutr_preds_long, spp=input$species)
    g
  })
  
  # Finfish nutrition
  output$finfishNutrHistPlot <- renderPlot({
    g <- plot_vaitla_finfish_nutr_hist(vaitla_nutr_preds_long, vaitla_nutr_preds_key, spp=input$species)
    g
  })
  
  
  # National seafood importance
  ######################################################################
  
  # National seafood importance map
  output$natl_seafood_impt_map <- renderPlot({
    g <- plot_natl_seafood_impt_map(pdiet_seafood_cntry_yr, 
                                pnutrient_seafood_cntry_2011, 
                                cntry=input$country)
    g
  })
  
  # National seafood importance
  output$natl_seafood_impt <- renderPlot({
    g <- plot_natl_seafood_impt(pdiet_seafood_cntry_yr, 
                                pnutrient_seafood_cntry_2011, 
                                cntry=input$country)
    g
  })
  
  # National nutrient consumption over time and by age/sex
  ######################################################################
  
  # Seafood nutrients
  #####################################
  
  output$natl_nutr_stats_protein <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Protein")
    g
  })
  
  output$natl_nutr_stats_fat <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Fat")
    g
  })

  output$natl_nutr_stats_pfacids <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Polyunsaturated fatty acids")
    g
  })

  output$natl_nutr_stats_iron <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Iron")
    g
  })

  output$natl_nutr_stats_zinc <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Zinc")
    g
  })

  output$natl_nutr_stats_vitA <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Vitamin A")
    g
  })

  output$natl_nutr_stats_vitC <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Vitamin C")
    g
  })
  
  # Other nutrients
  #####################################
  
  output$natl_nutr_stats_calcium <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Calcium")
    g
  })
  
  output$natl_nutr_stats_calories <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Calories")
    g
  })
  
  output$natl_nutr_stats_carbs <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Carbohydrates")
    g
  })
  
  output$natl_nutr_stats_copper <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Copper")
    g
  })
  
  output$natl_nutr_stats_fiber <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Dietary fiber")
    g
  })
  
  output$natl_nutr_stats_folate <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Folate")
    g
  })
  
  output$natl_nutr_stats_magnesium <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Magnesium")
    g
  })
  
  output$natl_nutr_stats_mfacids <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Monounsaturated fatty acids")
    g
  })
  
  output$natl_nutr_stats_niacin <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Niacin")
    g
  })
  
  output$natl_nutr_stats_phos <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Phosphorus")
    g
  })
  
  output$natl_nutr_stats_potassium <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Potassium")
    g
  })
  
  output$natl_nutr_stats_potassium <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Potassium")
    g
  })
  
  output$natl_nutr_stats_ribo <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Riboflavin")
    g
  })
  
  output$natl_nutr_stats_ribo <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Riboflavin")
    g
  })
  
  output$natl_nutr_stats_sfacids <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Saturated fatty acids")
    g
  })
  
  output$natl_nutr_stats_sfacids <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Saturated fatty acids")
    g
  })
  
  output$natl_nutr_stats_sodium <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Sodium")
    g
  })
  
  output$natl_nutr_stats_thiamin <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Thiamin")
    g
  })
  
  output$natl_nutr_stats_vitb6 <- renderPlot({
    g <- plot_natl_nutrient_supply(nut_cntry_yr, nut_cntry_age_sex_2011, cntry=input$country, nutr="Vitamin B6")
    g
  })

}

shinyApp(ui = ui, server = server)
