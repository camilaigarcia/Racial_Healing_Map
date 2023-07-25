#IMPORTING PACKAGES
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(shinydashboard)
library(reshape2)
library(plotly)
library(ggplot2)
library(hms)
library(DT)
library(stringr)
library(tools)
library(readr)
library(fontawesome)
library(geojsonR)
library(purrr)
library(RColorBrewer)


#IMPORTING DATA FILE
data <- read.csv("merged_outcomes_v2.csv") 
#census_data <- read.csv("chr_race_measures.csv") 
colnames(data)[45] <- "Pct_Black"
colnames(data)[46] <- "Pct_American_Indian"
colnames(data)[47] <- "Pct_Asian"  
colnames(data)[48] <- "Pct_Native_Hawaiian" 
colnames(data)[49] <- "Pct_Hispanic"  
colnames(data)[50] <- "Pct_White" 

#IMPORTING DATA DICTIONARY
dictionary <- read.csv("data_dictionary_V2.csv") 

#combined_data <- left_join(data, census_data, by = "County")

#IMPORTING SPATIAL DATA

#Loading  Counties (Polygons)
pa_bound <- st_read("Pennsylvania_County_Boundaries.shp")
pa_bound <- st_transform(pa_bound, crs = st_crs("+proj=longlat +datum=WGS84"))


#SPATIAL JOIN BETWEEN COUNTIES AND DATA

#Joining data sets
joined_sf <- left_join(pa_bound, data, by = "COUNTY_NAM")

#joined_sf <- subset(joined_sf, select = -c("FIPS.y", "Race"))

#joined_sf$Violent.Crime.Rate <- as.numeric(joined_sf$Violent.Crime.Rate)
#joined_sf$Population <- as.numeric(joined_sf$Population)
#joined_sf$Women.s.Median.Earnings <- as.numeric(joined_sf$Women.s.Median.Earnings)


# joined_sf <- joined_sf %>% rename(Pct_Low_birthweight = X..Low.birthweight, 
#                                   Pct_with.Annual.Mammogram = X..With.Annual.Mammogram, 
#                                   Pct_Children_in_Poverty = X..Children.in.Poverty, 
#                                   Pct_Frequent_Physical_Distress = X..Frequent.Physical.Distress, 
#                                   Pct_Frequent_Mental_Distress = X..Frequent.Mental.Distress, 
#                                   )


# Loop to update column names
for (i in seq_along(colnames(joined_sf))) {
  name <- colnames(joined_sf)[i]
  if (grepl("\\.", name)) {
    new_name <- gsub("\\.", "_", name)
  } else if (startsWith(name, "X")) {
    new_name <- paste0("Pct", substring(name, 2))
  } else {
    new_name <- name
  }
  colnames(joined_sf)[i] <- new_name
}


for (i in seq_along(colnames(joined_sf))) {
  name <- colnames(joined_sf)[i]
  if (startsWith(name, "X")) {
    new_name <- gsub("X", "Pct", name)
    colnames(joined_sf)[i] <- new_name
  }
}



# Check updated column names
#colnames(joined_sf)


#joined_sf$Population <- as.numeric(joined_sf$Population)

joined_sf$Pct_Other <- 100 - (joined_sf$Pct_Black +
                                joined_sf$Pct_American_Indian +
                                joined_sf$Pct_Asian +
                                joined_sf$Pct_Native_Hawaiian +
                                joined_sf$Pct_Hispanic +
                                joined_sf$Pct_White)

#USER INTERFACE

# Define UI for application that plots features -----------
ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                titlePanel(
                  title = div(
                    img(height = 150, width = 170, src = "logo.png"), HTML("<span style='font-weight: bold;'> Pennsylvania's Social and Economic Indicators Map </span>")),
                  windowTitle = "Racial Healing Map"),
                
                
                # Sidebar layout with a input and output definitions --------------
                sidebarLayout(
                  # Inputs: Select variables to plot ------------------------------
                  sidebarPanel(
                    
                    # Set neighborhood of the property -----------------------------
                    selectInput(inputId = "variable",
                                label = "Select an indicator:",
                                choices = list(
                                    "DEMOGRAPHICS" = list(
                                      "Population" = "Population",
                                      "% of Black" = "Pct_Black",
                                      "% of American Indian & Alaska Native" = "Pct_American_Indian",
                                      "% of Asian" = "Pct_Asian",
                                      "% of Native Hawaiian and Other Pacific Islander" = "Pct_Native_Hawaiian",
                                      "% of Hispanic" = "Pct_Hispanic",
                                      "% of White (Non-Hispanic)" = "Pct_White",
                                      "% Less Than 18 Years of Age" = "Pct__Less_Than_18_Years_of_Age",
                                      "% 65 and Over" = "Pct__65_and_Over",
                                      "% Rural" = "Pct__rural"),
                                    
                                    "EDUCATION" = list(
                                      "% Completed High School" = "Pct__Completed_High_School",
                                      "% Some College" = "Pct__Some_College",
                                      "Average Grade Performance (ELA)" = "ELA_Average_Grade_Performance",
                                      "Average Grade Performance (Math)" = "Math_Average_Grade_Performance",
                                      "Spending per-pupil" = "Spending_per_pupil",
                                      "% Disconnected Youth" = "Pct__Disconnected_Youth",
                                      "School Segregation" = "School_Segregation",
                                      "Residential Segregation - Black and White" = "Res_BW_Segregation",
                                      "Residential Segregation - Non-white and White" = "Res_WNW_Segregation"),
                                    
                                    
                                    "HOUSING AND LIVING CONDITIONS" = list(
                                      "% Homeowners" = "Pct__Homeowners",
                                      "% Severe Housing Problems" = "Pct__Severe_Housing_Problems",
                                      "% Severe Housing Cost Burden" = "Pct__Severe_Housing_Cost_Burden",
                                      "% Broadband Access" = "Pct__Broadband_Access",
                                      "Average Daily PM2.5" = "Average_Daily_PM2_5"),
                                    
                                    
                                    "EMPLOYMENT AND ECONOMIC CONDITIONS" = list(
                                      "Median Household Income" = "Median_Household_Income",
                                      #"Women's Median Earnings" = " Women_s_Median_Earnings",
                                      #"Men's Median Earnings" = "Men_s_Median_Earnings",
                                      "Gender Pay Gap" = "Gender_Pay_Gap",
                                      "% Unemployed" = "Pct__Unemployed",
                                      "% Children in Poverty" = "Pct__Children_in_Poverty",
                                      "% Children in Single-Parent Households" = "Pct__Children_in_Single_Parent_Households"),
                                    
                                    
                                    "BEHAVIORAL RISK FACTORS" = list(
                                      "% Excessive Drinking" = "Pct__Excessive_Drinking",
                                      "% Driving Deaths with Alcohol Involvement" = "Pct__Driving_Deaths_with_Alcohol_Involvement",
                                      "Drug Overdose Mortality Rate" = "Drug_Overdose_Mortality_Rate",
                                      "Suicide Rate" = "Suicide_Rate"),
                                    
                                    
                                    "COMMUNITY SAFETY" = list(
                                      "Violence Crime Rate" = "Violent_Crime_Rate",
                                      "Homicide Rate" = "Homicide_Rate",
                                      "Firearm Fatalities Rate" = "Firearm_Fatalities_Rate",
                                      "Juvenile Arrest Rate" = "Juvenile_Arrest_Rate",
                                      "Injury Death Rate" = "Injury_Death_Rate"),
                                    
                                    
                                    "HEALTH AND INSURANCE COVERAGE" = list(
                                      "COVID-19 death rate" = "COVID_19_death_rate",
                                      "HIV Prevalence Rate" = "HIV_Prevalence_Rate",
                                      "% Frequent Physical Distress" = "Pct__Frequent_Physical_Distress",
                                      "% Frequent Mental Distress" = "Pct__Frequent_Mental_Distress",
                                      "% Adults with Diabetes" = "Pct__Adults_with_Diabetes",
                                      "% Fair or Poor Health" = "Pct__Fair_or_Poor_Health",
                                      "% of Smokers" = "Pct__Smokers",
                                      "% Adults with Obesity" = "Pct__Adults_with_Obesity",
                                      "% Food Insecure" = "Pct__Food_Insecure",
                                      "Food Environment Index" = "Food_Environment_Index",
                                      "% Limited Access to Healthy Foods" = "Pct__Limited_Access_to_Healthy_Foods",
                                      "% Uninsured" = "Pct__Uninsured",
                                      "% Insufficient Sleep" = "Pct_Insufficient_Sleep",
                                      "% Enrolled in Free or Reduced Lunch" = "Pct__Enrolled_in_Free_or_Reduced_Lunch",
                                      "% Low birth weight" = "Pct__Low_birthweight",
                                      "Teen Birth Rate" = "Teen_Birth_Rate",
                                      "Life Expectancy" = "Life_Expectancy",
                                      "Age-adjusted Death Rate" = "Age_adjusted_Death_Rate",
                                      "Child Mortality Rate" = "Child_Mortality_Rate",
                                      "Infant Mortality Rate" = "Infant_Mortality_Rate",
                                      "Preventable Hospitalization Rate" = "Preventable_Hospitalization_Rate",
                                      "% With Annual Mammogram" = "Pct__With_Annual_Mammogram")
                                  
                                ),
                                selected = NA),
      
                    
                    # Show data table ---------------------------------------------
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    
                    # Add Download Button
                    downloadButton("download.data", "Download"),
                    h6("Press the download button to save the dataset"),
                    
                  ),
                  
                  # Output --------------------------------------------------------
                  mainPanel(
                    # Tabs to separate each graph
                    tabsetPanel(
                      #Map Tab ----------------------------------------------------
                      tabPanel("Map", shinyjs::useShinyjs(),
                               #Style the background and change the page
                               tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                               
                               # Set neighborhood of the property -----------------------------
                               selectInput(inputId = "county",
                                           label = "Select a county:",
                                           choices = unique(sort(joined_sf$County)),
                                           selected = "Allegheny", 
                                           multiple = TRUE),
                               
                               # Map Output
                               leafletOutput("map") 
  
                               
                               ),
                      
                      
                      
                      # Plots Tab -------------------------------------------------
                      tabPanel("Plots",
                               
                               #Scatter plot 
                               h4("Relation Between Indicators"),
                               # Select variable for y-axis ----------------------------------
                               selectInput(inputId = "y",
                                           label = "Y-axis:",
                                           choices = c("Population" = "Population",
                                                        "% of Black" = "Pct_Black",
                                                        "% of American Indian & Alaska Native" = "Pct_American_Indian",
                                                        "% of Asian" = "Pct_Asian",
                                                        "% of Native Hawaiian and Other Pacific Islander" = "Pct_Native_Hawaiian",
                                                        "% of Hispanic" = "Pct_Hispanic",
                                                        "% of White (Non-Hispanic)" = "Pct_White",
                                                        "% Less Than 18 Years of Age" = "Pct__Less_Than_18_Years_of_Age",
                                                        "% 65 and Over" = "Pct__65_and_Over",
                                                        "% Rural" = "Pct__rural",
                                                        "% Completed High School" = "Pct__Completed_High_School",
                                                        "% Some College" = "Pct__Some_College",
                                                        "Average Grade Performance (ELA)" = "ELA_Average_Grade_Performance",
                                                        "Average Grade Performance (Math)" = "Math_Average_Grade_Performance",
                                                        "Spending per-pupil" = "Spending_per_pupil",
                                                        "% Disconnected Youth" = "Pct__Disconnected_Youth",
                                                        "School Segregation" = "School_Segregation",
                                                        "Residential Segregation - Black and White" = "Res_BW_Segregation",
                                                        "Residential Segregation - Non-white and White" = "Res_WNW_Segregation",
                                                        "% Homeowners" = "Pct__Homeowners",
                                                        "% Severe Housing Problems" = "Pct__Severe_Housing_Problems",
                                                        "% Severe Housing Cost Burden" = "Pct__Severe_Housing_Cost_Burden",
                                                        "% Broadband Access" = "Pct__Broadband_Access",
                                                        "Average Daily PM2.5" = "Average_Daily_PM2_5",
                                                        "Median Household Income" = "Median_Household_Income",
                                                        #"Women's Median Earnings" = " Women_s_Median_Earnings",
                                                        #"Men's Median Earnings" = "Men_s_Median_Earnings",
                                                        "Gender Pay Gap" = "Gender_Pay_Gap",
                                                        "% Unemployed" = "Pct__Unemployed",
                                                        "% Children in Poverty" = "Pct__Children_in_Poverty",
                                                        "% Children in Single-Parent Households" = "Pct__Children_in_Single_Parent_Households",
                                                       "% Excessive Drinking" = "Pct__Excessive_Drinking",
                                                       "% Driving Deaths with Alcohol Involvement" = "Pct__Driving_Deaths_with_Alcohol_Involvement",
                                                       "Drug Overdose Mortality Rate" = "Drug_Overdose_Mortality_Rate",
                                                       "Suicide Rate" = "Suicide_Rate",
                                                       "Violence Crime Rate" = "Violent_Crime_Rate",
                                                       "Homicide Rate" = "Homicide_Rate",
                                                       "Firearm Fatalities Rate" = "Firearm_Fatalities_Rate",
                                                       "Juvenile Arrest Rate" = "Juvenile_Arrest_Rate",
                                                       "Injury Death Rate" = "Injury_Death_Rate",
                                                       "COVID-19 death rate" = "COVID_19_death_rate",
                                                       "HIV Prevalence Rate" = "HIV_Prevalence_Rate",
                                                       "% Frequent Physical Distress" = "Pct__Frequent_Physical_Distress",
                                                       "% Frequent Mental Distress" = "Pct__Frequent_Mental_Distress",
                                                       "% Adults with Diabetes" = "Pct__Adults_with_Diabetes",
                                                       "% Fair or Poor Health" = "Pct__Fair_or_Poor_Health",
                                                       "% of Smokers" = "Pct__Smokers",
                                                       "% Adults with Obesity" = "Pct__Adults_with_Obesity",
                                                       "% Food Insecure" = "Pct__Food_Insecure",
                                                       "Food Environment Index" = "Food_Environment_Index",
                                                       "% Limited Access to Healthy Foods" = "Pct__Limited_Access_to_Healthy_Foods",
                                                       "% Uninsured" = "Pct__Uninsured",
                                                       "% Insufficient Sleep" = "Pct_Insufficient_Sleep",
                                                       "% Enrolled in Free or Reduced Lunch" = "Pct__Enrolled_in_Free_or_Reduced_Lunch",
                                                       "% Low birth weight" = "Pct__Low_birthweight",
                                                       "Teen Birth Rate" = "Teen_Birth_Rate",
                                                       "Life Expectancy" = "Life_Expectancy",
                                                       "Age-adjusted Death Rate" = "Age_adjusted_Death_Rate",
                                                       "Child Mortality Rate" = "Child_Mortality_Rate",
                                                       "Infant Mortality Rate" = "Infant_Mortality_Rate",
                                                       "Preventable Hospitalization Rate" = "Preventable_Hospitalization_Rate",
                                                       "% With Annual Mammogram" = "Pct__With_Annual_Mammogram"),
                                           selected = "COVID_19_death_rate"),
                               
                               
                               # Select variable for x-axis ----------------------------------
                               selectInput(inputId = "x",
                                           label = "X-axis:",
                                           choices = c("Population" = "Population",
                                                       "% of Black" = "Pct_Black",
                                                       "% of American Indian & Alaska Native" = "Pct_American_Indian",
                                                       "% of Asian" = "Pct_Asian",
                                                       "% of Native Hawaiian and Other Pacific Islander" = "Pct_Native_Hawaiian",
                                                       "% of Hispanic" = "Pct_Hispanic",
                                                       "% of White (Non-Hispanic)" = "Pct_White",
                                                       "% Less Than 18 Years of Age" = "Pct__Less_Than_18_Years_of_Age",
                                                       "% 65 and Over" = "Pct__65_and_Over",
                                                       "% Rural" = "Pct__rural",
                                                       "% Completed High School" = "Pct__Completed_High_School",
                                                       "% Some College" = "Pct__Some_College",
                                                       "Average Grade Performance (ELA)" = "ELA_Average_Grade_Performance",
                                                       "Average Grade Performance (Math)" = "Math_Average_Grade_Performance",
                                                       "Spending per-pupil" = "Spending_per_pupil",
                                                       "% Disconnected Youth" = "Pct__Disconnected_Youth",
                                                       "School Segregation" = "School_Segregation",
                                                       "Residential Segregation - Black and White" = "Res_BW_Segregation",
                                                       "Residential Segregation - Non-white and White" = "Res_WNW_Segregation",
                                                       "% Homeowners" = "Pct__Homeowners",
                                                       "% Severe Housing Problems" = "Pct__Severe_Housing_Problems",
                                                       "% Severe Housing Cost Burden" = "Pct__Severe_Housing_Cost_Burden",
                                                       "% Broadband Access" = "Pct__Broadband_Access",
                                                       "Average Daily PM2.5" = "Average_Daily_PM2_5",
                                                       "Median Household Income" = "Median_Household_Income",
                                                       #"Women's Median Earnings" = " Women_s_Median_Earnings",
                                                       #"Men's Median Earnings" = "Men_s_Median_Earnings",
                                                       "Gender Pay Gap" = "Gender_Pay_Gap",
                                                       "% Unemployed" = "Pct__Unemployed",
                                                       "% Children in Poverty" = "Pct__Children_in_Poverty",
                                                       "% Children in Single-Parent Households" = "Pct__Children_in_Single_Parent_Households",
                                                       "% Excessive Drinking" = "Pct__Excessive_Drinking",
                                                       "% Driving Deaths with Alcohol Involvement" = "Pct__Driving_Deaths_with_Alcohol_Involvement",
                                                       "Drug Overdose Mortality Rate" = "Drug_Overdose_Mortality_Rate",
                                                       "Suicide Rate" = "Suicide_Rate",
                                                       "Violence Crime Rate" = "Violent_Crime_Rate",
                                                       "Homicide Rate" = "Homicide_Rate",
                                                       "Firearm Fatalities Rate" = "Firearm_Fatalities_Rate",
                                                       "Juvenile Arrest Rate" = "Juvenile_Arrest_Rate",
                                                       "Injury Death Rate" = "Injury_Death_Rate",
                                                       "COVID-19 death rate" = "COVID_19_death_rate",
                                                       "HIV Prevalence Rate" = "HIV_Prevalence_Rate",
                                                       "% Frequent Physical Distress" = "Pct__Frequent_Physical_Distress",
                                                       "% Frequent Mental Distress" = "Pct__Frequent_Mental_Distress",
                                                       "% Adults with Diabetes" = "Pct__Adults_with_Diabetes",
                                                       "% Fair or Poor Health" = "Pct__Fair_or_Poor_Health",
                                                       "% of Smokers" = "Pct__Smokers",
                                                       "% Adults with Obesity" = "Pct__Adults_with_Obesity",
                                                       "% Food Insecure" = "Pct__Food_Insecure",
                                                       "Food Environment Index" = "Food_Environment_Index",
                                                       "% Limited Access to Healthy Foods" = "Pct__Limited_Access_to_Healthy_Foods",
                                                       "% Uninsured" = "Pct__Uninsured",
                                                       "% Insufficient Sleep" = "Pct_Insufficient_Sleep",
                                                       "% Enrolled in Free or Reduced Lunch" = "Pct__Enrolled_in_Free_or_Reduced_Lunch",
                                                       "% Low birth weight" = "Pct__Low_birthweight",
                                                       "Teen Birth Rate" = "Teen_Birth_Rate",
                                                       "Life Expectancy" = "Life_Expectancy",
                                                       "Age-adjusted Death Rate" = "Age_adjusted_Death_Rate",
                                                       "Child Mortality Rate" = "Child_Mortality_Rate",
                                                       "Infant Mortality Rate" = "Infant_Mortality_Rate",
                                                       "Preventable Hospitalization Rate" = "Preventable_Hospitalization_Rate",
                                                       "% With Annual Mammogram" = "Pct__With_Annual_Mammogram"),
                                           selected = "Pct__Food_Insecure"),
                               
                              
                               plotlyOutput(outputId = "scatterplot"),
                               
                               
                               #Divisor Line 
                               hr(),
                  
                               #Pie Chart
                               h4("Population Breakdown by County"),
                               
                               
                               # Set neighborhood of the property -----------------------------
                               selectInput(inputId = "tcounty",
                                           label = "Select a county:",
                                           choices = unique(sort(joined_sf$County)),
                                           selected = "Allegheny"),
                               
                               plotlyOutput(outputId = "pie.chart"),
                               
                               #Divisor Line 
                               hr(),
                               
                               
                               #Bar Chart
                               h4("Ranking of Counties for Selected Variable"),
                               plotlyOutput(outputId = "bar.chart"),
                      
                          
                              #Pie Chart
                              #hr(),
                              #h4("Population Distribution For Selected County"),
                              #plotlyOutput(outputId = "pie.chart")
                
                      ),
                      
                      # Dictionary Tab -------------------------------------------------
                      tabPanel("Dictionary", 
                               fluidPage(
                                 wellPanel(DT::dataTableOutput(outputId = "dictionary"))
                               )),
                      
                      
                      # Data table Tab ---------------------------------------------
                      tabPanel("Data Table",
                               fluidPage(
                                 wellPanel(DT::dataTableOutput(outputId = "table"))
                                 
                               ))))))

#SERVER

# Define server function required to create the scatter plot -------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    joined_sf %>% filter(!!sym(input$variable) != "")
  })
  
  filtered_data2 <- reactive({
    joined_sf 
  })
  
  
  # Data subset with reactive function for neighborhood layer
  county.subset <- reactive({
    req(input$county)
    filter(joined_sf, County %in% input$county)
  }) 
  
  
  # Data subset with reactive function for neighborhood layer
  pie.subset <- reactive({
    req(input$tcounty)
    filter(joined_sf, County %in% input$tcounty)
  }) 
  
  #UPDATE
  
  output$map <- renderLeaflet({
    DataInf <- filtered_data2()
    pal <- colorNumeric(palette = "Reds", domain = DataInf[[input$variable]], na.color = "lightgray")
    
    leaflet(data = DataInf) %>%
      addProviderTiles("CartoDB.Positron",  options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -77.194527, lat = 41.203323, zoom = 7) %>%
      addPolygons(color = "black",
                  weight = 1,
                  fillOpacity = 1,
                  fillColor = ~ pal(DataInf[[input$variable]]),
                  popup = ~paste0("<span style='font-weight:bold'>County: </span>", DataInf$County, "<br>",
                                  "<span style='font-weight:bold'>Value of Selected Indicator: </span>", round(DataInf[[input$variable]], 2),"<br>",
                                  "<span style='font-weight:bold'>% of Black Population: </span>", round(DataInf[["Pct_Black"]], 2),"<br>",
                                  "<span style='font-weight:bold'>% of Asian Population: </span>", round(DataInf[["Pct_Asian"]], 2),"<br>",
                                  "<span style='font-weight:bold'>% of Hispanic Population: </span>", round(DataInf[["Pct_Hispanic"]], 2),"<br>"
                  )) %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = ~DataInf[[input$variable]],
                title = "Legend",
                labels = legend_labels(pal, DataInf[[input$variable]], "Not available"))
  })
  
  
  # County - Creating county layer using polygons------------
  observe({
    CountyInf <- county.subset()
    leafletProxy("map", data = CountyInf) %>%
      clearGroup(group = "County") %>%
      addPolygons(popup = ~paste0("<b>", County, "</b>"), group = "County", layerId = ~County, fill = FALSE, color = "black")
    
  })
  
  
  # Create Bar Chart -----------------------------------------------------------
  output$bar.chart <- renderPlotly({
    DataInf <- filtered_data2()
    p <- ggplot(data = DataInf, aes(x = !!sym(input$variable), y = reorder(County, !!sym(input$variable)), fill = !!sym(input$variable))) +
      geom_col(color = 'red', width = 1,
               aes(text = paste("County:", County, "<br>",
                                toTitleCase(str_replace_all(input$variable, "\\_", " ")), ":", round(!!sym(input$variable), 2)))) +
      ylab("County") +
      xlab(gsub("_", " ", input$variable)) +
      scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd"), na.value = "gray90") +
      theme_classic() +
      guides(fill = guide_legend(title = NULL, keywidth = 1.5, keyheight = 0.7, override.aes = list(size = 3)))
    
    ggplotly(p, tooltip = "text", height = 800, width = 1000)
  })
  
  
  # Create scatter plot --------------------------------------------------------
  library(stringr)
  output$scatterplot <- renderPlotly({
    ggplotly(
      ggplot(data = filtered_data(), aes_string(x = input$x, y = input$y, text = "County")) +
        geom_point(color = "orange") +
        scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
              axis.title.y = element_text(face = "bold")) + 
        labs(x = str_replace_all(toTitleCase(input$x), "_", " "),
             y = str_replace_all(toTitleCase(input$y), "_", " "))
    )  
  })
  
  
  # # Create Pie Chart----------------------------------------------------------
  
  output$pie.chart <- renderPlotly({
    # Plotting the pie chart using plot_ly() function
    CountyInf <- pie.subset()
    
    oranges <- brewer.pal(7, "Oranges")
    
    # Combine reds and oranges into a single color scale
    red_orange_colors <- c(oranges)
    
    # Filter the data frame based on the county name
    County_data <- CountyInf[CountyInf$County == input$tcounty]
    
    pie_data <- c("Hispanic" = County_data$Pct_Hispanic,
                  "Black" = County_data$Pct_Black,
                  "Asian" = County_data$Pct_Asian,
                  "American Indian & Alaska Native" = County_data$Pct_American_Indian,
                  "Native Hawaiian and Other Pacific Islander" = County_data$Pct_Native_Hawaiian,
                  "White (Non-Hispanic)" = County_data$Pct_White,
                  "Other" = County_data$Pct_Other)
    
    # Create a named vector of labels for the pie chart
    pie_labels <- names(pie_data)
    
    pie <- plot_ly(labels = pie_labels, values = pie_data,
                   type = "pie",
                   textinfo = "none", 
                   marker = list(colors = red_orange_colors),
                   hovertemplate = "<b>%{label}</b><br>Percent of total: %{percent}<extra></extra>")
    
    return(pie)
    
  })
  
  
  # Print dictionary for specific columns and formated -------------------------
  output$dictionary <- DT::renderDataTable(
    if(input$show_data){
      #housing.updated <- housing.subset() %>%
      DataInf <- dictionary
      DT::datatable(data = DataInf,
                    options = list(pageLength = 30, scrollX = TRUE),
                    rownames = FALSE)
      #colnames = c("COVID-19 death rate", "% Frequent Physical Distress",
      #             "% Frequent Mental Distress", "% Adults with Diabetes",
      #             "HIV Prevalence Rate", "% Food Insecure",
      #             "% Limited Access to Healthy Foods", "% Insufficient Sleep"))
    }
  )
  
  
  # Print data table for specific columns and formatted -------------------------
  output$table <- DT::renderDataTable(
    if(input$show_data){
      #housing.updated <- housing.subset() %>%
      DataInf <- filtered_data()
      st_geometry(DataInf) <- NULL
      DT::datatable(data = DataInf[, c(26:90)],
                    options = list(pageLength = 30, scrollX = TRUE),
                    rownames = FALSE)
      #colnames = c("COVID-19 death rate", "% Frequent Physical Distress",
      #             "% Frequent Mental Distress", "% Adults with Diabetes",
      #             "HIV Prevalence Rate", "% Food Insecure",
      #             "% Limited Access to Healthy Foods", "% Insufficient Sleep"))
    }
  )
  
  # Download data function------------------------------------------------
  output$download.data <- downloadHandler(
    filename = function() {
      paste("publichealth.data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
}

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)
















