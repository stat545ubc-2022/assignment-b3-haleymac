library(shiny)
library(tidyverse)
library(datateachr)
library(lubridate)
library(DT)
library(leaflet)
library(Polychrome)
library(viridis)
library(bslib)


#altering dataset to make it more functional:
trees <- vancouver_trees %>%
  na.omit %>%
  #convert diameter from inches to feet 
  mutate(diameter = diameter/12) %>%
  #calculate sq feet of trees
  mutate(tree_size = ((2 * 3.14 *(diameter/2)*height_range_id) + (2*3.14)*((diameter/2)**2))) %>%
  #delete trees of size 0
  filter(tree_size > 0) %>%
  #categorize trees by size
  mutate(tree_size_categories = factor(cut(tree_size, breaks = c(-Inf,10,50,200,500,Inf), labels = c("X-Small", "Small", "Medium", "Large", "X-Large")))) %>%
  select(tree_id, std_street, genus_name, species_name, neighbourhood_name, date_planted, tree_size, tree_size_categories, latitude, longitude) %>%
  rename(
    genus = genus_name,
    neighbourhood = neighbourhood_name,
    street = std_street,
    species = species_name)

#add tree age to dataset:
this_year = year(today())
trees<- mutate(trees, tree_age_years = this_year - year(ymd(date_planted)))


#create genus vector for input variable for genus selection feature 
unique_genuses <-trees %>%
  pull(genus) %>%
  unique() %>%
  sort()

#create neighbourhood vector for neighbourhood selection feature 
neighbourhood_vector <- trees %>%
  pull(neighbourhood) %>%
  unique()

#create size and age vectors for colour selection: 
size_vector <- trees %>%
  pull(tree_size_categories) %>%
  unique()
age_vector <- trees %>%
  pull(tree_age_years) %>%
  sort() %>%
  unique()

#create colour palettes for colour-coding feature: 

#neighbouhood colour palette:
neighbourhood_colours = glasbey.colors(22)
neighbourhood_pal <- colorFactor(
  neighbourhood_colours,
  neighbourhood_vector)

#tree size colour palette:
size_colours = glasbey.colors(4)
size_pal <- colorFactor(
  size_colours,
  size_vector)

#age colour palette:
age_pal <- colorNumeric(
  magma(33),
  age_vector)


ui <- fluidPage(
  #Theme chosen to make the app look better:
  theme = bs_theme(
    bg = "#FAF0E6", 
    fg = "#008000"),
  headerPanel("Vancouver Trees", windowTitle = "Vancouver Trees"),
  sidebarLayout(
    sidebarPanel(
      h3("Set the following parameters to view the resulting trees on the map:"),
      #Tree genus selection feature: Allows users to customize table results and investigate trees types of interest
      selectInput("genusInput", "Tree Genus:",
                  choices = unique_genuses),
      
      #Age selection feature: Allows users to select trees within a specified age range 
      sliderInput("ageInput", "Tree age range (years):",
                  min = 0,
                  max = max(trees$tree_age_years),
                  value = c(0, max(trees$tree_age_years))),
      
      #Tree size selection feature: Allows users to select trees within a specific size range and customize table output accordingly.
      sliderInput("sizeInput", "Tree size range (sq.ft.):", 
                  min = round(min(trees$tree_size)), 
                  max = round(max(trees$tree_size)),
                  value = c(round(min(trees$tree_size)), round(max(trees$tree_size)))),
      
      #Neighbourhood selection feature
      checkboxGroupInput("neighbourhoodInput", "Neighbourhood(s):", 
                         neighbourhood_vector, 
                         selected = "MARPOLE"),
    
      #Colour-coding selection feature: Allows users to decide which variable to colour-code the points on the map with 
      radioButtons("choiceInput", "Colour-Code Trees on Map by:",
                   choices = c("Neighbourhood", "Size Category", "Age", "Don't Colour Code"),
                   selected = "Don't Colour Code")
      ),

    mainPanel(
      br(),
      #Inserted image to make the app look better:
      img(src='trees.png', height="10%", width="100%"),
      h2("Explore the Trees of Vancouver"),
      #Text output feature that tells users how many trees match their specifications 
      p("There are ", textOutput("tree_counter", inline=T), " trees that match your parameters."),
      br(), br(),
      h3(strong("Vancouver Tree Map")),
      #Map feature: allows a user to see trees within their filtered dataset on a map
      leafletOutput("tree_map"),
      br(), br(),
      h3(strong("Table of Trees Within Your Specified Parameters:")),
      # Interactive table feature: converted table to be interactive using the DT package
      DT::dataTableOutput("tree_table")
      )
  )
)



server <- function(input, output) {
  
  filtered_trees <- reactive({
    trees %>%
      filter(genus == input$genusInput,
             tree_size >= input$sizeInput[1],
             tree_size <= input$sizeInput[2],
             neighbourhood %in% input$neighbourhoodInput,
             tree_age_years >= input$ageInput[1],
             tree_age_years <= input$ageInput[2])})
  
  colour_coding_choice <- reactive({
    if (input$choiceInput == "Neighbourhood"){colour_coding_choice <- ~ neighbourhood_pal(neighbourhood)}
    else if (input$choiceInput == "Size Category"){colour_coding_choice <- ~ size_pal(tree_size_categories)}
    else if (input$choiceInput == "Age"){colour_coding_choice <- ~ age_pal(tree_age_years)}
    else {colour_coding_choice <- "blue"}})

  output$neighbourhood_plot <- renderPlot({
    no_neighbourhood_filtered_trees() %>%
      count(neighbourhood, .drop = FALSE) %>%
      ggplot(aes(neighbourhood, n)) +
      geom_col() +
      scale_x_discrete(drop = FALSE)
  })
  
  output$tree_map <- renderLeaflet({
    leaflet(filtered_trees()) %>% 
      addTiles() %>%
      setView(lng = -123.0978, lat = 49.24655, zoom = 12) %>%
      fitBounds(-123.2238, 49.20073, -123.0171, 49.29403) %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = 2, 
                       color = colour_coding_choice(), 
                       popup = NULL, 
                       stroke = FALSE, fillOpacity = 0.5)
    })
   


  output$tree_table <- DT::renderDataTable({
    datatable(filtered_trees())
    })
  
  
  output$tree_counter <- renderText({
    {nrow(filtered_trees())}
    
  })
}

shinyApp(ui = ui, server = server)
