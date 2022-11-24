library(shiny)
library(tidyverse)
library(datateachr)
library(ggplot2)
library(DT)

#altering dataset to make it more functional:
trees <- vancouver_trees %>%
  #convert diameter from inches to feet 
  mutate(diameter = diameter/12) %>%
  #calculate sq feet of trees
  mutate(tree_size = ((2 * 3.14 *(diameter/2)*height_range_id) + (2*3.14)*((diameter/2)**2))) %>%
  #delete trees of size 0
  filter(tree_size > 0) %>%
  #categorize trees by size
  mutate(tree_size_categories = factor(cut(tree_size, breaks = c(-Inf,200,800,1500,8000,Inf), labels = c("X-Small", "Small", "Medium", "Large", "X-Large")))) %>%
  #rename categories for better readability
  rename(
    genus = genus_name,
    neighbourhood = neighbourhood_name,
    size = tree_size_categories)

#create vector for easy input variable
unique_genuses <-trees %>%
  pull(genus) %>%
  unique() %>%
  sort()

ui <- fluidPage(
  titlePanel("Vancouver Trees", windowTitle = "Vancouver Trees"),
  sidebarLayout(
    sidebarPanel(
      h4(strong("Map Options:")),
      # Map Colour Selection Feature: radio buttons added to customize map colour coding to reflect different variables. This helps to visualize tree variables in relation to their location in the city.
      radioButtons("colourInput", "Colour code map by:",
                   choices = c("neighbourhood", "genus", "size"),
                   selected = "neighbourhood"),
      h4(strong("Table Options:")),
      
      #Tree genus selection feature: Allows users to customize table results and investigate trees types of interest
      selectInput("genusInput", "Which tree genus is your favourite?",
                  choices = unique_genuses),
      
      #Tree size selection feature: Allows users to select trees within a specific size range and customize table output accordingly.
      sliderInput("sizeInput", "Tree size range (sq.ft.):", 
                  min = round(min(trees$tree_size)), 
                  max = round(max(trees$tree_size)),
                  value = c(round(min(trees$tree_size)), round(max(trees$tree_size))))),
    mainPanel(
      h3(strong("Coordinate Plot of Trees")),
      plotOutput("tree_plot"),
      br(), br(),
      h3(strong("Trees By Genus")),
      # Interactive table feature: converted table to be interactive using the DT package
      DT::dataTableOutput("tree_table"))
  )
)


server <- function(input, output) {
  output$tree_plot <- renderPlot({
      ggplot(trees, mapping = aes(latitude, longitude)) +
      geom_point(size = 0.2, aes_string(colour = input$colourInput)) +
      theme(legend.key.size = unit(1, 'cm'),
            legend.key.height = unit(1, 'cm'), 
            legend.key.width = unit(1, 'cm'), 
            legend.title = element_text(size=7), 
            legend.text = element_text(size=7)) 
  })

  
  output$tree_table <- DT::renderDataTable({
    tree_table <- trees %>%
      select(tree_id, std_street, genus, species_name, neighbourhood, date_planted, tree_size, size) %>%
      rename(street = std_street,
             species = species_name,
             size_category = size) %>%
      filter(genus == input$genusInput,
             tree_size >= input$sizeInput[1],
             tree_size <= input$sizeInput[2])
    
    datatable(tree_table)
    
  })
}


shinyApp(ui = ui, server = server)