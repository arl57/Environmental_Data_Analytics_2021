#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
# Read in PeterPaul processed dataset for nutrients. 
# Specify the date column as a date
# Remove negative values for depth_id 
# Include only lakename and sampledate through po4 columns
nutrient_data <- read.csv("./Lessons/12_Shiny/PeterPaulApp_Partial/Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <-  nutrient_data %>%
  filter(depth_id >= 0) %>%
  select(lakename, sampledate:po4)
  

#### Define UI ----
ui <- fluidPage(theme = shinytheme("yeti"),
  # Choose a title
  titlePanel("Nutrient Data for Peter Lake and Paul Lake at the NTL-LTER"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "y",
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tn_ug"),
      
      # Select depth
      checkboxGroupInput(inputId = "fill",
                         label = "Depth ID",
                         choices = unique(nutrient_data$depth_id),
                         selected = c(1,7)),
      
      # Select lake
      checkboxGroupInput(inputId = "shape",
                         label = "Lake",
                         choices = c("Peter Lake", "Paul Lake"),
                         selected = "Paul Lake"),

      # Select date range to be plotted
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("1991-05-01"),
                  max = as.Date("2016-12-31"),
                  value = c(as.Date("1995-01-01"), as.Date("1999-12-31")))),
      
    # Output: Description, lineplot, and reference
    mainPanel(
      # Specify a plot output
      plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
      # Specify a table output
      tableOutput("table_out")
    )))

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
        nutrient_data %>%
         # Filter for dates in slider range
         filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
         # Filter for depth_id selected by user
         filter(depth_id %in% input$fill) %>%
         # Filter for lakename selected by user
         filter(lakename %in% input$shape) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot(filtered_nutrient_data(),#dataset
               aes_string(x = "sampledate", y = input$y, 
                          fill = "depth_id", shape = "lakename")) +
          geom_point() +
          theme_classic() +
          #scale_shape_manual() +
          labs(x = "Date", y = "Concentration ug/L", shape = "Lake", fill = "Depth ID") +
          #scale_fill_distiller()
          scale_fill_viridis_c(option = "viridis", begin = 1, end = 0)
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints(filtered_nutrient_data(),# dataset, 
                                    input$scatterplot_brush) # input
       }) 
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

# ***I know the colors are not showing up properly but I cannot figure out how to fix it***