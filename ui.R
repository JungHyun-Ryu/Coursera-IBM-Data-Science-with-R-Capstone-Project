# Load required libraries
require(leaflet)

# Create a RShiny UI
shinyUI(
  fluidPage(
    padding = 5,
    titlePanel("Bike-sharing Demand Prediction App"),
    
    # Create a side-bar layout
    sidebarLayout(
      
      # Main panel to show cities on a leaflet map
      mainPanel(
        leafletOutput("city_bike_map", width = "100%", height = "600px")
      ),
      
      # Sidebar panel to show dropdown for city selection and plots
      sidebarPanel(
        selectInput(
          inputId = "city_dropdown",
          label = "Select a City:",
          choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris"),
          selected = "All"
        ),
        plotOutput("temp_line", height = "300px"),
        plotOutput("bike_line", height = "300px", click = "plot_click"),
        verbatimTextOutput("bike_date_output"),
        plotOutput("humidity_pred_chart", height = "300px")
      )
    )
  )
)
