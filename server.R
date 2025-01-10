# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

# Import model_prediction.R which contains methods to call OpenWeather API and make predictions
source("model_prediction.R")

# Create an RShiny server
shinyServer(function(input, output) {
  
  # Generate city_weather_bike_df using the function from model_prediction.R
  city_weather_bike_df <- generate_city_weather_bike_data()
  
  # Create a data frame called `cities_max_bike` with each row containing city location info 
  # and max bike prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII, LNG, LAT) %>%
    summarize(
      MAX_BIKE_PREDICTION = max(BIKE_PREDICTION, na.rm = TRUE),
      BIKE_PREDICTION_LEVEL = BIKE_PREDICTION_LEVEL[which.max(BIKE_PREDICTION)],
      LABEL = LABEL[which.max(BIKE_PREDICTION)],
      DETAILED_LABEL = DETAILED_LABEL[which.max(BIKE_PREDICTION)]
    )
  
  # Observe dropdown selection event
  observeEvent(input$city_dropdown, {
    
    if (input$city_dropdown == "All") {
      
      # Render map for all cities with circle markers and popup weather LABEL
      output$city_bike_map <- renderLeaflet({
        leaflet(data = cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~LNG,
            lat = ~LAT,
            radius = ~case_when(
              BIKE_PREDICTION_LEVEL == "small" ~ 6,
              BIKE_PREDICTION_LEVEL == "medium" ~ 10,
              BIKE_PREDICTION_LEVEL == "large" ~ 12
            ),
            color = ~case_when(
              BIKE_PREDICTION_LEVEL == "small" ~ "green",
              BIKE_PREDICTION_LEVEL == "medium" ~ "yellow",
              BIKE_PREDICTION_LEVEL == "large" ~ "red"
            ),
            fillOpacity = 0.7,
            popup = ~LABEL
          )
      })
      
    } else {
      
      # Filter data for the selected city
      selected_city <- city_weather_bike_df %>%
        filter(CITY_ASCII == input$city_dropdown)
      
      # Render map for the selected city with a single marker and DETAILED_LABEL popup
      output$city_bike_map <- renderLeaflet({
        leaflet(data = selected_city) %>%
          addTiles() %>%
          addMarkers(
            lng = ~LNG,
            lat = ~LAT,
            popup = ~DETAILED_LABEL
          )
      })
      
      # Render temperature trend line plot
      output$temp_line <- renderPlot({
        ggplot(selected_city, aes(x = as.POSIXct(FORECASTDATETIME), y = TEMPERATURE)) +
          geom_line(color = "blue") +
          geom_point(color = "blue") +
          geom_text(aes(label = round(TEMPERATURE, 1)), vjust = -1) +
          labs(title = paste("Temperature Trend in", input$city_dropdown),
               x = "Datetime", y = "Temperature (°C)") +
          theme_minimal()
      })
      
      # Render bike-sharing demand prediction trend line plot
      output$bike_line <- renderPlot({
        ggplot(selected_city, aes(x = as.POSIXct(FORECASTDATETIME), y = BIKE_PREDICTION)) +
          geom_line(color = "red") +
          geom_point(color = "red") +
          geom_text(aes(label = round(BIKE_PREDICTION)), vjust = -1) +
          labs(title = paste("Bike Demand Prediction Trend in", input$city_dropdown),
               x = "Datetime", y = "Bike Demand Prediction") +
          theme_minimal()
      })
      
      # Render text output for clicked point on bike-sharing demand prediction trend line
      output$bike_date_output <- renderText({
        req(input$plot_click)
        clicked_point <- nearPoints(selected_city, input$plot_click, xvar="FORECASTDATETIME", yvar="BIKE_PREDICTION")
        if (nrow(clicked_point) > 0) {
          paste0("Datetime: ", clicked_point$FORECASTDATETIME[1], 
                 "\nBike Demand Prediction: ", clicked_point$BIKE_PREDICTION[1])
        } else {
          "No point clicked."
        }
      })
      
      # Render humidity vs. bike-sharing demand correlation plot
      output$humidity_pred_chart <- renderPlot({
        ggplot(selected_city, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(color = "purple") +
          geom_smooth(method = lm, formula = y ~ poly(x, 4), color="blue") +
          labs(title = paste("Humidity vs Bike Demand in", input$city_dropdown),
               x = "Humidity (%)", y = "Bike Demand Prediction") +
          theme_minimal()
      })
    }
  })
})
