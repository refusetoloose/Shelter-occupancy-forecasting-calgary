library(shiny)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(zoo)
library(tseries)

# Load and preprocess the data
data <- read.csv("shelter_data.csv")
data$date <- as.Date(data$date, format="%m/%d/%Y")

# Drop unnecessary columns
data <- data[, !(colnames(data) %in% c("Organization", "Shelter", "Daytime", "year", "month"))]

# Shelters and Shelter Types to exclude
exclude_shelter_types <- c("COVID19 Expanded Shelter", 
                           "COVID19 Isolation Site", 
                           "COVID19 Social Distancing Measures",
                           "Emergency",
                           "Winter Emergency",
                           "Winter Emergency - Temperature Dependent")

exclude_shelter_names <- tolower(c(
  "Alpha House Lethbridge Emergency Intox",
  "Alpha House - Winter Emergency", 
  "Blackfoot Family Lodge Society - Short Term Supportive", 
  "Calgary YWCA - Winter Emergency", 
  "Edwardson Place - Short Term Supportive", 
  "Elizabeth House - Short Term Supportive", 
  "Hope Mission - Youth",
  "Hillhurst Centre",
  "John Howard Society - Wildfire",
  "Lakeland Out of the Elements",
  "Late Night Café",
  "Leduc Hub - Wildfire",
  "Mustard Seed - Winter Emergency",
  "Reflections Society for Empowering People to Succeed - Wildfire",
  "river front",
  "riverfront - combined",
  "river front - short term supportive",
  "river front - winter emergency", 
  "Safe Harbour Society - Peoples Place", 
  "Safe Harbour Society - Warming Centre",
  "Safe Harbour Society - Winter Emergency",
  "Sagitawa Basement Shelter - Wildfire",
  "Town of Drayton Valley Community Mats",
  "Wapiti Community Dorm - Intox Mats",
  "Wapiti House - Wildfire"
))

# Filter the data to exclude specific ShelterType and ShelterName values
data_filtered <- data %>%
  mutate(ShelterName = str_replace(ShelterName, "Brenda’s House", "Brendas House")) %>%
  mutate(OccupancyRate = (Overnight / Capacity) * 100) %>%
  select(date, city, ShelterType, ShelterName, OccupancyRate) %>%
  filter(!(ShelterType %in% exclude_shelter_types | tolower(ShelterName) %in% exclude_shelter_names)) %>%
  filter(!(ShelterType %in% "Women Emergency" & ShelterName %in% "Mustard Seed")) %>%
  filter(city == "Calgary") %>%
  filter(!is.na(OccupancyRate))  #This removes only missing values, not zeros

# Save the cleaned data to a CSV file
write.csv(data_filtered, "data_cleaned.csv", row.names = FALSE)

# Define UI
ui <- fluidPage(
  
  # Custom Header with title and styling
  tags$style(HTML("
    .custom-header {
      background-color: #000000; 
      color: #FFFFFF;
      text-align: center; 
      padding: 30px;
      font-family: Arial, sans-serif;
    }

    .custom-header h1 {
      font-size: 40px;
      margin: 0;
    }

    #Sidebar and main content styling
    .sidebar-panel {
      background-color: #f4f4f4;
      padding: 20px;
      border-radius: 5px;
    }

    .main-panel {
      padding: 20px;
      background-color: #ffffff;
      border-radius: 5px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
    }

    .main-content {
      padding-bottom: 80px;
    }

    .footer {
      position: fixed;
      left: 0;
      bottom: 0;
      width: 100%;
      background-color: #f4f4f4;
      color: #000000;
      text-align: center;
      padding: 15px;
      font-size: 10px;
      font-family: Arial, sans-serif;
    }

    .footer p {
      margin: 5px 0;
    }

    .footer a {
      color: #000000;
      text-decoration: none;
    }

    .footer a:hover {
      text-decoration: underline;
    }

    # Custom button styling
    .action-button {
      background-color: #000000;
      color: white;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
      border-radius: 5px;
      cursor: pointer;
    }

    .action-button:hover {
      background-color: #000000;
      color: white;
    }
    
    #occupancy_level_output {
      font-size: 16px;
      font-weight: bold;
      text-align: left;
      background-color: blue;
      color: white;
      padding: 10px;
      border-radius: 5px;
    }
  ")),
  
  # Header section
  tags$div(class = "custom-header",
           h1("Shelter Occupancy Rate Forecast in the City of Calgary")
  ),
  
  # Sidebar and main layout
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      selectInput("city", "Select City:", 
                  choices = c("Calgary"), 
                  selected = "Calgary"),  
      selectInput("shelter_type", "Select Shelter Type:", choices = unique(data_filtered$ShelterType)),
      selectInput("shelter_name", "Select Shelter Name:", choices = NULL),
      sliderInput("forecast_days", 
                  "Select Forecast Days:", 
                  min = 0, max = 730, value = 365, step = 1),
      actionButton("start_forecast", "Start Forecast", class = "action-button")
    ),
    mainPanel(
      class = "main-panel",
      div(class = "main-content",
          plotOutput("time_series_plot"),
          plotOutput("forecast_plot"),
          div(id = "occupancy_level_output", 
              tableOutput("occupancy_level_output")),
          tableOutput("model_summary"),  
          tableOutput("debug_output"),  
          downloadButton("download_data", "Download Forecast Data")
      )
    )
  ),
  
  # Footer content
  div(
    class = "footer",
    p(style = "margin: 0;", "This project is for educational purposes only | © 2025 All rights reserved."),
    p(style = "margin: 0;", "Developed by Team: Deanna Rose Quiambao, Thi Hanh Nguyen Phan, Saran Poocharoen, Prajwal Nagaraj, & Genevieve Ababa"),
    p(style = "margin: 0;", "For inquiries, please contact ", 
      a(href = "mailto:quiambao.deannarose@gmail.com", "quiambao.deannarose@gmail.com"))
  )
)


# Define Server
server <- function(input, output, session) {
  
  # Update Shelter Name options based on selected City and ShelterType
  observe({
    df <- data_filtered %>%
      filter(city == input$city, ShelterType == input$shelter_type) %>%
      select(ShelterName) %>%
      distinct()
    
    df <- df %>% filter(!(tolower(ShelterName) %in% exclude_shelter_names))
    
    updateSelectInput(session, "shelter_name", choices = df$ShelterName, selected = NULL)
    
    if (nrow(df) == 0) {
      updateSelectInput(session, "shelter_name", choices = NULL, selected = NULL)
      showNotification("No shelters available for the selected city and shelter type.", type = "error")
    }
  })
  
  filtered_data <- reactive({
    req(input$city, input$shelter_type, input$shelter_name)
    
    df <- data_filtered %>%
      filter(city == input$city,
             ShelterType == input$shelter_type,
             ShelterName == input$shelter_name) %>%
      group_by(date) %>%
      summarise(OccupancyRate = mean(OccupancyRate, na.rm = TRUE), .groups = "drop")
    
    df <- df %>% drop_na(OccupancyRate)  # Remove rows with NA values
    
    # Detect and remove outliers using IQR method
    Q1 <- quantile(df$OccupancyRate, 0.25)
    Q3 <- quantile(df$OccupancyRate, 0.75)
    IQR_value <- IQR(df$OccupancyRate)
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Filter out outliers
    df_no_outliers <- df %>% filter(OccupancyRate >= lower_bound & OccupancyRate <= upper_bound)
    
    return(df_no_outliers)
  })
  
  output$time_series_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)  # Ensure data is available
    
    ggplot(df, aes(x = date, y = OccupancyRate)) +
      geom_line(color = "#000000") +
      labs(title = "Historical Occupancy Rate", x = "Date", y = "Occupancy Rate (%)") +
      theme_minimal()
  })
  
  observeEvent(input$start_forecast, {
    df <- filtered_data()
    req(nrow(df) > 0)  # Ensure there is data
    
    # Convert data to time series object with daily frequency
    ts_data <- ts(df$OccupancyRate, frequency = 365, start = c(year(min(df$date)), yday(min(df$date))))
    
    # Check for missing values again
    if (any(is.na(ts_data))) {
      showNotification("Time series contains missing values. Cannot proceed with forecasting.", type = "error")
      return(NULL)
    }
    
    if (length(ts_data) < (2 * 365)) {  # Ensure at least two years of data
      showNotification("Not enough data for seasonal naïve forecasting.", type = "error")
      return(NULL)
    }
    
    forecast_period <- input$forecast_days
    
    # Fit the Seasonal and Trend decomposition using Loess Forecasting with 90% confidence interval
    fit <- stlf(ts_data, method = "arima", h = forecast_period, level = 90)  # Change confidence level to 90%
    
    last_date <- max(df$date)
    forecast_dates <- seq.Date(from = last_date + 1, by = "day", length.out = forecast_period)
    
    forecast_df <- data.frame(
      date = forecast_dates,
      forecasted_occupancy_rate = round(as.numeric(fit$mean), 2),
      lower_90 = round(as.numeric(fit$lower[,1]), 2),
      upper_90 = round(as.numeric(fit$upper[,1]), 2)
    )
    
    # Calculate the average of the forecasted occupancy rate
    avg_forecasted_occupancy <- mean(forecast_df$forecasted_occupancy_rate, na.rm = TRUE)
    
    # Determine the occupancy level based on average forecasted occupancy rate
    if (avg_forecasted_occupancy < 60) {
      occupancy_level <- "Low Occupancy. Resources are underutilized. Opportunity to optimize space or services."
    } else if (avg_forecasted_occupancy >= 60 && avg_forecasted_occupancy < 85) {
      occupancy_level <- "Moderate Occupancy. Normal operations, within capacity. Ensure monitoring for possible increases in demand."
    } else if (avg_forecasted_occupancy >= 85 && avg_forecasted_occupancy <= 100) {
      occupancy_level <- "High Occupancy. Approaching full capacity. Risk of overcrowding. Consider additional resources or management strategies."
    } else {
      occupancy_level <- "Over Occupancy. Immediate action required to provide additional beds and resources."
    }
    
    # Output the occupancy level to the main panel
    output$occupancy_level_output <- renderText({
      paste("The average forecasted occupancy rate for the next", input$forecast_days, "days is", round(avg_forecasted_occupancy,2), "%.", 
            "The occupancy level is", occupancy_level)
    })
    
    #Plot the forecast
    output$forecast_plot <- renderPlot({
      ggplot() +
        geom_line(data = df, aes(x = date, y = OccupancyRate), color = "black") +
        geom_line(data = forecast_df, aes(x = date, y = forecasted_occupancy_rate), color = "blue") +
        geom_ribbon(data = forecast_df, aes(x = date, ymin = lower_90, ymax = upper_90), fill = "blue", alpha = 0.2) +
        labs(title = "Occupancy Rate Forecast - Seasonal and Trend Decomposition (ARIMA)", x = "Date", y = "Predicted Occupancy Rate (%)") +
        theme_minimal()
    })
    
    # Compute performance metrics
    actual_values <- tail(df$OccupancyRate, forecast_period)
    predicted_values <- fit$mean
    
    # Ensure matching lengths
    min_length <- min(length(actual_values), length(predicted_values))
    actual_values <- actual_values[1:min_length]
    predicted_values <- predicted_values[1:min_length]
    
    # Compute the error
    errors <- actual_values - predicted_values
    errors[abs(errors) < 1e-6] <- 0  # Set small errors to zero
    
    # Calculate metrics
    me <- mean(errors, na.rm = TRUE)
    rmse <- sqrt(mean(errors^2, na.rm = TRUE))
    mae <- mean(abs(errors), na.rm = TRUE)
    mpe <- mean((errors / actual_values) * 100, na.rm = TRUE)
    mape <- mean(abs(errors / actual_values) * 100, na.rm = TRUE)
    mase <- mae / mean(abs(diff(actual_values)), na.rm = TRUE)
    acf1 <- acf(errors, lag.max = 1, plot = FALSE)$acf[2]
    theil_u <- sqrt(mean((predicted_values - actual_values)^2, na.rm = TRUE)) / sqrt(mean(actual_values^2, na.rm = TRUE))
    
    # Create a summary of metrics
    model_metrics <- data.frame(
      Metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "Theil's U"),
      STLF_Arima = c(round(me, 2), round(rmse, 2), round(mae, 2), round(mpe, 2), round(mape, 2), round(mase, 2), round(acf1, 2), round(theil_u, 2))
    )
    
    model_metrics_t <- as.data.frame(t(model_metrics))
    colnames(model_metrics_t) <- model_metrics_t[1,]
    model_metrics_t <- model_metrics_t[-1, , drop = FALSE]
    
    # Display model summary
    #output$model_summary <- renderTable({
    #  model_metrics_t
    #}, rownames = TRUE)
    
    # Display forecast data for debugging
    output$debug_output <- renderTable({
      forecast_df %>%
        mutate(date = format(date, "%m/%d/%Y"))
    })
    
    # Enable data download
    output$download_data <- downloadHandler(
      filename = function() {
        paste("forecast_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(forecast_df, file, row.names = FALSE)
      }
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)