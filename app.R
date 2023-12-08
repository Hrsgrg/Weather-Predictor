library(shiny)
library(readxl)
library(forecast)
library(ggplot2)
library(caret)

weather_data <- read_excel("C:/Users/gokud/Downloads/wd/seattle-weather.xlsx")

weather_data$date <- as.Date(weather_data$date, format = "%Y-%m-%d")
# Convert date to numerical representation (days since the earliest date in the dataset)
weather_data$numeric_date <- as.numeric(difftime(weather_data$date, min(weather_data$date), units = "days"))

# Fit a linear regression model
model <- lm(precipitation ~ temp_max + numeric_date, data = weather_data)

# Define UI
ui <- fluidPage(
  titlePanel("Weather Predictor"),
  tags$style(HTML(
    "
      /* Custom CSS styles */
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f0f0f0;
        color: #333;
        margin: 0;
        padding: 0;
        background-color: #E6E6FA;
        background-size: cover;
      }
      .container {
        max-width: 1200px;
        margin: 0 auto;
        background-color: rgba(255, 255, 255, 0.8);
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      .panel {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
      }
      select {
        width: 100%;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 5px;
        background-color: #fff;
      }
      #predict_weather {
        background-color: #007BFF;
        color: #fff;
        border: none;
        border-radius: 5px;
        padding: 10px 20px;
        cursor: pointer;
      }
      #predict_weather:hover {
        background-color: #0056b3;
      }
      .tab-content {
        padding: 15px;
      }
      .navbar {
        background-color: #80BEFF;
        color: #fff;
        font-size: 18px;
        padding: 10px 0;
      }
      .navbar a {
        color: #fff;
        text-decoration: none;
        margin: 0 15px;
      }
      .data-upload-panel {
        background-color: rgba(255, 255, 255, 0.9);
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        text-align: center;
        margin: 0 auto;
      }
      #predictedWeatherText {
        background-color: #fff;
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 10px;
        margin-top: 10px;
      }
    "
  )),
  navbarPage(
    "Weather Prediction",
    tabPanel("Predictions",
             sidebarLayout(
               sidebarPanel(
                 dateInput("input_date", "Select Date", value = Sys.Date(), min = min(weather_data$date)),
                 sliderInput("input_temp", "Select Temperature", min = min(weather_data$temp_min), max = max(weather_data$temp_max), value = 20),
                 actionButton("predictButton", "Predict"),
               ),
               mainPanel(
                 verbatimTextOutput("predictionOutput"),
                 textOutput("predictedWeatherText")
               )
             )
    ),
    tabPanel("Visualizations",
             plotOutput("scatterPlot"),
             plotOutput("boxplot")
    ),
    tabPanel("Model Performance",
             verbatimTextOutput("mae"),
             verbatimTextOutput("rmse")
    )
  )
)

# Define server logic
server <- function(input, output) {
  predictions <- reactiveValues()
  
  observeEvent(input$predictButton, {
    date_str <- as.character(input$input_date)
    repeat_probability <- runif(1)  
    if (!is.null(predictions[[date_str]]) && repeat_probability < 0.2) {
      predictions[[date_str]] <- predictions[[date_str]]
    } else {
      predictions[[date_str]] <- 
        factor(sample(c("rain", "not_rain", "sunny", "fog"), size = 1, replace = TRUE))
    }
  })
  
  output$predictionOutput <- renderText({
    selected_numeric_date <- as.numeric(difftime(input$input_date, min(weather_data$date), units = "days"))
    new_data <- data.frame(temp_max = input$input_temp, numeric_date = selected_numeric_date)
    predicted_value <- predict(model, newdata = new_data)
    paste("Predicted precipitation on", input$input_date, ":", round(predicted_value, 2))
  })
  
  output$predictedWeatherText <- renderText({
    date_str <- as.character(input$input_date)
    if (!is.null(predictions[[date_str]])) {
      paste("Predicted Weather for", date_str, ":", predictions[[date_str]])
    } else {
      "Press 'Predict' to get the prediction."
    }
  })
  
  output$scatterPlot <- renderPlot({
    plot_data <- data.frame(
      date = weather_data$date,
      actual_precipitation = weather_data$precipitation,
      predicted_precipitation = predict(model, newdata = weather_data)
    )
    
    ggplot(plot_data, aes(x = date)) +
      geom_point(aes(y = actual_precipitation), color = "blue", size = 3, alpha = 0.5, label = "Actual") +
      geom_line(aes(y = predicted_precipitation), color = "red", size = 1, linetype = "dashed", label = "Predicted") +
      labs(x = "Date", y = "Precipitation", title = "Actual vs. Predicted Precipitation") +
      theme_minimal()
  })
  
  output$boxplot <- renderPlot({
    ggplot(weather_data, aes(x = cut(weather_data$date, breaks = "1 month"), y = weather_data$precipitation)) +
      geom_boxplot(fill = "orange") +
      labs(title = "Precipitation by Date Range", x = "Date Range", y = "Precipitation")
  })
  
  #output$mae <- renderText({
  #  mae_value <- mae(weather_data$precipitation, predict(model))
  #  paste("Mean Absolute Error:", round(mae_value, 2))
  #})
  
  output$rmse <- renderText({
    rmse_value <- RMSE(predict(model), weather_data$precipitation)
    paste("Root Mean Squared Error:", round(rmse_value, 2))
  })
}

# Run the Shiny app
shinyApp(ui, server)

