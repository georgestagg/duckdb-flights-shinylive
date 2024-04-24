library(shiny)
library(bslib)
library(ggplot2)
library(duckplyr)
Sys.setenv(DUCKPLYR_FORCE = TRUE)

ui <- page_sidebar(
  title = "Shinylive demo showing flight data processed by {duckplyr}",
  sidebar = sidebar(
    selectInput(inputId = "airport",
                label = "NYC Airport:",
                choices = c("EWR", "LGA", "JFK"),
                selected = "JFK"),
    dateInput("date", "Date:", value = "2013-06-18", min = "2013-01-01", max = "2013-12-31")
  ),
  tabsetPanel(
    tabPanel("Arrival Delays", plotOutput("arrival_delay"),
             h2("Carrier Summary"), tableOutput("arrival_summary")),
    tabPanel("Departure Delays", plotOutput("depature_delay"),
             h2("Carrier Summary"), tableOutput("departure_summary")),
    tabPanel("Data",DT::DTOutput('data')),
  )
)

server <- function(input, output) {
  data_url <- "https://raw.githubusercontent.com/ianmcook/dplyr-examples/master/data/flights.parquet"
  data_path <- "flights.parquet"

  download.file(data_url, data_path)
  flights_data <- duckplyr_df_from_parquet(data_path)

  flights_filtered <- reactive({
    input_month <- lubridate::month(input$date)
    input_day <- lubridate::day(input$date)
    input_airport <- input$airport
    flights_data |>
      filter(origin == input_airport & month == input_month & day == input_day)
  })
  
  output$data <- DT::renderDT({
    flights_filtered()
  })
  
  output$arrival_summary <- renderTable({
    flights_filtered() |>
      summarise(.by = c(carrier),
        `Count` = n(),
        `Average delay` = mean(arr_delay),
        `Maximum delay` = max(arr_delay),
        `Minimum delay` = min(arr_delay),
        `Missing data` = is.na(arr_delay) |> as.integer() |> sum() |> as.integer(),
      ) |>
      arrange(carrier) |>
      collect()
  }, striped = TRUE, spacing = "xs")

  output$departure_summary <- renderTable({
    flights_filtered() |>
      summarise(.by = c(carrier),
        `Count` = n(),
        `Average delay` = mean(dep_delay),
        `Maximum delay` = max(dep_delay),
        `Minimum delay` = min(dep_delay),
        `Missing data` = is.na(dep_delay) |> as.integer() |> sum() |> as.integer(),
      ) |>
      arrange(carrier) |>
      collect()
  }, striped = TRUE, spacing = "xs")
  
  output$arrival_delay <- renderPlot({
    input_date <- format(input$date, "%B %d, %Y")
    data <- flights_filtered() |> filter(!is.na(arr_delay) & arr_delay < 200)
    ggplot(data = data, aes(x = arr_delay, fill = origin)) +
      geom_density(color = "black", alpha = 0.6) +
      theme_light() +
      labs(
        x = "Minutes delayed",
        y = NULL, 
        title = glue::glue("Arrival delay for {input$airport} flights on {input_date}"),
        subtitle = "Negative times represent early arrivals."
      )
  })

  output$depature_delay <- renderPlot({
    input_date <- format(input$date, "%B %d, %Y")
    data <- flights_filtered() |> filter(!is.na(dep_delay) & dep_delay < 200)
    ggplot(data = data, aes(x = dep_delay, fill = origin)) +
      geom_density(color = "black", alpha = 0.6) +
      theme_light() +
      labs(
        x = "Minutes delayed",
        y = NULL, 
        title = glue::glue("Departure delay for {input$airport} flights on {input_date}"),
        subtitle = "Negative times represent early departures."
      )
  })

}

shinyApp(ui = ui, server = server)
