
library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

df <- read_csv("data/cleaned_price_of_healthy_diet.csv")
regions <- c("All", sort(unique(na.omit(df$region))))

region_colors <- c(
  "Africa" = "#B71226",
  "Asia" = "#FEE090",
  "North America" = "#2ECC71",
  "South America" = "#6BAED6",
  "Europe" = "#34419A",
  "Oceania" = "#FB9D59"
)

# UI
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    selectInput("region", "Region", choices = regions),
  ),
  h1("Global Cost of a Healthy Diet"),
  h5("Measured in Purchase Power Parity (PPP), USD"),
  
  # Build the value boxes
  p("Averages across all available data (2017–2024)", 
    style = "color: gray; font-style: italic; font-size: 13px;"),layout_columns(
    value_box("Countries", textOutput("n_countries"), theme = "bg-gradient-indigo-purple"),
    value_box("Avg Cost (USD/day)", textOutput("avg_cost"), theme = "bg-gradient-orange-red"),
    value_box("Min Cost", textOutput("min_cost"), theme = "bg-gradient-teal-cyan"),
    value_box("Max Cost", textOutput("max_cost"), theme = "bg-gradient-pink-orange"),
    col_widths = c(3, 3, 3, 3)
  ),
  
  # Build the bar and box
  layout_columns(
    card(plotlyOutput("plot_top5")),
    card(plotlyOutput("plot_box"))
  )
)

card(
  card_header("Countries by Region"),
  tableOutput("countries_table")
)

# Server
server <- function(input, output, session) {
  
  # Reactive calculation 1 - filtered data
  filtered <- reactive({
    if (input$region == "All") df
    else df |> filter(region == input$region)
  })
  
  # Reactive calculation 2 - aggregated data by region
  aggregated <- reactive({
    filtered() |>
      group_by(region) |>
      summarise(avg_cost = mean(cost_healthy_diet_ppp_usd, na.rm = TRUE))
  })
  
  # Create value boxes
  output$n_countries <- renderText(as.character(n_distinct(filtered()$country)))
  output$avg_cost <- renderText(sprintf("$%.2f", mean(filtered()$cost_healthy_diet_ppp_usd, na.rm = TRUE)))
  output$min_cost <- renderText(sprintf("$%.2f", min(filtered()$cost_healthy_diet_ppp_usd, na.rm = TRUE)))
  output$max_cost <- renderText(sprintf("$%.2f", max(filtered()$cost_healthy_diet_ppp_usd, na.rm = TRUE)))
  
  # Create bar chart
  output$plot_top5 <- renderPlotly({
    data <- filtered()
    min_year <- min(data$year)
    max_year <- max(data$year)
    
    stats <- data |>
      filter(year %in% c(min_year, max_year)) |>
      pivot_wider(id_cols = country, names_from = year,
                  values_from = cost_healthy_diet_ppp_usd,
                  values_fn = mean) |>
      drop_na()
    
    stats$increase <- stats[[as.character(max_year)]] - stats[[as.character(min_year)]]
    
    top5 <- stats |>
      arrange(desc(increase)) |>
      slice_head(n = 5)
    
    plot_ly(top5, x = ~reorder(country, increase), y = ~increase,
            type = "bar", color = ~country) |>
      layout(title = list(text = "Top 5 Countries by Cost Increase",
             font = list(size = 17)),
             xaxis = list(title = "Country",
             categoryorder = "total descending"),
             yaxis = list(title = "Cost Increase (USD)",
             titlefont = list(size=14)),
             showlegend = FALSE)
  })
  
  # Create box plot
  output$plot_box <- renderPlotly({
    plot_ly(filtered(), x = ~as.factor(year), y = ~cost_healthy_diet_ppp_usd,
            color = ~region, colors = region_colors,
            type = "box", boxpoints = "all",
            jitter = 0.3,
            pointpos = 0,
            hovertext = ~country) |>
      layout(title = "Cost Dist. by Region Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Avg Cost (USD)"))
  })

 # Display the countries table
output$countries_table <- renderTable({
  filtered() |>
    group_by(region) |>
    summarise(countries = paste(sort(unique(country)), collapse = ", "))
})

}
shinyApp(ui, server)