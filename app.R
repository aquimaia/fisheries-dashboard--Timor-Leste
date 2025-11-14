library(shiny)
library(plotly)
library(DT)
library(leaflet)
library(bslib)

# Fungsi bantu untuk baca CSV dengan aman
read_catch_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)

  # pastikan kolom wajib ada
  required_cols <- c(
    "date", "site", "gear", "species",
    "catch_kg", "effort_hours",
    "num_fishers", "lat", "lng"
  )
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste("Kolom hilang di CSV:", paste(missing, collapse = ", ")))
  }

  # konversi tipe
  df$date <- as.Date(df$date)
  df$catch_kg <- as.numeric(df$catch_kg)
  df$effort_hours <- as.numeric(df$effort_hours)
  df$num_fishers <- as.integer(df$num_fishers)
  df$lat <- as.numeric(df$lat)
  df$lng <- as.numeric(df$lng)

  # CPUE = catch per unit effort
  df$cpue <- df$catch_kg / df$effort_hours

  df
}

# ---------- UI ----------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Community Fisheries Dashboard"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file", "Upload catch CSV (optional)",
        accept = ".csv"
      ),
      selectInput(
        "site", "Filter by site:",
        choices = "All", selected = "All"
      ),
      width = 3
    ),

    mainPanel(
      fluidRow(
        column(4, h4("Total catch (kg)"), strong(textOutput("totalCatch"))),
        column(4, h4("Total trips"), strong(textOutput("totalTrips"))),
        column(4, h4("Average CPUE (kg/hour)"), strong(textOutput("avgCPUE")))
      ),
      br(),
      h4("Monthly CPUE trend"),
      plotlyOutput("plot_cpue"),
      br(),
      h4("Species composition"),
      plotlyOutput("plot_species"),
      br(),
      h4("Catch locations"),
      leafletOutput("map_sites", height = 300),
      br(),
      h4("Raw data"),
      DTOutput("table_data")
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {

  # Data mentah (dari file default atau upload)
  raw_data <- reactive({
    if (!is.null(input$file)) {
      read_catch_data(input$file$datapath)
    } else {
      # path relatif ke folder app
      read_catch_data("data/catch_data.csv")
    }
  })

  # update pilihan site setiap kali data berubah
  observe({
    df <- raw_data()
    sites <- sort(unique(df$site))
    updateSelectInput(
      session, "site",
      choices = c("All", sites),
      selected = "All"
    )
  })

  # data setelah filter site
  filtered_data <- reactive({
    df <- raw_data()
    if (!is.null(input$site) && input$site != "All") {
      df <- df[df$site == input$site, , drop = FALSE]
    }
    df
  })

  # ---------- KPI ----------
  output$totalCatch <- renderText({
    df <- filtered_data()
    round(sum(df$catch_kg, na.rm = TRUE), 1)
  })

  output$totalTrips <- renderText({
    nrow(filtered_data())
  })

  output$avgCPUE <- renderText({
    df <- filtered_data()
    round(mean(df$cpue, na.rm = TRUE), 2)
  })

  # ---------- GRAFIK CPUE per bulan ----------
  output$plot_cpue <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)

    # buat kolom bulan (YYYY-MM)
    df$month <- format(df$date, "%Y-%m")

    # aggregate rata2 CPUE per bulan
    agg <- aggregate(cpue ~ month, data = df, FUN = function(x) mean(x, na.rm = TRUE))
    agg$month_date <- as.Date(paste0(agg$month, "-01"))

    plot_ly(
      agg,
      x = ~month_date,
      y = ~cpue,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Average CPUE (kg/hour)")
      )
  })

  # ---------- PIE chart species ----------
  output$plot_species <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)

    agg <- aggregate(catch_kg ~ species, data = df, FUN = function(x) sum(x, na.rm = TRUE))

    plot_ly(
      agg,
      labels = ~species,
      values = ~catch_kg,
      type = "pie"
    ) %>%
      layout(legend = list(orientation = "h"))
  })

  # ---------- MAP ----------
  output$map_sites <- renderLeaflet({
    df <- filtered_data()
    df <- df[!is.na(df$lat) & !is.na(df$lng), , drop = FALSE]

    if (nrow(df) == 0) {
      leaflet() %>% addTiles()
    } else {
      leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 6,
          popup = ~paste0(
            "<b>", site, "</b><br>",
            species, ": ", catch_kg, " kg"
          )
        )
    }
  })

  # ---------- TABEL ----------
  output$table_data <- renderDT({
    df <- filtered_data()
    datatable(df)
  })
}

shinyApp(ui, server)
