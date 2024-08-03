# Load the required libraries
library(leaflet)
library(shiny)
library(tidyverse)

# setwd("/Users/drewhenderson/University of Essex/RO Work/CRAP/Interactive Map/IM2")
raw_WQ <- read.csv("DataCollectionForm - WQ data(1).csv")

markers_WQ <- raw_WQ %>%
  group_by(siteID) %>%
  summarise(
    N = n(),
    latitude..xx.xxxxxx.,
    longitude..x.xxxxxx.,
    NFP_EC = round(quantile(as.numeric(E.coli), 0.95, na.rm = T), 0),
    NC_EC = sum(str_count(E.coli, "NC")),
    NFP_ENT = round(quantile(as.numeric(entericEnterococci), 0.95, na.rm = T), 0),
    NC_ENT = sum(str_count(entericEnterococci, "NC"))
  ) %>%
  distinct(siteID, .keep_all = TRUE)

ENTbreaks <- c(-1, 100, 185, 200, 100000)
ECbreaks <- c(-1, 250, 400, 500, 100000)
ccolors <- c("#7CADCD", "#366F95", "#CAB717", "#9A6F3C")

# Define the UI for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  style = "text-align: left;",
  titlePanel("Colne River Action Plan (CRAP)"),
  fluidRow(
    column(
      width = 12,
      class = "main-content",
      selectInput(
        "parameter",
        "Select Bacteria",
        choices = c(
          "Enterococcus" = "NFP_ENT",
          "E. coli" = "NFP_EC"
        )
      ),
      leafletOutput("map")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(
        lng = 0.99,
        lat = 51.86,
        zoom = 11.4
      ) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        data = markers_WQ,
        lat = ~latitude..xx.xxxxxx.,
        lng = ~longitude..x.xxxxxx.,
        radius = 8,
        color = ~ {
          if (input$parameter == "NFP_ENT") {
            cut(markers_WQ$NFP_ENT,
              breaks = ENTbreaks,
              labels = ccolors
            )
          } else {
            cut(markers_WQ$NFP_EC,
              breaks = ECbreaks,
              labels = ccolors
            )
          }
        },
        fill = TRUE,
        fillOpacity = 1,
        popup = ~ paste(
          "Site:",
          siteID,
          "<br>",
          "<br>",
          if (input$parameter == "NFP_ENT") {
            popup_text <- ifelse(
              markers_WQ$NC_ENT > 2,
              "Enterococcus (CFU/100 mL): * !",
              paste(
                "Enterococcus (CFU/100 mL):",
                markers_WQ$NFP_ENT
              )
            )
          } else {
            popup_text <- ifelse(
              markers_WQ$NC_EC > 2,
              "<i>E. coli</i> (CFU/100 mL): * !",
              paste(
                "<i>E. coli</i> (CFU/100 mL):",
                markers_WQ$NFP_EC
              )
            )
          },
          "<br>",
          "<br>No. of samples:",
          N
        )
      ) %>%
      addLegend(
        "bottomright",
        colors = ccolors,
        labels = c("Excellent", "Good", "Sufficient", "Poor"),
        title = "Classification"
      )
  })
}

shinyApp(ui = ui, server = server)
