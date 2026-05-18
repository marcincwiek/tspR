library(shiny)
library(leaflet)
library(bslib)

ui <- page_navbar(
  title = "TSP Route Optimizer",
  theme = bs_theme(bootswatch = "flatly"),

  nav_panel("Planner",
            layout_sidebar(

              sidebar = sidebar(width = 300,

                                h5("Add a stop"),
                                textInput("city_input", NULL,
                                          placeholder = "e.g. Warsaw, Berlin..."),
                                actionButton("btn_add", "Add stop",
                                             icon  = icon("plus"),
                                             class = "btn-primary w-100 mb-1"),
                                actionButton("btn_clear", "Clear all",
                                             icon  = icon("trash"),
                                             class = "btn-outline-danger w-100"),
                                hr(),

                                h5("Your stops"),
                                tableOutput("tbl_stops"),
                                hr(),

                                actionButton("btn_solve", "Find optimal route",
                                             icon  = icon("route"),
                                             class = "btn-success w-100"),
                                hr(),

                                h5("Result"),
                                verbatimTextOutput("txt_result")
              ),

              leafletOutput("map", height = "800px")
            )
  )
)
