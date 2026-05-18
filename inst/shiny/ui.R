# ui.R
# Defines the visual layout of the Shiny dashboard.
# Two tabs: Planner (map + controls) and Benchmark (algorithm comparison).

library(shiny)
library(leaflet)
library(bslib)

ui <- page_navbar(
  title = "TSP Route Optimizer",
  theme = bs_theme(bootswatch = "flatly"),

  # ── Tab 1: Planner ────────────────────────────────────────────────────────
  nav_panel("Planner",
            layout_sidebar(

              sidebar = sidebar(width = 320,

                                # --- Add a stop ---
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

                                # --- Current stops list ---
                                h5("Your stops"),
                                tableOutput("tbl_stops"),

                                hr(),

                                # --- Algorithm controls ---
                                h5("Solve"),
                                selectInput("algo", "Algorithm",
                                            choices = c(
                                              "Nearest neighbour"        = "nn",
                                              "NN + 2-opt (recommended)" = "nn+2opt",
                                              "Simulated annealing"      = "sa"
                                            ),
                                            selected = "nn+2opt"
                                ),

                                # SA parameters only shown when SA is selected
                                conditionalPanel(
                                  condition = "input.algo == 'sa'",
                                  sliderInput("sa_temp", "Initial temperature",
                                              min = 100, max = 5000,
                                              value = 1000, step = 100),
                                  sliderInput("sa_cooling", "Cooling rate",
                                              min = 0.900, max = 0.999,
                                              value = 0.995, step = 0.001)
                                ),

                                actionButton("btn_solve", "Find optimal route",
                                             icon  = icon("route"),
                                             class = "btn-success w-100"),

                                hr(),

                                # --- Result summary ---
                                h5("Result"),
                                verbatimTextOutput("txt_result")
              ),

              # --- Main panel: map ---
              leafletOutput("map", height = "800px")
            )
  ),

  # ── Tab 2: Benchmark ──────────────────────────────────────────────────────
  nav_panel("Benchmark",
            card(
              card_header("Algorithm comparison"),
              p("Add at least 2 stops in the Planner tab, then click the button below."),
              actionButton("btn_benchmark", "Run benchmark",
                           icon  = icon("chart-bar"),
                           class = "btn-primary mb-3"),
              plotOutput("plt_benchmark", height = "350px"),
              tableOutput("tbl_benchmark")
            )
  )
)
