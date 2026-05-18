# server.R
# Handles all reactivity, geocoding, solving, and map updates.
#
# Key design decisions:
# - stops_rv and route_rv are reactiveVal objects — the correct Shiny way
#   to store mutable state that multiple outputs depend on.
# - output$tbl_stops is defined ONCE and updates automatically when
#   stops_rv() changes — never reassigned inside an observer.
# - TSPSolver is created fresh each time solve is clicked so the R6
#   object is never shared across reactive contexts.

library(shiny)
library(leaflet)
library(bslib)
library(tspR)

server <- function(input, output, session) {

  # ── Reactive state ─────────────────────────────────────────────────────────
  # All mutable data lives here. Nothing else stores state.

  stops_rv <- reactiveVal(
    data.frame(
      name = character(),
      lat  = numeric(),
      lng  = numeric(),
      stringsAsFactors = FALSE
    )
  )

  route_rv <- reactiveVal(NULL)

  # ── Base map ───────────────────────────────────────────────────────────────

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 51, zoom = 4)
  })

  # ── Add stop ───────────────────────────────────────────────────────────────

  observeEvent(input$btn_add, {

    # req() silently stops execution if city_input is blank
    req(nchar(trimws(input$city_input)) > 0)

    # Geocode the city name to lat/lng using OpenStreetMap
    result <- tryCatch({
      withProgress(message = "Looking up location...", value = 0.5, {
        tidygeocoder::geocode(
          data.frame(addr = trimws(input$city_input)),
          address = addr,
          method  = "osm"
        )
      })
    }, error = function(e) NULL)

    # Handle geocoding failure gracefully
    if (is.null(result) || is.na(result$lat) || is.na(result$long)) {
      showNotification(
        paste("Could not find:", input$city_input,
              "— try a more specific name."),
        type     = "error",
        duration = 4
      )
      return()
    }

    # Add new stop to reactive state
    new_stop <- data.frame(
      name = trimws(input$city_input),
      lat  = result$lat,
      lng  = result$long,
      stringsAsFactors = FALSE
    )
    stops_rv(rbind(stops_rv(), new_stop))

    # Invalidate any existing route — it no longer covers all stops
    route_rv(NULL)

    # Add marker to map without redrawing the whole map
    leafletProxy("map") %>%
      addMarkers(
        lng     = result$long,
        lat     = result$lat,
        label   = trimws(input$city_input),
        layerId = paste0("marker_", nrow(stops_rv()))
      )

    # Clear the text input ready for the next city
    updateTextInput(session, "city_input", value = "")
  })

  # ── Clear all ──────────────────────────────────────────────────────────────

  observeEvent(input$btn_clear, {
    stops_rv(data.frame(
      name = character(),
      lat  = numeric(),
      lng  = numeric(),
      stringsAsFactors = FALSE
    ))
    route_rv(NULL)

    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
  })

  # ── Stops table ────────────────────────────────────────────────────────────
  # Defined once — updates automatically whenever stops_rv() changes.

  output$tbl_stops <- renderTable({
    df <- stops_rv()
    if (nrow(df) == 0) {
      return(data.frame(Stops = "No stops added yet."))
    }
    data.frame(
      "#"    = seq_len(nrow(df)),
      Name   = df$name,
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  # ── Solve ──────────────────────────────────────────────────────────────────

  observeEvent(input$btn_solve, {
    df <- stops_rv()

    if (nrow(df) < 2) {
      showNotification(
        "Add at least 2 stops before solving.",
        type = "warning", duration = 3
      )
      return()
    }

    # Build solver, set algorithm, solve
    result <- tryCatch({
      withProgress(message = "Optimising route...", value = 0.5, {

        solver <- TSPSolver$new(df)

        # Forward SA parameters if SA is selected
        if (input$algo == "sa") {
          solver$set_algorithm("sa",
                               temp       = input$sa_temp,
                               cooling    = input$sa_cooling
          )
        } else {
          solver$set_algorithm(input$algo)
        }

        solver$solve()
      })
    }, error = function(e) {
      showNotification(
        paste("Solver error:", e$message),
        type = "error", duration = 5
      )
      NULL
    })

    if (is.null(result)) return()

    route_rv(result)

    # Draw optimised route on map as a polyline
    r       <- route_rv()
    ordered <- r$loc_set$locations[c(r$tour, r$tour[1]), ]

    leafletProxy("map") %>%
      clearShapes() %>%
      addPolylines(
        lng     = ordered$lng,
        lat     = ordered$lat,
        color   = "#e74c3c",
        weight  = 3,
        opacity = 0.85
      )

    showNotification(
      sprintf("Route found: %.1f km (%s)", r$distance, r$algorithm),
      type = "message", duration = 4
    )
  })

  # ── Result text ────────────────────────────────────────────────────────────

  output$txt_result <- renderPrint({
    r <- route_rv()
    if (is.null(r)) {
      cat("No route computed yet.\nAdd stops and click 'Find optimal route'.")
    } else {
      print(r)
    }
  })

  # ── Benchmark ──────────────────────────────────────────────────────────────

  # Store benchmark results reactively so plot and table share one computation
  benchmark_rv <- reactiveVal(NULL)

  observeEvent(input$btn_benchmark, {
    df <- stops_rv()

    if (nrow(df) < 2) {
      showNotification(
        "Add at least 2 stops in the Planner tab first.",
        type = "warning", duration = 3
      )
      return()
    }

    results <- tryCatch({
      withProgress(message = "Running all algorithms...", value = 0.5, {
        solver <- TSPSolver$new(df)
        solver$benchmark()
      })
    }, error = function(e) {
      showNotification(paste("Benchmark error:", e$message),
                       type = "error", duration = 5)
      NULL
    })

    benchmark_rv(results)
  })

  output$plt_benchmark <- renderPlot({
    results <- benchmark_rv()
    req(!is.null(results))

    cols <- c(
      "nn"      = "#3498db",
      "nn+2opt" = "#2ecc71",
      "sa"      = "#e67e22"
    )

    bp <- barplot(
      results$distance,
      names.arg = results$algorithm,
      col       = cols[results$algorithm],
      border    = NA,
      ylab      = "Total distance (km)",
      main      = "Algorithm comparison",
      ylim      = c(0, max(results$distance) * 1.15)
    )

    # Add distance labels above each bar
    text(
      x      = bp,
      y      = results$distance + max(results$distance) * 0.02,
      labels = sprintf("%.0f km\n%.4f s", results$distance, results$elapsed),
      cex    = 0.9,
      pos    = 3
    )
  })

  output$tbl_benchmark <- renderTable({
    results <- benchmark_rv()
    req(!is.null(results))
    names(results) <- c("Algorithm", "Distance (km)", "Time (s)")
    results
  }, striped = TRUE, hover = TRUE)

}
