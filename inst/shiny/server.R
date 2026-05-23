library(shiny)
library(leaflet)
library(bslib)
library(tspR)

server <- function(input, output, session) {

  # --- reactive state ---
  # All mutable data stored here — the correct Shiny pattern
  stops_rv <- reactiveVal(
    data.frame(
      name = character(),
      lat  = numeric(),
      lng  = numeric(),
      stringsAsFactors = FALSE
    )
  )

  route_rv <- reactiveVal(NULL)

  # --- base map ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 51, zoom = 4)
  })

  # --- add stop ---
  observeEvent(input$btn_add, {
    req(nchar(trimws(input$city_input)) > 0)

    result <- tryCatch({
      withProgress(message = "Looking up location...", value = 0.5, {
        tidygeocoder::geocode(
          data.frame(addr = trimws(input$city_input)),
          address = addr,
          method  = "osm"
        )
      })
    }, error = function(e) NULL)

    if (is.null(result) || is.na(result$lat) || is.na(result$long)) {
      showNotification(
        paste("Could not find:", input$city_input),
        type = "error", duration = 4
      )
      return()
    }

    # Append new stop to reactive data frame
    stops_rv(rbind(
      stops_rv(),
      data.frame(
        name = trimws(input$city_input),
        lat  = result$lat,
        lng  = result$long,
        stringsAsFactors = FALSE
      )
    ))

    route_rv(NULL)   # clear old route when stops change

    leafletProxy("map") %>%
      addMarkers(
        lng     = result$long,
        lat     = result$lat,
        label   = trimws(input$city_input),
        layerId = paste0("marker_", nrow(stops_rv()))
      )

    updateTextInput(session, "city_input", value = "")
  })

  # --- clear all ---
  observeEvent(input$btn_clear, {
    stops_rv(data.frame(
      name = character(),
      lat  = numeric(),
      lng  = numeric(),
      stringsAsFactors = FALSE
    ))
    route_rv(NULL)
    leafletProxy("map") %>% clearMarkers() %>% clearShapes()
  })

  # --- stops table ---
  # Defined once, updates automatically when stops_rv() changes
  output$tbl_stops <- renderTable({
    df <- stops_rv()
    if (nrow(df) == 0) return(data.frame(Stops = "No stops added yet."))
    data.frame("#" = seq_len(nrow(df)), Name = df$name, check.names = FALSE)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  # --- solve ---
  observeEvent(input$btn_solve, {
    df <- stops_rv()

    if (nrow(df) < 2) {
      showNotification("Add at least 2 stops first.",
                       type = "warning", duration = 3)
      return()
    }

    result <- tryCatch({
      withProgress(message = "Optimising route...", value = 0.5, {
        solver <- TSPSolver$new(df)
        solver$solve()
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      NULL
    })

    if (is.null(result)) return()

    route_rv(result)

    ordered <- result$loc_set$locations[c(result$tour, result$tour[1]), ]

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
      sprintf("Route found: %.1f km", result$distance),
      type = "message", duration = 4
    )
  })

  # --- result text ---
  output$txt_result <- renderPrint({
    r <- route_rv()
    if (is.null(r)) {
      cat("No route computed yet.\nAdd stops and click 'Find optimal route'.")
    } else {
      print(r)
    }
  })
}
