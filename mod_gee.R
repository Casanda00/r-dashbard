# ==========================================================================
# MODULE: Google Earth Engine  (canvas + tools, button-driven, NO user code)
# geeToolsUI(id) / geeCanvasUI(id) / geeServer(id)
# --------------------------------------------------------------------------
# Turns gee_dictionary() into buttons (+ parameter inputs). Clicking a button
# applies its operation to the current pipeline (an ee object). The map is a
# leaflet canvas (rgee adds EE tiles via Map$addLayer).
#
# REQUIRES (not on shinyapps.io): install.packages(c("rgee","leaflet",
#   "leaflet.extras","reticulate")); rgee::ee_install(); rgee::ee_Initialize();
#   a Google Earth Engine account (+ a service account for deployment).
# Everything here is GUARDED with requireNamespace(), so the app still builds
# and runs when these packages are absent (the screen shows a setup notice).
#
# To ENABLE: in global.R add `source("gee_dictionary.R"); source("mod_gee.R")`,
# add the views to ui.R's two navsets + a menu item, and call geeServer("gee")
# in server.R. (Left un-wired for now because it cannot run on shinyapps.io.)
# ==========================================================================

geeToolsUI <- function(id) {
  ns <- NS(id)
  cmds <- gee_dictionary()
  groups <- unique(vapply(cmds, function(x) x$group, character(1)))

  param_widget <- function(cmd, prm) {
    pid <- ns(paste0(cmd$id, "__", prm$name))
    switch(prm$type,
      "text"   = textInput(pid, prm$label, value = prm$default),
      "number" = numericInput(pid, prm$label, value = prm$default),
      "date"   = dateInput(pid, prm$label, value = prm$default),
      "select" = selectInput(pid, prm$label, choices = prm$choices, selected = prm$default),
      textInput(pid, prm$label, value = prm$default))
  }

  sections <- lapply(groups, function(g) {
    gcmds <- Filter(function(x) x$group == g, cmds)
    accordion_panel(g,
      lapply(gcmds, function(cmd) {
        div(class = "mb-2",
          if (length(cmd$params)) lapply(cmd$params, function(prm) param_widget(cmd, prm)),
          actionButton(ns(cmd$id), cmd$label, class = "btn-primary btn-sm", width = "100%"))
      }))
  })

  tagList(
    tags$h6(class = "text-uppercase text-muted small", "Earth Engine"),
    actionButton(ns("init"), "Connect to Earth Engine", class = "btn-success btn-sm", width = "100%"),
    div(class = "small text-muted my-2", textOutput(ns("status"))),
    hr(),
    markdown("*Area of Interest = the current map view (pan/zoom to set it).*"),
    hr(),
    do.call(accordion, c(list(id = ns("gee_acc"), open = FALSE), sections))
  )
}

geeCanvasUI <- function(id) {
  ns <- NS(id)
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    return(div(class = "p-4",
      div(class = "alert alert-warning",
        tags$b("Earth Engine needs setup."), tags$br(),
        "Install ", tags$code("rgee, leaflet, leaflet.extras, reticulate"), ", then run ",
        tags$code("rgee::ee_install()"), " and ", tags$code("rgee::ee_Initialize()"), ".", tags$br(),
        "rgee needs Python + a Google Earth Engine account and does ", tags$b("not"),
        " run on shinyapps.io — enable this screen after moving hosting.")))
  }
  div(
    card(card_header(class = "bg-light", "Earth Engine Map"),
         leaflet::leafletOutput(ns("map"), height = "560px")),
    card(card_header(class = "bg-light", "Pipeline"),
         div(style = "padding: 8px;", verbatimTextOutput(ns("pipeline_info"))))
  )
}

geeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    cmds <- gee_dictionary()
    rv <- reactiveValues(pipeline = NULL, aoi = NULL, ready = FALSE, log = character(0))
    has_rgee <- requireNamespace("rgee", quietly = TRUE)

    output$status <- renderText({
      if (!has_rgee) "rgee not installed (see the map panel for setup)."
      else if (!rv$ready) "Not connected. Click 'Connect to Earth Engine'."
      else "Connected to Earth Engine."
    })

    observeEvent(input$init, {
      if (!has_rgee) { showNotification("Install rgee first (see map panel).", type = "error"); return() }
      tryCatch({
        rgee::ee_Initialize()
        rv$ready <- TRUE
        showNotification("Earth Engine connected.", type = "message")
      }, error = function(e) showNotification(paste("EE init failed:", e$message), type = "error"))
    })

    if (requireNamespace("leaflet", quietly = TRUE)) {
      output$map <- leaflet::renderLeaflet({
        leaflet::setView(leaflet::addTiles(leaflet::leaflet()), lng = 25, lat = 62, zoom = 5)
      })
    }

    # AOI = current map view bounds, as an ee$Geometry$Rectangle.
    current_aoi <- function() {
      if (!is.null(rv$aoi)) return(rv$aoi)
      b <- input$map_bounds
      if (is.null(b) || !has_rgee) return(NULL)
      rgee::ee$Geometry$Rectangle(c(b$west, b$south, b$east, b$north))
    }

    # Wire every dictionary command to its button.
    lapply(cmds, function(cmd) {
      observeEvent(input[[cmd$id]], {
        if (!isTRUE(rv$ready)) { showNotification("Connect to Earth Engine first.", type = "warning"); return() }
        p <- list()
        for (prm in cmd$params) p[[prm$name]] <- input[[paste0(cmd$id, "__", prm$name)]]
        aoi <- current_aoi()
        if (identical(cmd$needs, "aoi") && is.null(aoi)) {
          showNotification("Set an AOI first (pan/zoom the map).", type = "warning"); return()
        }
        tryCatch({
          res <- cmd$run(rv$pipeline, p, aoi)
          rv$pipeline <- res
          rv$log <- c(rv$log, paste0("> ", cmd$label))
          showNotification(paste("Ran:", cmd$label), type = "message")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      }, ignoreInit = TRUE)
    })

    output$pipeline_info <- renderPrint({
      cat("Steps applied:\n")
      if (length(rv$log)) cat(paste(rv$log, collapse = "\n")) else cat("(none yet)")
    })
  })
}
