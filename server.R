# ==========================================================================
# server.R  --  GeoLibre-inspired shell
# Owns the global concerns: dataset pools, upload, the active-dataset
# selection (left rail), view switching (menubar -> both navsets), the status
# bar, and the View Data modal. Components plug in below.
# ==========================================================================

server <- function(input, output, session) {

  # Shared data state (re-used by every component).
  raw_pool     <- reactiveValues()   # untouched uploads
  dataset_pool <- reactiveValues()   # working/edited datasets
  raster_pool  <- reactiveValues()   # shared SpatRaster objects (RS search → Raster Analysis)

  dataset_names <- reactive({ names(reactiveValuesToList(dataset_pool)) })

  # Currently-active dataset (set by clicking the left rail, or newest upload).
  active_ds <- reactiveVal(NULL)
  observeEvent(input$active_dataset, { active_ds(input$active_dataset) })

  active_dataset <- reactive({
    ds <- active_ds()
    if (!isTruthy(ds) || !(ds %in% names(dataset_pool))) return(NULL)
    ds
  })

  # ---- Upload (global, left rail) ----
  observeEvent(input$upload_files, {
    req(input$upload_files)
    for (i in 1:nrow(input$upload_files)) {
      fname <- input$upload_files$name[i]
      fpath <- input$upload_files$datapath[i]
      ext <- tolower(tools::file_ext(fname))
      tryCatch({
        if (ext == "csv") df <- read.csv(fpath)
        else if (ext %in% c("xlsx", "xls")) df <- as.data.frame(readxl::read_excel(fpath))
        else if (ext == "txt") df <- read.delim(fpath)
        else stop("Unsupported file type.")
        clean_df <- init_data(df)
        raw_pool[[fname]] <- clean_df
        dataset_pool[[fname]] <- clean_df
        active_ds(fname)
      }, error = function(e) {
        showNotification(paste("Error loading", fname, ":", e$message), type = "error")
      })
    }
    showNotification("Dataset(s) uploaded.", type = "message")
  })

  # ---- Left rail: clickable datasets list ----
  output$datasets_list <- renderUI({
    nm <- dataset_names()
    if (length(nm) == 0) {
      return(div(class = "text-muted small fst-italic", "No datasets yet. Use Add Data."))
    }
    cur <- active_dataset()
    lapply(nm, function(n) {
      cls <- if (isTruthy(cur) && n == cur) "ds-item active" else "ds-item"
      tags$div(class = cls,
        onclick = sprintf("Shiny.setInputValue('active_dataset','%s',{priority:'event'})", n),
        tags$span(class = "dot"), tags$span(n))
    })
  })

  # ---- Menubar -> switch canvas + tools in lockstep ----
  observeEvent(input$current_view, {
    nav_select("canvas_view", input$current_view)
    nav_select("tools_view", input$current_view)
  })

  # ---- Status bar ----
  output$status_active <- renderText({ ds <- active_dataset(); if (is.null(ds)) "—" else ds })
  output$status_dims <- renderText({
    ds <- active_dataset()
    if (is.null(ds)) return("no dataset loaded")
    df <- dataset_pool[[ds]]
    paste0(nrow(df), " rows × ", ncol(df), " cols")
  })

  # ---- View Data modal (left rail button) ----
  observeEvent(input$view_data, {
    req(active_dataset())
    showModal(modalDialog(
      title = paste("Dataset Viewer:", active_dataset()),
      DT::dataTableOutput("global_data_table"),
      size = "xl", easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  output$global_data_table <- DT::renderDataTable({
    req(active_dataset())
    DT::datatable(dataset_pool[[active_dataset()]], options = list(pageLength = 15, scrollX = TRUE))
  })

  # --- Components (canvas + tools wired by the shell; servers bound once) ---
  # Each model server returns a reactive of its live "context" text for the AI Co-Pilot.
  data_ctx  <- dataServer("data", raw_pool, dataset_pool, dataset_names, active_dataset)
  lm_ctx    <- lmServer("lm", dataset_pool, active_dataset)
  lme_ctx   <- lmeServer("lme", dataset_pool, active_dataset)
  anova_ctx <- anovaServer("anova", dataset_pool, active_dataset)
  log_ctx   <- logisticServer("logistic", dataset_pool, active_dataset)
  rf_ctx    <- rfServer("rf", dataset_pool, active_dataset)
  clust_ctx <- clusteringServer("clustering", dataset_pool, active_dataset)
  clf_ctx   <- classificationServer("classification", dataset_pool, active_dataset)
  da_ctx    <- daServer("da", dataset_pool, active_dataset)
  lidar_ctx  <- lidarServer("lidar", dataset_pool)
  raster_ctx <- rasterServer("raster", dataset_pool, active_dataset, raster_pool)
  rs_ctx     <- rsSearchServer("rs_search", dataset_pool, active_dataset, raster_pool)

  module_ctx <- list(
    data = data_ctx,
    lm = lm_ctx, lme = lme_ctx, anova = anova_ctx, logistic = log_ctx, rf = rf_ctx,
    clustering = clust_ctx, classification = clf_ctx, da = da_ctx,
    pointcloud = lidar_ctx, chm_itd = lidar_ctx, metrics = lidar_ctx,
    raster = raster_ctx, rs_search = rs_ctx
  )

  chatServer("chat", dataset_pool, active_dataset, reactive(input$current_view), module_ctx)
}
