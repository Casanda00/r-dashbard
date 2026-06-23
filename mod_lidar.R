# ==========================================================================
# MODULE: Spatial & LiDAR  (canvas + tools contract)
# Three menu views (pointcloud / chm_itd / metrics) share ONE rv_lidar state,
# so this is a single module: six UI fns (3 tools + 3 canvas) + one server.
#   lidarPointcloudToolsUI / lidarPointcloudCanvasUI
#   lidarChmToolsUI        / lidarChmCanvasUI
#   lidarMetricsToolsUI    / lidarMetricsCanvasUI
#   lidarServer(id, dataset_pool)
# Wire all six with the SAME id ("lidar"); the server binds once.
# Extracted plot metrics are written to dataset_pool so the left rail picks them up.
# ==========================================================================

# ---- Point Cloud & 3D ----
lidarPointcloudToolsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h6(class = "text-uppercase text-muted small", "LiDAR Pre-Processing"),
    fileInput(ns("lidar_file"), "Upload .laz File", accept = c(".las", ".laz")),
    fileInput(ns("shp_file"), "Upload Plot Shapefile (.shp, .shx, .dbf, .prj)", multiple = TRUE),
    hr(),
    markdown("**Sub-setting & Memory Limits**"),
    numericInput(ns("clip_xmin"), "X Min:", value = NA),
    numericInput(ns("clip_xmax"), "X Max:", value = NA),
    numericInput(ns("clip_ymin"), "Y Min:", value = NA),
    numericInput(ns("clip_ymax"), "Y Max:", value = NA),
    actionButton(ns("clip_las"), "Clip LAS File", class = "btn-warning", width = "100%"),
    hr(),
    markdown("**Height Normalization (DTM)**"),
    sliderInput(ns("dtm_res"), "DTM Resolution:", min = 0.5, max = 5, value = 1, step = 0.5),
    actionButton(ns("run_norm"), "Normalize Height (Z)", class = "btn-primary", width = "100%"),
    hr(),
    markdown("**Outlier & Noise Filter**"),
    sliderInput(ns("int_max"), "Max Intensity Cutoff:", min = 100, max = 1000, value = 300, step = 50),
    actionButton(ns("run_filter"), "Filter Noise", class = "btn-primary", width = "100%")
  )
}

lidarPointcloudCanvasUI <- function(id) {
  ns <- NS(id)
  div(
    card(
      card_header(class = "bg-light", "Interactive 3D Point Cloud Viewer"),
      rglwidgetOutput(ns("lidar_3d_viewer"), height = "500px")
    ),
    # Headless static render: works on shinyapps.io (no WebGL screenshot needed),
    # is downloadable, and is the image the AI Co-Pilot can actually see.
    card(
      card_header(class = "d-flex justify-content-between align-items-center bg-light", "Static 3D Snapshot (download / AI view)",
                  downloadButton(ns("download_3d"), "Download Plot", class = "btn-sm btn-outline-success")),
      div(class = "d-flex align-items-center gap-2 px-2",
          sliderInput(ns("snap_pts"), "Max display points (both 3D viewers):", min = 10000, max = 200000, value = 60000, step = 10000, width = "320px")),
      plotOutput(ns("static_3d"), height = "430px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(class = "bg-light", "LAS Summary"), verbatimTextOutput(ns("las_summary"))),
      card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Elevation & Intensity Distributions",
                       downloadButton(ns("download_hists"), "Download Plot", class = "btn-sm btn-outline-success")),
           plotOutput(ns("las_hists")))
    )
  )
}

# ---- CHM & ITD ----
lidarChmToolsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h6(class = "text-uppercase text-muted small", "Canopy Height Model"),
    sliderInput(ns("chm_res"), "CHM Resolution:", min = 0.1, max = 2, value = 0.5, step = 0.1),
    textInput(ns("pitfree_thresh"), "Pitfree Thresholds (comma-sep):", value = "0, 5, 10, 15, 20, 25"),
    actionButton(ns("run_chm"), "Generate CHM", class = "btn-primary", width = "100%"),
    hr(),
    markdown("**Individual Tree Detection (ITD)**"),
    markdown("*LMF Window Size: `a + b * height^2`*"),
    numericInput(ns("lmf_a"), "Parameter a:", value = 1.2),
    numericInput(ns("lmf_b"), "Parameter b:", value = 0.003),
    actionButton(ns("run_itd"), "Detect Trees", class = "btn-primary", width = "100%")
  )
}

lidarChmCanvasUI <- function(id) {
  ns <- NS(id)
  div(
    card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "2D CHM & Detected Trees",
                     downloadButton(ns("download_chm"), "Download Plot", class = "btn-sm btn-outline-success")),
         plotOutput(ns("chm_plot"), height = "500px")),
    card(card_header(class = "bg-light", "ITD Output Table"), DT::dataTableOutput(ns("itd_table")))
  )
}

# ---- Metric Extraction & Evaluation ----
lidarMetricsToolsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h6(class = "text-uppercase text-muted small", "Area-Based & Model Evaluation"),
    actionButton(ns("extract_metrics"), "Extract Plot Metrics", class = "btn-primary", width = "100%"),
    hr(),
    markdown("**Evaluate Volume Models**"),
    selectInput(ns("eval_target"), "Observed Variable (e.g., v):", choices = NULL),
    selectInput(ns("eval_pred"), "Predicted Variable (e.g., v_itd):", choices = NULL),
    actionButton(ns("run_eval"), "Calculate Error Metrics", class = "btn-success", width = "100%")
  )
}

lidarMetricsCanvasUI <- function(id) {
  ns <- NS(id)
  div(
    card(card_header(class = "bg-light", "Extracted Plot Predictors"), DT::dataTableOutput(ns("metrics_table"))),
    card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Model Evaluation (RMSE, Bias)",
                     downloadButton(ns("download_eval"), "Download Plot", class = "btn-sm btn-outline-success")),
         verbatimTextOutput(ns("eval_metrics_out")), plotOutput(ns("eval_plot")))
  )
}

lidarServer <- function(id, dataset_pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv_lidar <- reactiveValues(raw_las = NULL, las = NULL, dtm = NULL, chm = NULL, tops = NULL, plot_shp = NULL, itd_metrics = NULL)

    observeEvent(input$lidar_file, {
      req(input$lidar_file)
      withProgress(message = 'Reading LiDAR data...', value = 0.5, {
        tryCatch({
          las <- lidR::readLAS(input$lidar_file$datapath)
          rv_lidar$raw_las <- las; rv_lidar$las <- las
          showNotification("LiDAR data loaded successfully.", type = "message")
        }, error = function(e) showNotification(paste("Error reading LAS:", e$message), type = "error"))
      })
    })

    observeEvent(input$shp_file, {
      req(input$shp_file)
      withProgress(message = 'Loading Shapefile...', value = 0.5, {
        tryCatch({
          temp_dir <- tempdir()
          for (i in 1:nrow(input$shp_file)) file.copy(input$shp_file$datapath[i], file.path(temp_dir, input$shp_file$name[i]))
          shp_path <- file.path(temp_dir, input$shp_file$name[grep("\\.shp$", input$shp_file$name, ignore.case = TRUE)])
          if (length(shp_path) > 0) { rv_lidar$plot_shp <- sf::st_read(shp_path[1]); showNotification("Shapefile loaded successfully.", type = "message") }
          else showNotification("No .shp file found in upload.", type = "error")
        }, error = function(e) showNotification(paste("Error reading Shapefile:", e$message), type = "error"))
      })
    })

    observeEvent(input$clip_las, {
      req(rv_lidar$raw_las)
      xmin <- input$clip_xmin; xmax <- input$clip_xmax; ymin <- input$clip_ymin; ymax <- input$clip_ymax
      if (is.na(xmin) || is.na(xmax) || is.na(ymin) || is.na(ymax)) { showNotification("Please provide all 4 coordinates.", type = "warning"); return() }
      withProgress(message = 'Clipping LAS...', value = 0.5, {
        rv_lidar$las <- lidR::clip_rectangle(rv_lidar$raw_las, xmin, ymin, xmax, ymax)
        showNotification("LAS file clipped.", type = "message")
      })
    })

    observeEvent(input$run_norm, {
      req(rv_lidar$las)
      withProgress(message = 'Normalizing Height (DTM)...', value = 0, {
        incProgress(0.2, detail = "Rasterizing Terrain...")
        rv_lidar$dtm <- lidR::rasterize_terrain(rv_lidar$las, res = input$dtm_res, algorithm = lidR::tin())
        incProgress(0.5, detail = "Subtracting DTM from LAS...")
        rv_lidar$las <- rv_lidar$las - rv_lidar$dtm
        rv_lidar$las$Z[rv_lidar$las$Z < 0] <- 0
        incProgress(0.9)
        showNotification("Height normalization complete.", type = "message")
      })
    })

    observeEvent(input$run_filter, {
      req(rv_lidar$las)
      withProgress(message = 'Filtering Noise...', value = 0.5, {
        tmp <- lidR::classify_noise(rv_lidar$las, lidR::ivf(res = 5, n = 2))
        tmp <- lidR::filter_poi(tmp, Classification != lidR::LASNOISE)
        tmp <- lidR::filter_poi(tmp, Intensity < input$int_max)
        rv_lidar$las <- tmp
        showNotification("Noise filtered.", type = "message")
      })
    })

    observeEvent(input$run_chm, {
      req(rv_lidar$las)
      withProgress(message = 'Generating CHM...', value = 0.5, {
        thresh <- as.numeric(trimws(unlist(strsplit(input$pitfree_thresh, ","))))
        rv_lidar$chm <- lidR::rasterize_canopy(rv_lidar$las, res = input$chm_res, algorithm = lidR::pitfree(thresholds = thresh))
        showNotification("CHM Generated.", type = "message")
      })
    })

    observeEvent(input$run_itd, {
      req(rv_lidar$chm)
      withProgress(message = 'Detecting Trees...', value = 0.5, {
        f_win <- function(height) { input$lmf_a + input$lmf_b * height^2 }
        rv_lidar$tops <- lidR::locate_trees(rv_lidar$chm, lidR::lmf(f_win))
        rv_lidar$tops$h <- 1.2 + rv_lidar$tops$Z * 1.01
        showNotification("Individual Tree Detection complete.", type = "message")
      })
    })

    output$lidar_3d_viewer <- renderRglwidget({
      req(rv_lidar$las)
      las_full <- rv_lidar$las
      n   <- nrow(las_full@data)
      cap <- min(n, as.integer(input$snap_pts %||% 60000L))
      # Decimate before rendering to prevent WebSocket OOM / server disconnect.
      las_disp <- if (n > cap) {
        idx <- sort(sample.int(n, cap))
        las_full@data <- las_full@data[idx]
        las_full
      } else {
        las_full
      }
      rgl::clear3d()
      lidR::plot(las_disp, color = "Z", bg = "white", size = 2, clear_artifacts = FALSE)
      rgl::rglwidget()
    })

    # Static, decimated 3D scatter (headless-safe) for download + AI vision.
    static3d_fn <- function() {
      if (is.null(rv_lidar$las)) { show_placeholder("Load a .laz file to see the 3D snapshot."); return() }
      d <- rv_lidar$las@data
      n <- nrow(d)
      cap <- if (isTruthy(input$snap_pts)) input$snap_pts else 60000
      idx <- if (n > cap) sample(n, cap) else seq_len(n)
      z <- d$Z[idx]
      zr <- range(z, na.rm = TRUE)
      bins <- if (diff(zr) > 0) cut(z, breaks = 50, labels = FALSE) else rep(1L, length(z))
      cols <- grDevices::terrain.colors(50)[bins]
      scatterplot3d::scatterplot3d(d$X[idx], d$Y[idx], z, color = cols, pch = 20, cex.symbols = 0.3,
        xlab = "X", ylab = "Y", zlab = "Z (height)", main = paste0("Point cloud (", length(idx), " pts, decimated)"))
    }
    output$static_3d <- renderPlot({ static3d_fn() })
    output$download_3d <- downloadHandler(
      filename = function() { paste0("pointcloud_3d_", Sys.Date(), ".png") },
      content = function(file) { png(file, width = 900, height = 800); static3d_fn(); dev.off() }
    )

    output$las_summary <- renderPrint({ req(rv_lidar$las); print(summary(rv_lidar$las)) })

    hists_fn <- function() {
      req(rv_lidar$las)
      par(mfrow = c(1, 2))
      hist(rv_lidar$las$Z, main = "Height (Z)", col = "lightblue", xlab = "Z")
      hist(rv_lidar$las$Intensity, main = "Intensity", col = "lightgreen", xlab = "Intensity")
    }
    output$las_hists <- renderPlot({ hists_fn() })
    output$download_hists <- downloadHandler(
      filename = function() { paste0("las_distributions_", Sys.Date(), ".png") },
      content = function(file) { png(file, width = 900, height = 450); hists_fn(); dev.off() }
    )

    chm_fn <- function() {
      req(rv_lidar$chm)
      terra::plot(rv_lidar$chm, main = "Canopy Height Model (CHM)")
      if (!is.null(rv_lidar$plot_shp)) plot(sf::st_geometry(rv_lidar$plot_shp), add = TRUE, border = "white", lwd = 2)
      if (!is.null(rv_lidar$tops)) plot(sf::st_geometry(rv_lidar$tops), add = TRUE, col = "red", pch = 16, cex = 0.5)
    }
    output$chm_plot <- renderPlot({ chm_fn() })
    output$download_chm <- downloadHandler(
      filename = function() { paste0("chm_", Sys.Date(), ".png") },
      content = function(file) { png(file, width = 800, height = 800); chm_fn(); dev.off() }
    )

    output$itd_table <- DT::renderDataTable({
      req(rv_lidar$tops)
      DT::datatable(sf::st_drop_geometry(rv_lidar$tops), options = list(pageLength = 10, scrollX = TRUE))
    })

    observeEvent(input$extract_metrics, {
      req(rv_lidar$las, rv_lidar$plot_shp)
      withProgress(message = 'Extracting Plot Metrics...', value = 0.5, {
        tryCatch({
          d <- lidR::polygon_metrics(rv_lidar$las, ~lidR::stdmetrics(X, Y, Z, Intensity, ReturnNumber, Classification, dz = 1), rv_lidar$plot_shp)
          d <- cbind(rv_lidar$plot_shp, d)
          d_df <- sf::st_set_geometry(d, NULL)
          rv_lidar$itd_metrics <- d_df
          dataset_pool[["LiDAR_Plot_Metrics"]] <- d_df  # appears in the left Datasets rail
          updateSelectInput(session, "eval_target", choices = names(d_df))
          updateSelectInput(session, "eval_pred", choices = names(d_df))
          showNotification("Metrics extracted and added to the Datasets rail!", type = "message")
        }, error = function(e) showNotification(paste("Metric extraction failed:", e$message), type = "error"))
      })
    })

    output$metrics_table <- DT::renderDataTable({
      req(rv_lidar$itd_metrics)
      DT::datatable(rv_lidar$itd_metrics, options = list(pageLength = 10, scrollX = TRUE))
    })

    eval_data <- reactiveVal(NULL)
    observeEvent(input$run_eval, {
      req(rv_lidar$itd_metrics, input$eval_target, input$eval_pred)
      eval_data(list(obs = rv_lidar$itd_metrics[[input$eval_target]],
                     pred = rv_lidar$itd_metrics[[input$eval_pred]],
                     target = input$eval_target, pred_name = input$eval_pred))
    })

    output$eval_metrics_out <- renderPrint({
      e <- eval_data()
      if (is.null(e)) return(cat("Run 'Calculate Error Metrics' to evaluate."))
      if (is.null(e$obs) || is.null(e$pred)) { cat("Variables not found."); return() }
      print(uef_evaluation(e$pred, e$obs))
    })

    eval_plot_fn <- function() {
      e <- eval_data()
      if (is.null(e) || is.null(e$obs) || is.null(e$pred)) { show_placeholder("Run 'Calculate Error Metrics'."); return() }
      plot(e$pred, e$obs, xlab = paste("Predicted (", e$pred_name, ")"), ylab = paste("Observed (", e$target, ")"), main = "Prediction Accuracy", pch = 16, col = "blue")
      abline(0, 1, col = "red", lwd = 2)
    }
    output$eval_plot <- renderPlot({ eval_plot_fn() })
    output$download_eval <- downloadHandler(
      filename = function() { paste0("model_evaluation_", Sys.Date(), ".png") },
      content = function(file) { png(file, width = 700, height = 600); eval_plot_fn(); dev.off() }
    )

    # Context (+ plot) for the AI Co-Pilot (shared across the 3 LiDAR views).
    list(
      context = reactive({
        parts <- c()
        if (!is.null(rv_lidar$las)) parts <- c(parts, "LAS point cloud loaded")
        if (!is.null(rv_lidar$dtm)) parts <- c(parts, "height-normalized (DTM)")
        if (!is.null(rv_lidar$chm)) parts <- c(parts, "CHM generated")
        if (!is.null(rv_lidar$tops)) parts <- c(parts, paste(nrow(rv_lidar$tops), "trees detected"))
        if (!is.null(rv_lidar$itd_metrics)) parts <- c(parts, paste(ncol(rv_lidar$itd_metrics), "plot metrics extracted"))
        paste0("Spatial & LiDAR workflow. ",
               if (length(parts)) paste(parts, collapse = "; ") else "No LiDAR data loaded yet.")
      }),
      plot = function() {
        if (!is.null(isolate(eval_data()))) eval_plot_fn()
        else if (!is.null(rv_lidar$chm)) chm_fn()
        else if (!is.null(rv_lidar$las)) static3d_fn()
        else show_placeholder("No LiDAR data loaded yet.")
      }
    )
  })
}
