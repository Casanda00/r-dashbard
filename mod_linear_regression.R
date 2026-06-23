# ==========================================================================
# MODULE: Linear Regression  (canvas + tools contract)
# --------------------------------------------------------------------------
# lmToolsUI(id)   -> right panel: model parameters + formula editor
# lmCanvasUI(id)  -> center canvas: diagnostics + summary + ANOVA
# lmServer(id, dataset_pool, active_dataset) -> bound once in server.R
# Reads the globally-selected dataset via active_dataset(); no own picker.
# ==========================================================================

lmToolsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h6(class = "text-uppercase text-muted small", "Model Parameters"),
    selectInput(ns("y"), "Dependent Variable (Y):", choices = NULL),
    hr(),
    markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"),
    textAreaInput(ns("formula_text"), "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Soiltype2 * Texture"),
    div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
        markdown("**Quick Builder**"),
        selectInput(ns("build_var"), "Select Variable:", choices = NULL),
        selectInput(ns("build_trans"), "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")),
        fluidRow(
          column(6, actionButton(ns("btn_add_var"), "Insert", class = "btn-primary btn-sm", width = "100%", style = "margin-bottom:5px;")),
          column(3, actionButton(ns("btn_add_plus"), " + ", class = "btn-secondary btn-sm", width = "100%", style = "margin-bottom:5px;")),
          column(3, actionButton(ns("btn_add_star"), " * ", class = "btn-secondary btn-sm", width = "100%", style = "margin-bottom:5px;"))
        ),
        actionButton(ns("btn_clear"), "Clear Formula", class = "btn-outline-danger btn-sm", width = "100%")
    )
  )
}

lmCanvasUI <- function(id) {
  ns <- NS(id)
  div(
    card(
      card_header(class = "d-flex justify-content-between align-items-center bg-light", "Model Diagnostics",
                  div(class = "d-flex align-items-center gap-2 header-controls",
                      radioGroupButtons(ns("view_mode"), label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"),
                      uiOutput(ns("single_selector")),
                      downloadButton(ns("download_plot"), "Download Plot", class = "btn-sm btn-outline-success"))
      ),
      div(style = "overflow-y: auto; height: 520px; padding: 5px;", uiOutput(ns("dynamic_plot_ui")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(class = "bg-light", "Model Summary"),
        div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", textOutput(ns("formula_display"))),
        div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput(ns("summary")))
      ),
      card(
        card_header(class = "bg-light", "Analysis of Variance (ANOVA)"),
        div(style = "overflow-y: auto; height: 345px; padding: 5px;", verbatimTextOutput(ns("anova")))
      )
    )
  )
}

lmServer <- function(id, dataset_pool, active_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_data <- reactive({
      ds <- active_dataset()
      if (is.null(ds)) return(NULL)
      dataset_pool[[ds]]
    })

    observe({
      df <- active_data()
      req(df)
      cols <- names(df)
      curr_y <- if (isTruthy(isolate(input$y)) && isolate(input$y) %in% cols) isolate(input$y) else cols[1]
      curr_build <- if (isTruthy(isolate(input$build_var)) && isolate(input$build_var) %in% cols) isolate(input$build_var) else cols[1]
      updateSelectInput(session, "y", choices = cols, selected = curr_y)
      updateSelectInput(session, "build_var", choices = cols, selected = curr_build)
    })

    observeEvent(input$btn_add_var, {
      var <- paste0("`", input$build_var, "`")
      term <- switch(input$build_trans,
                     "raw" = var,
                     "log" = paste0("log(", var, ")"),
                     "sqrt" = paste0("sqrt(", var, ")"),
                     "poly" = paste0("I(", var, "^2)"))
      current <- trimws(input$formula_text)
      new_text <- if (nchar(current) > 0) paste(current, term) else term
      updateTextAreaInput(session, "formula_text", value = new_text)
    })

    observeEvent(input$btn_add_plus, {
      current <- trimws(input$formula_text)
      if (nchar(current) > 0) updateTextAreaInput(session, "formula_text", value = paste(current, "+ "))
    })

    observeEvent(input$btn_add_star, {
      current <- trimws(input$formula_text)
      if (nchar(current) > 0) updateTextAreaInput(session, "formula_text", value = paste(current, "* "))
    })

    observeEvent(input$btn_clear, { updateTextAreaInput(session, "formula_text", value = "") })

    formula_str <- reactive({
      if (!isTruthy(input$y)) return("Y ~ ...")
      safe_y <- paste0("`", input$y, "`")
      x_side <- trimws(input$formula_text)
      if (nchar(x_side) == 0) return(paste(safe_y, "~ ..."))
      paste(safe_y, "~", x_side)
    })

    output$formula_display <- renderText({ formula_str() })

    lm_model <- reactive({
      df <- active_data()
      if (is.null(df)) return("Awaiting dataset...")
      form_str <- formula_str()
      if (grepl("\\.\\.\\.", form_str)) return("Awaiting Predictors: Please use the builder or type a formula.")
      tryCatch({ lm(as.formula(form_str), data = df) }, error = function(e) { return(paste("Syntax Error in Formula:", e$message)) })
    })

    output$summary <- renderPrint({
      model <- lm_model()
      if (is.character(model)) cat(model) else { print(model$call); cat("\n"); print(summary(model)) }
    })

    output$anova <- renderPrint({
      model <- lm_model()
      if (is.character(model)) return(cat("Awaiting valid model..."))
      tryCatch({ print(anova(model)) }, error = function(e) cat("ANOVA computation error:", e$message))
    })

    output$single_selector <- renderUI({
      req(input$view_mode == "Single Plot")
      selectInput(ns("zoom_target"), label = NULL, choices = c("Fitted vs Actual", "Residual Plot", "Target Distribution"), width = "200px")
    })

    output$dynamic_plot_ui <- renderUI({
      req(input$view_mode)
      plotOutput(ns("diag_plot"), height = "500px")
    })

    output$diag_plot <- renderPlot({
      req(input$view_mode)
      if (input$view_mode == "Single Plot") req(input$zoom_target)
      df <- active_data()
      if (is.null(df)) { show_placeholder("Awaiting dataset..."); return() }
      plot_lm_diagnostics(lm_model(), df, input$y, input$view_mode, input$zoom_target)
    })

    output$download_plot <- downloadHandler(
      filename = function() { paste0("linear_model_diagnostic_", Sys.Date(), ".png") },
      content = function(file) {
        png(file, width = 800, height = 600)
        plot_lm_diagnostics(lm_model(), active_data(), input$y, input$view_mode, input$zoom_target)
        dev.off()
      }
    )

    # Context (+ plot) for the AI Co-Pilot.
    list(
      context = reactive({
        m <- lm_model()
        if (is.character(m)) return(paste("Linear Regression — no valid model yet:", m))
        paste0("Linear Regression. Formula: ", formula_str(), "\n\nModel summary:\n",
               paste(utils::capture.output(summary(m)), collapse = "\n"))
      }),
      plot = function() plot_lm_diagnostics(lm_model(), active_data(), input$y, input$view_mode, input$zoom_target)
    )
  })
}
