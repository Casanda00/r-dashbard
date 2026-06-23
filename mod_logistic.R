# ==========================================================================
# MODULE: Logistic Regression (multinomial)  (canvas + tools contract)
# logisticToolsUI / logisticCanvasUI / logisticServer(id, dataset_pool, active_dataset)
# ==========================================================================

logisticToolsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h6(class = "text-uppercase text-muted small", "Classification Setup"),
    selectInput(ns("y"), "Categorical Target (Y):", choices = NULL),
    hr(),
    markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"),
    textAreaInput(ns("formula_text"), "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Organic_depth"),
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

logisticCanvasUI <- function(id) {
  ns <- NS(id)
  div(
    card(
      card_header(class = "d-flex justify-content-between align-items-center bg-light", "Model Evaluation Plot",
                  downloadButton(ns("download_plot"), "Download Plot", class = "btn-sm btn-outline-success")),
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
        card_header(class = "bg-light", "Confusion Matrix & Accuracy"),
        div(style = "overflow-y: auto; height: 345px; padding: 5px;", verbatimTextOutput(ns("matrix")), hr(), tags$b(textOutput(ns("accuracy"))))
      )
    )
  )
}

logisticServer <- function(id, dataset_pool, active_dataset) {
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
      cat_cols <- names(df)[sapply(df, is_safe_cat)]
      all_cols <- names(df)
      curr_y <- if (isTruthy(isolate(input$y)) && isolate(input$y) %in% cat_cols) isolate(input$y) else if (length(cat_cols) > 0) cat_cols[1] else NULL
      curr_build <- if (isTruthy(isolate(input$build_var)) && isolate(input$build_var) %in% all_cols) isolate(input$build_var) else all_cols[1]
      updateSelectInput(session, "y", choices = cat_cols, selected = curr_y)
      updateSelectInput(session, "build_var", choices = all_cols, selected = curr_build)
    })

    observeEvent(input$btn_add_var, {
      var <- paste0("`", input$build_var, "`")
      term <- switch(input$build_trans,
                     "raw" = var, "log" = paste0("log(", var, ")"),
                     "sqrt" = paste0("sqrt(", var, ")"), "poly" = paste0("I(", var, "^2)"))
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

    model_obj <- reactive({
      df <- active_data()
      if (is.null(df)) return("Awaiting dataset...")
      form_str <- formula_str()
      if (grepl("\\.\\.\\.", form_str)) return("Awaiting Predictors: Please use the builder or type a formula.")
      all_vars <- all.vars(as.formula(form_str))
      missing_vars <- setdiff(all_vars, names(df))
      if (length(missing_vars) > 0) return(paste("Error: Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
      clean_df <- df[, all_vars, drop = FALSE]
      clean_df <- clean_df[complete.cases(clean_df), , drop = FALSE]
      if (nrow(clean_df) < 10) return("Data Error: Insufficient complete cases after removing NAs.")
      clean_df[[input$y]] <- as.factor(clean_df[[input$y]])
      if (length(unique(clean_df[[input$y]])) < 2) return("Data Error: Target variable has less than 2 distinct levels.")
      tryCatch({
        model <- nnet::multinom(as.formula(form_str), data = clean_df, trace = FALSE)
        list(model = model, data = clean_df)
      }, error = function(e) { return(paste("Syntax Error in Formula:", e$message)) })
    })

    output$summary <- renderPrint({
      res <- model_obj()
      if (is.character(res)) cat(res) else { print(res$model$call); cat("\n"); print(summary(res$model)) }
    })

    output$matrix <- renderPrint({
      res <- model_obj()
      if (is.character(res)) return(cat("Awaiting valid model parameters..."))
      preds <- predict(res$model)
      table(Predicted = preds, Actual = res$data[[input$y]])
    })

    output$accuracy <- renderText({
      res <- model_obj()
      if (is.character(res)) return("")
      preds <- predict(res$model)
      acc <- mean(preds == res$data[[input$y]]) * 100
      paste("Model Accuracy:", round(acc, 2), "%")
    })

    output$dynamic_plot_ui <- renderUI({ plotOutput(ns("diag_plot"), height = "500px") })

    output$diag_plot <- renderPlot({ plot_log_diagnostics(model_obj(), input$y) })

    output$download_plot <- downloadHandler(
      filename = function() { paste0("logistic_evaluation_", Sys.Date(), ".png") },
      content = function(file) {
        png(file, width = 800, height = 600)
        plot_log_diagnostics(model_obj(), input$y)
        dev.off()
      }
    )

    # Context (+ plot) for the AI Co-Pilot.
    list(
      context = reactive({
        res <- model_obj()
        if (is.character(res)) return(paste("Logistic Regression (multinomial) —", res))
        preds <- predict(res$model)
        acc <- mean(preds == res$data[[input$y]]) * 100
        cm <- table(Predicted = preds, Actual = res$data[[input$y]])
        paste0("Multinomial Logistic Regression. Target: ", input$y, " ; Accuracy: ", round(acc, 2), "%\n\nConfusion matrix:\n",
               paste(utils::capture.output(cm), collapse = "\n"),
               "\n\nModel summary:\n", paste(utils::capture.output(summary(res$model)), collapse = "\n"))
      }),
      plot = function() plot_log_diagnostics(model_obj(), input$y)
    )
  })
}
