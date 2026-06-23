library(shiny)
library(nnet)
library(readxl)
library(tools)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(base64enc)
library(ggplot2) 
library(MASS)
library(cluster)
library(factoextra)
library(ape) 
library(randomForest)
library(pdp)
library(nlme)
library(MuMIn)
library(lidR)
library(sf)
library(terra)
library(rgl)

# Source custom functions
source("evaluation_function.R")

# =========================================================
# AI CONFIGURATION & HELPERS
# =========================================================
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

capture_plot_as_base64 <- function(plot_fn) {
  tmp_file <- tempfile(fileext = ".png")
  png(tmp_file, width = 800, height = 600)
  plot_fn() 
  dev.off()
  b64 <- base64enc::base64encode(tmp_file)
  unlink(tmp_file)
  return(b64)
}

show_placeholder <- function(msg) {
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(msg), cex = 1.2, col = "#6c757d")
}

ask_openai_vision <- function(context, user_msg, image_b64 = NULL) {
  if (OPENAI_API_KEY == "" || is.null(OPENAI_API_KEY)) {
    return("⚠️ System Error: API Key not configured in environment.")
  }
  
  sys_prompt <- "You are a data analysis assistant. Use ONLY the provided statistical context and the provided image. Do not use external knowledge. Do not hallucinate. If the answer is not in the context or image, state that."
  
  content_list <- list(list(type = "text", text = paste("Context:", context, "\nUser Question:", user_msg)))
  
  if (!is.null(image_b64)) {
    content_list[[length(content_list) + 1]] <- list(
      type = "image_url",
      image_url = list(url = paste0("data:image/png;base64,", image_b64))
    )
  }
  
  res <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
      content_type_json(),
      body = toJSON(list(
        model = "gpt-5.4-nano",
        messages = list(
          list(role = "system", content = sys_prompt),
          list(role = "user", content = content_list)
        ),
        temperature = 0.0
      ), auto_unbox = TRUE)
    )
  }, error = function(e) return(NULL))
  
  if (is.null(res) || res$status_code != 200) return("⚠️ API Error: Connection failed or invalid request.")
  parsed <- content(res, "parsed")
  if (!is.null(parsed$choices[[1]]$message$content)) {
    return(parsed$choices[[1]]$message$content)
  } else {
    return("⚠️ Error: Received an empty response from the API.")
  }
}

# =========================================================
# GLOBAL HELPERS & PLOTTING ENGINES
# =========================================================

is_safe_cat <- function(vec) {
  if (!(is.factor(vec) || is.character(vec))) return(FALSE)
  lvls <- length(unique(vec[!is.na(vec)]))
  return(lvls > 1 && lvls <= 50)
}

init_data <- function(df) {
  if ("Organic_depth" %in% names(df)) {
    df$Organic_depth <- as.numeric(as.character(df$Organic_depth))
  }
  return(df)
}

plot_relationships <- function(df, num1, num2, cat_var, view_mode = "Grid View", target = NULL) {
  if (is.null(df) || !isTruthy(cat_var) || !isTruthy(num1) || !isTruthy(num2) || !(cat_var %in% names(df))) {
    show_placeholder("Awaiting valid numeric and categorical variables...")
    return()
  }
  
  plot_df <- df[complete.cases(df[, c(num1, num2, cat_var)]), ]
  if (nrow(plot_df) == 0) { 
    show_placeholder("Data Error: No complete cases available.")
    return() 
  }
  
  plot_df[[cat_var]] <- droplevels(as.factor(plot_df[[cat_var]]))
  fac <- plot_df[[cat_var]]
  num_levels <- length(levels(fac))
  pal <- if(num_levels > 8) rainbow(num_levels) else palette()[1:num_levels]
  
  wrap_text <- function(x) paste(strwrap(x, width = 15), collapse = "\n")
  wrapped_lvls <- sapply(levels(fac), wrap_text)
  counts <- as.numeric(table(fac))
  wrapped_lvls_with_n <- paste0(wrapped_lvls, "\n(n=", counts, ")")
  
  safe_num1 <- paste0("`", num1, "`")
  safe_num2 <- paste0("`", num2, "`")
  safe_cat <- paste0("`", cat_var, "`")
  
  if (view_mode == "Grid View") {
    rows <- 1 + ceiling(num_levels / 3)
    old_par <- par(mfrow = c(rows, 3), mar = c(6, 5, 4, 1) + 0.1, mgp = c(3, 1, 0))
    on.exit(par(old_par))
    
    form1 <- as.formula(paste(safe_num1, "~", safe_cat))
    boxplot(form1, data = plot_df, main = paste(num1, "by", cat_var), ylab = num1, col = "lightblue", 
            names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.4, outline = FALSE)
    stripchart(form1, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.25), cex = 0.8)
    text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.1)
    
    form2 <- as.formula(paste(safe_num2, "~", safe_cat))
    boxplot(form2, data = plot_df, main = paste(num2, "by", cat_var), ylab = num2, col = "lightgreen", 
            names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.4, outline = FALSE)
    stripchart(form2, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.25), cex = 0.8)
    text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.1)
    
    plot(plot_df[[num1]], plot_df[[num2]], col = pal[as.numeric(fac)], pch = 16,
         main = paste(num1, "vs", num2, "\n(All Data)"), xlab = num1, ylab = num2, cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
    legend("bottomright", legend = levels(fac), col = pal, pch = 16, cex = 1.1, bty = "n")
    
    for (i in seq_along(levels(fac))) {
      lvl <- levels(fac)[i]
      sub_df <- plot_df[plot_df[[cat_var]] == lvl, ]
      if(nrow(sub_df) > 0) {
        plot(sub_df[[num1]], sub_df[[num2]], col = pal[i], pch = 16,
             main = paste0(wrap_text(lvl), "\n(n=", nrow(sub_df), ")"), xlab = num1, ylab = num2, cex.lab = 1.3, cex.axis = 1.2, cex.main = 1.4)
      } else { show_placeholder("No data") }
    }
  } else {
    old_par <- par(mar = c(11, 7, 7, 2) + 0.1, mgp = c(5, 1.5, 0))
    on.exit(par(old_par))
    if (is.null(target)) { show_placeholder("Select a plot to zoom."); return() }
    
    if (target == paste("Boxplot:", num1)) {
      form1 <- as.formula(paste(safe_num1, "~", safe_cat))
      boxplot(form1, data = plot_df, main = paste(num1, "by", cat_var), ylab = num1, col = "lightblue", 
              names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8, outline = FALSE)
      stripchart(form1, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.3), cex = 1.2)
      text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.2)
      
    } else if (target == paste("Boxplot:", num2)) {
      form2 <- as.formula(paste(safe_num2, "~", safe_cat))
      boxplot(form2, data = plot_df, main = paste(num2, "by", cat_var), ylab = num2, col = "lightgreen", 
              names = rep("", num_levels), xlab = "", las = 1, cex.lab = 1.5, cex.axis = 1.2, cex.main = 1.8, outline = FALSE)
      stripchart(form2, data = plot_df, vertical = TRUE, method = "jitter", add = TRUE, pch = 16, col = rgb(0, 0, 0, 0.3), cex = 1.2)
      text(x = 1:num_levels, y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.03, labels = wrapped_lvls_with_n, xpd = NA, srt = 0, adj = c(0.5, 1), cex = 1.2)
      
    } else if (target == "Scatter: All Data") {
      plot(plot_df[[num1]], plot_df[[num2]], col = pal[as.numeric(fac)], pch = 16, cex = 1.5,
           main = paste(num1, "vs", num2, "\n(All Data)"), xlab = num1, ylab = num2, cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.8)
      legend("bottomright", legend = levels(fac), col = pal, pch = 16, cex = 1.2, bty = "n")
    } else {
      lvl <- target; idx <- match(lvl, levels(fac))
      sub_df <- plot_df[plot_df[[cat_var]] == lvl, ]
      if(nrow(sub_df) > 0) {
        plot(sub_df[[num1]], sub_df[[num2]], col = pal[idx], pch = 16, cex = 1.5,
             main = paste0(wrap_text(lvl), "\n(n=", nrow(sub_df), ")"), xlab = num1, ylab = num2, cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.8)
      } else { show_placeholder("No data") }
    }
  }
}

plot_lm_diagnostics <- function(model, dataset, y_var, view_mode, target) {
  if (is.character(model)) { 
    show_placeholder(model)
    return() 
  }
  
  if (view_mode == "Grid View") {
    old_par <- par(mfrow = c(1, 3), mar = c(6, 6, 5, 2) + 0.1, mgp = c(4, 1.2, 0))
    on.exit(par(old_par))
    plot(model$fitted.values, model$model[[1]], main = "Actual vs. Fitted", xlab = "Fitted Values", ylab = "Actual Data", pch = 16, col = rgb(0.2, 0.5, 0.8, 0.5), cex.lab=1.3, cex.main=1.5)
    abline(0, 1, col = "red", lwd = 2, lty = 2)
    plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = rgb(0.3, 0.3, 0.3, 0.5), cex.lab=1.3, cex.main=1.5)
    abline(h = 0, col = "red", lwd = 2)
    hist(dataset[[y_var]], main = paste("Distribution of", y_var), xlab = y_var, col = "lightblue", border = "white", cex.lab=1.3, cex.main=1.5)
  } else {
    old_par <- par(mar = c(6, 6, 5, 2) + 0.1, mgp = c(4.5, 1.2, 0))
    on.exit(par(old_par))
    if (is.null(target)) return()
    if (target == "Fitted vs Actual") {
      plot(model$fitted.values, model$model[[1]], main = "Actual vs. Fitted Values", xlab = "Model Predicted (Fitted)", ylab = "Actual Data", pch = 16, cex = 1.5, col = rgb(0.2, 0.5, 0.8, 0.5), cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
      abline(0, 1, col = "red", lwd = 2, lty = 2)
    } else if (target == "Residual Plot") {
      plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 16, cex = 1.5, col = rgb(0.3, 0.3, 0.3, 0.5), cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
      abline(h = 0, col = "red", lwd = 2)
    } else {
      hist(dataset[[y_var]], main = paste("Distribution of", y_var), xlab = y_var, col = "lightblue", border = "white", cex.lab=1.4, cex.axis=1.2, cex.main=1.6)
    }
  }
}

plot_aov_diagnostics <- function(model, view_mode, target) {
  if (is.character(model)) { 
    show_placeholder(model)
    return() 
  }
  
  if (view_mode == "Grid View") {
    old_par <- par(mfrow = c(1, 2), mar = c(6, 6, 5, 2) + 0.1, mgp = c(4, 1.2, 0))
    on.exit(par(old_par))
    plot(model, which = 1, cex.lab=1.3, cex.main=1.5) 
    plot(model, which = 2, cex.lab=1.3, cex.main=1.5) 
  } else {
    old_par <- par(mar = c(6, 6, 5, 2) + 0.1, mgp = c(4.5, 1.2, 0))
    on.exit(par(old_par))
    if (is.null(target)) return()
    if (target == "Residuals vs Fitted") plot(model, which = 1, cex.lab=1.4, cex.axis=1.2, cex.main=1.6, cex=1.5)
    else plot(model, which = 2, cex.lab=1.4, cex.axis=1.2, cex.main=1.6, cex=1.5)
  }
}

plot_log_diagnostics <- function(model_obj, target_var) {
  if (is.character(model_obj)) {
    show_placeholder(model_obj)
    return()
  }
  preds <- predict(model_obj$model)
  actual <- model_obj$data[[target_var]]
  tbl <- table(Actual = actual, Predicted = preds)
  
  old_par <- par(mar = c(4, 4, 2, 2))
  on.exit(par(old_par))
  mosaicplot(tbl, main = "Classification: Actual vs Predicted", 
             color = c("lightgray", "lightblue", "lightgreen", "lightcoral"), 
             las = 1, cex.axis = 1.1)
}

# =========================================================
# SERVER LOGIC
# =========================================================
server <- function(input, output, session) {
  
  raw_pool <- reactiveValues()
  dataset_pool <- reactiveValues()
  rv <- reactiveValues(working_data = NULL, current_rename_levels = NULL)
  
  chat_state <- reactiveVal(list(list(role = "assistant", content = "Hello! I am your embedded AI Data Analyst. Ask me to interpret any statistical summary or diagnostic plot currently on your screen.")))

  # Reactive list of dataset names; modules use this to keep their pickers in sync.
  dataset_names <- reactive({ names(reactiveValuesToList(dataset_pool)) })

  # ---- MODULES (one self-contained file per model screen; see mod_*.R) ----
  linearRegressionServer("lm", dataset_pool, dataset_names)

  # ---- STAGE 1: DATA ENGINEERING & IMPORT (ETL) ----
  observeEvent(input$user_files, {
    req(input$user_files)
    for(i in 1:nrow(input$user_files)) {
      fname <- input$user_files$name[i]
      fpath <- input$user_files$datapath[i]
      ext <- tolower(tools::file_ext(fname))
      tryCatch({
        if (ext == "csv") df <- read.csv(fpath)
        else if (ext %in% c("xlsx", "xls")) df <- as.data.frame(readxl::read_excel(fpath))
        else if (ext == "txt") df <- read.delim(fpath)
        else stop("Unsupported file type.")
        
        clean_df <- init_data(df)
        raw_pool[[fname]] <- clean_df
        dataset_pool[[fname]] <- clean_df
      }, error = function(e) {
        showNotification(paste("Error loading", fname, ":", e$message), type = "error")
      })
    }
    new_choices <- names(dataset_pool)
    updateSelectInput(session, "eng_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    # NOTE: lm_dataset is now owned by the linearRegression module (synced via dataset_names()).
    updateSelectInput(session, "lme_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "aov_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "log_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "da_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "clust_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "clf_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "rf_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "batch_targets", choices = new_choices)
    updateSelectInput(session, "join_target", choices = new_choices)
    
    showNotification("Custom datasets uploaded and globally processed!", type = "message")
  })
  
  observeEvent(input$eng_dataset, {
    req(input$eng_dataset)
    if(input$eng_dataset == "Awaiting Data Upload...") return()
    rv$working_data <- dataset_pool[[input$eng_dataset]]
  }, ignoreNULL = TRUE)
  
  observeEvent(input$reset_data, {
    req(input$eng_dataset)
    raw <- raw_pool[[input$eng_dataset]]
    rv$working_data <- raw
    dataset_pool[[input$eng_dataset]] <- raw
    showNotification("Dataset reset to original raw data across all tabs.", type = "message")
  })
  
  observeEvent(rv$working_data, {
    req(rv$working_data)
    df <- rv$working_data
    cols <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[!sapply(df, is.numeric)]
    
    updatePickerInput(session, "eng_subset_cols", choices = names(raw_pool[[input$eng_dataset]]), selected = cols)
    updatePickerInput(session, "eng_drop_cols", choices = cols, selected = NULL)
    updateSelectInput(session, "rename_col_target", choices = cols)
    updateSelectInput(session, "mutate_col1", choices = num_cols)
    updateSelectInput(session, "mutate_col2", choices = num_cols)
    updateSelectInput(session, "filter_col", choices = cols)
    updateSelectInput(session, "bin_col", choices = num_cols)
    updateSelectInput(session, "coalesce_primary", choices = cols)
    updateSelectInput(session, "coalesce_secondary", choices = cols)
    updateSelectInput(session, "join_by", choices = cols)
    
    updatePickerInput(session, "convert_to_num", choices = cat_cols, selected = NULL)
    updatePickerInput(session, "convert_to_cat", choices = num_cols, selected = NULL)
    updateSelectInput(session, "group_id", choices = cols, selected = if("final_id" %in% cols) "final_id" else cols[1])
    updatePickerInput(session, "group_nums", choices = num_cols, selected = num_cols)
    updatePickerInput(session, "group_cats", choices = cat_cols, selected = cat_cols)
    curr_rename <- if(isTruthy(isolate(input$rename_col)) && isolate(input$rename_col) %in% cat_cols) isolate(input$rename_col) else cat_cols[1]
    updateSelectInput(session, "rename_col", choices = cat_cols, selected = curr_rename)
    curr_agg <- if(isTruthy(isolate(input$agg_col)) && isolate(input$agg_col) %in% cat_cols) isolate(input$agg_col) else cat_cols[1]
    updateSelectInput(session, "agg_col", choices = cat_cols, selected = curr_agg)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("cleaned_", input$eng_dataset, "_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(rv$working_data, file, row.names = FALSE) }
  )
  
  observeEvent(input$apply_subset, {
    req(input$eng_dataset, input$eng_subset_cols)
    full_raw <- raw_pool[[input$eng_dataset]]
    safe_cols <- intersect(input$eng_subset_cols, names(full_raw))
    df <- full_raw[, safe_cols, drop = FALSE] 
    rv$working_data <- df
    dataset_pool[[input$eng_dataset]] <- df
    showNotification(paste("Subset applied globally. Columns reduced to:", length(safe_cols)), type = "message")
  })
  
  observeEvent(input$apply_drop, {
    req(input$eng_dataset, input$eng_drop_cols)
    df <- rv$working_data
    df <- df[, !(names(df) %in% input$eng_drop_cols), drop = FALSE]
    rv$working_data <- df
    dataset_pool[[input$eng_dataset]] <- df
    showNotification(paste("Dropped", length(input$eng_drop_cols), "columns globally."), type = "message")
  })
  
  observeEvent(input$apply_col_rename, {
    req(input$eng_dataset, input$rename_col_target, input$rename_col_new_name)
    df <- rv$working_data
    if (input$rename_col_new_name != "") {
      names(df)[names(df) == input$rename_col_target] <- input$rename_col_new_name
      rv$working_data <- df
      dataset_pool[[input$eng_dataset]] <- df
      showNotification(paste("Column renamed to", input$rename_col_new_name), type = "message")
    }
  })
  
  observeEvent(input$apply_mutate, {
    req(input$eng_dataset, input$mutate_col1, input$mutate_col2, input$mutate_op, input$mutate_new_name)
    df <- rv$working_data
    c1 <- df[[input$mutate_col1]]
    c2 <- df[[input$mutate_col2]]
    if (input$mutate_new_name != "") {
      new_col <- tryCatch({
        switch(input$mutate_op,
               "+" = c1 + c2,
               "-" = c1 - c2,
               "*" = c1 * c2,
               "/" = c1 / c2)
      }, error = function(e) NULL)
      
      if (!is.null(new_col)) {
        df[[input$mutate_new_name]] <- new_col
        rv$working_data <- df
        dataset_pool[[input$eng_dataset]] <- df
        showNotification(paste("Created new column:", input$mutate_new_name), type = "message")
      } else {
        showNotification("Error in mutation.", type = "error")
      }
    }
  })
  
  output$filter_condition_ui <- renderUI({
    req(rv$working_data, input$filter_col)
    col_data <- rv$working_data[[input$filter_col]]
    if (is.numeric(col_data)) {
      tagList(
        selectInput("filter_op", "Condition:", choices = c(">", "<", "==", ">=", "<=", "!=")),
        numericInput("filter_val_num", "Value:", value = 0)
      )
    } else {
      lvls <- unique(as.character(na.omit(col_data)))
      tagList(
        selectInput("filter_op", "Condition:", choices = c("==", "!=", "in", "not in")),
        pickerInput("filter_val_cat", "Value(s):", choices = lvls, multiple = TRUE, options = list(`live-search` = TRUE))
      )
    }
  })
  
  observeEvent(input$apply_filter, {
    req(input$eng_dataset, input$filter_col, input$filter_op)
    df <- rv$working_data
    col_data <- df[[input$filter_col]]
    
    keep_idx <- tryCatch({
      if (is.numeric(col_data)) {
        val <- req(input$filter_val_num)
        switch(input$filter_op,
               ">" = col_data > val,
               "<" = col_data < val,
               "==" = col_data == val,
               ">=" = col_data >= val,
               "<=" = col_data <= val,
               "!=" = col_data != val)
      } else {
        val <- req(input$filter_val_cat)
        if (input$filter_op %in% c("in", "not in") && length(val) == 0) return(rep(TRUE, length(col_data)))
        switch(input$filter_op,
               "==" = col_data == val[1],
               "!=" = col_data != val[1],
               "in" = col_data %in% val,
               "not in" = !(col_data %in% val))
      }
    }, error = function(e) rep(TRUE, length(col_data)))
    
    keep_idx[is.na(keep_idx)] <- FALSE
    df <- df[keep_idx, , drop = FALSE]
    rv$working_data <- df
    dataset_pool[[input$eng_dataset]] <- df
    showNotification(paste("Filter applied. Rows remaining:", nrow(df)), type = "message")
  })
  observeEvent(input$apply_bin, {
    req(input$eng_dataset, input$bin_col, input$bin_breaks, input$bin_labels, input$bin_new_name)
    df <- rv$working_data
    tryCatch({
      breaks_vec <- as.numeric(trimws(unlist(strsplit(input$bin_breaks, ","))))
      labels_vec <- trimws(unlist(strsplit(input$bin_labels, ",")))
      if (length(breaks_vec) - 1 != length(labels_vec)) {
        showNotification("Number of labels must be exactly one less than the number of breaks.", type = "error")
        return()
      }
      new_col <- cut(df[[input$bin_col]], breaks = breaks_vec, labels = labels_vec, right = FALSE)
      df[[input$bin_new_name]] <- new_col
      rv$working_data <- df
      dataset_pool[[input$eng_dataset]] <- df
      showNotification(paste("Created binned column:", input$bin_new_name), type = "message")
    }, error = function(e) {
      showNotification(paste("Error in binning:", e$message), type = "error")
    })
  })
  
  observeEvent(input$apply_coalesce, {
    req(input$eng_dataset, input$coalesce_primary, input$coalesce_secondary)
    df <- rv$working_data
    prim <- df[[input$coalesce_primary]]
    sec <- df[[input$coalesce_secondary]]
    
    nas <- is.na(prim) | prim == ""
    prim[nas] <- sec[nas]
    
    df[[input$coalesce_primary]] <- prim
    rv$working_data <- df
    dataset_pool[[input$eng_dataset]] <- df
    showNotification("Conditional Imputation (Coalesce) applied.", type = "message")
  })
  
  observeEvent(input$apply_join, {
    req(input$eng_dataset, input$join_target, input$join_type, input$join_by)
    df1 <- rv$working_data
    df2 <- dataset_pool[[input$join_target]]
    
    if (!(input$join_by %in% names(df2))) {
      showNotification(paste("Column", input$join_by, "not found in target dataset."), type = "error")
      return()
    }
    
    tryCatch({
      if (input$join_type == "left") {
        new_df <- merge(df1, df2, by = input$join_by, all.x = TRUE)
      } else if (input$join_type == "right") {
        new_df <- merge(df1, df2, by = input$join_by, all.y = TRUE)
      } else if (input$join_type == "inner") {
        new_df <- merge(df1, df2, by = input$join_by, all = FALSE)
      } else if (input$join_type == "full") {
        new_df <- merge(df1, df2, by = input$join_by, all = TRUE)
      }
      rv$working_data <- new_df
      dataset_pool[[input$eng_dataset]] <- new_df
      showNotification(paste(input$join_type, "join completed successfully."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error joining datasets:", e$message), type = "error")
    })
  })
  
  observeEvent(input$view_full_data, {
    req(rv$working_data)
    showModal(modalDialog(
      title = paste("Dataset Viewer:", input$eng_dataset),
      DT::dataTableOutput("full_data_table"),
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$full_data_table <- DT::renderDataTable({
    req(rv$working_data)
    DT::datatable(rv$working_data, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  observeEvent(input$apply_conversion, {
    req(rv$working_data)
    df <- rv$working_data
    raw <- raw_pool[[input$eng_dataset]]
    tryCatch({
      if (length(input$convert_to_num) > 0) {
        for (col in input$convert_to_num) {
          df[[col]] <- as.numeric(as.character(df[[col]]))
          if(col %in% names(raw)) raw[[col]] <- as.numeric(as.character(raw[[col]]))
        }
      }
      if (length(input$convert_to_cat) > 0) {
        for (col in input$convert_to_cat) {
          df[[col]] <- as.factor(df[[col]])
          if(col %in% names(raw)) raw[[col]] <- as.factor(raw[[col]])
        }
      }
      rv$working_data <- df
      dataset_pool[[input$eng_dataset]] <- df 
      raw_pool[[input$eng_dataset]] <- raw
      showNotification("Column types successfully converted and state preserved!", type = "message")
      updateSelectInput(session, "convert_to_num", selected = "")
      updateSelectInput(session, "convert_to_cat", selected = "")
    }, error = function(e) {
      showNotification(paste("Warning: Failed to convert. Ensure text columns contain numbers."), type = "warning")
    })
  })
  
  observeEvent(input$apply_group, {
    req(rv$working_data, input$group_id, input$group_nums, input$group_cats)
    df <- rv$working_data
    tryCatch({
      safe_nums <- paste0("`", input$group_nums, "`")
      safe_id <- paste0("`", input$group_id, "`")
      num_form <- as.formula(paste("cbind(", paste(safe_nums, collapse=","), ") ~", safe_id))
      plot_nums <- aggregate(num_form, data = df, FUN = mean, na.rm = TRUE)
      cat_cols <- c(input$group_id, input$group_cats)
      plot_cats <- unique(df[, cat_cols, drop = FALSE])
      plot_data <- merge(plot_nums, plot_cats, by = input$group_id)
      rv$working_data <- plot_data
      dataset_pool[[input$eng_dataset]] <- plot_data
      showNotification(paste("Data aggregated by", input$group_id, "globally! Rows reduced to:", nrow(plot_data)), type = "message")
    }, error = function(e) { showNotification(paste("Aggregation Error:", e$message), type = "error") })
  })
  
  observeEvent(input$apply_batch, {
    req(input$batch_targets)
    subset_cols <- isolate(input$eng_subset_cols)
    conv_num <- isolate(input$convert_to_num)
    conv_cat <- isolate(input$convert_to_cat)
    grp_id <- isolate(input$group_id)
    grp_nums <- isolate(input$group_nums)
    grp_cats <- isolate(input$group_cats)
    success_log <- c()
    
    for (target in input$batch_targets) {
      if (target == input$eng_dataset) next 
      df <- raw_pool[[target]] 
      tryCatch({
        if (length(conv_num) > 0) {
          safe_num <- intersect(conv_num, names(df))
          for(col in safe_num) df[[col]] <- as.numeric(as.character(df[[col]]))
        }
        if (length(conv_cat) > 0) {
          safe_cat <- intersect(conv_cat, names(df))
          for(col in safe_cat) df[[col]] <- as.factor(df[[col]])
        }
        raw_pool[[target]] <- df 
        if (length(subset_cols) > 0) {
          safe_cols <- intersect(subset_cols, names(df))
          if(length(safe_cols) > 0) df <- df[, safe_cols, drop = FALSE]
        }
        if (isTruthy(grp_id) && grp_id %in% names(df) && length(grp_nums) > 0) {
          safe_nums <- intersect(grp_nums, names(df))
          safe_cats <- intersect(grp_cats, names(df))
          if (length(safe_nums) > 0) {
            df[[grp_id]] <- as.character(df[[grp_id]])
            backtick_nums <- paste0("`", safe_nums, "`")
            backtick_id <- paste0("`", grp_id, "`")
            num_form <- as.formula(paste("cbind(", paste(backtick_nums, collapse=","), ") ~", backtick_id))
            plot_nums <- aggregate(num_form, data = df, FUN = mean, na.rm = TRUE)
            cat_cols <- c(grp_id, safe_cats)
            plot_cats <- unique(df[, cat_cols, drop = FALSE])
            df <- merge(plot_nums, plot_cats, by = grp_id)
          }
        }
        if(nrow(df) > 0 && ncol(df) > 0) {
          dataset_pool[[target]] <- df
          success_log <- c(success_log, paste0(target, " (", nrow(df), " rows)"))
        } else {
          showNotification(paste("Batch failed for", target, "- resulted in empty dataset."), type = "error")
        }
      }, error = function(e) { showNotification(paste("Error batching", target, ":", e$message), type = "error") })
    }
    if(length(success_log) > 0) showNotification(paste("Batch successfully applied to:", paste(success_log, collapse = ", ")), type = "message", duration = 8)
  })
  
  output$dynamic_rename_ui <- renderUI({
    req(rv$working_data, input$rename_col)
    lvls <- as.character(unique(na.omit(rv$working_data[[input$rename_col]])))
    rv$current_rename_levels <- lvls 
    if (length(lvls) == 0) return(markdown("*No levels found.*"))
    if (length(lvls) > 30) return(markdown("*Too many levels to rename manually (>30).*"))
    ui_list <- lapply(seq_along(lvls), function(i) {
      textInput(paste0("rename_lvl_", i), label = paste("Rename:", lvls[i]), value = lvls[i])
    })
    do.call(tagList, ui_list)
  })
  
  observeEvent(input$apply_rename, {
    req(rv$working_data, input$rename_col, rv$current_rename_levels)
    df <- rv$working_data
    raw <- raw_pool[[input$eng_dataset]]
    col <- input$rename_col
    old_lvls <- rv$current_rename_levels
    tryCatch({
      new_lvls <- sapply(seq_along(old_lvls), function(i) { input[[paste0("rename_lvl_", i)]] })
      vec_work <- as.character(df[[col]])
      for(i in seq_along(old_lvls)) vec_work[vec_work == old_lvls[i]] <- new_lvls[i]
      df[[col]] <- as.factor(vec_work)
      rv$working_data <- df
      dataset_pool[[input$eng_dataset]] <- df
      if(col %in% names(raw)) {
        vec_raw <- as.character(raw[[col]])
        for(i in seq_along(old_lvls)) vec_raw[vec_raw == old_lvls[i]] <- new_lvls[i]
        raw[[col]] <- as.factor(vec_raw)
        raw_pool[[input$eng_dataset]] <- raw
      }
      showNotification(paste("Levels in", col, "successfully renamed globally and preserved."), type = "message")
    }, error = function(e) { showNotification(paste("Rename Error:", e$message), type = "error") })
  })
  
  observeEvent(input$agg_col, {
    req(rv$working_data, input$agg_col)
    levels_avail <- unique(as.character(na.omit(rv$working_data[[input$agg_col]])))
    updateSelectInput(session, "agg_levels", choices = levels_avail)
  })
  
  observeEvent(input$apply_merge, {
    req(rv$working_data, input$agg_col, input$agg_levels, input$agg_new_name)
    df <- rv$working_data
    raw <- raw_pool[[input$eng_dataset]]
    df[[input$agg_col]] <- as.character(df[[input$agg_col]])
    df[[input$agg_col]][df[[input$agg_col]] %in% input$agg_levels] <- input$agg_new_name
    df[[input$agg_col]] <- droplevels(as.factor(df[[input$agg_col]]))
    rv$working_data <- df
    dataset_pool[[input$eng_dataset]] <- df
    if(input$agg_col %in% names(raw)) {
      raw[[input$agg_col]] <- as.character(raw[[input$agg_col]])
      raw[[input$agg_col]][raw[[input$agg_col]] %in% input$agg_levels] <- input$agg_new_name
      raw[[input$agg_col]] <- droplevels(as.factor(raw[[input$agg_col]]))
      raw_pool[[input$eng_dataset]] <- raw
    }
    updateTextInput(session, "agg_new_name", value = "")
    updateSelectInput(session, "agg_levels", selected = "")
    showNotification("Levels dynamically merged and preserved.", type = "message")
  })
  
  observe({
    req(rv$working_data)
    cols <- names(rv$working_data)
    curr_view <- if(isTruthy(isolate(input$eng_view_col)) && isolate(input$eng_view_col) %in% cols) isolate(input$eng_view_col) else cols[1]
    updateSelectInput(session, "eng_view_col", choices = cols, selected = curr_view)
  })
  
  output$eng_str <- renderPrint({ 
    req(rv$working_data)
    cat("Active Variables:", ncol(rv$working_data), "| Total Observations:", nrow(rv$working_data), "\n")
    cat("-----------------------------------------------------------------\n")
    str(rv$working_data) 
  })
  
  output$eng_table <- renderPrint({
    req(rv$working_data, input$eng_view_col)
    vec <- rv$working_data[[input$eng_view_col]]
    if (is.numeric(vec)) summary(vec) else table(vec, useNA = "ifany")
  })
  
  eng_plot_fn <- function() {
    req(rv$working_data, input$eng_view_col)
    vec <- rv$working_data[[input$eng_view_col]]
    if (is.numeric(vec)) {
      par(mar = c(4.5, 4.5, 2, 1))
      boxplot(vec, horizontal = TRUE, main = paste("Distribution of", input$eng_view_col), xlab = input$eng_view_col, col = "lightgray", outline = TRUE)
      stripchart(vec, method = "jitter", add = TRUE, pch = 16, col = rgb(0,0,0,0.25), cex = 0.8)
    } else {
      par(mar = c(4.5, 12, 2, 1)) 
      counts <- rev(sort(table(vec))) 
      barplot(counts, horiz = TRUE, las = 1, main = paste("Frequencies of", input$eng_view_col), col = "lightgray", cex.names = 0.9, xlab = "Count")
    }
  }
  
  output$eng_plot <- renderPlot({ eng_plot_fn() })
  
  output$download_dist_plot <- downloadHandler(
    filename = function() { paste0("distribution_", input$eng_view_col, "_", Sys.Date(), ".png") },
    content = function(file) {
      png(file, width = 800, height = 600)
      eng_plot_fn()
      dev.off()
    }
  )
  
  # ---- STAGE 2: EXPLORATORY DATA ANALYSIS (EDA) ----
  observe({
    req(input$eng_dataset)
    if(input$eng_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$eng_dataset]]
    req(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, is_safe_cat)] 
    
    curr_num1 <- if(isTruthy(isolate(input$eda_num1)) && isolate(input$eda_num1) %in% num_cols) isolate(input$eda_num1) else if(length(num_cols)>0) num_cols[1] else NULL
    curr_num2 <- if(isTruthy(isolate(input$eda_num2)) && isolate(input$eda_num2) %in% num_cols) isolate(input$eda_num2) else if(length(num_cols)>1) num_cols[2] else curr_num1
    curr_cat <- if(isTruthy(isolate(input$eda_category)) && isolate(input$eda_category) %in% cat_cols) isolate(input$eda_category) else if(length(cat_cols)>0) cat_cols[1] else NULL
    
    updateSelectInput(session, "eda_num1", choices = num_cols, selected = curr_num1)
    updateSelectInput(session, "eda_num2", choices = num_cols, selected = curr_num2)
    updateSelectInput(session, "eda_category", choices = cat_cols, selected = curr_cat)
  })
  
  output$eda_single_selector <- renderUI({
    req(input$eda_view_mode == "Single Plot", input$eng_dataset)
    if(input$eng_dataset == "Awaiting Data Upload...") return()
    
    df <- dataset_pool[[input$eng_dataset]]
    choices <- c(paste("Boxplot:", input$eda_num1), paste("Boxplot:", input$eda_num2), "Scatter: All Data")
    
    if (isTruthy(input$eda_category) && input$eda_category %in% names(df)) {
      fac <- na.omit(unique(as.character(df[[input$eda_category]])))
      choices <- c(choices, fac)
    }
    selectInput("eda_zoom_target", label = NULL, choices = choices, width = "200px")
  })
  
  output$dynamic_eda_plot_ui <- renderUI({
    req(input$eng_dataset, input$eda_view_mode)
    if(input$eng_dataset == "Awaiting Data Upload...") return()
    
    if(input$eda_view_mode == "Grid View") {
      df <- dataset_pool[[input$eng_dataset]]
      
      if (isTruthy(input$eda_category) && input$eda_category %in% names(df)) {
        fac <- as.factor(df[[input$eda_category]])
        num_lvls <- length(unique(na.omit(fac)))
        rows <- if(num_lvls > 0) 1 + ceiling(num_lvls / 3) else 1
      } else {
        rows <- 1
      }
      dynamic_height <- max(500, rows * 350)
      plotOutput("relationship_plots", height = paste0(dynamic_height, "px"))
    } else {
      plotOutput("relationship_plots", height = "700px")
    }
  })
  
  output$relationship_plots <- renderPlot({
    req(input$eng_dataset)
    if(input$eng_dataset == "Awaiting Data Upload...") {
      show_placeholder("Awaiting valid dataset...")
      return()
    }
    if(input$eda_view_mode == "Single Plot") req(input$eda_zoom_target)
    
    plot_relationships(dataset_pool[[input$eng_dataset]], input$eda_num1, input$eda_num2, input$eda_category, 
                       view_mode = input$eda_view_mode, target = input$eda_zoom_target)
  })
  
  output$download_eda_plot <- downloadHandler(
    filename = function() { paste0("eda_relationships_", Sys.Date(), ".png") },
    content = function(file) {
      df <- dataset_pool[[input$eng_dataset]]
      
      if (isTruthy(input$eda_category) && input$eda_category %in% names(df)) {
        fac <- as.factor(df[[input$eda_category]])
        num_lvls <- length(unique(na.omit(fac)))
        rows <- if(num_lvls > 0) 1 + ceiling(num_lvls / 3) else 1
      } else {
        rows <- 1
      }
      png_height <- if(input$eda_view_mode == "Grid View") max(600, rows * 400) else 600
      
      png(file, width = 1000, height = png_height)
      plot_relationships(dataset_pool[[input$eng_dataset]], input$eda_num1, input$eda_num2, input$eda_category, 
                         view_mode = input$eda_view_mode, target = input$eda_zoom_target)
      dev.off()
    }
  )
  
  # ---- STAGE 3: LINEAR REGRESSION ----
  # Moved to mod_linear_regression.R (linearRegressionServer), wired near the top
  # of this server function. Kept here only as a signpost for the other stages.

  # ---- STAGE 4: ANOVA ----
  observe({
    req(input$aov_dataset)
    if(input$aov_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$aov_dataset]]
    req(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, is_safe_cat)]
    
    curr_y <- if(isTruthy(isolate(input$aov_y)) && isolate(input$aov_y) %in% num_cols) isolate(input$aov_y) else if(length(num_cols)>0) num_cols[1] else NULL
    curr_x <- if(isTruthy(isolate(input$aov_x)) && isolate(input$aov_x) %in% cat_cols) isolate(input$aov_x) else if(length(cat_cols)>0) cat_cols[1] else NULL
    
    updateSelectInput(session, "aov_y", choices = num_cols, selected = curr_y)
    updateSelectInput(session, "aov_x", choices = cat_cols, selected = curr_x)
  })
  
  aov_model <- reactive({
    req(input$aov_dataset)
    if(input$aov_dataset == "Awaiting Data Upload...") return("Awaiting dataset...")
    df <- dataset_pool[[input$aov_dataset]]
    if (!isTruthy(input$aov_y) || !isTruthy(input$aov_x)) return("Awaiting Predictors: Select a Continuous Y and Categorical X.")
    
    clean_df <- df[, c(input$aov_y, input$aov_x), drop = FALSE]
    clean_df <- clean_df[complete.cases(clean_df), , drop = FALSE]
    
    if (nrow(clean_df) < 10) return("Data Error: Insufficient complete cases.")
    clean_df[[input$aov_x]] <- droplevels(as.factor(clean_df[[input$aov_x]]))
    if (length(levels(clean_df[[input$aov_x]])) < 2) return("Data Error: Categorical variable must have at least 2 valid levels.")
    
    form_str <- paste0("`", input$aov_y, "` ~ `", input$aov_x, "`")
    tryCatch({ aov(as.formula(form_str), data = clean_df) }, error = function(e) { return(paste("ANOVA Error:", e$message)) })
  })
  
  output$aov_summary <- renderPrint({
    model <- aov_model()
    if (is.character(model)) cat(model) else print(summary(model))
  })
  
  output$aov_tukey <- renderPrint({
    model <- aov_model()
    if (is.character(model)) return(cat("Awaiting valid model parameters..."))
    tryCatch({ print(TukeyHSD(model)) }, error = function(e) { cat("Tukey HSD Test Requires Factors. Error:\n", e$message) })
  })
  
  output$aov_single_selector <- renderUI({
    req(input$aov_view_mode == "Single Plot")
    selectInput("aov_zoom_target", label = NULL, choices = c("Residuals vs Fitted", "Normal Q-Q"), width = "200px")
  })
  
  output$dynamic_aov_plot_ui <- renderUI({
    req(input$aov_view_mode)
    plotOutput("aov_diag_plot", height = "500px")
  })
  
  output$aov_diag_plot <- renderPlot({
    req(input$aov_view_mode)
    if(input$aov_view_mode == "Single Plot") req(input$aov_zoom_target)
    plot_aov_diagnostics(aov_model(), input$aov_view_mode, input$aov_zoom_target)
  })
  
  output$download_aov_plot <- downloadHandler(
    filename = function() { paste0("anova_diagnostic_", Sys.Date(), ".png") },
    content = function(file) {
      png(file, width = 800, height = 600)
      plot_aov_diagnostics(aov_model(), input$aov_view_mode, input$aov_zoom_target)
      dev.off()
    }
  )
  
  # ---- STAGE 5: LOGISTIC REGRESSION (MULTINOMIAL) ----
  observe({
    req(input$log_dataset)
    if(input$log_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$log_dataset]]
    req(df)
    cat_cols <- names(df)[sapply(df, is_safe_cat)]
    all_cols <- names(df)
    
    curr_y <- if(isTruthy(isolate(input$log_y)) && isolate(input$log_y) %in% cat_cols) isolate(input$log_y) else if(length(cat_cols)>0) cat_cols[1] else NULL
    curr_build <- if(isTruthy(isolate(input$log_build_var)) && isolate(input$log_build_var) %in% all_cols) isolate(input$log_build_var) else all_cols[1]
    
    updateSelectInput(session, "log_y", choices = cat_cols, selected = curr_y)
    updateSelectInput(session, "log_build_var", choices = all_cols, selected = curr_build)
  })
  
  observeEvent(input$log_btn_add_var, {
    var <- paste0("`", input$log_build_var, "`")
    trans <- input$log_build_trans
    term <- switch(trans, "raw" = var, "log" = paste0("log(", var, ")"), "sqrt" = paste0("sqrt(", var, ")"), "poly" = paste0("I(", var, "^2)"))
    current <- trimws(input$log_formula_text)
    new_text <- if (nchar(current) > 0) paste(current, term) else term
    updateTextAreaInput(session, "log_formula_text", value = new_text)
  })
  
  observeEvent(input$log_btn_add_plus, {
    current <- trimws(input$log_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "log_formula_text", value = paste(current, "+ "))
  })
  
  observeEvent(input$log_btn_add_star, {
    current <- trimws(input$log_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "log_formula_text", value = paste(current, "* "))
  })
  
  observeEvent(input$log_btn_clear, { updateTextAreaInput(session, "log_formula_text", value = "") })
  
  log_formula_str <- reactive({
    if (!isTruthy(input$log_y)) return("Y ~ ...")
    safe_y <- paste0("`", input$log_y, "`")
    x_side <- trimws(input$log_formula_text)
    if (nchar(x_side) == 0) return(paste(safe_y, "~ ..."))
    paste(safe_y, "~", x_side)
  })
  
  output$log_formula_display <- renderText({ log_formula_str() })
  
  log_model_obj <- reactive({
    req(input$log_dataset)
    if(input$log_dataset == "Awaiting Data Upload...") return("Awaiting dataset...")
    df <- dataset_pool[[input$log_dataset]]
    form_str <- log_formula_str()
    
    if (grepl("\\.\\.\\.", form_str)) return("Awaiting Predictors: Please use the builder or type a formula.")
    
    all_vars <- all.vars(as.formula(form_str))
    missing_vars <- setdiff(all_vars, names(df))
    if(length(missing_vars) > 0) return(paste("Error: Variables not found in dataset:", paste(missing_vars, collapse=", ")))
    
    clean_df <- df[, all_vars, drop = FALSE]
    clean_df <- clean_df[complete.cases(clean_df), , drop = FALSE]
    
    if (nrow(clean_df) < 10) return("Data Error: Insufficient complete cases after removing NAs.")
    clean_df[[input$log_y]] <- as.factor(clean_df[[input$log_y]])
    if (length(unique(clean_df[[input$log_y]])) < 2) return("Data Error: Target variable has less than 2 distinct levels.")
    
    tryCatch({
      model <- multinom(as.formula(form_str), data = clean_df, trace = FALSE)
      list(model = model, data = clean_df)
    }, error = function(e) { return(paste("Syntax Error in Formula:", e$message)) })
  })
  
  output$log_summary <- renderPrint({
    res <- log_model_obj()
    if (is.character(res)) cat(res) else { print(res$model$call); cat("\n"); print(summary(res$model)) }
  })
  
  output$log_matrix <- renderPrint({
    res <- log_model_obj()
    if (is.character(res)) return(cat("Awaiting valid model parameters..."))
    preds <- predict(res$model)
    table(Predicted = preds, Actual = res$data[[input$log_y]])
  })
  
  output$log_accuracy <- renderText({
    res <- log_model_obj()
    if (is.character(res)) return("")
    preds <- predict(res$model)
    acc <- mean(preds == res$data[[input$log_y]]) * 100
    paste("Model Accuracy:", round(acc, 2), "%")
  })
  
  output$dynamic_log_plot_ui <- renderUI({
    plotOutput("log_diag_plot", height = "500px")
  })
  
  output$log_diag_plot <- renderPlot({
    plot_log_diagnostics(log_model_obj(), input$log_y)
  })
  
  output$download_log_plot <- downloadHandler(
    filename = function() { paste0("logistic_evaluation_", Sys.Date(), ".png") },
    content = function(file) {
      png(file, width = 800, height = 600)
      plot_log_diagnostics(log_model_obj(), input$log_y)
      dev.off()
    }
  )
  
  # ---- STAGE 6: DISCRIMINANT ANALYSIS ----
  observe({
    req(input$da_dataset)
    if(input$da_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$da_dataset]]
    req(df)
    
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, is_safe_cat)]
    all_cols <- names(df)
    
    curr_cat <- if(isTruthy(isolate(input$da_category)) && isolate(input$da_category) %in% cat_cols) isolate(input$da_category) else if(length(cat_cols)>0) cat_cols[1] else NULL
    updateSelectInput(session, "da_category", choices = cat_cols, selected = curr_cat)
    
    curr_num1 <- if(isTruthy(isolate(input$da_ellipses_x)) && isolate(input$da_ellipses_x) %in% num_cols) isolate(input$da_ellipses_x) else if(length(num_cols)>0) num_cols[1] else NULL
    curr_num2 <- if(isTruthy(isolate(input$da_ellipses_y)) && isolate(input$da_ellipses_y) %in% num_cols) isolate(input$da_ellipses_y) else if(length(num_cols)>1) num_cols[2] else curr_num1
    curr_box <- if(isTruthy(isolate(input$da_box_y)) && isolate(input$da_box_y) %in% num_cols) isolate(input$da_box_y) else curr_num1
    curr_norm <- if(isTruthy(isolate(input$da_norm_var)) && isolate(input$da_norm_var) %in% num_cols) isolate(input$da_norm_var) else curr_num1
    curr_stat_var <- if(isTruthy(isolate(input$stat_shapiro_var)) && isolate(input$stat_shapiro_var) %in% num_cols) isolate(input$stat_shapiro_var) else curr_num1
    
    updateSelectInput(session, "da_ellipses_x", choices = num_cols, selected = curr_num1)
    updateSelectInput(session, "da_ellipses_y", choices = num_cols, selected = curr_num2)
    updateSelectInput(session, "da_box_y", choices = num_cols, selected = curr_box)
    updateSelectInput(session, "da_norm_var", choices = num_cols, selected = curr_norm)
    updateSelectInput(session, "stat_shapiro_var", choices = num_cols, selected = curr_stat_var)
    
    if (isTruthy(curr_cat) && curr_cat %in% names(df)) {
      grp_levels <- unique(as.character(na.omit(df[[curr_cat]])))
      curr_grp <- if(isTruthy(isolate(input$stat_shapiro_group)) && isolate(input$stat_shapiro_group) %in% grp_levels) isolate(input$stat_shapiro_group) else grp_levels[1]
      updateSelectInput(session, "stat_shapiro_group", choices = grp_levels, selected = curr_grp)
    }
    
    curr_boxm <- if(isTruthy(isolate(input$stat_boxm_vars)) && all(isolate(input$stat_boxm_vars) %in% num_cols)) isolate(input$stat_boxm_vars) else if (length(num_cols) >= 2) num_cols[1:2] else num_cols
    updateSelectInput(session, "stat_boxm_vars", choices = num_cols, selected = curr_boxm)
    
    curr_build_da <- if(isTruthy(isolate(input$da_lda_build_var)) && isolate(input$da_lda_build_var) %in% all_cols) isolate(input$da_lda_build_var) else all_cols[1]
    updateSelectInput(session, "da_lda_build_var", choices = all_cols, selected = curr_build_da)
  })
  
  observeEvent(input$da_lda_btn_add_var, {
    var <- paste0("`", input$da_lda_build_var, "`")
    current <- trimws(input$da_lda_formula_text)
    new_text <- if (nchar(current) > 0) paste(current, var) else var
    updateTextAreaInput(session, "da_lda_formula_text", value = new_text)
  })
  observeEvent(input$da_lda_btn_add_plus, {
    current <- trimws(input$da_lda_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "da_lda_formula_text", value = paste(current, "+ "))
  })
  observeEvent(input$da_lda_btn_clear, { updateTextAreaInput(session, "da_lda_formula_text", value = "") })
  
  da_lda_formula_str <- reactive({
    if (!isTruthy(input$da_category)) return("Y ~ ...")
    safe_y <- paste0("`", input$da_category, "`")
    x_side <- trimws(input$da_lda_formula_text)
    if (nchar(x_side) == 0) return(paste(safe_y, "~ ..."))
    paste(safe_y, "~", x_side)
  })
  
  da_lda_model_obj <- reactive({
    req(input$da_dataset, input$da_category)
    if(input$da_dataset == "Awaiting Data Upload...") return("Awaiting dataset...")
    df <- dataset_pool[[input$da_dataset]]
    form_str <- da_lda_formula_str()
    
    if (grepl("\\.\\.\\.", form_str)) return("Awaiting Predictors: Please build a formula.")
    
    all_vars <- all.vars(as.formula(form_str))
    missing_vars <- setdiff(all_vars, names(df))
    if(length(missing_vars) > 0) return(paste("Error: Variables not found:", paste(missing_vars, collapse=", ")))
    
    clean_df <- df[, all_vars, drop = FALSE]
    clean_df <- clean_df[complete.cases(clean_df), , drop = FALSE]
    
    if (nrow(clean_df) < 10) return("Data Error: Insufficient complete cases.")
    clean_df[[input$da_category]] <- as.factor(clean_df[[input$da_category]])
    if (length(unique(clean_df[[input$da_category]])) < 2) return("Data Error: Target requires >= 2 distinct levels.")
    
    method <- if(isTruthy(input$da_method_type)) input$da_method_type else "LDA"
    predictors <- all_vars[-1]
    
    tryCatch({
      if (method == "LDA") {
        model <- MASS::lda(as.formula(form_str), data = clean_df)
        preds <- predict(model)
        list(model = model, data = clean_df, preds = preds, pred_class = preds$class, 
             target_var = input$da_category, predictors = predictors, method_name = "LDA",
             has_ld = TRUE, ld_scores = as.data.frame(preds$x))
        
      } else if (method == "WLDA") {
        weight_type <- if(isTruthy(input$da_wlda_weight_type)) input$da_wlda_weight_type else "inverse"
        class_counts <- table(clean_df[[input$da_category]])
        N <- nrow(clean_df)
        
        if (weight_type == "inverse") {
          weights <- as.numeric(1 / class_counts[clean_df[[input$da_category]]])
        } else if (weight_type == "proportional") {
          weights <- as.numeric(class_counts[clean_df[[input$da_category]]] / N)
        } else {
          weights <- rep(1, N)
        }
        
        model <- MASS::lda(as.formula(form_str), data = clean_df, weights = weights)
        preds <- predict(model)
        list(model = model, data = clean_df, preds = preds, pred_class = preds$class,
             target_var = input$da_category, predictors = predictors, method_name = "Weighted LDA",
             has_ld = TRUE, ld_scores = as.data.frame(preds$x))
        
      } else if (method == "QDA") {
        model <- MASS::qda(as.formula(form_str), data = clean_df)
        preds <- predict(model)
        list(model = model, data = clean_df, preds = preds, pred_class = preds$class,
             target_var = input$da_category, predictors = predictors, method_name = "QDA",
             has_ld = FALSE, ld_scores = NULL)
        
      } else if (method == "RLDA") {
        if (!requireNamespace("klaR", quietly = TRUE)) return("Package 'klaR' required. Install with: install.packages('klaR')")
        model <- klaR::rda(as.formula(form_str), data = clean_df,
                           gamma = seq(0, 1, 0.1), lambda = seq(0, 1, 0.1))
        preds <- predict(model)
        list(model = model, data = clean_df, preds = preds, pred_class = preds$class,
             target_var = input$da_category, predictors = predictors, method_name = "Regularized LDA",
             has_ld = FALSE, ld_scores = NULL)
        
      } else if (method == "KDA") {
        if (!requireNamespace("kernlab", quietly = TRUE)) return("Package 'kernlab' required. Install with: install.packages('kernlab')")
        sigma_val <- if(isTruthy(input$da_kda_sigma)) input$da_kda_sigma else 0.01
        C_val <- if(isTruthy(input$da_kda_C)) input$da_kda_C else 0.1
        model <- kernlab::ksvm(as.formula(form_str), data = clean_df,
                               kernel = "rbfdot", kpar = list(sigma = sigma_val),
                               C = C_val, prob.model = TRUE)
        pred_class <- kernlab::predict(model, clean_df)
        list(model = model, data = clean_df, preds = NULL, pred_class = pred_class,
             target_var = input$da_category, predictors = predictors, method_name = "Kernel DA (SVM-RBF)",
             has_ld = FALSE, ld_scores = NULL)
        
      } else if (method == "LLDA") {
        if (!requireNamespace("klaR", quietly = TRUE)) return("Package 'klaR' required. Install with: install.packages('klaR')")
        k_val <- if(isTruthy(input$da_llda_k)) input$da_llda_k else 5
        # Jitter numeric vars to avoid ties
        clean_df_j <- clean_df
        for(p in predictors) {
          if(is.numeric(clean_df_j[[p]])) clean_df_j[[p]] <- jitter(clean_df_j[[p]], amount = 0.0001)
        }
        model <- klaR::loclda(as.formula(form_str), data = clean_df_j, k = k_val)
        preds <- predict(model)
        list(model = model, data = clean_df, preds = preds, pred_class = preds$class,
             target_var = input$da_category, predictors = predictors, method_name = "Locally Linear DA",
             has_ld = FALSE, ld_scores = NULL)
        
      } else if (method == "MMC") {
        if (!requireNamespace("kernlab", quietly = TRUE)) return("Package 'kernlab' required. Install with: install.packages('kernlab')")
        C_val <- if(isTruthy(input$da_mmc_C)) input$da_mmc_C else 1
        model <- kernlab::ksvm(as.formula(form_str), data = clean_df,
                               kernel = "vanilladot", C = C_val)
        pred_class <- kernlab::predict(model, clean_df)
        list(model = model, data = clean_df, preds = NULL, pred_class = pred_class,
             target_var = input$da_category, predictors = predictors, method_name = "Maximum Margin (Linear SVM)",
             has_ld = FALSE, ld_scores = NULL)
        
      } else if (method == "RF") {
        if (!requireNamespace("randomForest", quietly = TRUE)) return("Package 'randomForest' required. Install with: install.packages('randomForest')")
        ntree_val <- if(isTruthy(input$da_rf_ntree)) input$da_rf_ntree else 1000
        mtry_val <- if(isTruthy(input$da_rf_mtry)) input$da_rf_mtry else 2
        mtry_val <- min(mtry_val, length(predictors))
        model <- randomForest::randomForest(as.formula(form_str), data = clean_df,
                                            ntree = ntree_val, mtry = mtry_val, importance = TRUE)
        pred_class <- predict(model)
        list(model = model, data = clean_df, preds = NULL, pred_class = pred_class,
             target_var = input$da_category, predictors = predictors, method_name = "Random Forest",
             has_ld = FALSE, ld_scores = NULL, has_importance = TRUE)
        
      } else if (method == "NN") {
        size_val <- if(isTruthy(input$da_nn_size)) input$da_nn_size else 5
        decay_val <- if(isTruthy(input$da_nn_decay)) input$da_nn_decay else 0.01
        model <- nnet::nnet(as.formula(form_str), data = clean_df,
                            size = size_val, decay = decay_val, maxit = 200, trace = FALSE)
        pred_class <- predict(model, clean_df, type = "class")
        list(model = model, data = clean_df, preds = NULL, pred_class = pred_class,
             target_var = input$da_category, predictors = predictors, method_name = "Neural Network",
             has_ld = FALSE, ld_scores = NULL)
        
      } else {
        return("Unknown method selected.")
      }
    }, error = function(e) { return(paste("Model Error:", e$message)) })
  })
  
  da_plot_ellipses_fn <- function() {
    if(!isTruthy(input$da_dataset) || input$da_dataset == "Awaiting Data Upload...") {
      show_placeholder("Awaiting dataset...")
      return()
    }
    if(!isTruthy(input$da_category) || !isTruthy(input$da_ellipses_x) || !isTruthy(input$da_ellipses_y)) {
      show_placeholder("Awaiting Variable Selection...")
      return()
    }
    df <- dataset_pool[[input$da_dataset]]
    df <- df[complete.cases(df[, c(input$da_ellipses_x, input$da_ellipses_y, input$da_category)]), ]
    if(nrow(df) == 0) { show_placeholder("No valid data"); return() }
    df[[input$da_category]] <- as.factor(df[[input$da_category]])
    
    p <- ggplot(df, aes(x = .data[[input$da_ellipses_x]], y = .data[[input$da_ellipses_y]], col = .data[[input$da_category]])) +
      geom_point(size = 2, alpha = 0.7) +
      stat_ellipse(linewidth = 1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    print(p)
  }
  
  da_plot_box_fn <- function() {
    if(!isTruthy(input$da_dataset) || input$da_dataset == "Awaiting Data Upload...") {
      show_placeholder("Awaiting dataset...")
      return()
    }
    if(!isTruthy(input$da_category) || !isTruthy(input$da_box_y)) {
      show_placeholder("Awaiting Variable Selection...")
      return()
    }
    df <- dataset_pool[[input$da_dataset]]
    df <- df[complete.cases(df[, c(input$da_box_y, input$da_category)]), ]
    if(nrow(df) == 0) { show_placeholder("No valid data"); return() }
    df[[input$da_category]] <- as.factor(df[[input$da_category]])
    
    p <- ggplot(df, aes(x = .data[[input$da_category]], y = .data[[input$da_box_y]], fill = .data[[input$da_category]])) +
      geom_boxplot(alpha = 0.5, outlier.size = 2, outlier.colour = "red") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    print(p)
  }
  
  da_plot_qq_fn <- function() {
    if(!isTruthy(input$da_dataset) || input$da_dataset == "Awaiting Data Upload...") {
      show_placeholder("Awaiting dataset...")
      return()
    }
    if(!isTruthy(input$da_category) || !isTruthy(input$da_norm_var)) {
      show_placeholder("Awaiting Variable Selection...")
      return()
    }
    df <- dataset_pool[[input$da_dataset]]
    df <- df[complete.cases(df[, c(input$da_norm_var, input$da_category)]), ]
    if(nrow(df) == 0) { show_placeholder("No valid data"); return() }
    df[[input$da_category]] <- as.factor(df[[input$da_category]])
    
    p <- ggplot(df, aes(sample = .data[[input$da_norm_var]], col = .data[[input$da_category]])) +
      stat_qq(size = 2, alpha = 0.7) +
      stat_qq_line(col = "black", linetype = "dashed") +
      facet_wrap(as.formula(paste("~", paste0("`", input$da_category, "`")))) + 
      theme_minimal(base_size = 14) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme(legend.position = "none")
    print(p)
  }
  
  da_plot_density_fn <- function() {
    if(!isTruthy(input$da_dataset) || input$da_dataset == "Awaiting Data Upload...") {
      show_placeholder("Awaiting dataset...")
      return()
    }
    if(!isTruthy(input$da_category) || !isTruthy(input$da_norm_var)) {
      show_placeholder("Awaiting Variable Selection...")
      return()
    }
    df <- dataset_pool[[input$da_dataset]]
    df <- df[complete.cases(df[, c(input$da_norm_var, input$da_category)]), ]
    if(nrow(df) == 0) { show_placeholder("No valid data"); return() }
    df[[input$da_category]] <- as.factor(df[[input$da_category]])
    
    p <- ggplot(df, aes(x = .data[[input$da_norm_var]], fill = .data[[input$da_category]], col = .data[[input$da_category]])) +
      geom_density(alpha = 0.3, linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(y = "Density") +
      theme(legend.position = "bottom")
    print(p)
  }
  
  output$plot_ellipses <- renderPlot({ da_plot_ellipses_fn() })
  output$plot_box      <- renderPlot({ da_plot_box_fn() })
  output$plot_qq       <- renderPlot({ da_plot_qq_fn() })
  output$plot_density  <- renderPlot({ da_plot_density_fn() })
  
  output$stat_test_results <- renderPrint({
    if(!isTruthy(input$da_dataset) || input$da_dataset == "Awaiting Data Upload...") return(cat("Awaiting dataset..."))
    req(input$da_view == "5. Statistical Tests", input$stat_test_type, input$da_category)
    df <- dataset_pool[[input$da_dataset]]
    
    if (input$stat_test_type == "Shapiro-Wilk (Normality)") {
      req(input$stat_shapiro_var, input$stat_shapiro_group)
      sub_data <- df[[input$stat_shapiro_var]][df[[input$da_category]] == input$stat_shapiro_group]
      sub_data <- na.omit(sub_data)
      if (length(sub_data) < 3) { cat("Error: Not enough data points to run Shapiro-Wilk (requires at least 3).")
      } else {
        cat("=== Shapiro-Wilk Normality Test ===\nVariable:", input$stat_shapiro_var, "\nGroup:", input$stat_shapiro_group, "\n\n")
        print(shapiro.test(sub_data))
      }
    } else if (input$stat_test_type == "Box's M (Equal Covariance)") {
      req(input$stat_boxm_vars)
      if (length(input$stat_boxm_vars) < 2) { cat("Error: Box's M test requires at least 2 numeric variables.")
      } else {
        if (!requireNamespace("heplots", quietly = TRUE)) { cat("Error: 'heplots' package is required.\nPlease run 'install.packages(\"heplots\")'.")
        } else {
          cat("=== Box's M-Test for Homogeneity of Covariance Matrices ===\nVariables Included:", paste(input$stat_boxm_vars, collapse = ", "), "\nGrouping Variable:", input$da_category, "\n\n")
          test_data <- df[complete.cases(df[, c(input$stat_boxm_vars, input$da_category)]), ]
          print(heplots::boxM(test_data[, input$stat_boxm_vars], test_data[[input$da_category]]))
        }
      }
    }
  })
  
  output$download_da_assumption_plot <- downloadHandler(
    filename = function() { paste0("assumption_check_", Sys.Date(), ".png") },
    content = function(file) {
      png(file, width = 800, height = 600)
      if (input$da_view == "1. Covariance Ellipses") da_plot_ellipses_fn()
      else if (input$da_view == "2. Equal Variance (Boxplots)") da_plot_box_fn()
      else if (input$da_view == "3. Normality (Q-Q Plots)") da_plot_qq_fn()
      else if (input$da_view == "4. Distribution Density") da_plot_density_fn()
      dev.off()
    }
  )
  
  plot_lda_single <- function(model_obj, plot_name) {
    if (is.character(model_obj)) { 
      show_placeholder(model_obj)
      return() 
    }
    
    if(plot_name == "Pairs Plot") {
      old_par <- par(mar = c(2, 2, 2, 2))
      on.exit(par(old_par))
      # Only use numeric predictors for pairs plot
      num_preds <- model_obj$predictors[sapply(model_obj$data[, model_obj$predictors, drop=FALSE], is.numeric)]
      if(length(num_preds) < 2) { show_placeholder("Need at least 2 numeric predictors for pairs plot."); return() }
      pairs(model_obj$data[, num_preds, drop=FALSE],
            main = paste("Pairs Plot -", model_obj$method_name),
            col = as.numeric(model_obj$data[[model_obj$target_var]]),
            pch = 16)
      
    } else if (plot_name == "Stacked Histogram") {
      if(isTRUE(model_obj$has_ld) && !is.null(model_obj$ld_scores) && ncol(model_obj$ld_scores) >= 1) {
        df_plot <- data.frame(
          Class = model_obj$data[[model_obj$target_var]],
          LD1 = model_obj$ld_scores[, 1]
        )
        p <- ggplot(df_plot, aes(x = LD1, fill = Class)) + 
          geom_histogram(color = "darkgray", bins = 30, alpha = 0.8) + 
          facet_wrap(~ Class, ncol = 1, scales = "free_y") + 
          theme_minimal(base_size = 14) + 
          labs(title = paste("Stacked Histogram of LD1 Scores -", model_obj$method_name), x = "LD1 Score", y = "Count") +
          theme(
            legend.position = "none",
            strip.background = element_rect(fill = "#e9ecef", color = NA),
            strip.text = element_text(face = "bold", size = 12),
            panel.spacing = unit(1, "lines")
          )
        print(p)
      } else {
        show_placeholder(paste("LD scores not available for", model_obj$method_name, ". This plot is only for LDA-based methods."))
      }
      
    } else if (plot_name == "Biplot (ggord)") {
      if(!isTRUE(model_obj$has_ld)) {
        show_placeholder(paste("Biplot not available for", model_obj$method_name, ". Only available for LDA/Weighted LDA."))
        return()
      }
      if(requireNamespace("ggord", quietly = TRUE)) {
        tryCatch({
          print(ggord::ggord(model_obj$model))
        }, error = function(e) {
          show_placeholder(paste("ggord error:", e$message))
        })
      } else {
        show_placeholder("Please install 'ggord' from GitHub: remotes::install_github('fawda123/ggord')")
      }
      
    } else if (plot_name == "Partition Plot (partimat)") {
      if(requireNamespace("klaR", quietly = TRUE)) {
        tryCatch({
          # Only use numeric predictors for partimat
          num_preds <- model_obj$predictors[sapply(model_obj$data[, model_obj$predictors, drop=FALSE], is.numeric)]
          if(length(num_preds) < 2) { show_placeholder("Need at least 2 numeric predictors."); return() }
          vars <- if(length(num_preds) > 4) num_preds[1:4] else num_preds
          form <- as.formula(paste(paste0("`", model_obj$target_var, "`"), "~", paste(paste0("`", vars, "`"), collapse="+")))
          klaR::partimat(form, data = model_obj$data, method = "lda")
        }, error = function(e) {
          show_placeholder(paste("partimat error:", e$message))
        })
      } else {
        show_placeholder("Please install 'klaR' package to view Partition Plots.")
      }
      
    } else if (plot_name == "LD Scatter/Density") {
      if(isTRUE(model_obj$has_ld) && !is.null(model_obj$ld_scores)) {
        df_plot <- data.frame(Class = model_obj$data[[model_obj$target_var]])
        df_plot <- cbind(df_plot, model_obj$ld_scores)
        if(ncol(model_obj$ld_scores) == 1) {
          p <- ggplot(df_plot, aes(x = LD1, fill = Class)) + geom_density(alpha = 0.5) + theme_minimal(base_size = 14) + labs(title = paste("Score Density (LD1) -", model_obj$method_name))
        } else {
          p <- ggplot(df_plot, aes(x = LD1, y = LD2, color = Class)) + geom_point(size = 3, alpha = 0.8) + stat_ellipse() + theme_minimal(base_size = 14) + labs(title = paste("Scatter (LD1 vs LD2) -", model_obj$method_name))
        }
        print(p)
      } else {
        # PCA fallback for non-LDA methods
        num_preds <- model_obj$predictors[sapply(model_obj$data[, model_obj$predictors, drop=FALSE], is.numeric)]
        if(length(num_preds) < 2) { show_placeholder("Need at least 2 numeric predictors for PCA projection."); return() }
        pca_data <- model_obj$data[, num_preds, drop=FALSE]
        pca_data <- scale(pca_data)
        pca_res <- prcomp(pca_data, center = FALSE, scale. = FALSE)
        df_plot <- data.frame(
          PC1 = pca_res$x[, 1],
          PC2 = pca_res$x[, min(2, ncol(pca_res$x))],
          Predicted = as.factor(model_obj$pred_class),
          Actual = model_obj$data[[model_obj$target_var]]
        )
        p <- ggplot(df_plot, aes(x = PC1, y = PC2, color = Predicted, shape = Actual)) + 
          geom_point(size = 3, alpha = 0.8) + 
          stat_ellipse(aes(group = Predicted), linetype = "dashed") +
          theme_minimal(base_size = 14) + 
          labs(title = paste("PCA Projection -", model_obj$method_name), x = "PC1", y = "PC2")
        print(p)
      }
      
    } else if (plot_name == "Variable Importance") {
      if(isTRUE(model_obj$has_importance) && model_obj$method_name == "Random Forest") {
        old_par <- par(mar = c(4, 8, 3, 2))
        on.exit(par(old_par))
        randomForest::varImpPlot(model_obj$model, main = "Variable Importance (Random Forest)")
      } else if(isTRUE(model_obj$has_ld)) {
        # LDA scaling coefficients
        scaling <- model_obj$model$scaling
        df_imp <- data.frame(Variable = rownames(scaling), Importance = abs(scaling[, 1]))
        df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]
        p <- ggplot(df_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() + theme_minimal(base_size = 14) +
          labs(title = paste("Variable Importance -", model_obj$method_name), x = "", y = "|LD1 Loading|")
        print(p)
      } else {
        show_placeholder(paste("Variable importance not available for", model_obj$method_name))
      }
    }
  }
  
  output$da_lda_formula_display <- renderText({ da_lda_formula_str() })
  
  output$da_lda_summary <- renderPrint({
    res <- da_lda_model_obj()
    if (is.character(res)) {
      cat(res) 
    } else { 
      cat("Method:", res$method_name, "\n\n")
      tryCatch({
        if(res$method_name %in% c("LDA", "Weighted LDA")) {
          print(res$model$call); cat("\n"); print(res$model)
        } else if(res$method_name == "QDA") {
          print(res$model$call); cat("\n"); print(res$model)
        } else if(res$method_name == "Random Forest") {
          print(res$model)
        } else if(res$method_name == "Neural Network") {
          print(summary(res$model))
        } else {
          print(res$model)
        }
      }, error = function(e) cat("Summary not available:", e$message))
    }
  })
  
  output$da_lda_matrix <- renderPrint({
    res <- da_lda_model_obj()
    if (is.character(res)) return(cat("Awaiting valid model..."))
    table(Predicted = res$pred_class, Actual = res$data[[input$da_category]])
  })
  
  output$da_lda_accuracy <- renderText({
    res <- da_lda_model_obj()
    if (is.character(res)) return("")
    acc <- mean(as.character(res$pred_class) == as.character(res$data[[input$da_category]])) * 100
    paste(res$method_name, "Accuracy:", round(acc, 2), "%")
  })
  
  output$da_lda_plot_pairs    <- renderPlot({ plot_lda_single(da_lda_model_obj(), "Pairs Plot") })
  output$da_lda_plot_hist     <- renderPlot({ plot_lda_single(da_lda_model_obj(), "Stacked Histogram") })
  output$da_lda_plot_biplot   <- renderPlot({ plot_lda_single(da_lda_model_obj(), "Biplot (ggord)") })
  output$da_lda_plot_partimat <- renderPlot({ plot_lda_single(da_lda_model_obj(), "Partition Plot (partimat)") })
  output$da_lda_plot_scatter  <- renderPlot({ plot_lda_single(da_lda_model_obj(), "LD Scatter/Density") })
  output$da_lda_plot_importance <- renderPlot({ plot_lda_single(da_lda_model_obj(), "Variable Importance") })
  
  output$da_lda_single_selector <- renderUI({
    req(input$da_lda_selected_plots)
    selectInput("da_lda_single_plot_choice", "Select Plot to View:", choices = input$da_lda_selected_plots, width = "200px")
  })
  
  output$da_lda_single_plot   <- renderPlot({ req(input$da_lda_single_plot_choice); plot_lda_single(da_lda_model_obj(), input$da_lda_single_plot_choice) })
  
  output$dynamic_da_lda_plot_ui <- renderUI({
    req(input$da_lda_view_mode)
    
    if(input$da_lda_view_mode == "Single Plot") {
      plotOutput("da_lda_single_plot", height = "500px")
    } else {
      sel <- input$da_lda_selected_plots
      if(length(sel) == 0) return(markdown("*Please select plots from the sidebar.*"))
      
      ui_list <- list()
      if("Pairs Plot" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_pairs", height="450px")))
      if("Stacked Histogram" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_hist", height="450px")))
      if("Biplot (ggord)" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_biplot", height="450px")))
      if("Partition Plot (partimat)" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_partimat", height="450px")))
      if("LD Scatter/Density" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_scatter", height="450px")))
      if("Variable Importance" %in% sel) ui_list <- append(ui_list, list(plotOutput("da_lda_plot_importance", height="450px")))
      
      col_count <- ifelse(length(sel) == 1, 12, 6)
      do.call(layout_columns, c(ui_list, list(col_widths = col_count)))
    }
  })
  
  output$download_da_lda_plot <- downloadHandler(
    filename = function() { paste0("da_diagnostic_", Sys.Date(), ".png") },
    content = function(file) {
      if(input$da_lda_view_mode == "Single Plot") {
        png(file, width = 800, height = 600)
        plot_lda_single(da_lda_model_obj(), input$da_lda_single_plot_choice)
        dev.off()
      } else {
        png(file, width = 600, height = 400)
        show_placeholder("Please switch to 'Single Plot' mode to download.")
        dev.off()
      }
    }
  )
  
  output$dynamic_da_content <- renderUI({
    mode <- input$da_main_mode
    if (!isTruthy(mode)) return(div(style="padding:20px;", h4("Loading analysis modules...", class="text-muted")))
    
    if(mode == "1. Assumption Checks") {
      view <- input$da_view
      if (!isTruthy(view)) return(div(style="padding:20px;", h4("Loading diagnostic views...", class="text-muted")))
      
      make_header <- function(title) {
        card_header(class = "d-flex justify-content-between align-items-center bg-light", 
                    title, 
                    downloadButton("download_da_assumption_plot", "Download Plot", class = "btn-sm btn-outline-success")
        )
      }
      
      if (view == "1. Covariance Ellipses") {
        card(make_header("Covariance Ellipses"), plotOutput("plot_ellipses", height = "500px"))
      } else if (view == "2. Equal Variance (Boxplots)") {
        card(make_header("Equal Variance Check"), plotOutput("plot_box", height = "500px"))
      } else if (view == "3. Normality (Q-Q Plots)") {
        card(make_header("Multivariate Normality (Q-Q)"), plotOutput("plot_qq", height = "500px"))
      } else if (view == "4. Distribution Density") {
        card(make_header("Density Overlap"), plotOutput("plot_density", height = "500px"))
      } else if (view == "5. Statistical Tests") {
        card(card_header(class = "bg-dark text-white", "Statistical Assumption Checks"), div(style = "padding: 15px; background-color: #f8f9fa; height: 400px; overflow-y: auto;", verbatimTextOutput("stat_test_results")))
      }
      
    } else {
      div(
        card(
          card_header(class = "d-flex justify-content-between align-items-center bg-light", "Discriminant Diagnostics", 
                      div(class = "d-flex align-items-center gap-2 header-controls", 
                          radioGroupButtons("da_lda_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), 
                          uiOutput("da_lda_single_selector"), 
                          downloadButton("download_da_lda_plot", "Download Plot", class = "btn-sm btn-outline-success"))
          ),
          div(style = "overflow-y: auto; height: 520px; padding: 5px;", 
              uiOutput("dynamic_da_lda_plot_ui"))
        ),
        layout_columns(
          col_widths = c(6, 6), 
          card(
            card_header(class="bg-light", "Model Summary"), 
            div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", textOutput("da_lda_formula_display")), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("da_lda_summary"))
          ), 
          card(
            card_header(class="bg-light", "Confusion Matrix & Accuracy"), 
            div(style = "overflow-y: auto; height: 345px; padding: 5px;", 
                verbatimTextOutput("da_lda_matrix"), hr(), tags$b(textOutput("da_lda_accuracy")))
          )
        )
      )
    }
  })
  
  # ---- STAGE 7: CLUSTERING ANALYSIS ----
  
  observe({
    req(input$clust_dataset)
    if(input$clust_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$clust_dataset]]
    req(df)
    all_cols <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    updateSelectInput(session, "clust_vars", choices = all_cols, selected = num_cols)
    
    curr_x <- if(isTruthy(isolate(input$clust_scatter_x)) && isolate(input$clust_scatter_x) %in% num_cols) isolate(input$clust_scatter_x) else if(length(num_cols)>0) num_cols[1] else NULL
    curr_y <- if(isTruthy(isolate(input$clust_scatter_y)) && isolate(input$clust_scatter_y) %in% num_cols) isolate(input$clust_scatter_y) else if(length(num_cols)>1) num_cols[2] else curr_x
    
    updateSelectInput(session, "clust_scatter_x", choices = num_cols, selected = curr_x)
    updateSelectInput(session, "clust_scatter_y", choices = num_cols, selected = curr_y)
  })
  
  # Reactive to track if clustering data is mixed (contains categorical)
  clust_is_mixed <- reactive({
    if(!isTruthy(input$clust_dataset) || input$clust_dataset == "Awaiting Data Upload...") return(FALSE)
    if(length(input$clust_vars) < 2) return(FALSE)
    df_raw <- dataset_pool[[input$clust_dataset]]
    if(is.null(df_raw)) return(FALSE)
    df <- df_raw[, input$clust_vars, drop = FALSE]
    any(!sapply(df, is.numeric))
  })
  
  clust_prepared_data <- reactive({
    if(!isTruthy(input$clust_dataset) || input$clust_dataset == "Awaiting Data Upload...") return(NULL)
    if(length(input$clust_vars) < 2) return(NULL) 
    
    df_raw <- dataset_pool[[input$clust_dataset]]
    if(is.null(df_raw)) return(NULL)
    
    df <- df_raw[, input$clust_vars, drop = FALSE]
    df <- df[complete.cases(df), , drop=FALSE] 
    
    if (nrow(df) < 3) return(NULL)
    
    has_cat <- any(!sapply(df, is.numeric))
    
    if (has_cat) {
      # Convert character columns to factor for daisy
      for(col in names(df)) {
        if(is.character(df[[col]])) df[[col]] <- as.factor(df[[col]])
      }
      # Scale numeric columns if requested
      if (input$clust_scale_data) {
        for(col in names(df)) {
          if(is.numeric(df[[col]])) df[[col]] <- as.numeric(scale(df[[col]]))
        }
      }
      return(df)
    } else {
      if (input$clust_scale_data) df <- scale(df)
      return(as.data.frame(df))
    }
  })
  
  get_clusters <- reactive({
    req(input$clust_k)
    df <- clust_prepared_data()
    req(df)
    
    is_mixed <- clust_is_mixed()
    
    if (is_mixed) {
      # Use Gower distance + PAM for mixed data
      d <- cluster::daisy(df, metric = "gower")
      
      if (input$clust_method == "K-Means") {
        # PAM is the appropriate analog of K-Means for Gower distance
        pam_result <- cluster::pam(d, k = input$clust_k)
        return(list(obj = pam_result, vector = pam_result$clustering, dist = d, is_pam = TRUE))
      } else {
        hc <- hclust(d, method = input$hclust_link)
        clusters <- cutree(hc, k = input$clust_k)
        return(list(obj = hc, vector = clusters, dist = d, is_pam = FALSE))
      }
    } else {
      if (input$clust_method == "K-Means") {
        set.seed(123) 
        km <- kmeans(df, centers = input$clust_k, nstart = 25)
        return(list(obj = km, vector = km$cluster, is_pam = FALSE))
      } else {
        withProgress(message = 'Processing Hierarchical Clustering...', value = 0, {
          incProgress(0.2, detail = "Calculating distance matrix (this may take a moment)...")
          d <- get_dist(df, method = input$hclust_dist)
        
          incProgress(0.5, detail = "Building linkage tree...")
          hc <- hclust(d, method = input$hclust_link)
        
          incProgress(0.2, detail = "Cutting tree into clusters...")
          clusters <- cutree(hc, k = input$clust_k)
        
          return(list(obj = hc, vector = clusters, is_pam = FALSE))
        })
      }
    }
  })
  
  output$clust_explanation <- renderUI({
    req(input$clust_view)
    if(input$clust_view == "1. Optimal k (Elbow Method)") {
      markdown("*The **Elbow Method** calculates total within-cluster sum of squares (WSS). Look for the 'bend' or 'elbow' in the plot to find the optimal number of clusters.*")
    } else if(input$clust_view == "2. Optimal k (Silhouette Method)") {
      markdown("*The **Silhouette Method** measures cluster quality. Look for the highest peak to identify the optimal $k$.*")
    } else if(input$clust_view == "3. Cluster Map (PCA/Dendrogram)") {
      markdown("*Visualizes the high-dimensional clusters. **K-Means** uses PCA to project the groups onto a 2D plane. **Hierarchical** maps the exact lineage of the merges.*")
    } else if(input$clust_view == "4. Custom Scatter Plot") {
      markdown("*Maps the calculated cluster assignments onto the raw, unscaled data so you can interpret actual real-world boundaries.*")
    } else if(input$clust_view == "5. Silhouette Profile") {
      markdown("*Evaluates how well each individual point fits inside its assigned cluster. Values near 1 are excellent. Negative values suggest the point may belong in a neighboring cluster.*")
    } else if(input$clust_view == "6. Phylogenetic Tree (ape)") {
      markdown("*Visualizes the hierarchical clustering as an unrooted circular tree (phylogram). This requires the `ape` package.*")
    }
  })
  
  main_clust_plot_fn <- function() {
    req(input$clust_view)
    
    if(!isTruthy(input$clust_dataset) || input$clust_dataset == "Awaiting Data Upload...") {
      return(show_placeholder("Awaiting dataset..."))
    }
    
    if(length(input$clust_vars) < 2) {
      return(show_placeholder("Please select at least 2 variables to cluster."))
    }
    
    df <- clust_prepared_data()
    if (is.null(df)) {
      return(show_placeholder("Awaiting valid data..."))
    }
    
    is_mixed <- clust_is_mixed()
    
    tryCatch({
      if (input$clust_view == "1. Optimal k (Elbow Method)") {
        withProgress(message = 'Calculating Elbow Method...', detail = 'Running multiple cluster models...', value = 0.5, {
          if (is_mixed) {
            d <- cluster::daisy(df, metric = "gower")
            p <- fviz_nbclust(d, FUNcluster = cluster::pam, method = "wss") + 
              theme_minimal(base_size = 14) + labs(title = "Elbow Method (Gower + PAM)")
          } else if (input$clust_method == "K-Means") {
            p <- fviz_nbclust(df, FUNcluster = kmeans, method = "wss") + 
              theme_minimal(base_size = 14) + labs(title = "Elbow Method for Optimal k")
          } else {
            p <- fviz_nbclust(df, FUNcluster = factoextra::hcut, method = "wss", 
                              hc_method = input$hclust_link, hc_metric = input$hclust_dist) + 
              theme_minimal(base_size = 14) + labs(title = "Elbow Method for Optimal k (Hierarchical)")
          }
          print(p)
        })
        
      } else if (input$clust_view == "2. Optimal k (Silhouette Method)") {
        withProgress(message = 'Calculating Silhouette Method...', detail = 'Evaluating cluster quality...', value = 0.5, {
          if (is_mixed) {
            d <- cluster::daisy(df, metric = "gower")
            p <- fviz_nbclust(d, FUNcluster = cluster::pam, method = "silhouette") + 
              theme_minimal(base_size = 14) + labs(title = "Silhouette Method (Gower + PAM)")
          } else if (input$clust_method == "K-Means") {
            p <- fviz_nbclust(df, FUNcluster = kmeans, method = "silhouette") + 
              theme_minimal(base_size = 14) + labs(title = "Silhouette Method for Optimal k")
          } else {
            p <- fviz_nbclust(df, FUNcluster = factoextra::hcut, method = "silhouette", 
                              hc_method = input$hclust_link, hc_metric = input$hclust_dist) + 
              theme_minimal(base_size = 14) + labs(title = "Silhouette Method for Optimal k (Hierarchical)")
          }
          print(p)
        })
        
      } else if (input$clust_view == "3. Cluster Map (PCA/Dendrogram)") {
        c_data <- get_clusters()
        if (isTRUE(c_data$is_pam)) {
          # PAM with Gower: use fviz_cluster which handles PAM objects
          p <- fviz_cluster(c_data$obj,
                            ellipse.type = "convex", geom = "point",
                            ggtheme = theme_minimal(base_size = 14),
                            main = paste("PAM Cluster Map (k =", input$clust_k, ") — Gower Distance"))
          print(p)
        } else if (input$clust_method == "K-Means") {
          p <- fviz_cluster(c_data$obj, data = df, 
                            ellipse.type = "convex", geom = "point",
                            repel = TRUE, ggtheme = theme_minimal(base_size = 14),
                            main = paste("K-Means Map (k =", input$clust_k, ")"))
          print(p)
        } else {
          p <- fviz_dend(c_data$obj, k = input$clust_k, 
                         cex = 0.9, lwd = 0.4, 
                         rect = TRUE, rect_fill = TRUE, rect_border = "jco",
                         color_labels_by_k = TRUE,
                         ggtheme = theme_classic(base_size = 14),
                         main = "Hierarchical Dendrogram")
          print(p)
        }
        
      } else if (input$clust_view == "4. Custom Scatter Plot") {
        req(input$clust_scatter_x, input$clust_scatter_y)
        raw_full <- dataset_pool[[input$clust_dataset]]
        raw_df <- raw_full[complete.cases(raw_full[, input$clust_vars]), input$clust_vars, drop = FALSE]
        c_data <- get_clusters()
        raw_df$Cluster <- as.factor(c_data$vector)
        
        p <- ggplot(raw_df, aes_string(x = input$clust_scatter_x, y = input$clust_scatter_y, col = "Cluster")) +
          geom_point(size = 3, alpha = 0.8) +
          theme_minimal(base_size = 14) +
          labs(title = paste("Clusters mapped to", input$clust_scatter_x, "vs", input$clust_scatter_y))
        print(p)
        
      } else if (input$clust_view == "5. Silhouette Profile") {
        c_data <- get_clusters()
        if (is_mixed) {
          dist_matrix <- cluster::daisy(df, metric = "gower")
        } else {
          dist_matrix <- if(input$clust_method == "Hierarchical") get_dist(df, method = input$hclust_dist) else get_dist(df, method = "euclidean")
        }
        sil <- silhouette(c_data$vector, dist_matrix)
        
        p <- fviz_silhouette(sil, print.summary = FALSE, ggtheme = theme_minimal(base_size = 14)) +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
        print(p)
        
      } else if (input$clust_view == "6. Phylogenetic Tree (ape)") {
        if (input$clust_method == "K-Means") {
          show_placeholder("Phylogenetic trees are only available for Hierarchical Clustering.")
        } else {
          if(requireNamespace("ape", quietly = TRUE)) {
            c_data <- get_clusters()
            phy <- ape::as.phylo(c_data$obj)
            
            colors <- if(input$clust_k <= 8) palette()[1:input$clust_k] else rainbow(input$clust_k)
            tip_colors <- colors[c_data$vector]
            
            old_par <- par(mar = c(1, 1, 3, 1))
            on.exit(par(old_par))
            
            plot(phy, type = "fan", tip.color = tip_colors, 
                 cex = 0.8, font = 2, no.margin = TRUE, 
                 main = paste("Circular Phylogenetic Tree (k =", input$clust_k, ")"))
          } else {
            show_placeholder("Please install 'ape' package to view Phylogenetic Trees.\nRun: install.packages('ape')")
          }
        }
      }
    }, error = function(e) {
      show_placeholder(paste("Plot Error:", e$message))
    })
  }
  
  output$main_clust_plot <- renderPlot({ main_clust_plot_fn() })
  
  output$download_clust_plot <- downloadHandler(
    filename = function() { paste0("clustering_", Sys.Date(), ".png") },
    content = function(file) {
      png(file, width = 800, height = 600)
      main_clust_plot_fn()
      dev.off()
    }
  )
  
  output$cluster_summary <- renderPrint({
    if(!isTruthy(input$clust_dataset) || input$clust_dataset == "Awaiting Data Upload...") return(cat("Awaiting dataset..."))
    if(length(input$clust_vars) < 2) return(cat("Please select at least 2 numeric variables."))
    
    df <- dataset_pool[[input$clust_dataset]]
    df_raw <- df[complete.cases(df[, input$clust_vars]), input$clust_vars, drop = FALSE]
    c_data <- get_clusters()
    req(c_data)
    
    cat("Cluster Profiles (Raw Data):\n\n")
    
    num_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
    cat_cols <- names(df_raw)[!sapply(df_raw, is.numeric)]
    
    if(length(num_cols) > 0) {
      cat("--- Numeric Variables (Mean by Cluster) ---\n")
      print(aggregate(df_raw[, num_cols, drop=FALSE], by = list(Cluster = c_data$vector), FUN = mean))
    }
    if(length(cat_cols) > 0) {
      cat("\n--- Categorical Variables (Mode by Cluster) ---\n")
      get_mode <- function(x) { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] }
      for(col in cat_cols) {
        cat("\n", col, ":\n")
        print(tapply(df_raw[[col]], c_data$vector, get_mode))
      }
    }
  })
  
  output$cluster_assignments <- renderPrint({
    if(!isTruthy(input$clust_dataset) || input$clust_dataset == "Awaiting Data Upload...") return(cat("Awaiting dataset..."))
    if(length(input$clust_vars) < 2) return(cat("Please select at least 2 variables."))
    
    c_data <- get_clusters()
    req(c_data)
    res <- data.frame(Cluster = c_data$vector)
    print(res)
  })
  
  # ---- STAGE 8: CLASSIFICATION (ONE-VS-ALL) ----
  
  observe({
    req(input$clf_dataset)
    if(input$clf_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$clf_dataset]]
    req(df)
    cat_cols <- names(df)[sapply(df, is_safe_cat)]
    all_cols <- names(df)
    
    curr_y <- if(isTruthy(isolate(input$clf_target)) && isolate(input$clf_target) %in% cat_cols) isolate(input$clf_target) else if(length(cat_cols)>0) cat_cols[1] else NULL
    curr_build <- if(isTruthy(isolate(input$clf_build_var)) && isolate(input$clf_build_var) %in% all_cols) isolate(input$clf_build_var) else all_cols[1]
    
    updateSelectInput(session, "clf_target", choices = cat_cols, selected = curr_y)
    updateSelectInput(session, "clf_build_var", choices = all_cols, selected = curr_build)
  })
  
  # Update class exclusion when target changes
  observeEvent(input$clf_target, {
    req(input$clf_dataset, input$clf_target)
    if(input$clf_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$clf_dataset]]
    req(df)
    if(input$clf_target %in% names(df)) {
      classes <- unique(as.character(na.omit(df[[input$clf_target]])))
      updatePickerInput(session, "clf_exclude_classes", choices = classes, selected = character(0))
    }
  })
  
  # Formula builder
  observeEvent(input$clf_btn_add_var, {
    var <- paste0("`", input$clf_build_var, "`")
    trans <- input$clf_build_trans
    term <- switch(trans, "raw" = var, "log" = paste0("log(", var, ")"), "sqrt" = paste0("sqrt(", var, ")"), "poly" = paste0("I(", var, "^2)"))
    current <- trimws(input$clf_formula_text)
    new_text <- if (nchar(current) > 0) paste(current, term) else term
    updateTextAreaInput(session, "clf_formula_text", value = new_text)
  })
  observeEvent(input$clf_btn_add_plus, {
    current <- trimws(input$clf_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "clf_formula_text", value = paste(current, "+ "))
  })
  observeEvent(input$clf_btn_add_star, {
    current <- trimws(input$clf_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "clf_formula_text", value = paste(current, "* "))
  })
  observeEvent(input$clf_btn_clear, { updateTextAreaInput(session, "clf_formula_text", value = "") })
  
  clf_formula_str <- reactive({
    x_side <- trimws(input$clf_formula_text)
    if (nchar(x_side) == 0) return("target ~ ...")
    paste("target ~", x_side)
  })
  
  output$clf_formula_display <- renderText({
    if (!isTruthy(input$clf_target)) return("Awaiting target variable...")
    x_side <- trimws(input$clf_formula_text)
    if (nchar(x_side) == 0) return(paste(input$clf_target, "~ ..."))
    paste(input$clf_target, "~", x_side)
  })
  
  # Classification reactive - triggered by Run button
  clf_results <- reactiveVal(NULL)
  clf_confusion <- reactiveVal(NULL)
  
  observeEvent(input$clf_run, {
    req(input$clf_dataset, input$clf_target)
    if(input$clf_dataset == "Awaiting Data Upload...") {
      showNotification("Please upload a dataset first.", type = "warning")
      return()
    }
    
    df <- dataset_pool[[input$clf_dataset]]
    form_str_template <- clf_formula_str()
    
    if (grepl("\\.\\.\\.", form_str_template)) {
      showNotification("Please build a formula with predictor variables first.", type = "warning")
      return()
    }
    
    threshold <- input$clf_threshold
    exclude <- input$clf_exclude_classes
    
    # Filter data
    data_filtered <- df
    if(length(exclude) > 0) {
      data_filtered <- data_filtered[!data_filtered[[input$clf_target]] %in% exclude, , drop = FALSE]
      data_filtered[[input$clf_target]] <- droplevels(as.factor(data_filtered[[input$clf_target]]))
    }
    
    # Get classes
    classes <- unique(as.character(na.omit(data_filtered[[input$clf_target]])))
    
    if(length(classes) < 2) {
      showNotification("Need at least 2 classes after exclusions.", type = "error")
      return()
    }
    
    # Get all variables from formula
    all_pred_vars <- tryCatch({
      all.vars(as.formula(form_str_template))[-1]  # remove "target"
    }, error = function(e) {
      showNotification(paste("Formula error:", e$message), type = "error")
      return(NULL)
    })
    if(is.null(all_pred_vars)) return()
    
    # Check all vars exist
    needed_cols <- c(input$clf_target, all_pred_vars)
    missing <- setdiff(needed_cols, names(data_filtered))
    if(length(missing) > 0) {
      showNotification(paste("Variables not found:", paste(missing, collapse=", ")), type = "error")
      return()
    }
    
    # Clean data
    clean_df <- data_filtered[, needed_cols, drop = FALSE]
    clean_df <- clean_df[complete.cases(clean_df), , drop = FALSE]
    
    if(nrow(clean_df) < 10) {
      showNotification("Insufficient complete cases (< 10).", type = "error")
      return()
    }
    
    withProgress(message = 'Running Classification...', value = 0, {
      results_list <- list()
      confusion_list <- list()
      n_classes <- length(classes)
      
      for(i in seq_along(classes)) {
        cl <- classes[i]
        incProgress(1/n_classes, detail = paste("Processing class:", cl))
        
        tryCatch({
          clean_df$target <- ifelse(as.character(clean_df[[input$clf_target]]) == cl, 1, 0)
          
          model <- glm(as.formula(form_str_template), data = clean_df, family = binomial)
          
          probs <- predict(model, type = "response")
          preds <- ifelse(probs > threshold, 1, 0)
          
          TP <- sum(preds == 1 & clean_df$target == 1)
          TN <- sum(preds == 0 & clean_df$target == 0)
          FP <- sum(preds == 1 & clean_df$target == 0)
          FN <- sum(preds == 0 & clean_df$target == 1)
          
          accuracy  <- (TP + TN) / (TP + TN + FP + FN)
          precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
          recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
          
          f1 <- ifelse(
            is.na(precision) | is.na(recall) | (precision + recall) == 0,
            NA,
            2 * (precision * recall) / (precision + recall)
          )
          
          results_list[[cl]] <- data.frame(
            Class = cl,
            N = sum(clean_df$target == 1),
            Accuracy = round(accuracy, 4),
            Precision = round(precision, 4),
            Recall = round(recall, 4),
            F1 = round(f1, 4),
            stringsAsFactors = FALSE
          )
          
          confusion_list[[cl]] <- data.frame(
            Class = cl, TP = TP, TN = TN, FP = FP, FN = FN,
            stringsAsFactors = FALSE
          )
          
        }, error = function(e) {
          results_list[[cl]] <<- data.frame(
            Class = cl, N = NA, Accuracy = NA, Precision = NA, Recall = NA, F1 = NA,
            stringsAsFactors = FALSE
          )
          confusion_list[[cl]] <<- data.frame(
            Class = cl, TP = NA, TN = NA, FP = NA, FN = NA,
            stringsAsFactors = FALSE
          )
        })
      }
      
      clean_df$target <- NULL  # cleanup
      
      clf_results(do.call(rbind, results_list))
      clf_confusion(do.call(rbind, confusion_list))
    })
    
    showNotification(paste("Classification complete!", length(classes), "classes evaluated."), type = "message")
  })
  
  output$clf_f1_plot <- renderPlot({
    res <- clf_results()
    if(is.null(res)) {
      show_placeholder("Click 'Run Classification' to begin analysis.")
      return()
    }
    
    p <- ggplot(res, aes(x = reorder(Class, -F1), y = F1)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
      geom_text(aes(label = ifelse(is.na(F1), "NA", sprintf("%.3f", F1))), 
                vjust = -0.5, size = 4, fontface = "bold") +
      ylim(0, min(1.15, max(res$F1, na.rm = TRUE) * 1.2)) +
      theme_minimal(base_size = 14) +
      labs(title = "One-vs-All Classification: F1 Score by Class", x = "Class", y = "F1 Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
    print(p)
  })
  
  output$clf_metrics_table <- renderPrint({
    res <- clf_results()
    if(is.null(res)) return(cat("Awaiting classification results...\nBuild a formula and click 'Run Classification'."))
    
    cat("=== One-vs-All Classification Metrics ===\n")
    cat("Threshold:", isolate(input$clf_threshold), "\n\n")
    print(res, row.names = FALSE)
  })
  
  output$clf_confusion_details <- renderPrint({
    conf <- clf_confusion()
    if(is.null(conf)) return(cat("Awaiting classification results..."))
    
    cat("=== Confusion Matrix Components (Per Class) ===\n\n")
    for(i in 1:nrow(conf)) {
      cat("--- Class:", conf$Class[i], "---\n")
      cat("  True Positives  (TP):", conf$TP[i], "\n")
      cat("  True Negatives  (TN):", conf$TN[i], "\n")
      cat("  False Positives (FP):", conf$FP[i], "\n")
      cat("  False Negatives (FN):", conf$FN[i], "\n\n")
    }
  })
  
  output$download_clf_plot <- downloadHandler(
    filename = function() { paste0("classification_f1_", Sys.Date(), ".png") },
    content = function(file) {
      res <- clf_results()
      if(is.null(res)) return()
      png(file, width = 900, height = 600)
      p <- ggplot(res, aes(x = reorder(Class, -F1), y = F1)) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
        geom_text(aes(label = ifelse(is.na(F1), "NA", sprintf("%.3f", F1))), vjust = -0.5, size = 4) +
        ylim(0, 1.1) + theme_minimal(base_size = 14) +
        labs(title = "One-vs-All Classification: F1 Score by Class", x = "Class", y = "F1 Score") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
      dev.off()
    }
  )
  
  # =========================================================
  # STAGE 3.5: LINEAR MIXED EFFECTS (LME)
  # =========================================================
  observeEvent(input$lme_dataset, {
    req(input$lme_dataset)
    if(input$lme_dataset == "Awaiting Data Upload...") return()
    df <- dataset_pool[[input$lme_dataset]]
    cols <- names(df)
    updateSelectInput(session, "lme_y", choices = cols)
    updateSelectInput(session, "lme_build_var", choices = cols)
  })
  
  observeEvent(input$lme_btn_add_var, {
    var <- input$lme_build_var
    trans <- input$lme_build_trans
    
    term <- switch(trans,
                   "raw" = var,
                   "log" = paste0("log(", var, ")"),
                   "sqrt" = paste0("sqrt(", var, ")"),
                   "poly" = paste0("I(", var, "^2)"))
    
    curr <- input$lme_fixed_text
    new_text <- if(curr == "") term else paste(curr, "+", term)
    updateTextAreaInput(session, "lme_fixed_text", value = new_text)
  })
  observeEvent(input$lme_btn_add_plus, { updateTextAreaInput(session, "lme_fixed_text", value = paste(input$lme_fixed_text, "+ ")) })
  observeEvent(input$lme_btn_add_star, { updateTextAreaInput(session, "lme_fixed_text", value = paste(input$lme_fixed_text, "* ")) })
  observeEvent(input$lme_btn_clear, { updateTextAreaInput(session, "lme_fixed_text", value = "") })
  
  output$lme_formula_display <- renderText({
    if (input$lme_fixed_text == "" && input$lme_random_text == "") return("Awaiting formula...")
    paste(input$lme_y, "~", input$lme_fixed_text, "\nRandom:", input$lme_random_text)
  })
  
  lme_model_obj <- reactiveVal(NULL)
  
  observeEvent(input$run_lme, {
    req(input$lme_dataset, input$lme_y, input$lme_fixed_text, input$lme_random_text)
    df <- dataset_pool[[input$lme_dataset]]
    
    fixed_form_str <- paste(input$lme_y, "~", input$lme_fixed_text)
    random_form_str <- input$lme_random_text
    
    withProgress(message = 'Fitting LME Model...', value = 0.5, {
      tryCatch({
        fixed_form <- as.formula(fixed_form_str)
        random_form <- as.formula(random_form_str)
        
        fit <- nlme::lme(fixed = fixed_form, random = random_form, data = df, na.action = na.omit)
        
        r2 <- tryCatch(MuMIn::r.squaredGLMM(fit), error = function(e) matrix(NA, ncol=2, dimnames=list(NULL, c("R2m", "R2c"))))
        
        lme_model_obj(list(model = fit, data = df, target = input$lme_y, r2 = r2))
        showNotification("LME Model fitted successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error fitting LME:", e$message), type = "error")
      })
    })
  })
  
  output$lme_summary <- renderPrint({
    obj <- lme_model_obj()
    if(is.null(obj)) return(cat("Awaiting model training..."))
    summary(obj$model)
  })
  
  output$lme_performance <- renderPrint({
    obj <- lme_model_obj()
    if(is.null(obj)) return(cat("Awaiting model training..."))
    
    cat("=== Nakagawa R-squared (GLMM) ===\n")
    cat("Marginal R2 (Fixed effects only):  ", round(obj$r2[1, "R2m"], 4), "\n")
    cat("Conditional R2 (Fixed + Random):   ", round(obj$r2[1, "R2c"], 4), "\n\n")
    
    cat("=== Variance Inflation Factors (VIF) ===\n")
    tryCatch({
      v <- diag(vcov(obj$model))
      cor_mat <- cov2cor(vcov(obj$model))
      vifs <- diag(solve(cor_mat))
      print(vifs)
    }, error = function(e) { cat("VIF not available for this model structure.") })
  })
  
  output$lme_diagnostics_plot <- renderPlot({
    obj <- lme_model_obj()
    if(is.null(obj)) return()
    
    old_par <- par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
    on.exit(par(old_par))
    
    fit_vals <- fitted(obj$model)
    res_vals <- resid(obj$model, type="pearson")
    
    plot(fit_vals, res_vals, main="Residuals vs Fitted", xlab="Fitted values", ylab="Standardized Residuals", pch=16, col=rgb(0.2,0.5,0.8,0.5), cex.lab=1.2)
    abline(h=0, col="red", lwd=2, lty=2)
    
    qqnorm(res_vals, main="Normal Q-Q Plot", pch=16, col=rgb(0.3,0.3,0.3,0.5), cex.lab=1.2)
    qqline(res_vals, col="red", lwd=2)
  })

  # =========================================================
  # STAGE 5.5: RANDOM FOREST
  # =========================================================
  observeEvent(rv$working_data, {
    req(rv$working_data)
    df <- rv$working_data
    cols <- names(df)
    updateSelectInput(session, "rf_target", choices = cols)
    updatePickerInput(session, "rf_predictors", choices = cols, selected = NULL)
    updateSelectInput(session, "rf_pdp_var", choices = cols)
  })
  
  rf_model_obj <- reactiveVal(NULL)
  
  observeEvent(input$run_rf, {
    req(input$rf_dataset, input$rf_target, input$rf_predictors)
    df <- dataset_pool[[input$rf_dataset]]
    
    valid_cols <- c(input$rf_target, input$rf_predictors)
    df <- df[complete.cases(df[, valid_cols, drop = FALSE]), ]
    
    if (nrow(df) < 5) {
      showNotification("Not enough complete rows to train Random Forest.", type = "error")
      return()
    }
    
    form_str <- paste(input$rf_target, "~", paste(input$rf_predictors, collapse = " + "))
    
    withProgress(message = 'Training Random Forest...', value = 0, {
      tryCatch({
        incProgress(0.5, detail = paste(input$rf_ntree, "trees"))
        
        p <- length(input$rf_predictors)
        default_mtry <- if(is.numeric(df[[input$rf_target]])) max(floor(p/3), 1) else floor(sqrt(p))
        
        rf_fit <- randomForest::randomForest(
          as.formula(form_str), 
          data = df, 
          ntree = input$rf_ntree,
          mtry = default_mtry,
          importance = TRUE
        )
        
        cv_res <- NULL
        if (input$rf_run_cv) {
          incProgress(0.8, detail = "Running 10-fold CV")
          cv_res <- randomForest::rfcv(df[, input$rf_predictors, drop=FALSE], df[[input$rf_target]], cv.fold=10)
        }
        
        rf_model_obj(list(model = rf_fit, data = df, cv = cv_res, target = input$rf_target))
        showNotification("Random Forest trained successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error training RF:", e$message), type = "error")
      })
    })
  })
  
  output$rf_summary <- renderPrint({
    obj <- rf_model_obj()
    if(is.null(obj)) return(cat("Awaiting model training..."))
    print(obj$model)
    if(!is.null(obj$cv)) {
      cat("\n\n--- 10-Fold CV Error by Number of Variables ---\n")
      print(obj$cv$error.cv)
    }
  })
  
  output$rf_varimp <- renderPlot({
    obj <- rf_model_obj()
    if(is.null(obj)) return()
    randomForest::varImpPlot(obj$model, main = paste("Variable Importance for", obj$target))
  })
  
  rf_pdp_plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$run_pdp, {
    req(rf_model_obj(), input$rf_pdp_var)
    obj <- rf_model_obj()
    
    if (!(input$rf_pdp_var %in% rownames(obj$model$importance))) {
      showNotification("Selected variable is not a predictor in the model.", type = "error")
      return()
    }
    
    withProgress(message = 'Generating PDP...', detail = input$rf_pdp_var, value = 0.5, {
      tryCatch({
        p <- pdp::partial(obj$model, pred.var = input$rf_pdp_var, train = obj$data)
        p_plot <- pdp::plotPartial(p, main = paste("Partial Dependence on", input$rf_pdp_var))
        rf_pdp_plot_obj(p_plot)
      }, error = function(e) {
        showNotification(paste("Error generating PDP:", e$message), type = "error")
      })
    })
  })
  
  output$rf_pdp_plot <- renderPlot({
    p <- rf_pdp_plot_obj()
    if(is.null(p)) {
      show_placeholder("Select a predictor and click 'Generate PDP'.")
      return()
    }
    print(p)
  })

  # =========================================================
  # AI CO-PILOT CHAT ENGINE (VISION-ENABLED)
  # =========================================================
  get_active_context <- reactive({
    current_tab <- input$main_tabs
    if (is.null(current_tab)) return("No data context available.")
    
    tryCatch({
      if (current_tab == "Linear Regression") {
        model <- lm_model()
        if (is.character(model)) return("The user has not built a valid Linear Regression model yet.")
        paste(capture.output(summary(model)), collapse = "\n")
      } else if (current_tab == "ANOVA") {
        model <- aov_model()
        if (is.character(model)) return("The user has not built a valid ANOVA model yet.")
        res <- paste(capture.output(summary(model)), collapse = "\n")
        tukey <- tryCatch(paste(capture.output(TukeyHSD(model)), collapse = "\n"), error=function(e) "No Tukey HSD available.")
        paste("ANOVA Results:\n", res, "\n\nTukey Post-Hoc:\n", tukey)
      } else if (current_tab == "Logistic Regression") {
        res <- log_model_obj()
        if (is.character(res)) return("The user has not built a valid Logistic model yet.")
        preds <- predict(res$model)
        conf_mat <- table(Predicted = preds, Actual = res$data[[input$log_y]])
        acc <- mean(preds == res$data[[input$log_y]]) * 100
        paste("Logistic Regression Model Summary:\n",
              paste(capture.output(summary(res$model)), collapse = "\n"),
              "\n\nConfusion Matrix:\n",
              paste(capture.output(conf_mat), collapse = "\n"),
              "\n\nAccuracy: ", round(acc, 2), "%")
      } else if (current_tab == "Discriminant Analysis") {
        if(input$da_main_mode == "1. Assumption Checks") {
          if (input$da_view == "5. Statistical Tests") {
            paste("The user is looking at statistical test results for Discriminant Analysis assumptions. Grouping variable:", input$da_category)
          } else {
            paste("The user is looking at Discriminant Analysis assumption checks: ", input$da_view, " grouped by ", input$da_category)
          }
        } else {
          res <- da_lda_model_obj()
          if (is.character(res)) return("The user has not built a valid Discriminant model yet.")
          conf_mat <- table(Predicted = res$pred_class, Actual = res$data[[input$da_category]])
          acc <- mean(as.character(res$pred_class) == as.character(res$data[[input$da_category]])) * 100
          
          # Handle varying model print outputs gracefully for different methods
          model_summary <- tryCatch(paste(capture.output(if(res$method_name == "Neural Network") summary(res$model) else print(res$model)), collapse = "\n"), error = function(e) "Summary not available.")
          
          paste(res$method_name, "Model Summary:\n",
                model_summary,
                "\n\nConfusion Matrix:\n",
                paste(capture.output(conf_mat), collapse = "\n"),
                "\n\nAccuracy: ", round(acc, 2), "%")
        }
      } else if (current_tab == "Clustering Analysis") {
        if(input$clust_view %in% c("1. Optimal k (Elbow Method)", "2. Optimal k (Silhouette Method)")) {
          "The user is determining the optimal number of clusters using diagnostic plots."
        } else {
          c_data <- get_clusters()
          if(is.null(c_data)) return("Awaiting valid clustering model.")
          sizes <- table(c_data$vector)
          is_pam_msg <- if(isTRUE(c_data$is_pam)) " (PAM with Gower Distance for Mixed Data)" else ""
          paste("Clustering Analysis active. Method:", input$clust_method, is_pam_msg, "with", input$clust_k, "clusters. Cluster sizes:", paste(sizes, collapse=", "))
        }
      } else if (current_tab == "Classification") {
        res <- clf_results()
        if(is.null(res)) return("The user is setting up a One-vs-All Classification model but hasn't run it yet.")
        paste("One-vs-All Classification Model active. Target:", input$clf_target,
              "\nThreshold:", input$clf_threshold,
              "\nExcluded Classes:", paste(input$clf_exclude_classes, collapse = ", "),
              "\n\nMetrics Summary:\n",
              paste(capture.output(print(res, row.names = FALSE)), collapse = "\n"))
      } else if (current_tab == "Data & Exploration") {
        req(rv$working_data)
        paste("The user is looking at the raw dataset structure:\n", paste(capture.output(str(rv$working_data)), collapse = "\n"))
      } else {
        "The user is looking at Exploratory Data Analysis."
      }
    }, error = function(e) { "Error fetching context." })
  })
  
  output$chat_history <- renderUI({
    msgs <- chat_state()
    ui_list <- lapply(msgs, function(msg) {
      if(msg$role == "user") {
        div(class = "chat-user", tags$strong("You: "), msg$content)
      } else {
        div(class = "chat-ai", markdown(msg$content))
      }
    })
    
    ui_list[[length(ui_list) + 1]] <- tags$script(HTML("
      $('.temp-msg').remove(); 
      $('#chat_loading').hide(); 
      $('#chat_input').css('color', ''); 
      setTimeout(function() { 
          let $cont = $('#chat_history_container'); 
          $cont.scrollTop($cont[0].scrollHeight); 
      }, 100);
    "))
    
    do.call(tagList, ui_list)
  })
  
  observeEvent(input$send_chat, {
    user_text <- trimws(input$chat_input)
    req(nchar(user_text) > 0)
    
    history <- chat_state()
    history[[length(history) + 1]] <- list(role = "user", content = user_text)
    chat_state(history)
    updateTextInput(session, "chat_input", value = "")
    
    active_b64 <- NULL
    if (input$main_tabs == "Linear Regression") {
      active_b64 <- capture_plot_as_base64(function() {
        plot_lm_diagnostics(lm_model(), dataset_pool[[input$lm_dataset]], input$lm_y, input$lm_view_mode, input$lm_zoom_target)
      })
    } else if (input$main_tabs == "ANOVA") {
      active_b64 <- capture_plot_as_base64(function() {
        plot_aov_diagnostics(aov_model(), input$aov_view_mode, input$aov_zoom_target)
      })
    } else if (input$main_tabs == "Logistic Regression") {
      active_b64 <- capture_plot_as_base64(function() {
        plot_log_diagnostics(log_model_obj(), input$log_y)
      })
    } else if (input$main_tabs == "Discriminant Analysis") {
      if(input$da_main_mode == "1. Assumption Checks") {
        if (input$da_view == "1. Covariance Ellipses") active_b64 <- capture_plot_as_base64(da_plot_ellipses_fn)
        else if (input$da_view == "2. Equal Variance (Boxplots)") active_b64 <- capture_plot_as_base64(da_plot_box_fn)
        else if (input$da_view == "3. Normality (Q-Q Plots)") active_b64 <- capture_plot_as_base64(da_plot_qq_fn)
        else if (input$da_view == "4. Distribution Density") active_b64 <- capture_plot_as_base64(da_plot_density_fn)
      } else {
        if (input$da_lda_view_mode == "Single Plot" && isTruthy(input$da_lda_single_selector)) {
          active_b64 <- capture_plot_as_base64(function() { plot_lda_single(da_lda_model_obj(), input$da_lda_single_selector) })
        }
      }
    } else if (input$main_tabs == "Clustering Analysis") {
      active_b64 <- capture_plot_as_base64(main_clust_plot_fn)
    }
    
    ai_response <- ask_openai_vision(get_active_context(), user_text, image_b64 = active_b64)
    
    history[[length(history) + 1]] <- list(role = "assistant", content = ai_response)
    chat_state(history)
  })
  
  # =========================================================
  # STAGE 4: SPATIAL & LIDAR ANALYSIS
  # =========================================================
  
  rv_lidar <- reactiveValues(
    raw_las = NULL,
    las = NULL, 
    dtm = NULL,
    chm = NULL,
    tops = NULL,
    plot_shp = NULL,
    itd_metrics = NULL
  )
  
  # 1. Load LAS
  observeEvent(input$lidar_file, {
    req(input$lidar_file)
    withProgress(message = 'Reading LiDAR data...', value = 0.5, {
      tryCatch({
        las <- lidR::readLAS(input$lidar_file$datapath)
        rv_lidar$raw_las <- las
        rv_lidar$las <- las
        showNotification("LiDAR data loaded successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error reading LAS:", e$message), type = "error")
      })
    })
  })
  
  # 2. Load Plots
  observeEvent(input$shp_file, {
    req(input$shp_file)
    withProgress(message = 'Loading Shapefile...', value = 0.5, {
      tryCatch({
        # Move all uploaded files to a temp directory to keep them together (for shp/dbf/shx/prj)
        temp_dir <- tempdir()
        for(i in 1:nrow(input$shp_file)){
          file.copy(input$shp_file$datapath[i], file.path(temp_dir, input$shp_file$name[i]))
        }
        shp_path <- file.path(temp_dir, input$shp_file$name[grep("\\.shp$", input$shp_file$name, ignore.case = TRUE)])
        if(length(shp_path) > 0) {
          rv_lidar$plot_shp <- sf::st_read(shp_path[1])
          showNotification("Shapefile loaded successfully.", type = "message")
        } else {
          showNotification("No .shp file found in upload.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading Shapefile:", e$message), type = "error")
      })
    })
  })
  
  # 3. Clip LAS
  observeEvent(input$clip_las, {
    req(rv_lidar$raw_las)
    xmin <- input$clip_xmin; xmax <- input$clip_xmax
    ymin <- input$clip_ymin; ymax <- input$clip_ymax
    if(is.na(xmin) || is.na(xmax) || is.na(ymin) || is.na(ymax)) {
      showNotification("Please provide all 4 coordinates.", type = "warning")
      return()
    }
    withProgress(message = 'Clipping LAS...', value = 0.5, {
      rv_lidar$las <- lidR::clip_rectangle(rv_lidar$raw_las, xmin, ymin, xmax, ymax)
      showNotification("LAS file clipped.", type = "message")
    })
  })
  
  # 4. Normalize Height
  observeEvent(input$run_norm, {
    req(rv_lidar$las)
    withProgress(message = 'Normalizing Height (DTM)...', value = 0, {
      incProgress(0.2, detail = "Rasterizing Terrain...")
      rv_lidar$dtm <- lidR::rasterize_terrain(rv_lidar$las, res=input$dtm_res, algorithm=lidR::tin())
      incProgress(0.5, detail = "Subtracting DTM from LAS...")
      rv_lidar$las <- rv_lidar$las - rv_lidar$dtm
      
      # Correct negative Z
      rv_lidar$las$Z[rv_lidar$las$Z < 0] <- 0
      incProgress(0.9)
      showNotification("Height normalization complete.", type = "message")
    })
  })
  
  # 5. Filter Noise
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
  
  # 6. Generate CHM
  observeEvent(input$run_chm, {
    req(rv_lidar$las)
    withProgress(message = 'Generating CHM...', value = 0.5, {
      thresh <- as.numeric(trimws(unlist(strsplit(input$pitfree_thresh, ","))))
      rv_lidar$chm <- lidR::rasterize_canopy(rv_lidar$las, res = input$chm_res, algorithm = lidR::pitfree(thresholds = thresh))
      showNotification("CHM Generated.", type = "message")
    })
  })
  
  # 7. ITD Detect Trees
  observeEvent(input$run_itd, {
    req(rv_lidar$chm)
    withProgress(message = 'Detecting Trees...', value = 0.5, {
      f_win <- function(height) { input$lmf_a + input$lmf_b * height^2 }
      rv_lidar$tops <- lidR::locate_trees(rv_lidar$chm, lidR::lmf(f_win))
      rv_lidar$tops$h <- 1.2 + rv_lidar$tops$Z * 1.01 # Simple height correction
      showNotification("Individual Tree Detection complete.", type = "message")
    })
  })
  
  # 8. Render 3D Viewer
  output$lidar_3d_viewer <- renderRglwidget({
    req(rv_lidar$las)
    rgl::clear3d()
    lidR::plot(rv_lidar$las, color="Z", bg="white", size=2, clear_artifacts=FALSE)
    rgl::rglwidget()
  })
  
  output$las_summary <- renderPrint({
    req(rv_lidar$las)
    print(summary(rv_lidar$las))
  })
  
  output$las_hists <- renderPlot({
    req(rv_lidar$las)
    par(mfrow=c(1,2))
    hist(rv_lidar$las$Z, main="Height (Z)", col="lightblue", xlab="Z")
    hist(rv_lidar$las$Intensity, main="Intensity", col="lightgreen", xlab="Intensity")
  })
  
  output$chm_plot <- renderPlot({
    req(rv_lidar$chm)
    terra::plot(rv_lidar$chm, main="Canopy Height Model (CHM)")
    if(!is.null(rv_lidar$plot_shp)) {
      plot(sf::st_geometry(rv_lidar$plot_shp), add=TRUE, border="white", lwd=2)
    }
    if(!is.null(rv_lidar$tops)) {
      plot(sf::st_geometry(rv_lidar$tops), add=TRUE, col="red", pch=16, cex=0.5)
    }
  })
  
  output$itd_table <- DT::renderDataTable({
    req(rv_lidar$tops)
    df <- sf::st_drop_geometry(rv_lidar$tops)
    DT::datatable(df, options=list(pageLength=10, scrollX=TRUE))
  })
  
  # 9. Extract Metrics
  observeEvent(input$extract_metrics, {
    req(rv_lidar$las, rv_lidar$plot_shp)
    withProgress(message = 'Extracting Plot Metrics...', value = 0.5, {
      tryCatch({
        # Since uef_metrics is missing, use standard lidR metrics
        d <- lidR::polygon_metrics(rv_lidar$las, ~lidR::stdmetrics(X, Y, Z, Intensity, ReturnNumber, Classification, dz = 1), rv_lidar$plot_shp)
        d <- cbind(rv_lidar$plot_shp, d)
        d_df <- sf::st_set_geometry(d, NULL)
        
        rv_lidar$itd_metrics <- d_df
        
        # Add to global workspace
        dataset_pool[["LiDAR_Plot_Metrics"]] <<- d_df
        updateSelectInput(session, "eng_dataset", choices = names(dataset_pool), selected = "LiDAR_Plot_Metrics")
        updateSelectInput(session, "eval_target", choices = names(d_df))
        updateSelectInput(session, "eval_pred", choices = names(d_df))
        showNotification("Metrics extracted and added to workspace!", type = "message")
      }, error = function(e) {
        showNotification(paste("Metric extraction failed:", e$message), type = "error")
      })
    })
  })
  
  output$metrics_table <- DT::renderDataTable({
    req(rv_lidar$itd_metrics)
    DT::datatable(rv_lidar$itd_metrics, options=list(pageLength=10, scrollX=TRUE))
  })
  
  observeEvent(input$run_eval, {
    req(rv_lidar$itd_metrics, input$eval_target, input$eval_pred)
    obs <- rv_lidar$itd_metrics[[input$eval_target]]
    pred <- rv_lidar$itd_metrics[[input$eval_pred]]
    
    output$eval_metrics_out <- renderPrint({
      if(is.null(obs) || is.null(pred)) {
        cat("Variables not found.")
        return()
      }
      res <- uef_evaluation(pred, obs)
      print(res)
    })
    
    output$eval_plot <- renderPlot({
      if(is.null(obs) || is.null(pred)) return()
      plot(pred, obs, xlab=paste("Predicted (", input$eval_pred, ")"), ylab=paste("Observed (", input$eval_target, ")"), main="Prediction Accuracy", pch=16, col="blue")
      abline(0, 1, col="red", lwd=2)
    })
  })
  
  observeEvent(input$btn_footer_summary, { updateTabsetPanel(session, "main_tabs", selected = "Data & Exploration") })
  observeEvent(input$btn_footer_dist, { updateTabsetPanel(session, "main_tabs", selected = "Data & Exploration") })
  observeEvent(input$btn_footer_str, { updateTabsetPanel(session, "main_tabs", selected = "Data & Exploration") })
  
  observe({
    print(paste("ACTIVE TAB SWITCHED TO:", input$main_tabs))
  })
}
