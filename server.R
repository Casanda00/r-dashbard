library(shiny)
library(nnet)
library(readxl)
library(tools)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(base64enc)

# =========================================================
# AI CONFIGURATION & HELPERS
# =========================================================
# ⚠️ PASTE YOUR REAL API KEY HERE BEFORE DEPLOYING ⚠️
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# Captures active plots as Base64 for the vision model
capture_plot_as_base64 <- function(plot_expr) {
  tmp_file <- tempfile(fileext = ".png")
  png(tmp_file, width = 800, height = 600)
  print(plot_expr)
  dev.off()
  b64 <- base64enc::base64encode(tmp_file)
  unlink(tmp_file)
  return(b64)
}

# Vision-enabled API call with strict guardrails
ask_openai_vision <- function(context, user_msg, image_b64 = NULL) {
  if (OPENAI_API_KEY == "sk-proj-YOUR_REAL_KEY_HERE" || OPENAI_API_KEY == "") {
    return("⚠️ System Error: Please replace the placeholder API Key in server.R with your real key.")
  }
  
  # STRICT GUARDRAIL: No external knowledge, no hallucination.
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
        temperature = 0.0 # Deterministic mode to prevent hallucinations
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
  if (is.null(df) || !(cat_var %in% names(df)) || !(num1 %in% names(df)) || !(num2 %in% names(df))) {
    plot.new(); text(0.5, 0.5, "Awaiting valid variables...", cex=1.2); return()
  }
  
  plot_df <- df[complete.cases(df[, c(num1, num2, cat_var)]), ]
  if (nrow(plot_df) == 0) { plot.new(); text(0.5, 0.5, "Data Error: No complete cases available.", cex=1.2); return() }
  
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
    old_par <- par(mfrow = c(rows, 3), mar = c(11, 6, 7, 2) + 0.1, mgp = c(4.5, 1.2, 0))
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
      } else { plot.new(); text(0.5, 0.5, paste("No data for\n", wrap_text(lvl))) }
    }
  } else {
    old_par <- par(mar = c(11, 7, 7, 2) + 0.1, mgp = c(5, 1.5, 0))
    on.exit(par(old_par))
    if (is.null(target)) { plot.new(); text(0.5, 0.5, "Select a plot to zoom."); return() }
    
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
      } else { plot.new(); text(0.5, 0.5, paste("No data for\n", wrap_text(lvl)), cex = 1.5) }
    }
  }
}

plot_lm_diagnostics <- function(model, dataset, y_var, view_mode, target) {
  if (is.character(model)) { plot.new(); text(0.5, 0.5, "Awaiting valid formula...", cex=1.2); return() }
  
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
  if (is.character(model)) { plot.new(); text(0.5, 0.5, "Awaiting model parameters...", cex=1.2); return() }
  
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
    plot.new(); text(0.5, 0.5, "Awaiting model to generate plot...", cex=1.2); return()
  }
  preds <- predict(model_obj$model)
  actual <- model_obj$data[[target_var]]
  tbl <- table(Actual = actual, Predicted = preds)
  
  par(mar = c(4, 4, 2, 2))
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
  
  # AI Chat State
  chat_state <- reactiveVal(list(list(role = "assistant", content = "Hello! I am your embedded AI Data Analyst. Ask me to interpret any statistical summary or diagnostic plot currently on your screen.")))
  
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
    updateSelectInput(session, "eda_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "lm_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "aov_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updateSelectInput(session, "log_dataset", choices = new_choices, selected = new_choices[length(new_choices)])
    updatePickerInput(session, "batch_targets", choices = new_choices)
    showNotification("Custom datasets uploaded and globally processed!", type = "message")
  })
  
  observeEvent(input$eng_dataset, {
    req(input$eng_dataset)
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
    updatePickerInput(session, "convert_to_num", choices = cat_cols)
    updatePickerInput(session, "convert_to_cat", choices = num_cols)
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
      updatePickerInput(session, "convert_to_num", selected = character(0))
      updatePickerInput(session, "convert_to_cat", selected = character(0))
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
    updatePickerInput(session, "agg_levels", choices = levels_avail)
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
    updatePickerInput(session, "agg_levels", selected = character(0))
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
  
  output$eng_plot <- renderPlot({
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
  })
  
  # ---- STAGE 2: EXPLORATORY DATA ANALYSIS (EDA) ----
  observe({
    req(input$eda_dataset)
    df <- dataset_pool[[input$eda_dataset]]
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
    req(input$eda_view_mode == "Single Plot", input$eda_dataset, input$eda_category, input$eda_num1, input$eda_num2)
    df <- dataset_pool[[input$eda_dataset]]
    fac <- na.omit(unique(as.character(df[[input$eda_category]])))
    choices <- c(paste("Boxplot:", input$eda_num1), paste("Boxplot:", input$eda_num2), "Scatter: All Data", fac)
    selectInput("eda_zoom_target", label = NULL, choices = choices, width = "200px")
  })
  
  output$dynamic_eda_plot_ui <- renderUI({
    req(input$eda_dataset, input$eda_category, input$eda_view_mode)
    if(input$eda_view_mode == "Grid View") {
      df <- dataset_pool[[input$eda_dataset]]
      fac <- as.factor(df[[input$eda_category]])
      num_lvls <- length(unique(na.omit(fac)))
      rows <- 1 + ceiling(num_lvls / 3)
      dynamic_height <- max(500, rows * 350)
      plotOutput("relationship_plots", height = paste0(dynamic_height, "px"))
    } else {
      plotOutput("relationship_plots", height = "700px")
    }
  })
  
  output$relationship_plots <- renderPlot({
    req(input$eda_dataset, input$eda_num1, input$eda_num2, input$eda_category)
    if(input$eda_view_mode == "Single Plot") req(input$eda_zoom_target)
    plot_relationships(dataset_pool[[input$eda_dataset]], input$eda_num1, input$eda_num2, input$eda_category, 
                       view_mode = input$eda_view_mode, target = input$eda_zoom_target)
  })
  
  # ---- STAGE 3: LINEAR REGRESSION ----
  observe({
    req(input$lm_dataset)
    df <- dataset_pool[[input$lm_dataset]]
    req(df)
    cols <- names(df)
    curr_y <- if(isTruthy(isolate(input$lm_y)) && isolate(input$lm_y) %in% cols) isolate(input$lm_y) else cols[1]
    curr_build <- if(isTruthy(isolate(input$lm_build_var)) && isolate(input$lm_build_var) %in% cols) isolate(input$lm_build_var) else cols[1]
    
    updateSelectInput(session, "lm_y", choices = cols, selected = curr_y)
    updateSelectInput(session, "lm_build_var", choices = cols, selected = curr_build)
  })
  
  observeEvent(input$lm_btn_add_var, {
    var <- paste0("`", input$lm_build_var, "`")
    trans <- input$lm_build_trans
    term <- switch(trans, "raw" = var, "log" = paste0("log(", var, ")"), "sqrt" = paste0("sqrt(", var, ")"), "poly" = paste0("I(", var, "^2)"))
    current <- trimws(input$lm_formula_text)
    new_text <- if (nchar(current) > 0) paste(current, term) else term
    updateTextAreaInput(session, "lm_formula_text", value = new_text)
  })
  
  observeEvent(input$lm_btn_add_plus, {
    current <- trimws(input$lm_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "lm_formula_text", value = paste(current, "+ "))
  })
  
  observeEvent(input$lm_btn_add_star, {
    current <- trimws(input$lm_formula_text)
    if(nchar(current) > 0) updateTextAreaInput(session, "log_formula_text", value = paste(current, "* "))
  })
  
  observeEvent(input$lm_btn_clear, { updateTextAreaInput(session, "lm_formula_text", value = "") })
  
  lm_formula_str <- reactive({
    if (!isTruthy(input$lm_y)) return("Y ~ ...")
    safe_y <- paste0("`", input$lm_y, "`")
    x_side <- trimws(input$lm_formula_text)
    if (nchar(x_side) == 0) return(paste(safe_y, "~ ..."))
    paste(safe_y, "~", x_side)
  })
  
  output$lm_formula_display <- renderText({ lm_formula_str() })
  
  lm_model <- reactive({
    req(input$lm_dataset)
    df <- dataset_pool[[input$lm_dataset]]
    form_str <- lm_formula_str()
    if (grepl("\\.\\.\\.", form_str)) return("Awaiting Predictors: Please use the builder or type a formula.")
    tryCatch({ lm(as.formula(form_str), data = df) }, error = function(e) { return(paste("Syntax Error in Formula:", e$message)) })
  })
  
  output$lm_summary <- renderPrint({
    model <- lm_model()
    if (is.character(model)) cat(model) else { print(model$call); cat("\n"); print(summary(model)) }
  })
  
  output$lm_single_selector <- renderUI({
    req(input$lm_view_mode == "Single Plot")
    selectInput("lm_zoom_target", label = NULL, choices = c("Fitted vs Actual", "Residual Plot", "Target Distribution"), width = "200px")
  })
  
  output$dynamic_lm_plot_ui <- renderUI({
    req(input$lm_view_mode)
    if(input$lm_view_mode == "Grid View") {
      plotOutput("lm_diag_plot", height = "400px")
    } else {
      plotOutput("lm_diag_plot", height = "600px")
    }
  })
  
  output$lm_diag_plot <- renderPlot({
    req(input$lm_view_mode)
    if(input$lm_view_mode == "Single Plot") req(input$lm_zoom_target)
    plot_lm_diagnostics(lm_model(), dataset_pool[[input$lm_dataset]], input$lm_y, input$lm_view_mode, input$lm_zoom_target)
  })
  
  # ---- STAGE 4: ANOVA ----
  observe({
    req(input$aov_dataset)
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
    if(input$aov_view_mode == "Grid View") {
      plotOutput("aov_diag_plot", height = "400px")
    } else {
      plotOutput("aov_diag_plot", height = "600px")
    }
  })
  
  output$aov_diag_plot <- renderPlot({
    req(input$aov_view_mode)
    if(input$aov_view_mode == "Single Plot") req(input$aov_zoom_target)
    plot_aov_diagnostics(aov_model(), input$aov_view_mode, input$aov_zoom_target)
  })
  
  # ---- STAGE 5: LOGISTIC REGRESSION (MULTINOMIAL) ----
  observe({
    req(input$log_dataset)
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
    plotOutput("log_diag_plot", height = "300px")
  })
  
  output$log_diag_plot <- renderPlot({
    plot_log_diagnostics(log_model_obj(), input$log_y)
  })
  
  # =========================================================
  # AI CO-PILOT CHAT ENGINE (VISION-ENABLED)
  # =========================================================
  get_active_context <- reactive({
    current_tab <- input$main_tabs
    if (is.null(current_tab)) return("No data context available.")
    
    tryCatch({
      if (current_tab == "3. Linear Regression (LM)") {
        model <- lm_model()
        if (is.character(model)) return("The user has not built a valid Linear Regression model yet.")
        paste(capture.output(summary(model)), collapse = "\n")
      } else if (current_tab == "4. ANOVA") {
        model <- aov_model()
        if (is.character(model)) return("The user has not built a valid ANOVA model yet.")
        res <- paste(capture.output(summary(model)), collapse = "\n")
        tukey <- tryCatch(paste(capture.output(TukeyHSD(model)), collapse = "\n"), error=function(e) "No Tukey HSD available.")
        paste("ANOVA Results:\n", res, "\n\nTukey Post-Hoc:\n", tukey)
      } else if (current_tab == "5. Logistic Regression") {
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
      } else if (current_tab == "1. Data Import & Engineering") {
        req(rv$working_data)
        paste("The user is looking at the raw dataset structure:\n", paste(capture.output(str(rv$working_data)), collapse = "\n"))
      } else {
        "The user is looking at Exploratory Data Analysis. Ask what specific variables they are visualizing."
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
    do.call(tagList, ui_list)
  })
  
  observeEvent(input$send_chat, {
    user_text <- trimws(input$chat_input)
    req(nchar(user_text) > 0)
    
    # 1. Update UI History immediately
    history <- chat_state()
    history[[length(history) + 1]] <- list(role = "user", content = user_text)
    chat_state(history)
    updateTextInput(session, "chat_input", value = "")
    
    # 2. Vision Pipeline: Capture active plot based on current tab
    active_b64 <- NULL
    if (input$main_tabs == "3. Linear Regression (LM)") {
      active_b64 <- capture_plot_as_base64(plot_lm_diagnostics(lm_model(), dataset_pool[[input$lm_dataset]], input$lm_y, input$lm_view_mode, input$lm_zoom_target))
    } else if (input$main_tabs == "4. ANOVA") {
      active_b64 <- capture_plot_as_base64(plot_aov_diagnostics(aov_model(), input$aov_view_mode, input$aov_zoom_target))
    } else if (input$main_tabs == "5. Logistic Regression") {
      active_b64 <- capture_plot_as_base64(plot_log_diagnostics(log_model_obj(), input$log_y))
    }
    
    # 3. Call Vision-enabled AI
    ai_response <- ask_openai_vision(get_active_context(), user_text, image_b64 = active_b64)
    
    # 4. Update UI History with AI response
    history[[length(history) + 1]] <- list(role = "assistant", content = ai_response)
    chat_state(history)
  })
}