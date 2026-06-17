library(shiny)
library(bslib)
library(shinyWidgets)

ui <- page_navbar(
  # --- SOFTWARE TITLE & THEME ---
  title = "TerraTrack: Forest Trafficability Engine",
  id = "main_tabs",
  theme = bs_theme(preset = "flatly"), # Applies a sleek, professional "software" look globally
  
  header = tags$head(
    tags$style(HTML("
      /* --- THE RESIZE VISUAL CUES --- */
      .bslib-sidebar-layout > aside, 
      .bslib-sidebar-layout > .sidebar {
          border-right: 4px solid #dee2e6 !important; 
          transition: border-color 0.2s ease;
      }
      .bslib-sidebar-layout > aside:hover, 
      .bslib-sidebar-layout > .sidebar:hover {
          border-right-color: #adb5bd !important; 
      }
      .bslib-sidebar-layout > aside.is-resizing, 
      .bslib-sidebar-layout > .sidebar.is-resizing {
          border-right-color: #0d6efd !important; 
      }
      
      pre, code { white-space: pre !important; word-wrap: normal !important; overflow-x: visible !important; }
      .formula-box pre { margin: 0; background-color: transparent; border: none; font-weight: bold; color: #2c3e50; }
      .chat-user { background-color: #e9ecef; padding: 10px; border-radius: 12px 12px 0px 12px; margin-bottom: 10px; text-align: right; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }
      .chat-ai { background-color: #d1ecf1; padding: 10px; border-radius: 12px 12px 12px 0px; margin-bottom: 10px; text-align: left; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }
    ")),
    
    # --- BULLETPROOF JAVASCRIPT COORDINATE TRACKER ---
    tags$script(HTML("
      $(document).ready(function() {
          let isResizing = false;
          let currentAside = null;
          let currentLayout = null;
          const EDGE_TOLERANCE = 10; 

          $(document).on('mousemove', function(e) {
              if (isResizing && currentAside && currentLayout) {
                  let newWidth = e.pageX - currentLayout.offset().left;
                  
                  if (newWidth < 200) newWidth = 200;
                  if (newWidth > $(window).width() * 0.6) newWidth = $(window).width() * 0.6;

                  currentLayout.css({
                      '--bslib-sidebar-width': newWidth + 'px',
                      '--_sidebar-width': newWidth + 'px'
                  });
                  return;
              }

              let $aside = $(e.target).closest('.bslib-sidebar-layout > aside, .bslib-sidebar-layout > .sidebar');
              if ($aside.length) {
                  let rightEdge = $aside.offset().left + $aside.outerWidth();
                  if (Math.abs(e.pageX - rightEdge) <= EDGE_TOLERANCE) {
                      $aside.css('cursor', 'col-resize');
                  } else {
                      $aside.css('cursor', '');
                  }
              }
          });

          $(document).on('mousedown', function(e) {
              let $aside = $(e.target).closest('.bslib-sidebar-layout > aside, .bslib-sidebar-layout > .sidebar');
              if ($aside.length) {
                  let rightEdge = $aside.offset().left + $aside.outerWidth();
                  if (Math.abs(e.pageX - rightEdge) <= EDGE_TOLERANCE) {
                      isResizing = true;
                      currentAside = $aside;
                      currentLayout = $aside.closest('.bslib-sidebar-layout');
                      
                      currentAside.addClass('is-resizing');
                      $('body').css({'cursor': 'col-resize', 'user-select': 'none'});
                      e.preventDefault(); 
                  }
              }
          });

          $(document).on('mouseup', function(e) {
              if (isResizing) {
                  isResizing = false;
                  if (currentAside) currentAside.removeClass('is-resizing');
                  currentAside = null;
                  currentLayout = null;
                  $('body').css({'cursor': '', 'user-select': ''});
              }
          });
      });
    "))
  ),
  
  # ---- STAGE 1: DATA IMPORT & ENGINEERING (ETL) ----
  nav_panel(
    title = "1. Data Engineering",
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Operations",
        id = "sidebar_stage1",
        width = 350, 
        accordion(
          id = "etl_accordion", open = "Step 1: Import Data",
          accordion_panel("Step 1: Import Data", fileInput("user_files", "Upload Custom Datasets (.csv, .xlsx, .txt)", multiple = TRUE, accept = c(".csv", ".txt", ".xlsx", ".xls"))),
          accordion_panel("Step 2: Subsetting", pickerInput("eng_subset_cols", "Columns to Keep:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")), actionButton("apply_subset", "Apply Subset", class="btn-primary", width = "100%"), br(), actionButton("reset_data", "Reset to Raw Data", class="btn-warning", width = "100%")),
          accordion_panel("Step 3: Type Conversion", pickerInput("convert_to_num", "Force to Numeric:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)), pickerInput("convert_to_cat", "Force to Categorical:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)), actionButton("apply_conversion", "Apply Conversions", class="btn-info", width = "100%")),
          accordion_panel("Step 4: Plot Aggregation", selectInput("group_id", "Plot ID Column (e.g., final_id):", choices = NULL), pickerInput("group_nums", "Numeric Columns to Average:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")), pickerInput("group_cats", "Categorical Columns to Keep:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")), actionButton("apply_group", "Aggregate Data", class="btn-primary", width = "100%")),
          accordion_panel("Step 5: Batch Apply Pipeline", markdown("*Instantly apply active settings to other datasets.*"), pickerInput("batch_targets", "Select Datasets to Update:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)), actionButton("apply_batch", "Batch Apply Settings", class="btn-danger", width = "100%")),
          accordion_panel("Step 6: Level Management", navset_pill(nav_panel("Rename Levels", selectInput("rename_col", "Categorical Column:", choices = NULL), uiOutput("dynamic_rename_ui"), actionButton("apply_rename", "Apply Renames", class="btn-success", width = "100%")), nav_panel("Merge Levels", selectInput("agg_col", "Categorical Column:", choices = NULL), pickerInput("agg_levels", "Levels to Merge:", choices = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)), textInput("agg_new_name", "New Combined Name:", placeholder = "e.g., Wetland"), actionButton("apply_merge", "Merge Levels", class="btn-success", width = "100%"))))
        )
      ),
      
      selectInput("eng_dataset", "Active Workspace Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      layout_columns(
        col_widths = c(12),
        card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Dataset Structure", downloadButton("download_data", "Download CSV", class = "btn-sm btn-outline-success")), div(style = "overflow-y: auto; height: 250px; padding: 5px;", verbatimTextOutput("eng_str"))),
        card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Active Column Distributions", downloadButton("download_dist_plot", "Download Plot", class = "btn-sm btn-outline-success")), div(style = "padding: 5px;", selectInput("eng_view_col", "View Frequency/Summary of:", choices = NULL), layout_columns(col_widths = c(6, 6), plotOutput("eng_plot", height = "350px"), div(style = "overflow-y: auto; height: 350px;", verbatimTextOutput("eng_table")))))
      )
    )
  ),
  
  # ---- STAGE 2: EXPLORATORY DATA ANALYSIS (EDA) ----
  nav_panel(
    title = "2. Exploratory Analysis",
    layout_sidebar(
      sidebar = sidebar(
        title = "EDA Control Panel",
        id = "sidebar_stage2",
        width = 350, 
        markdown("**Select Variables to Explore:**"),
        selectInput("eda_num1", "Numeric Y-Axis (e.g., Height):", choices = NULL),
        selectInput("eda_num2", "Numeric X-Axis (e.g., Diameter):", choices = NULL),
        selectInput("eda_category", "Grouping Category (Color):", choices = NULL),
        hr(),
        uiOutput("eda_plot_selector_ui") 
      ),
      
      selectInput("eda_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      card(
        card_header(class = "d-flex justify-content-between align-items-center bg-light", "Dynamic Relationship Visualizations", div(class = "d-flex align-items-center gap-2 header-controls", radioGroupButtons("eda_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), uiOutput("eda_single_selector"), downloadButton("download_eda_plot", "Download Plot", class = "btn-sm btn-outline-success"))),
        div(style = "padding: 5px; overflow-x: hidden; overflow-y: auto;", uiOutput("dynamic_eda_plot_ui"))
      )
    )
  ),
  
  # ---- STAGE 3: LINEAR REGRESSION (LM) ----
  nav_panel(
    title = "3. Linear Regression",
    layout_sidebar(
      sidebar = sidebar(
        title = "Model Parameters",
        id = "sidebar_stage3",
        width = 350, 
        selectInput("lm_y", "Dependent Variable (Y):", choices = NULL),
        hr(),
        markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"),
        textAreaInput("lm_formula_text", "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Soiltype2 * Texture"),
        div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
            markdown("**Quick Builder**"),
            selectInput("lm_build_var", "Select Variable:", choices = NULL),
            selectInput("lm_build_trans", "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")),
            fluidRow(column(6, actionButton("lm_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("lm_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("lm_btn_add_star", " * ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))),
            actionButton("lm_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%")
        ),
        hr(),
        pickerInput("lm_selected_plots", "Select Diagnostics to View:", choices = c("Fitted vs Actual", "Residual Plot", "Target Distribution"), selected = c("Fitted vs Actual", "Residual Plot", "Target Distribution"), multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
      ),
      
      selectInput("lm_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      layout_columns(
        col_widths = c(12),
        card(card_header(class="bg-light", "Model Summary"), div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", verbatimTextOutput("lm_formula_display")), div(style = "overflow-y: auto; height: 350px; padding: 5px;", verbatimTextOutput("lm_summary"))),
        card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Model Diagnostics", div(class = "d-flex align-items-center gap-2 header-controls", radioGroupButtons("lm_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), uiOutput("lm_single_selector"), downloadButton("download_lm_plot", "Download Plot", class = "btn-sm btn-outline-success"))), div(style = "padding: 5px; overflow-x: hidden; overflow-y: auto;", uiOutput("dynamic_lm_plot_ui")))
      )
    )
  ),
  
  # ---- STAGE 4: ANOVA ----
  nav_panel(
    title = "4. ANOVA",
    layout_sidebar(
      sidebar = sidebar(
        title = "ANOVA Parameters",
        id = "sidebar_stage4",
        width = 350, 
        markdown("*Tests continuous differences across categorical groups.*"),
        selectInput("aov_y", "Continuous Target (Y):", choices = NULL),
        selectInput("aov_x", "Categorical Group (X):", choices = NULL),
        hr(),
        pickerInput("aov_selected_plots", "Select Diagnostics to View:", choices = c("Residuals vs Fitted", "Normal Q-Q"), selected = c("Residuals vs Fitted", "Normal Q-Q"), multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
      ),
      
      selectInput("aov_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      layout_columns(
        col_widths = c(12),
        navset_card_tab(title = "Statistical Results", nav_panel("ANOVA Table", div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("aov_summary"))), nav_panel("Tukey HSD (Post-Hoc)", div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("aov_tukey")))),
        card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Diagnostics", div(class = "d-flex align-items-center gap-2 header-controls", radioGroupButtons("aov_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), uiOutput("aov_single_selector"), downloadButton("download_aov_plot", "Download Plot", class = "btn-sm btn-outline-success"))), div(style = "padding: 5px; overflow-x: hidden; overflow-y: auto;", uiOutput("dynamic_aov_plot_ui")))
      )
    )
  ),
  
  # ---- STAGE 5: LOGISTIC REGRESSION (MULTINOMIAL) ----
  nav_panel(
    title = "5. Logistic Regression",
    layout_sidebar(
      sidebar = sidebar(
        title = "Classification Setup",
        id = "sidebar_stage5",
        width = 350, 
        selectInput("log_y", "Categorical Target (Y):", choices = NULL), hr(), markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"), textAreaInput("log_formula_text", "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Organic_depth"), div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;", markdown("**Quick Builder**"), selectInput("log_build_var", "Select Variable:", choices = NULL), selectInput("log_build_trans", "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")), fluidRow(column(6, actionButton("log_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("log_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("log_btn_add_star", " * ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))), actionButton("log_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%"))
      ),
      selectInput("log_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      layout_columns(
        col_widths = c(6, 6), 
        card(card_header(class="bg-light", "Model Summary"), div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", verbatimTextOutput("log_formula_display")), div(style = "overflow-y: auto; height: 350px; padding: 5px;", verbatimTextOutput("log_summary"))), 
        card(card_header(class="bg-light", "Model Evaluation"), div(style = "overflow-y: auto; height: 400px; padding: 5px;", uiOutput("dynamic_log_plot_ui"), h5("Confusion Matrix", class="text-muted"), verbatimTextOutput("log_matrix"), hr(), tags$b(textOutput("log_accuracy")))))
    )
  ),
  
  # =========================================================
  # AI CO-PILOT SIDEBAR PANEL
  # =========================================================
  tags$div(
    style = "position: fixed; top: 0; right: 0; height: 100vh; z-index: 9999; display: flex; flex-direction: row; align-items: flex-end; pointer-events: none;",
    
    div(
      style = "position: absolute; bottom: 20px; right: 20px; z-index: 10000; pointer-events: auto;",
      actionButton("toggle_chat", "Ask Co-Pilot", icon = icon("robot"), class = "btn-primary btn-lg", style = "border-radius: 50px; box-shadow: 0px 4px 15px rgba(0,0,0,0.2); font-weight: bold; letter-spacing: 0.5px;")
    ),
    
    conditionalPanel(
      condition = "input.toggle_chat % 2 == 1",
      card(
        style = "width: 400px; height: 100vh; margin: 0; border-radius: 0; border-left: 1px solid #dee2e6; box-shadow: -5px 0px 25px rgba(0,0,0,0.15); pointer-events: auto;",
        card_header(
          class = "bg-primary text-white d-flex justify-content-between align-items-center", 
          tags$strong(icon("robot"), " AI Co-Pilot"),
          tags$span(style = "font-size: 11px; opacity: 0.8;", "Context: Auto-Synced")
        ),
        div(id = "chat_history_container", style = "flex-grow: 1; overflow-y: auto; padding: 15px; display: flex; flex-direction: column;", uiOutput("chat_history")),
        div(style = "padding: 15px; border-top: 1px solid #dee2e6; background: #f8f9fa;", 
            fluidRow(
              column(10, style = "padding-right: 5px;", textInput("chat_input", label = NULL, placeholder = "Ask about this model...", width = "100%")), 
              # ICON-ONLY SEND BUTTON FIX
              column(2, style = "padding-left: 0;", actionButton("send_chat", label = icon("paper-plane"), class = "btn-primary", width = "100%", style="padding: 6px 12px;"))
            )
        )
      )
    )
  )
)