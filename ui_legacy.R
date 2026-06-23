library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(rgl)

ui <- page_navbar(
  fillable = TRUE,
  title = NULL,
  id = "main_tabs",
  theme = bs_theme(
    preset = "zephyr", 
    primary = "#2e7d32",     
    secondary = "#4caf50",   
    success = "#4caf50",     
    info = "#4caf50"
  ),
  navbar_options = navbar_options(bg = "#2e7d32", theme = "dark"),

  header = tagList(
    tags$head(
      tags$style(HTML("
      /* --- GLOBAL LAYOUT FIXES --- */
      #eng_dataset + .selectize-control .selectize-dropdown { top: auto !important; bottom: 100% !important; margin-bottom: 5px; }

      :root {
        --app-green-dark: #2e7d32;
        --app-green-mid: #4caf50;
        --app-green-pale: #e8f5e9;
      }

      body { padding-bottom: 62px; }

      .navbar, .bslib-page-navbar > nav { position: relative; z-index: 2100 !important; }
      .navbar .dropdown-menu { z-index: 2200 !important; pointer-events: auto !important; }
      .dropdown-item.active, .dropdown-item:active {
        background-color: var(--app-green-dark) !important;
        color: #fff !important;
      }

      .btn-primary, .btn-info, .btn-success, .btn-warning, .btn-danger {
        background-color: var(--app-green-dark) !important;
        border-color: var(--app-green-dark) !important;
        color: #fff !important;
      }
      .btn-secondary, .btn-outline-success {
        border-color: var(--app-green-mid) !important;
        color: var(--app-green-dark) !important;
      }
      .btn-primary:hover, .btn-info:hover, .btn-success:hover, .btn-warning:hover, .btn-danger:hover,
      .btn-secondary:hover, .btn-outline-success:hover {
        background-color: var(--app-green-mid) !important;
        border-color: var(--app-green-mid) !important;
        color: #fff !important;
      }
      .bg-primary, .bg-info, .bg-success, .bg-warning, .bg-danger {
        background-color: var(--app-green-dark) !important;
      }
      .bg-light { background-color: var(--app-green-pale) !important; }


      
      /* --- SIDEBAR RESIZER --- */
      .bslib-sidebar-layout > aside, 
      .bslib-sidebar-layout > .sidebar {
          border-right: 4px solid var(--app-green-pale) !important; 
          transition: border-color 0.2s ease;
      }
      .bslib-sidebar-layout > aside:hover, 
      .bslib-sidebar-layout > .sidebar:hover { border-right-color: var(--app-green-mid) !important; }
      .bslib-sidebar-layout > aside.is-resizing, 
      .bslib-sidebar-layout > .sidebar.is-resizing { border-right-color: var(--app-green-mid) !important; }
      
      pre, code { white-space: pre !important; word-wrap: normal !important; overflow-x: visible !important; }
      .formula-box pre { margin: 0; background-color: transparent; border: none; font-weight: bold; color: var(--app-green-dark); }
      .chat-user { background-color: var(--app-green-pale); border-left: 4px solid var(--app-green-mid); padding: 10px; border-radius: 8px; margin-bottom: 10px; text-align: right; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }
      .chat-ai { background-color: var(--app-green-pale); border-left: 4px solid var(--app-green-dark); padding: 10px; border-radius: 8px; margin-bottom: 10px; text-align: left; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }
      
      /* --- TERMINAL STYLING FIXES (SCOPED STRICTLY TO AVOID BREAKING UI) --- */
      #r_console_output { background: transparent !important; color: var(--app-green-mid) !important; border: none !important; padding: 0 !important; overflow-wrap: break-word; font-family: monospace; }
      #r_code { height: 100% !important; font-family: monospace !important; font-size: 13px; resize: none; border: none; box-shadow: none; }
      #bottom_terminal .shiny-input-container { height: 100%; margin-bottom: 0 !important; } 
    ")),
    
    # --- BULLETPROOF JAVASCRIPT ---
    tags$script(HTML("
      $(document).ready(function() {
          let isResizingH = false;
          let currentAside = null;
          let currentLayout = null;
          const EDGE_TOLERANCE = 10; 

          $(document).on('mousemove', function(e) {
              if (isResizingH && currentAside && currentLayout) {
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
                      isResizingH = true;
                      currentAside = $aside;
                      currentLayout = $aside.closest('.bslib-sidebar-layout');
                      currentAside.addClass('is-resizing');
                      $('body').css({'cursor': 'col-resize', 'user-select': 'none'});
                      e.preventDefault(); 
                  }
              }
          });

          $(document).on('mouseup', function(e) {
              if (isResizingH) {
                  isResizingH = false;
                  if (currentAside) currentAside.removeClass('is-resizing');
                  currentAside = null;
                  currentLayout = null;
                  $('body').css({'cursor': '', 'user-select': ''});
              }
          });
          
          // NOTE: dropdown (nav_menu) tab switching is handled natively by
          // Bootstrap 5 via the data-bs-toggle attributes bslib emits, and
          // input.main_tabs is synced automatically by the navbar id. Do NOT add
          // a manual show/hide handler here -- jQuery .hide() leaves an inline
          // display:none that Bootstrap's class-only Tab plugin can never clear,
          // which freezes every panel. We only close the mobile menu after a pick.
          $(document).on('click', '.navbar .dropdown-menu a[data-bs-toggle=tab]', function() {
              $('.navbar-collapse.show').collapse('hide');
          });

          $(document).on('keypress', '#chat_input', function(e) {
              if(e.which === 13) {
                  e.preventDefault();
                  $('#send_chat').click();
              }
          });

          $(document).on('click', '#send_chat', function() {
              let text = $('#chat_input').val().trim();
              if(text !== '') {
                  let safeText = text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
                  let tempUserMsg = '<div class=\"chat-user temp-msg\"><strong>You: </strong>' + safeText + '</div>';
                  $(tempUserMsg).insertBefore('#chat_loading');
                  $('#chat_loading').show();
                  $('#chat_input').css('color', 'transparent');
                  setTimeout(function() {
                      let $cont = $('#chat_history_container');
                      $cont.scrollTop($cont[0].scrollHeight);
                  }, 50);
              }
          });
      });
    "))
  ),
  
  # ---- STAGE 1 & 2: DATA IMPORT, ENGINEERING & EDA ----
  nav_panel(
    title = "Data & Exploration",
    value = "Data & Exploration",
      layout_sidebar(
        sidebar = sidebar(
          title = "Processing Toolbox",
          position = "right",
          width = 350,
          accordion(
            id = "etl_accordion",
            
            accordion_panel("Import Data", 
              fileInput("user_files", "Upload Custom Datasets (.csv, .xlsx, .txt)", multiple = TRUE, accept = c(".csv", ".txt", ".xlsx", ".xls"))
            ),
            
            accordion_panel("Column Management", 
              markdown("**Keep/Drop Columns**"),
              pickerInput("eng_subset_cols", "Columns to Keep:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
              actionButton("apply_subset", "Apply Subset", class="btn-primary btn-sm", width = "100%"),
              hr(),
              pickerInput("eng_drop_cols", "Columns to Drop:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
              actionButton("apply_drop", "Drop Selected", class="btn-danger btn-sm", width = "100%"),
              hr(),
              markdown("**Rename Column**"),
              selectInput("rename_col_target", "Select Column:", choices = NULL),
              textInput("rename_col_new_name", "New Name:", placeholder = "Enter new name"),
              actionButton("apply_col_rename", "Rename Column", class="btn-primary btn-sm", width="100%"),
              hr(),
              markdown("**Mutate (Add Numeric Col)**"),
              selectInput("mutate_col1", "Numeric Col 1:", choices = NULL),
              selectInput("mutate_op", "Operation:", choices = c("+", "-", "*", "/")),
              selectInput("mutate_col2", "Numeric Col 2:", choices = NULL),
              textInput("mutate_new_name", "New Column Name:", placeholder = "e.g., area_calc"),
              actionButton("apply_mutate", "Create Column", class="btn-primary btn-sm", width="100%"),
              hr(),
              actionButton("reset_data", "Reset to Raw Data", class="btn-warning btn-sm", width = "100%")
            ),
            
            accordion_panel("Row Filtering",
              selectInput("filter_col", "Select Column to Filter:", choices = NULL),
              uiOutput("filter_condition_ui"),
              actionButton("apply_filter", "Apply Filter", class="btn-primary btn-sm", width = "100%")
            ),
            
            accordion_panel("Type Conversion", 
              pickerInput("convert_to_num", "Convert to Numeric:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)), 
              pickerInput("convert_to_cat", "Convert to Categorical:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)), 
              actionButton("apply_conversion", "Apply Conversions", class="btn-primary btn-sm", width = "100%")
            ),
            
            accordion_panel("Level Management", 
              markdown("**Rename Levels**"),
              selectInput("rename_col", "Categorical Column:", choices = NULL), 
              uiOutput("dynamic_rename_ui"), 
              actionButton("apply_rename", "Apply Renames", class="btn-primary btn-sm", width = "100%"),
              hr(),
              markdown("**Merge Levels**"),
              selectInput("agg_col", "Categorical Column:", choices = NULL), 
              selectInput("agg_levels", "Levels to Merge:", choices = NULL, multiple = TRUE), 
              textInput("agg_new_name", "New Combined Name:", placeholder = "e.g., Wetland"), 
              actionButton("apply_merge", "Merge Levels", class="btn-primary btn-sm", width = "100%")
            ),
            
            accordion_panel("Aggregation", 
              selectInput("group_id", "Aggregate by:", choices = NULL), 
              pickerInput("group_nums", "Numeric Columns to Average:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)), 
              pickerInput("group_cats", "Categorical Columns to Keep:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "{0} columns selected")), 
              actionButton("apply_group", "Aggregate Data", class="btn-primary btn-sm", width = "100%")
            ),
            
            accordion_panel("Bin/Cut Numeric",
              selectInput("bin_col", "Numeric Column to Bin:", choices = NULL),
              textInput("bin_breaks", "Breaks (e.g. -Inf,30,50,Inf):", placeholder = "-Inf, 30, 50, Inf"),
              textInput("bin_labels", "Labels (comma-separated):", placeholder = "Winter, Dry Summer, Summer"),
              textInput("bin_new_name", "New Column Name:", placeholder = "Trafficability_Class"),
              actionButton("apply_bin", "Create Bins", class="btn-primary btn-sm", width="100%")
            ),
            
            accordion_panel("Conditional Imputation",
              markdown("*Fill missing values (NA) in the Primary column using values from the Secondary column.*"),
              selectInput("coalesce_primary", "Primary Column (Target):", choices = NULL),
              selectInput("coalesce_secondary", "Secondary Column (Source):", choices = NULL),
              actionButton("apply_coalesce", "Impute Missing Values", class="btn-primary btn-sm", width="100%")
            ),
            
            accordion_panel("Merge/Join Datasets",
              selectInput("join_target", "Dataset to Join With:", choices = NULL),
              selectInput("join_type", "Join Type:", choices = c("Left Join" = "left", "Inner Join" = "inner", "Full Join" = "full", "Right Join" = "right")),
              selectInput("join_by", "Common ID Column:", choices = NULL),
              actionButton("apply_join", "Merge Datasets", class="btn-primary btn-sm", width="100%")
            ),
            
            accordion_panel("Batch Apply Pipeline", 
              markdown("*Instantly apply active settings to other datasets.*"), 
              selectInput("batch_targets", "Select Datasets to Update:", choices = NULL, multiple = TRUE), 
              actionButton("apply_batch", "Batch Apply Settings", class="btn-danger btn-sm", width = "100%")
            ),
            
            accordion_panel("EDA Controls",
              markdown("**Select Variables to Explore:**"),
              selectInput("eda_num1", "Numeric Y-Axis (e.g., Height):", choices = NULL),
              selectInput("eda_num2", "Numeric X-Axis (e.g., Diameter):", choices = NULL),
              selectInput("eda_category", "Grouping Category (Color):", choices = NULL),
              hr(),
              uiOutput("eda_plot_selector_ui") 
            )
          )
        ),
        
        navset_card_tab(
          nav_panel("Dataset Overview",
            layout_columns(
              col_widths = c(12),
              card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Dataset Structure", downloadButton("download_data", "Download CSV", class = "btn-sm btn-outline-success")), div(style = "overflow-y: auto; height: 250px; padding: 5px;", verbatimTextOutput("eng_str"))),
              card(card_header(class = "d-flex justify-content-between align-items-center bg-light", "Active Column Distributions", downloadButton("download_dist_plot", "Download Plot", class = "btn-sm btn-outline-success")), div(style = "padding: 5px;", selectInput("eng_view_col", "View Frequency/Summary of:", choices = NULL), layout_columns(col_widths = c(6, 6), plotOutput("eng_plot", height = "350px"), div(style = "overflow-y: auto; height: 350px;", verbatimTextOutput("eng_table")))))
            )
          ),
          nav_panel("Exploratory Plots",
            div(class = "d-flex align-items-center justify-content-end gap-2 header-controls mb-2", radioGroupButtons("eda_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), uiOutput("eda_single_selector"), downloadButton("download_eda_plot", "Download Plot", class = "btn-sm btn-outline-success")),
            div(style = "padding: 5px; overflow-x: hidden; overflow-y: auto; height: 600px;", uiOutput("dynamic_eda_plot_ui"))
          )
        )
      )
    )
  ),
  
  nav_menu("Statistical Models",
    # ---- STAGE 3: LINEAR REGRESSION (LM) -- modularized in mod_linear_regression.R ----
    linearRegressionUI("lm"),
    
    # ---- STAGE 3.5: LINEAR MIXED EFFECTS (LME) ----
    nav_panel(
      title = "Linear Mixed Effects (LME)",
      value = "Linear Mixed Effects (LME)",
      layout_sidebar(
        sidebar = sidebar(
          title = "LME Parameters",
          id = "sidebar_lme",
          width = 350,
          selectInput("lme_y", "Dependent Variable (Y):", choices = NULL),
          hr(),
          markdown("**Fixed Effects Formula**\n*Predictors (X)*"),
          textAreaInput("lme_fixed_text", "Fixed Effects (~):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Soiltype2 * Texture"),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
              markdown("**Quick Builder**"),
              selectInput("lme_build_var", "Select Variable:", choices = NULL),
              selectInput("lme_build_trans", "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")),
              fluidRow(column(6, actionButton("lme_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("lme_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("lme_btn_add_star", " * ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))),
              actionButton("lme_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%")
          ),
          hr(),
          markdown("**Random Effects Formula**"),
          textInput("lme_random_text", "Random Intercept (e.g., ~1 | Group):", placeholder = "~ 1 | PlotID"),
          hr(),
          actionButton("run_lme", "Fit LME Model", class = "btn-primary", width = "100%")
        ),
        selectInput("lme_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
        div(
          card(
            card_header(class = "bg-light", "Model Diagnostics"),
            div(style = "overflow-y: auto; height: 400px; padding: 5px;", plotOutput("lme_diagnostics_plot"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(class="bg-light", "Model Summary"), 
              div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", textOutput("lme_formula_display")), 
              div(style = "overflow-y: auto; height: 400px; padding: 5px;", verbatimTextOutput("lme_summary"))
            ),
            card(
              card_header(class="bg-light", "Performance Metrics (Nakagawa R²) & VIF"), 
              div(style = "overflow-y: auto; height: 400px; padding: 5px;", verbatimTextOutput("lme_performance"))
            )
          )
        )
      )
    ),
    
    # ---- STAGE 4: ANOVA ----
  nav_panel(
    title = "ANOVA",
    value = "ANOVA",
    layout_sidebar(
      sidebar = sidebar(
        title = "ANOVA Parameters",
        id = "sidebar_stage4",
        width = 350, 
        markdown("*Tests continuous differences across categorical groups.*"),
        selectInput("aov_y", "Continuous Target (Y):", choices = NULL),
        selectInput("aov_x", "Categorical Group (X):", choices = NULL),
        hr(),
        selectInput("aov_selected_plots", "Select Diagnostics to View:", choices = c("Residuals vs Fitted", "Normal Q-Q"), selected = c("Residuals vs Fitted", "Normal Q-Q"), multiple = TRUE)
      ),
      selectInput("aov_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      div(
        card(
          card_header(class = "d-flex justify-content-between align-items-center bg-light", "Diagnostics", 
                      div(class = "d-flex align-items-center gap-2 header-controls", radioGroupButtons("aov_view_mode", label = NULL, choices = c("Grid View", "Single Plot"), selected = "Grid View", size = "sm", status = "primary"), uiOutput("aov_single_selector"), downloadButton("download_aov_plot", "Download Plot", class = "btn-sm btn-outline-success"))
          ), 
          div(style = "overflow-y: auto; height: 520px; padding: 5px;", uiOutput("dynamic_aov_plot_ui"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class="bg-light", "ANOVA Table"), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("aov_summary"))
          ),
          card(
            card_header(class="bg-light", "Tukey HSD (Post-Hoc)"), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("aov_tukey"))
          )
        )
      )
    )
  ),
  
  # ---- STAGE 5: LOGISTIC REGRESSION (MULTINOMIAL) ----
  nav_panel(
    title = "Logistic Regression",
    value = "Logistic Regression",
    layout_sidebar(
      sidebar = sidebar(
        title = "Classification Setup",
        id = "sidebar_stage5",
        width = 350, 
        selectInput("log_y", "Categorical Target (Y):", choices = NULL), hr(), markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"), textAreaInput("log_formula_text", "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Organic_depth"), div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;", markdown("**Quick Builder**"), selectInput("log_build_var", "Select Variable:", choices = NULL), selectInput("log_build_trans", "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")), fluidRow(column(6, actionButton("log_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("log_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("log_btn_add_star", " * ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))), actionButton("log_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%"))
      ),
      selectInput("log_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      div(
        card(
          card_header(class = "d-flex justify-content-between align-items-center bg-light", "Model Evaluation Plot", 
                      downloadButton("download_log_plot", "Download Plot", class = "btn-sm btn-outline-success")
          ), 
          div(style = "overflow-y: auto; height: 520px; padding: 5px;", uiOutput("dynamic_log_plot_ui"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class="bg-light", "Model Summary"), 
            div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", textOutput("log_formula_display")), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("log_summary"))
          ),
          card(
            card_header(class="bg-light", "Confusion Matrix & Accuracy"), 
            div(style = "overflow-y: auto; height: 345px; padding: 5px;", verbatimTextOutput("log_matrix"), hr(), tags$b(textOutput("log_accuracy")))
          )
        )
      )
    )
  )
  ),
  
  nav_menu("Machine Learning",
    # ---- STAGE 5.5: RANDOM FOREST ----
    nav_panel(
      title = "Random Forest",
      value = "Random Forest",
      layout_sidebar(
        sidebar = sidebar(
          title = "Random Forest Setup",
          id = "sidebar_rf",
          width = 350,
          selectInput("rf_target", "Target Variable (Y):", choices = NULL),
          pickerInput("rf_predictors", "Predictors (X):", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          hr(),
          sliderInput("rf_ntree", "Number of Trees (ntree):", min = 100, max = 2000, value = 500, step = 100),
          checkboxInput("rf_run_cv", "Run 10-Fold CV (May be slow)", value = FALSE),
          actionButton("run_rf", "Train Random Forest", class = "btn-primary", width = "100%")
        ),
        selectInput("rf_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-light", "Model Summary"),
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("rf_summary"))
          ),
          card(
            card_header(class = "bg-light", "Variable Importance"),
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", plotOutput("rf_varimp"))
          )
        ),
        card(
          card_header(class = "bg-light", "Partial Dependence Plots (PDP)"),
          div(class = "d-flex align-items-end gap-2", 
              selectInput("rf_pdp_var", "Select Predictor for PDP:", choices = NULL), 
              div(style = "margin-bottom: 16px;", actionButton("run_pdp", "Generate PDP", class="btn-info"))
          ),
          div(style = "overflow-y: auto; height: 400px; padding: 5px;", plotOutput("rf_pdp_plot"))
        )
      )
    ),
    
    # ---- STAGE 6: DISCRIMINANT ANALYSIS ----
    nav_panel(
      title = "Discriminant Analysis",
      value = "Discriminant Analysis",
    layout_sidebar(
      sidebar = sidebar(
        title = "DA Controls",
        id = "sidebar_stage6",
        width = 350,
        
        selectInput(
          "da_main_mode", 
          "Analysis Mode:", 
          choices = c("1. Assumption Checks", "2. Run Model"),
          selected = "1. Assumption Checks"
        ),
        
        selectInput("da_category", "Target Variable (Y):", choices = NULL),
        hr(),
        
        conditionalPanel(
          condition = "input.da_main_mode == '1. Assumption Checks'",
          selectInput(
            "da_view", 
            "Select Diagnostic View:", 
            choices = c("1. Covariance Ellipses", "2. Equal Variance (Boxplots)", "3. Normality (Q-Q Plots)", "4. Distribution Density", "5. Statistical Tests"),
            selected = "1. Covariance Ellipses"
          ),
          hr(),
          conditionalPanel(
            condition = "input.da_view == '1. Covariance Ellipses'",
            markdown("**Ellipses Parameters**"),
            selectInput("da_ellipses_x", "X-Axis Variable:", choices = NULL),
            selectInput("da_ellipses_y", "Y-Axis Variable:", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.da_view == '2. Equal Variance (Boxplots)'",
            markdown("**Boxplot Parameters**"),
            selectInput("da_box_y", "Analyze Variable:", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.da_view == '3. Normality (Q-Q Plots)' || input.da_view == '4. Distribution Density'",
            markdown("**Distribution Parameters**"),
            selectInput("da_norm_var", "Assess Normality of:", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.da_view == '5. Statistical Tests'",
            markdown("**Statistical Parameters**"),
            selectInput("stat_test_type", "Select Test:", choices = c("Shapiro-Wilk (Normality)", "Box's M (Equal Covariance)")),
            conditionalPanel(
              condition = "input.stat_test_type == 'Shapiro-Wilk (Normality)'",
              selectInput("stat_shapiro_var", "Numeric Variable:", choices = NULL),
              selectInput("stat_shapiro_group", "Group Level:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.stat_test_type == 'Box\\'s M (Equal Covariance)'",
              selectInput("stat_boxm_vars", "Variables to Include:", choices = NULL, multiple = TRUE)
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.da_main_mode == '2. Run Model'",
          
          selectInput("da_method_type", "Discriminant Method:", 
            choices = c(
              "LDA (Linear)" = "LDA",
              "Weighted LDA" = "WLDA",
              "QDA (Quadratic)" = "QDA",
              "Regularized LDA (rda)" = "RLDA",
              "Kernel DA (SVM-RBF)" = "KDA",
              "Locally Linear DA" = "LLDA",
              "Maximum Margin (Linear SVM)" = "MMC",
              "Random Forest" = "RF",
              "Neural Network" = "NN"
            ),
            selected = "LDA"
          ),
          
          markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"),
          textAreaInput("da_lda_formula_text", "Predictors (X):", value = "", rows = 3, placeholder = "e.g., Sepal.Length + Sepal.Width"),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
              markdown("**Quick Builder**"),
              selectInput("da_lda_build_var", "Select Variable:", choices = NULL),
              fluidRow(column(6, actionButton("da_lda_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(6, actionButton("da_lda_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))),
              actionButton("da_lda_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%")
          ),
          hr(),
          
          # Method-specific parameters
          conditionalPanel(
            condition = "input.da_method_type == 'WLDA'",
            markdown("**Weighted LDA Parameters**"),
            selectInput("da_wlda_weight_type", "Weighting Scheme:", 
                        choices = c("Inverse Frequency (1/N)" = "inverse", 
                                    "Proportional (N)" = "proportional", 
                                    "Equal Weights" = "equal"), 
                        selected = "inverse")
          ),
          conditionalPanel(
            condition = "input.da_method_type == 'KDA'",
            markdown("**Kernel DA Parameters**"),
            numericInput("da_kda_sigma", "Sigma (RBF width):", value = 0.01, min = 0.001, max = 10, step = 0.01),
            numericInput("da_kda_C", "Cost (C):", value = 0.1, min = 0.01, max = 100, step = 0.1)
          ),
          conditionalPanel(
            condition = "input.da_method_type == 'LLDA'",
            markdown("**Locally Linear DA Parameters**"),
            sliderInput("da_llda_k", "Number of Neighbors (k):", min = 3, max = 30, value = 5, step = 1)
          ),
          conditionalPanel(
            condition = "input.da_method_type == 'MMC'",
            markdown("**Maximum Margin Parameters**"),
            numericInput("da_mmc_C", "Cost (C):", value = 1, min = 0.01, max = 100, step = 0.1)
          ),
          conditionalPanel(
            condition = "input.da_method_type == 'RF'",
            markdown("**Random Forest Parameters**"),
            sliderInput("da_rf_ntree", "Number of Trees:", min = 100, max = 2000, value = 1000, step = 100),
            sliderInput("da_rf_mtry", "Variables per Split (mtry):", min = 1, max = 10, value = 2, step = 1)
          ),
          conditionalPanel(
            condition = "input.da_method_type == 'NN'",
            markdown("**Neural Network Parameters**"),
            sliderInput("da_nn_size", "Hidden Neurons:", min = 1, max = 20, value = 5, step = 1),
            numericInput("da_nn_decay", "Decay (Regularization):", value = 0.01, min = 0.001, max = 1, step = 0.01)
          ),
          
          hr(),
          selectInput(
            "da_lda_selected_plots", 
            "Select Diagnostics to View:", 
            choices = c("LD Scatter/Density", "Stacked Histogram", "Biplot (ggord)", "Pairs Plot", "Partition Plot (partimat)", "Variable Importance"), 
            selected = c("LD Scatter/Density", "Stacked Histogram"), 
            multiple = TRUE
          )
        )
      ),
      
      selectInput("da_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      uiOutput("dynamic_da_content")
    )
  ),
  
  # ---- STAGE 7: CLUSTERING ANALYSIS ----
  nav_panel(
    title = "Clustering Analysis",
    value = "Clustering Analysis",
    layout_sidebar(
      sidebar = sidebar(
        title = "Clustering Controls",
        id = "sidebar_stage7",
        width = 350,
        
        markdown("**1. Data Preparation**"),
        checkboxInput("clust_scale_data", "Standardize Numeric Data (Z-Score)", value = TRUE),
        selectInput("clust_vars", "Variables to Cluster (Numeric & Categorical):", choices = NULL, multiple = TRUE),
        markdown("*Mixed numeric + categorical variables will use Gower distance with PAM clustering.*"),
        
        hr(),
        
        markdown("**2. Algorithm Selection**"),
        selectInput("clust_method", "Clustering Method:", choices = c("K-Means", "Hierarchical"), selected = "K-Means"),
        
        conditionalPanel(
          condition = "input.clust_method == 'Hierarchical'",
          selectInput("hclust_dist", "Distance Metric:", choices = c("euclidean", "manhattan", "pearson"), selected = "euclidean"),
          selectInput("hclust_link", "Linkage Method:", choices = c("complete", "average", "single", "ward.D2"), selected = "ward.D2")
        ),
        
        hr(),
        
        markdown("**3. Diagnostic View**"),
        selectInput(
          "clust_view", 
          "Select Display:", 
          choices = c(
            "1. Optimal k (Elbow Method)",
            "2. Optimal k (Silhouette Method)",
            "3. Cluster Map (PCA/Dendrogram)", 
            "4. Custom Scatter Plot",
            "5. Silhouette Profile",
            "6. Phylogenetic Tree (ape)"
          ),
          selected = "3. Cluster Map (PCA/Dendrogram)"
        ),
        
        conditionalPanel(
          condition = "input.clust_view == '3. Cluster Map (PCA/Dendrogram)' || input.clust_view == '4. Custom Scatter Plot' || input.clust_view == '5. Silhouette Profile' || input.clust_view == '6. Phylogenetic Tree (ape)'",
          sliderInput("clust_k", "Number of Clusters (k):", min = 2, max = 10, value = 3, step = 1)
        ),
        
        conditionalPanel(
          condition = "input.clust_view == '4. Custom Scatter Plot'",
          selectInput("clust_scatter_x", "X-Axis Variable:", choices = NULL),
          selectInput("clust_scatter_y", "Y-Axis Variable:", choices = NULL)
        )
      ),
      
      selectInput("clust_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      
      div(
        card(
          card_header(class = "d-flex justify-content-between align-items-center bg-primary text-white", "Visual Diagnostics",
                      downloadButton("download_clust_plot", "Download Plot", class = "btn-sm btn-outline-light")),
          div(style = "padding: 10px;", uiOutput("clust_explanation")),
          div(style = "height: 500px; padding: 10px;", plotOutput("main_clust_plot", height = "480px"))
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class="bg-light", "Cluster Profiles (Raw Means)"), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("cluster_summary"))
          ),
          card(
            card_header(class="bg-light", "Cluster Assignments"), 
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("cluster_assignments"))
          )
        )
      )
    )
  ),
  
  # ---- STAGE 8: CLASSIFICATION (ONE-VS-ALL) ----
  nav_panel(
    title = "Classification",
    value = "Classification",
    layout_sidebar(
      sidebar = sidebar(
        title = "Classification Setup",
        id = "sidebar_stage8",
        width = 350,
        
        markdown("**1. Target & Predictors**"),
        selectInput("clf_target", "Target Variable (Categorical):", choices = NULL),
        hr(),
        
        markdown("**Formula Editor**\n*Type freely or use the builder buttons below.*"),
        textAreaInput("clf_formula_text", "Predictors (X):", value = "", rows = 3, placeholder = "e.g., ih5_dm + Nutrient_class + Nutrient_add"),
        div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
            markdown("**Quick Builder**"),
            selectInput("clf_build_var", "Select Variable:", choices = NULL),
            selectInput("clf_build_trans", "Apply Transformation:", choices = c("None (Raw)" = "raw", "Logarithm (log)" = "log", "Square Root (sqrt)" = "sqrt", "Quadratic (^2)" = "poly")),
            fluidRow(column(6, actionButton("clf_btn_add_var", "Insert", class="btn-primary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("clf_btn_add_plus", " + ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;")), column(3, actionButton("clf_btn_add_star", " * ", class="btn-secondary btn-sm", width="100%", style="margin-bottom:5px;"))),
            actionButton("clf_btn_clear", "Clear Formula", class="btn-outline-danger btn-sm", width="100%")
        ),
        
        hr(),
        markdown("**2. Classification Settings**"),
        sliderInput("clf_threshold", "Decision Threshold:", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
        
        hr(),
        markdown("**3. Exclude Classes (Optional)**"),
        pickerInput("clf_exclude_classes", "Classes to Exclude:", choices = NULL, multiple = TRUE, 
                    options = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "None excluded")),
        
        hr(),
        actionButton("clf_run", "Run Classification", class = "btn-primary", width = "100%", icon = icon("play"))
      ),
      
      selectInput("clf_dataset", "Active Dataset:", choices = c("Awaiting Data Upload..." = ""), width = "350px"),
      
      div(
        card(
          card_header(class = "d-flex justify-content-between align-items-center bg-primary text-white", "Classification Performance (F1 Score by Class)",
                      downloadButton("download_clf_plot", "Download Plot", class = "btn-sm btn-outline-light")),
          div(style = "height: 450px; padding: 10px;", plotOutput("clf_f1_plot", height = "430px"))
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class="bg-light", "Per-Class Metrics"),
            div(class = "formula-box", style = "padding: 10px; background-color: #e9ecef; border-bottom: 1px solid #dee2e6;", textOutput("clf_formula_display")),
            div(style = "overflow-y: auto; height: 300px; padding: 5px;", verbatimTextOutput("clf_metrics_table"))
          ),
          card(
            card_header(class="bg-light", "Per-Class Confusion Matrices"),
            div(style = "overflow-y: auto; height: 345px; padding: 5px;", verbatimTextOutput("clf_confusion_details"))
          )
        )
      )
    )
  )
  ),
  
  nav_menu("Spatial & LiDAR Analysis",
    nav_panel("Point Cloud & 3D Viewer",
      layout_sidebar(
        sidebar = sidebar(
          title = "LiDAR Pre-Processing", width = 350,
          fileInput("lidar_file", "Upload .laz File", accept = c(".las", ".laz")),
          fileInput("shp_file", "Upload Plot Shapefile (.shp, .shx, .dbf, .prj)", multiple = TRUE),
          hr(),
          markdown("**Sub-setting & Memory Limits**"),
          numericInput("clip_xmin", "X Min:", value = NA),
          numericInput("clip_xmax", "X Max:", value = NA),
          numericInput("clip_ymin", "Y Min:", value = NA),
          numericInput("clip_ymax", "Y Max:", value = NA),
          actionButton("clip_las", "Clip LAS File", class="btn-warning", width="100%"),
          hr(),
          markdown("**Height Normalization (DTM)**"),
          sliderInput("dtm_res", "DTM Resolution:", min=0.5, max=5, value=1, step=0.5),
          actionButton("run_norm", "Normalize Height (Z)", class="btn-primary", width="100%"),
          hr(),
          markdown("**Outlier & Noise Filter**"),
          sliderInput("int_max", "Max Intensity Cutoff:", min=100, max=1000, value=300, step=50),
          actionButton("run_filter", "Filter Noise", class="btn-primary", width="100%")
        ),
        div(
          card(
            card_header(class="bg-light", "Interactive 3D Point Cloud Viewer"),
            rglwidgetOutput("lidar_3d_viewer", height="500px")
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header(class="bg-light", "LAS Summary"), verbatimTextOutput("las_summary")),
            card(card_header(class="bg-light", "Elevation & Intensity Distributions"), plotOutput("las_hists"))
          )
        )
      )
    ),
    nav_panel("CHM & ITD",
      layout_sidebar(
        sidebar = sidebar(
          title = "Canopy Height Model", width = 350,
          sliderInput("chm_res", "CHM Resolution:", min=0.1, max=2, value=0.5, step=0.1),
          textInput("pitfree_thresh", "Pitfree Thresholds (comma-sep):", value="0, 5, 10, 15, 20, 25"),
          actionButton("run_chm", "Generate CHM", class="btn-primary", width="100%"),
          hr(),
          markdown("**Individual Tree Detection (ITD)**"),
          markdown("*LMF Window Size: `a + b * height^2`*"),
          numericInput("lmf_a", "Parameter a:", value = 1.2),
          numericInput("lmf_b", "Parameter b:", value = 0.003),
          actionButton("run_itd", "Detect Trees", class="btn-primary", width="100%")
        ),
        div(
          card(
            card_header(class="bg-light", "2D CHM & Detected Trees"),
            plotOutput("chm_plot", height="500px")
          ),
          card(
            card_header(class="bg-light", "ITD Output Table"),
            DT::dataTableOutput("itd_table")
          )
        )
      )
    ),
    nav_panel("Metric Extraction & Evaluation",
      layout_sidebar(
        sidebar = sidebar(
          title = "Area-Based & Model Evaluation", width = 350,
          actionButton("extract_metrics", "Extract Plot Metrics", class="btn-primary", width="100%"),
          hr(),
          markdown("**Evaluate Volume Models**"),
          selectInput("eval_target", "Observed Variable (e.g., v):", choices = NULL),
          selectInput("eval_pred", "Predicted Variable (e.g., v_itd):", choices = NULL),
          actionButton("run_eval", "Calculate Error Metrics", class="btn-success", width="100%")
        ),
        div(
          card(
            card_header(class="bg-light", "Extracted Plot Predictors"),
            DT::dataTableOutput("metrics_table")
          ),
          card(
            card_header(class="bg-light", "Model Evaluation (RMSE, Bias)"),
            verbatimTextOutput("eval_metrics_out"),
            plotOutput("eval_plot")
          )
        )
      )
    )
  ),
  
  # =========================================================
  # FOOTER: AI CO-PILOT ONLY 
  # =========================================================
  footer = tagList(
    tags$div(
      style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: #e8f5e9; border-top: 1px solid #4caf50; z-index: 900; padding: 5px 20px; display: flex; align-items: center; justify-content: space-between; height: 50px;",
      div(style="display: flex; align-items: center; gap: 15px;",
        tags$strong("Active Workspace:"),
        div(style="width: 250px;", selectInput("eng_dataset", label=NULL, choices = c("Awaiting Data Upload..." = ""), width="100%")),
        actionButton("view_full_data", "View Data", class="btn-info btn-sm", icon=icon("table")),
        actionButton("btn_footer_summary", "Summary", class="btn-primary btn-sm", icon=icon("list")),
        actionButton("btn_footer_dist", "Distribution", class="btn-primary btn-sm", icon=icon("chart-bar")),
        actionButton("btn_footer_str", "Structure", class="btn-primary btn-sm", icon=icon("sitemap"))
      )
    ),
    tags$div(
      style = "position: fixed; top: 0; right: 0; height: 100vh; z-index: 950; display: flex; flex-direction: row; align-items: flex-end; pointer-events: none;",
      div(
        style = "position: absolute; bottom: 20px; right: 20px; pointer-events: auto;",
        actionButton("toggle_chat", "Ask Co-Pilot", icon = icon("robot"), class = "btn-primary btn-lg", style = "border-radius: 50px; box-shadow: 0px 4px 15px rgba(0,0,0,0.2); font-weight: bold; letter-spacing: 0.5px;")
      ),
      conditionalPanel(
        condition = "input.toggle_chat % 2 == 1",
        card(
          style = "width: 400px; height: 100vh; margin: 0; border-radius: 0; border-left: 1px solid #dee2e6; box-shadow: -5px 0px 25px rgba(0,0,0,0.15); pointer-events: auto;",
          card_header(
            class = "bg-primary text-white d-flex justify-content-between align-items-center", 
            tags$strong(icon("robot"), " AI Co-Pilot"),
            div(
              tags$span(style = "font-size: 11px; opacity: 0.8; margin-right: 15px;", "Context: Auto-Synced"),
              tags$a(href="#", icon("xmark"), style="color: white; font-size: 18px;", onclick="$('#toggle_chat').click(); return false;")
            )
          ),
          div(id = "chat_history_container", style = "flex-grow: 1; overflow-y: auto; padding: 15px; display: flex; flex-direction: column;", 
              uiOutput("chat_history"),
              div(id = "chat_loading", style = "display: none;", 
                  div(class = "chat-ai", style = "opacity: 0.7; max-width: 50%;", 
                      tags$em(icon("spinner", class="fa-spin"), " Thinking...")
                  )
              )
          ),
          div(style = "padding: 15px; border-top: 1px solid #dee2e6; background: #f8f9fa;", 
              fluidRow(
                column(10, style = "padding-right: 5px;", textInput("chat_input", label = NULL, placeholder = "Ask about this model...", width = "100%")), 
                column(2, style = "padding-left: 0;", actionButton("send_chat", label = icon("paper-plane"), class = "btn-primary", width = "100%", style="padding: 6px 12px;"))
              )
          )
        )
      )
    )
  )
)
