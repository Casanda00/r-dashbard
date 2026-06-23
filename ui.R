# ==========================================================================
# ui.R  --  GeoLibre-inspired shell
# --------------------------------------------------------------------------
# One persistent frame (not separate full-screen tabs):
#   - top menubar       : Data / Models / Machine Learning / Spatial & LiDAR
#   - left Datasets rail : upload + clickable list, sets the active dataset
#   - center canvas      : navset_hidden swapped by the menubar (current_view)
#   - right tools panel  : navset_hidden swapped in lockstep with the canvas
#   - bottom status bar  : active dataset + dimensions
# The menubar sets input$current_view; server.R nav_selects both navsets.
# ==========================================================================

# A top-menubar dropdown whose items set input$current_view on click.
.topMenu <- function(label, items) {
  tags$li(class = "nav-item dropdown",
    tags$a(class = "nav-link dropdown-toggle app-menu", href = "#", `data-bs-toggle` = "dropdown", role = "button", label),
    tags$ul(class = "dropdown-menu",
      lapply(items, function(it) {
        tags$li(tags$a(class = "dropdown-item", href = "#",
          onclick = sprintf("Shiny.setInputValue('current_view','%s',{priority:'event'});return false;", it[["value"]]),
          it[["label"]]))
      })
    )
  )
}

# A single (non-dropdown) menubar button.
.topItem <- function(label, value) {
  tags$li(class = "nav-item",
    tags$a(class = "nav-link app-menu", href = "#",
      onclick = sprintf("Shiny.setInputValue('current_view','%s',{priority:'event'});return false;", value),
      label))
}

# A hidden, switchable panel (one per view) used in both navsets.
.viewPanel <- function(value, ...) nav_panel(title = value, value = value, ...)

# Placeholder content for views not yet ported back.
.todo <- function(name) div(class = "p-5 text-center text-muted", h5(name), p("Coming back next — being ported into the new shell."))

ui <- page_fillable(
  theme = app_theme,
  padding = 0,
  gap = 0,

  tags$head(
    tags$style(HTML("
    html, body { height: 100%; }
    .app-shell { display: grid; grid-template-rows: auto 1fr auto; height: 100vh; }

    /* --- Top menubar --- */
    .app-topbar { background: #2e7d32; color: #fff; display: flex; align-items: center; gap: 6px; padding: 2px 12px; box-shadow: 0 1px 4px rgba(0,0,0,.15); }
    .app-topbar .brand { font-weight: 700; letter-spacing: .3px; margin-right: 10px; white-space: nowrap; }
    .app-topbar .navbar-nav, .app-topbar ul.nav { display: flex; flex-direction: row; margin: 0; padding: 0; list-style: none; }
    .app-topbar .nav-link.app-menu { color: #eafaef !important; padding: 4px 10px; font-size: 14px; cursor: pointer; }
    .app-topbar .nav-link.app-menu:hover { color: #fff !important; background: rgba(255,255,255,.12); border-radius: 4px; }
    .app-topbar .dropdown-item.active, .app-topbar .dropdown-item:active { background: #2e7d32; }

    /* --- Body: [left] [div] [canvas] [div] [right] -- resizable via CSS vars --- */
    .app-main { display: grid; --left-w: 240px; --right-w: 350px;
                grid-template-columns: var(--left-w) 5px minmax(0,1fr) 5px var(--right-w);
                min-height: 0; transition: grid-template-columns .12s ease; }
    .app-main.left-collapsed  { --left-w: 36px; }
    .app-main.right-collapsed { --right-w: 36px; }
    .app-left, .app-right { background: #f8faf8; overflow-y: auto; padding: 10px; position: relative; }
    .app-left  { border-right: 1px solid #dee2e6; }
    .app-right { border-left: 1px solid #dee2e6; }
    .app-center{ overflow: auto; padding: 10px; min-width: 0; }
    .app-left h6 { text-transform: uppercase; font-size: 11px; letter-spacing: .6px; color: #6c757d; }
    .app-main.left-collapsed .app-left .rail-body, .app-main.right-collapsed .app-right .rail-body { display: none; }
    .app-main.left-collapsed .app-left, .app-main.right-collapsed .app-right { padding: 8px 2px; overflow: hidden; }

    /* --- Resize dividers --- */
    .app-divider { cursor: col-resize; background: transparent; transition: background .1s; }
    .app-divider:hover, .app-divider.dragging { background: #4caf50; }
    .app-main.left-collapsed .app-divider.left, .app-main.right-collapsed .app-divider.right { pointer-events: none; }

    /* --- Collapse toggles (dock to icon strip) --- */
    .rail-toggle { border: none; background: transparent; color: #6c757d; cursor: pointer; font-size: 13px; padding: 0 4px; line-height: 1.4; }
    .rail-toggle:hover { color: #2e7d32; }
    .app-left  .rail-toggle { float: right; }
    .app-right .rail-toggle { float: left; }
    .rail-toggle .chev { display: inline-block; transition: transform .12s; }
    .app-main.left-collapsed  .app-left  .rail-toggle .chev,
    .app-main.right-collapsed .app-right .rail-toggle .chev { transform: rotate(180deg); }

    /* --- Datasets list --- */
    .ds-item { display: flex; align-items: center; gap: 6px; padding: 6px 8px; border-radius: 6px; cursor: pointer; font-size: 13px; }
    .ds-item:hover { background: #e8f5e9; }
    .ds-item.active { background: #2e7d32; color: #fff; }
    .ds-item .dot { width: 8px; height: 8px; border-radius: 50%; background: #4caf50; flex: 0 0 auto; }
    .ds-item.active .dot { background: #fff; }

    /* --- Status bar --- */
    .app-status { background: #eef3ee; border-top: 1px solid #dee2e6; font-size: 12px; color: #495057; display: flex; align-items: center; gap: 18px; padding: 3px 14px; }
    .app-status .sep { color: #adb5bd; }
    ")),
    tags$script(HTML("
      function toggleRail(side){
        var m = document.querySelector('.app-main');
        var cls = side==='left' ? 'left-collapsed' : 'right-collapsed';
        var v   = side==='left' ? '--left-w' : '--right-w';
        var def = side==='left' ? '240px' : '350px';
        if(m.classList.contains(cls)){
          m.classList.remove(cls);
          m.style.setProperty(v, m.dataset[side] || def);
        } else {
          m.dataset[side] = m.style.getPropertyValue(v) || def;
          m.classList.add(cls);
          m.style.setProperty(v, '36px');
        }
      }
      (function(){
        var dragging=null, startX=0, startW=0;
        document.addEventListener('mousedown', function(e){
          var d = e.target.closest('.app-divider'); if(!d) return;
          var m = document.querySelector('.app-main');
          dragging = d.classList.contains('left') ? 'left' : 'right';
          if(m.classList.contains(dragging+'-collapsed')){ dragging=null; return; }
          startX = e.clientX;
          var cur = m.style.getPropertyValue(dragging==='left'?'--left-w':'--right-w');
          startW = parseInt(cur) || (dragging==='left'?240:350);
          d.classList.add('dragging');
          document.body.style.userSelect='none'; document.body.style.cursor='col-resize';
          e.preventDefault();
        });
        document.addEventListener('mousemove', function(e){
          if(!dragging) return;
          var m = document.querySelector('.app-main');
          var dx = e.clientX - startX;
          var w = dragging==='left' ? startW+dx : startW-dx;
          w = Math.max(140, Math.min(window.innerWidth*0.5, w));
          m.style.setProperty(dragging==='left'?'--left-w':'--right-w', w+'px');
        });
        document.addEventListener('mouseup', function(){
          if(!dragging) return; dragging=null;
          document.querySelectorAll('.app-divider').forEach(function(d){ d.classList.remove('dragging'); });
          document.body.style.userSelect=''; document.body.style.cursor='';
          if(window.Shiny) window.dispatchEvent(new Event('resize'));
        });
      })();
    "))
  ),

  tags$div(class = "app-shell",

    # ---------- TOP MENUBAR ----------
    tags$div(class = "app-topbar",
      tags$span(class = "brand", "TerraTrack"),
      tags$ul(class = "nav",
        .topItem("Data", "data"),
        .topMenu("Models", list(
          list(value = "lm",       label = "Linear Regression"),
          list(value = "lme",      label = "Linear Mixed Effects (LME)"),
          list(value = "anova",    label = "ANOVA"),
          list(value = "logistic", label = "Logistic Regression")
        )),
        .topMenu("Machine Learning", list(
          list(value = "rf",             label = "Random Forest"),
          list(value = "da",             label = "Discriminant Analysis"),
          list(value = "clustering",     label = "Clustering Analysis"),
          list(value = "classification", label = "Classification")
        )),
        .topMenu("Spatial Analysis", list(
          list(value = "rs_search",  label = "Satellite Search & Download"),
          list(value = "raster",     label = "Raster Analysis"),
          list(value = "pointcloud", label = "Point Cloud & 3D Viewer"),
          list(value = "chm_itd",    label = "CHM & Individual Tree Detection"),
          list(value = "metrics",    label = "Metric Extraction & Evaluation")
        ))
      )
    ),

    # ---------- BODY ----------
    tags$div(class = "app-main",

      # Left: Datasets rail (global upload + active-dataset list)
      tags$div(class = "app-left",
        tags$button(class = "rail-toggle", onclick = "toggleRail('left')", title = "Collapse / expand", HTML('<span class="chev">&#9664;</span>')),
        tags$div(class = "rail-body",
          tags$h6("Datasets"),
          fileInput("upload_files", NULL, multiple = TRUE, accept = c(".csv", ".txt", ".xlsx", ".xls"), buttonLabel = "Add Data", placeholder = "no file"),
          uiOutput("datasets_list"),
          tags$hr(),
          actionButton("view_data", "View Data Table", class = "btn-sm btn-outline-success w-100", icon = icon("table"))
        )
      ),

      # Drag handle between left rail and canvas
      tags$div(class = "app-divider left"),

      # Center: canvas, swapped by current_view
      tags$div(class = "app-center",
        navset_hidden(id = "canvas_view",
          .viewPanel("data", dataCanvasUI("data")),
          .viewPanel("lm", lmCanvasUI("lm")),
          .viewPanel("lme", lmeCanvasUI("lme")),
          .viewPanel("anova", anovaCanvasUI("anova")),
          .viewPanel("logistic", logisticCanvasUI("logistic")),
          .viewPanel("rf", rfCanvasUI("rf")),
          .viewPanel("da", daCanvasUI("da")),
          .viewPanel("clustering", clusteringCanvasUI("clustering")),
          .viewPanel("classification", classificationCanvasUI("classification")),
          .viewPanel("raster",     rasterCanvasUI("raster")),
          .viewPanel("rs_search",  rsSearchCanvasUI("rs_search")),
          .viewPanel("pointcloud", lidarPointcloudCanvasUI("lidar")),
          .viewPanel("chm_itd", lidarChmCanvasUI("lidar")),
          .viewPanel("metrics", lidarMetricsCanvasUI("lidar"))
        )
      ),

      # Drag handle between canvas and right tools
      tags$div(class = "app-divider right"),

      # Right: contextual tools/params, swapped in lockstep with the canvas
      tags$div(class = "app-right",
        tags$button(class = "rail-toggle", onclick = "toggleRail('right')", title = "Collapse / expand", HTML('<span class="chev">&#9654;</span>')),
        tags$div(class = "rail-body",
        navset_hidden(id = "tools_view",
          .viewPanel("data", tags$div(tags$h6(class = "text-uppercase text-muted small", "Processing Toolbox"), dataToolsUI("data"))),
          .viewPanel("lm", lmToolsUI("lm")),
          .viewPanel("lme", lmeToolsUI("lme")),
          .viewPanel("anova", anovaToolsUI("anova")),
          .viewPanel("logistic", logisticToolsUI("logistic")),
          .viewPanel("rf", rfToolsUI("rf")),
          .viewPanel("da", daToolsUI("da")),
          .viewPanel("clustering", clusteringToolsUI("clustering")),
          .viewPanel("classification", classificationToolsUI("classification")),
          .viewPanel("raster",     rasterToolsUI("raster")),
          .viewPanel("rs_search",  rsSearchToolsUI("rs_search")),
          .viewPanel("pointcloud", lidarPointcloudToolsUI("lidar")),
          .viewPanel("chm_itd", lidarChmToolsUI("lidar")),
          .viewPanel("metrics", lidarMetricsToolsUI("lidar"))
        )
        )
      )
    ),

    # ---------- STATUS BAR ----------
    tags$div(class = "app-status",
      tags$span("Active: "), tags$strong(textOutput("status_active", inline = TRUE)),
      tags$span(class = "sep", "|"),
      textOutput("status_dims", inline = TRUE)
    )
  ),

  # ---------- AI CO-PILOT (floating, app-level) ----------
  chatUI("chat")
)
