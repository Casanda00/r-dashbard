# ==========================================================================
# gee_dictionary.R  --  popular Google Earth Engine commands, button-driven
# --------------------------------------------------------------------------
# A data-driven catalogue: the GEE module turns each entry into a button (+ any
# parameter inputs). NO user code — clicking a button runs its `run()` against
# the current pipeline object. Built on rgee (https://r-spatial.github.io/rgee/).
#
# Each command:
#   id     : unique key (also the button inputId, namespaced by the module)
#   label  : button text
#   group  : section header in the toolbox
#   needs  : "none" | "collection" | "image" | "aoi"  (precondition hint)
#   params : list of input defs -> auto-rendered widgets; each is
#            list(name=, label=, type = "text"|"number"|"date"|"select", default=, choices=)
#   run    : function(state, p, aoi) -> a NEW ee object, OR performs a side effect
#            (display/export). `state` = current pipeline ee object; `p` = named
#            list of this command's parameter values; `aoi` = current ee$Geometry.
#
# The run() bodies reference rgee globals (`ee`, `Map`, ee_as_raster, ...). They
# are NEVER evaluated until a button is clicked, so this file sources fine even
# when rgee is not installed.
# ==========================================================================

gee_dictionary <- function() {
  list(

    # ---------------- 1. Load collection / image ----------------
    list(id = "load_s2", label = "Sentinel-2 SR (Harmonized)", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")),
    list(id = "load_l8", label = "Landsat 8 L2", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")),
    list(id = "load_s1", label = "Sentinel-1 GRD (radar)", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$ImageCollection("COPERNICUS/S1_GRD")),
    list(id = "load_modis_ndvi", label = "MODIS NDVI 16-day", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$ImageCollection("MODIS/061/MOD13Q1")),
    list(id = "load_srtm", label = "SRTM DEM (30 m)", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$Image("USGS/SRTMGL1_003")),
    list(id = "load_worldcover", label = "ESA WorldCover (land cover)", group = "1. Load", needs = "none", params = list(),
         run = function(state, p, aoi) ee$ImageCollection("ESA/WorldCover/v200")$first()),

    # ---------------- 2. Filter collection ----------------
    list(id = "filter_date", label = "Filter date range", group = "2. Filter", needs = "collection",
         params = list(list(name = "start", label = "Start", type = "date", default = "2023-06-01"),
                       list(name = "end",   label = "End",   type = "date", default = "2023-09-01")),
         run = function(state, p, aoi) state$filterDate(p$start, p$end)),
    list(id = "filter_bounds", label = "Filter to AOI", group = "2. Filter", needs = "aoi",
         params = list(),
         run = function(state, p, aoi) state$filterBounds(aoi)),
    list(id = "filter_cloud", label = "Filter cloud cover <", group = "2. Filter", needs = "collection",
         params = list(list(name = "pct", label = "Max cloud %", type = "number", default = 20)),
         run = function(state, p, aoi) state$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", p$pct))),

    # ---------------- 3. Composite (collection -> image) ----------------
    list(id = "comp_median", label = "Median composite", group = "3. Composite", needs = "collection", params = list(),
         run = function(state, p, aoi) state$median()),
    list(id = "comp_mean", label = "Mean composite", group = "3. Composite", needs = "collection", params = list(),
         run = function(state, p, aoi) state$mean()),
    list(id = "comp_mosaic", label = "Mosaic (most recent)", group = "3. Composite", needs = "collection", params = list(),
         run = function(state, p, aoi) state$mosaic()),

    # ---------------- 4. Indices / bands (on an image) ----------------
    list(id = "idx_ndvi", label = "Add NDVI", group = "4. Indices", needs = "image",
         params = list(list(name = "nir", label = "NIR band", type = "text", default = "B8"),
                       list(name = "red", label = "Red band", type = "text", default = "B4")),
         run = function(state, p, aoi) state$normalizedDifference(c(p$nir, p$red))$rename("NDVI")),
    list(id = "idx_ndwi", label = "Add NDWI", group = "4. Indices", needs = "image",
         params = list(list(name = "green", label = "Green band", type = "text", default = "B3"),
                       list(name = "nir",   label = "NIR band",   type = "text", default = "B8")),
         run = function(state, p, aoi) state$normalizedDifference(c(p$green, p$nir))$rename("NDWI")),
    list(id = "select_bands", label = "Select bands", group = "4. Indices", needs = "image",
         params = list(list(name = "bands", label = "Bands (comma-sep)", type = "text", default = "B4,B3,B2")),
         run = function(state, p, aoi) state$select(trimws(strsplit(p$bands, ",")[[1]]))),

    # ---------------- 5. Terrain (on a DEM image) ----------------
    list(id = "ter_slope", label = "Slope", group = "5. Terrain", needs = "image", params = list(),
         run = function(state, p, aoi) ee$Terrain$slope(state)),
    list(id = "ter_hillshade", label = "Hillshade", group = "5. Terrain", needs = "image", params = list(),
         run = function(state, p, aoi) ee$Terrain$hillshade(state)),

    # ---------------- 6. Clip ----------------
    list(id = "clip_aoi", label = "Clip to AOI", group = "6. Clip", needs = "aoi", params = list(),
         run = function(state, p, aoi) state$clip(aoi)),

    # ---------------- 7. Display (side effects on the Map) ----------------
    list(id = "disp_truecolor", label = "Add: true colour", group = "7. Display", needs = "image",
         params = list(list(name = "max", label = "Stretch max", type = "number", default = 3000)),
         run = function(state, p, aoi) { Map$addLayer(state, list(bands = c("B4","B3","B2"), min = 0, max = p$max), "True colour"); state }),
    list(id = "disp_ndvi", label = "Add: NDVI palette", group = "7. Display", needs = "image", params = list(),
         run = function(state, p, aoi) { Map$addLayer(state, list(min = -0.2, max = 0.9, palette = c("brown","yellow","green")), "NDVI"); state }),
    list(id = "disp_center", label = "Center map on AOI", group = "7. Display", needs = "aoi", params = list(),
         run = function(state, p, aoi) { Map$centerObject(aoi, zoom = 11); state }),

    # ---------------- 8. Extract / export ----------------
    list(id = "ex_extract_aoi", label = "Extract mean over AOI", group = "8. Extract / export", needs = "aoi",
         params = list(),
         run = function(state, p, aoi) ee_extract(x = state, y = aoi, fun = ee$Reducer$mean(), sf = TRUE)),
    list(id = "ex_to_raster", label = "Download as raster (AOI)", group = "8. Extract / export", needs = "aoi",
         params = list(list(name = "scale", label = "Scale (m)", type = "number", default = 30)),
         run = function(state, p, aoi) ee_as_raster(image = state, region = aoi, scale = p$scale)),
    list(id = "ex_to_drive", label = "Export image to Google Drive", group = "8. Extract / export", needs = "aoi",
         params = list(list(name = "name", label = "File name", type = "text", default = "gee_export"),
                       list(name = "scale", label = "Scale (m)", type = "number", default = 30)),
         run = function(state, p, aoi) {
           task <- ee$batch$Export$image$toDrive(image = state, description = p$name, scale = p$scale, region = aoi)
           task$start(); task
         })
  )
}
