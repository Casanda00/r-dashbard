## lib
library(shiny)
library(bslib)
library(readxl)
library(MASS)
library(klaR)
library(MVN)
library(car)
library(biotools)
library(kernlab)
library(randomForest)

# --- SOURCE CUSTOM FUNCTIONS ---
source("clean_vmi.r")

# --- SOURCE SHINY MODULES (one file per model screen) ---
source("mod_linear_regression.R")

# Datasets are loaded dynamically from the upload controls in server.R.
# Do not require local Excel files at app startup.

# Universal categories array for dropdown matching
all_categories <- c(
  "land_class", "Soil_add", "Soil_ch", "habitat_type", 
  "Mixed", "Nutrient_class", "Nutrient_add", "Soiltype2", 
  "Texture", "organic_quality", "Soil_Depth", "Stones", "Ditches"
)

# --- INITIALIZE FEEDBACK LOG ---
comment_file <- "supervisor_comments.csv"

# If the file doesn't exist yet, create it with blank headers
if (!file.exists(comment_file)) {
  write.csv(data.frame(Timestamp = character(), Comment = character(), stringsAsFactors = FALSE), 
            file = comment_file, row.names = FALSE)
}