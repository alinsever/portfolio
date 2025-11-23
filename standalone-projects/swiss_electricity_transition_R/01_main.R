library(quarto)   
library(rmarkdown)

# === STEP 1: Run data cleaning script ===
message("Running data cleaning...")
source("02_d_cleaning.R")

# === STEP 2: Run EDA script ===
message("Running EDA...")
source("03_EDA.R")

# === STEP 3: Render energy and radiation report ===
message("Rendering project.qmd...")
quarto::quarto_render("Swiss_Electricity_Transition_Project.qmd")

# === STEP 4: Open the HTML report in default browser ===
browseURL("Swiss Electricity Under Transition.html")

message("All tasks completed.")