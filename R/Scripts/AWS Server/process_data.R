# Extracting Data

# Load necessary libraries
library(data.table)
library(parallel)
library(zoo)
library(here)

# Get arguments from the shell script
args <- commandArgs(trailingOnly = TRUE)

# Extract city and scenario from command-line arguments
city <- args[1]
scenario <- args[2]

# Convert underscores back to spaces for folder naming
city <- gsub("_", " ", city)
scenario <- gsub("_", " ", scenario)

# Process the Files ------------------------------------------------------------

# Function to perform file processing
process_file <- function(file_path) {
  
  newEnv <- new.env()
  load(file_path, envir = newEnv) 
  
  object_name <- gsub(".RData", "", basename(file_path))
  sim_data <- get(object_name, envir = newEnv)  # Dynamic lookup
  
  # Extract required dataframes
  extracted <- list(
    num.wane.1 = sim_data[["epi"]][["num.wane.1"]],
    num.wane.2 = sim_data[["epi"]][["num.wane.2"]],
    cuml.v1 = sim_data[["epi"]][["cuml.v1"]],
    cuml.v2 = sim_data[["epi"]][["cuml.v2"]],
    cuml.cases = sim_data[["epi"]][["cuml.cases"]],
    cuml.infs = sim_data[["epi"]][["cuml.infs"]],
    se.flow = sim_data[["epi"]][["se.flow"]],
    ei.flow = sim_data[["epi"]][["ei.flow"]],
    ea.flow = sim_data[["epi"]][["ea.flow"]],
    ir.flow = sim_data[["epi"]][["ir.flow"]],
    ar.flow = sim_data[["epi"]][["ar.flow"]],
    rs.flow = sim_data[["epi"]][["rs.flow"]],
    num.dose1 = sim_data[["epi"]][["num.dose1"]],
    num.dose2 = sim_data[["epi"]][["num.dose2"]]
  )
  
  # Remove runs where cuml.cases < 20
  indices_to_keep <- which(colSums(extracted$cuml.cases, na.rm=TRUE) >= 20)
  
  # Filter all dataframes
  extracted <- lapply(extracted, function(df) df[, indices_to_keep, drop=FALSE])
  
  return(extracted)
}

# Function to calculate summary statistics
calculate_stats <- function(df) {
  
  stats <- data.frame(
    median = apply(df, 1, median, na.rm = TRUE),
    mean = apply(df, 1, mean, na.rm = TRUE),
    sd = apply(df, 1, sd, na.rm = TRUE),
    min = apply(df, 1, min, na.rm = TRUE),
    max = apply(df, 1, max, na.rm = TRUE),
    perc2.5 = apply(df, 1, quantile, probs = 0.025, na.rm = TRUE),
    perc97.5 = apply(df, 1, quantile, probs = 0.975, na.rm = TRUE),
    perc25 = apply(df, 1, quantile, probs = 0.25, na.rm = TRUE),
    perc75 = apply(df, 1, quantile, probs = 0.75, na.rm = TRUE)
  )
  
  stats$date <- seq(from = as.Date("2022-05-01"), by = "day", length.out = nrow(stats))
  
  stats$median_7day_ma <- rollmean(stats$median, 7, align = "right", fill = NA)
  
  return(stats)
}

# Set up parallel processing
num_cores <- 100 # or set to a fixed number
cl <- makeCluster(num_cores)
clusterExport(cl, "calculate_stats")

# Export the necessary functions and libraries to the cluster nodes
clusterEvalQ(cl, {
  library(data.table)
  library(zoo)
  library(here)
})


# Path to the folder containing all files
folder_path <- here("mpox", "Output", city, scenario)

# List all files in the folder
files <- list.files(folder_path, full.names = TRUE)

# Process all files in parallel
processed_files <- parLapply(cl, files, process_file)

# Abbreviate the scenario and city names
abbreviate_name <- function(name) {
  abbr <- substr(name, 1, 3)  # Taking first three letters as an example
  return(tolower(abbr))
}

scenario_abbr <- abbreviate_name(scenario)
city_abbr <- abbreviate_name(city)

# Use the abbreviated names in the output filenames
output_name <- paste0(scenario_abbr, "_", city_abbr, "_processed_files.RData")

names(processed_files) <- sapply(files, function(x) {
  name_parts <- strsplit(gsub(".RData", "", basename(x)), "_")
  parameter_set <- tail(name_parts[[1]], -2)  # Skip the first two elements ("sim" and "chi" in this example)
  paste0(parameter_set, collapse = "_")
})

# Debugging print statements
print(paste0("Debug: scenario_abbr = ", scenario_abbr))
print(paste0("Debug: city_abbr = ", city_abbr))

# Explicitly setting the environment in `assign`
assign_name <- paste0("processed_", scenario_abbr, "_", city_abbr)
assign(assign_name, processed_files, envir = .GlobalEnv)

# Debugging print statement to verify that the new variable exists
print(paste0("Debug: Does the variable exist? ", exists(assign_name, envir = .GlobalEnv)))

save(processed_files, file = here("mpox","Output","Processed Files", output_name))

# Calculate Summary Stats ----------------------------------------------------
# Calculate statistics in parallel
stats_files <- parLapply(cl, processed_files, function(file_data) {
  lapply(file_data, calculate_stats)
})

output_name_stats <- paste0(scenario_abbr, "_", city_abbr, "_stats_files.RData")

names(stats_files) <- sapply(files, function(x) {
  name_parts <- strsplit(gsub(".RData", "", basename(x)), "_")
  parameter_set <- tail(name_parts[[1]], -2)  # Skip the first two elements ("sim" and "chi" in this example)
  paste0(parameter_set, collapse = "_")
})

# Debugging print statements
print(paste0("Debug: scenario_abbr = ", scenario_abbr))
print(paste0("Debug: city_abbr = ", city_abbr))

# Explicitly setting the environment in `assign`
assign_name <- paste0("stats_", scenario_abbr, "_", city_abbr)
assign(assign_name, stats_files, envir = .GlobalEnv)

# Debugging print statement to verify that the new variable exists
print(paste0("Debug: Does the variable exist? ", exists(assign_name, envir = .GlobalEnv)))

save(stats_files, file = here("mpox","Output","Processed Files", output_name_stats))

# Stop the cluster
stopCluster(cl)
gc()