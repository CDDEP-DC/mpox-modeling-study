#!/bin/bash

# Log file to keep track of iterations
log_file="/home/ec2-user/mpox/R/Scripts/logfile_second_set.txt"

# Declare an array of cities to loop through with corresponding folder names
declare -A city_folders=( ["nyc"]="New York City" )

# Function to log messages with a timestamp
log_with_timestamp() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a $log_file
}

# Function to check if an output file exists
output_file_exists() {
    local city="$1"
    local prop_reinfection=$(bc -l <<< "1 / ($2 * 30)")  # Calculate 1 / (value * 30)
    local rounded_prop_reinfection=$(printf "%.6f" "$prop_reinfection")  # Round to 6 digits
    local full_city_name="${city_folders[$city]}"
    local prob_infection="$3"
    local output_file="/home/ec2-user/mpox/Output/${full_city_name}/Reinfection Scenario/sim_${city}_as_${rounded_prop_reinfection}_inf_${prob_infection}.RData"
    
    if [ -f "$output_file" ]; then
        return 0  # Output file exists
    else
        return 1  # Output file does not exist
    fi
}

# Outer loop for cities
for city in "${!city_folders[@]}"
do
  log_with_timestamp "Running simulations for city: $city"

  # Nested loop for prop_asymp
  for prop_reinfection in $(seq 6 16)
  do

    # Nested loop for prob_infection
    for prob_infection in $(seq 0.3 0.1 0.9)
    do
    if ! output_file_exists "$city" "$prop_reinfection" "$prob_infection"; then
      log_with_timestamp "Running R script for $city with reinfection time=$prop_reinfection months and prob_infection=$prob_infection"
        cd /home/ec2-user
        Rscript /home/ec2-user/mpox/R/Scripts/run_reinfect.R $city $prop_reinfection $prob_infection
      else
        log_with_timestamp "Skipping $city with reinfection time=$prop_reinfection and prob_infection=$prob_infection (already processed)"
      fi   
    done
  done
done
