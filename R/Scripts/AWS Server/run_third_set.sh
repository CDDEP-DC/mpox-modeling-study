#!/bin/bash

# Log file to keep track of iterations
log_file="/home/ec2-user/mpox/R/Scripts/logfile_third_set.txt"

# Declare an array of cities to loop through with corresponding folder names
declare -A city_folders=( ["nyc"]="New York City" )

# Function to log messages with a timestamp
log_with_timestamp() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a $log_file
}

# Outer loop for cities
for city in "${!city_folders[@]}"
do
  log_with_timestamp "Running simulations for city: $city"

  # Nested loop for wane_partial
  for wane_partial in $(seq 6 3 16)  # Sequence from 6 to 16 with step 3
  do

    # Nested loop for wane_full
    for wane_full in $(seq 6 3 16)  # Sequence from 6 to 16 with step 3
    do
      if [ "$wane_partial" -ge "$wane_full" ]; then
        continue  # Skip the loop iteration if wane_partial >= wane_full
      fi

      # Nested loop for prob_infection
      for prob_infection in $(seq 0.3 0.1 0.9)  # Sequence from 0.3 to 0.9 with step 0.1
      do
        log_with_timestamp "Running R script for $city with wane_partial=$wane_partial, wane_full=$wane_full and prob_infection=$prob_infection"
        cd /home/ec2-user
        Rscript /home/ec2-user/mpox/R/Scripts/run_vax_wane.R $city $wane_partial $wane_full $prob_infection  # Replace with your R script's actual name
      done
    done
  done
done
