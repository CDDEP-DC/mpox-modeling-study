#!/bin/bash

mkdir -p /home/ec2-user/mpox/Output/Processed\ Files/
chmod 777 /home/ec2-user/mpox/Output/Processed\ Files/ # Caution: This gives everyone read-write-execute permission

# Array of cities
cities=("New_York_City")

# Array of scenarios
scenarios=("Asymptomatic_Scenario" "Reinfection_Scenario" "Vaccine_Waning_Scenario")

# Loop through all combinations of cities and scenarios
for city in "${cities[@]}"; do
  for scenario in "${scenarios[@]}"; do
    cd /home/ec2-user
    Rscript /home/ec2-user/mpox/R/Scripts/process_data_non_parallel.R "$city" "$scenario"
  done
done
