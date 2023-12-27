#!/bin/bash

# Start the first job/script
nohup /home/ec2-user/mpox/R/Scripts/run_first_set.sh > /home/ec2-user/mpox/R/Scripts/script_first_set.log 2>&1 &
pid1=$!

# Wait for all remaining jobs to complete
for pid in $pid1; do
  while kill -0 $pid 2>/dev/null; do
    sleep 10  # Wait for 10 seconds before checking again
  done
done

# Shutdown the instance
sudo shutdown -h now