#!/bin/bash

# Start the first job/script
nohup /home/ec2-user/mpox/R/Scripts/run_first_set.sh > /home/ec2-user/mpox/R/Scripts/script_first_set.log 2>&1 &
pid1=$!

# Start the second job/script
nohup /home/ec2-user/mpox/R/Scripts/run_second_set.sh > /home/ec2-user/mpox/R/Scripts/script_second_set.log 2>&1 &
pid2=$!

# Manually wait for any of the two initial jobs to complete
while true; do
  if ! kill -0 $pid1 2>/dev/null; then
    echo "First script finished"
    break
  fi
  if ! kill -0 $pid2 2>/dev/null; then
    echo "Second script finished"
    break
  fi
  sleep 10  # Wait for 10 seconds before checking again
done

# Start the third job/script after one of the initial scripts has finished
nohup /home/ec2-user/mpox/R/Scripts/run_third_set.sh > /home/ec2-user/mpox/R/Scripts/script_third_set.log 2>&1 &
pid3=$!

# Wait for all remaining jobs to complete
for pid in $pid1 $pid2 $pid3; do
  while kill -0 $pid 2>/dev/null; do
    sleep 10  # Wait for 10 seconds before checking again
  done
done

# Shutdown the instance
sudo shutdown -h now