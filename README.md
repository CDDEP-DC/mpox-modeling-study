# 📍 About
This GitHub repository accompanies the paper by Kipshidze, Klein, and Yang titled "Understanding the drivers of continued mpox transmission in the United States: a modeling study." The code and data files will recireate the simulations, analyses, and figures for this paper.

# 📌 How to recreate our analyses
We reccomend that these models are run on a remote server. We utilized an [c7i.48xlarge](https://aws.amazon.com/ec2/instance-types/c7i/) instance on Amazon Web Serivces (AWS) on a Linux platform with base R and associated packages. The average run-time for our model was approximatley 4-7 minutes per stochastic run when parallized. 

# 1️⃣ Model Fitting
You will need to set-up a shell script to run and pass arguments to the `run_LHS sims.R` script.
After running the models you will then use `post_estimating fit.R` to compute the negative-log likelihood  of each individual run, which is compared to the observed data. This script is set-up so that it can be pointed to the saved runs and do the rest for you. Finally, the `post_assess fit.R` will take the estimated NLL and filter down to the top 10% of all runs for each city and plot the fitted runs and distributions of the free parameters. Finally the script `post_estim shape params.R` will estimate the shape parameters.

# 1️⃣ Running Different Mechanisms
You will need to set-up a shell script to pass the different arguments of applicable mechanisms, the arguments that can be passed are listed below in the table. 

| Argument | Description |
| -- | -- | 
| asymp_switch | Whether underdetection alone is enabled, logical T/F |
| reduce_asymp | Whether underdetection should be reduced by some proportion, logcial T/F |
| reduce_asymp_timing | If reduce_asymp is true, you will need to pass a timing when this reduction should be initiated. |
| reinfection | Whether reinfections should be enabled, logical T/F | 
| reinfection_time | If reinffections are enabled, the timing in months. |
| vaxwane | Whether vaccine-waning is enabled, logical T/F. |
| vaxwane_p | The number of months to median time for waning, partial vaccination |
| vaxwane_f | The number of months to median time for waning, full vaccination |
| post_pride_seeds | Whether additional seeding should be enabled post-pride 2022. |
| mech_name | A name for the mechanism being evaluated. | 

# 3️⃣ Collapsing Runs and Manuscript Figures
Each mechanism was run 1,000 times for each city. To imporve efficiecny, we sent these runs across 90+ nodes on AWS, meaning each node saves its own .Rdata file. We created several scripts that read in all the files and collapses and summarizes the runs into the final figures seen in our manuscript. 


# 📝 Reccomended Citation
Kipshidze N., Klein E., Wan Y. (2023). Understanding the drivers of continued mpox transmission in the United States: a modeling study. GitHub. [https://github.com/CDDEP-DC/mpox-modeling-study](https://github.com/CDDEP-DC/mpox-modeling-study)
