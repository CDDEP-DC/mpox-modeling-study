# 📍 About
This GitHub repository accompanies the paper by Kipshidze, Klein, and Yang titled "Understanding the drivers of continued mpox transmission in the United States: a modeling study." The code and data files will recireate the simulations, analyses, and figures for this paper.

# 🗂️ General Structure
| Folder | Description |
| -- | -- |
| Data| Folders containing all data oon vaccination and incidence used for model fitting |
| Output | Output files, including figures, model runs, and data tables |
| R | All relevant script files and network objects |

# 📌 How to recreate our analyses
We reccomend that these models are run on a remote server. We utilized an [c7i.48xlarge](https://aws.amazon.com/ec2/instance-types/c7i/) instance on Amazon Web Serivces (AWS) on a Linux platform with base R and associated packages. The average run-time for our model was approximatley 4-7 minutes per stochastic run when parallized. 

## 1️⃣ Scaling Data
We first had to scale our vaccination and incidence data, which are located in R / Scripts with the pre_fitting_... prefix. 

## 2️⃣ Running the Models
Initliaze the shell scripts to run the models (run_scenarios_.. prefix). 

## 3️⃣ Generate analyses
Run the analyses with the post_ prefixes and generate the relevant plots with the plotting_ prefix.

# 📝 Reccomended Citation
Kipshidze N., Klein E., Wan Y. (2023). Understanding the drivers of continued mpox transmission in the United States: a modeling study. GitHub. [https://github.com/CDDEP-DC/mpox-modeling-study](https://github.com/CDDEP-DC/mpox-modeling-study)
