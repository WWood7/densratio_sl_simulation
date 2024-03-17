#!/bin/bash
#SBATCH --job-name=mediation1_diagnostic  # Job name
#SBATCH --output=/home/wwu227/thesis/mediation1_diagnostic/output_%j.txt  # Standard output; %j is replaced with the job ID
#SBATCH --error=/home/wwu227/thesis/mediation1_diagnostic/error_%j.txt    # Standard error; %j is replaced with the job ID
#SBATCH --ntasks=1
#SBATCH --partition=benkeser
# Activate conda environment
source ~/miniconda3/etc/profile.d/conda.sh
conda activate rproj

Rscript /home/wwu227/thesis/mediation1_diagnostic/mediation1_diagnostic.R
