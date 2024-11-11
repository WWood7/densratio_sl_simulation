#!/bin/bash
#SBATCH --array=1-100
#SBATCH --partition=day-long-cpu
#SBATCH --error=/home/wwu227/thesis/mediation9/scratch/err/sim2.err
#SBATCH --output=/home/wwu227/thesis/mediation9/scratch/out/sim2.out
source ~/miniconda3/etc/profile.d/conda.sh
conda activate rproj
Rscript mediation9.R 2
#SBATCH --mail-user=wwu227@emory.edu
#SBATCH --mail-type=ALL
