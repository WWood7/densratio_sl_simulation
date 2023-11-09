#SBATCH --array=1-10
#SBATCH --partition=day-long-cpu
#SBATCH --error=/home/wwu227/thesis/oracle/scratch/err/oracle3.err
#SBATCH --output=/home/wwu227/thesis/oracle/scratch/out/oracle3.out
source ~/miniconda3/etc/profile.d/conda.sh
conda activate rproj
Rscript sim_oracle.R 3
#SBATCH --mail-user=wwu227@emory.edu
#SBATCH --mail-type=ALL
