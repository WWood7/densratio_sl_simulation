# !/bin/bash
# chmod u+x sim.sh
# ./sim.sh sim.R sim

analysis=$2      # change for every analysis you run (2nd arg)
maildom='@emory.edu'   # your email domain (for receiving error messages)
myscratch="/home/wwu227/thesis/lmtp/scratch"  # location of your persistent scratch dir
resultdir="/home/wwu227/thesis/lmtp/scratch/out"  # This is a folder in permanent storage
script=$1      # your code as (R or Python) script (1st arg)
max_jobs=100  # max number of jobs 10
loops=5   # total number of jobs 2


username=$(id -nu)

# if scratch directory doesn't exist, make it
[ ! -d ${myscratch} ] && mkdir ${myscratch}
[ ! -d ${myscratch}/out ] && mkdir ${myscratch}/out
[ ! -d ${myscratch}/err ] && mkdir ${myscratch}/err

# submit first batch of jobs
for i in $(seq 1 ${loops}); do
	echo "#!/bin/bash" > script$i.sh
	echo "#SBATCH --array=1-$max_jobs" >> script$i.sh
	echo "#SBATCH --partition=week-long-cpu" >> script$i.sh
	echo "#SBATCH --error=${myscratch}/err/${analysis}$i.err" >> script$i.sh
	echo "#SBATCH --output=${myscratch}/out/${analysis}$i.out" >> script$i.sh

    echo "source ~/miniconda3/etc/profile.d/conda.sh" >> script$i.sh
	echo "conda activate rproj" >> script$i.sh

	echo "Rscript ${script} $i" >> script$i.sh
    echo "#SBATCH --mail-user=$username$maildom" >> script$i.sh
	echo "#SBATCH --mail-type=ALL" >> script$i.sh

    sbatch script$i.sh
done
