#!/bin/bash
#SBATCH -A CMS
#SBATCH -t 10:00:00
#SBATCH -N 1
#SBATCH -p shared
#SBATCH -n 1

#Set up your environment you wish to run in with module commands.
#module purge
module load R/3.2.0
module load gcc/5.2.0

#Actually codes starts here
now=$(date)
echo "Current time : $now"

export BOOSTROOT=/pic/projects/GCAM/GCAM-libraries/include
export BOOSTLIB=/pic/projects/GCAM/GCAM-libraries/lib/boost

Rscript /pic/projects/GCAM/Dorheim/CMS/hector-SA-npar/code/part_1/B.ini_generation_and_pic_run.R --nosave --no-restore

now=$(date)
echo "Current time : $now"
