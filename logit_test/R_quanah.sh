#!/bin/bash
#SBATCH --chdir=./
#SBATCH --job-name=logit_test
#SBATCH --output=./report/report_%j.txt
#SBATCB –-error=./report/err_%j.txt
#SBATCH --partition quanah
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=10:00:00


cd /home/ruiqliu/SGD_LEC/logit_test

#Load the latest version of the R language - compiled using the Intel compilers.
module load intel R

#Allow R to perform some automatic parallelization.
#	MKL_NUM_THREADS - The maximum number of threads you want R to spawn on your behalf.
#	$NSLOTS - This will be replaced by the number of slots you request in yout parallel environment.
#		Example:  -pe sm 36 -> $NSLOTS=36.
#export MKL_NUM_THREADS=$NSLOTS

#Run the example R script using the Rscript application.
#for t in 0.005 0.01 0.015 0.02 0.025
#for n in 200 500 1000 2000 5000 10000

RandomSeed=$1

for n in  100000 200000 500000 1000000
do
  for r in 0
  do
    Rscript ./R_main.R $n $r $RandomSeed;
  done
done



