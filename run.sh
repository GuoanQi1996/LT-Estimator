#PBS -N starShip_linear
#PBS -l nodes=1:ppn=20
#PBS -l mem=20G
#PBS -l walltime=6666:00:00
#PBS -j oe
cd $PBS_O_WORKDIR

Rscript LTEstimator.R --bfile bedFile --distribution linear --pheno allPhe.phe --prevalence continuousPhe.prevalence --out Related_81
