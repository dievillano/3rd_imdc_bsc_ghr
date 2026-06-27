#!/bin/bash
#SBATCH --job-name=IMDC_CV
#SBATCH --output=logs/IMDC_CV_%A_%a.out
#SBATCH --error=logs/IMDC_CV_%A_%a.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --time=08:00:00
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=diego.villa@bsc.es

module use /gpfs/projects/bsc32/software/rhel/9.2/modules/all
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3
module load R-bundle-CRAN/2023.12-foss-2023b

cd /gpfs/scratch/bsc32/bsc428966/imdc/sprint2026 || exit 1

Rscript --vanilla dengue/state/04-run_cv_fit.R "$SPLIT_ID" "$MODEL_ID"