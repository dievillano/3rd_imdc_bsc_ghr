#!/bin/bash
#SBATCH --job-name=IMDC_SCORE
#SBATCH --output=logs/IMDC_SCORE_%x_%j.out
#SBATCH --error=logs/IMDC_SCORE_%x_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --time=02:00:00

module use /gpfs/projects/bsc32/software/rhel/9.2/modules/all
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3
module load R-bundle-CRAN/2023.12-foss-2023b

cd /gpfs/scratch/bsc32/bsc428966/imdc/sprint2026 || exit 1

Rscript --vanilla dengue/state/05-score_state_predictions.R "$INPUT_FILE"