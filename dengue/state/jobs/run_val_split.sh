#!/bin/bash
#SBATCH --job-name=IMDC_VAL_%a
#SBATCH --output=logs/IMDC_VAL_%A_%a.out
#SBATCH --error=logs/IMDC_VAL_%A_%a.err
#SBATCH --cpus-per-task=8
#SBATCH --qos=gp_bsces
#SBATCH --account=bsc32
#SBATCH --time=24:00:00
#SBATCH --array=1-4

set -euo pipefail

mkdir -p logs

module purge
module use /gpfs/projects/bsc32/software/rhel/9.2/modules/all
module load R-bundle-Bioconductor/3.18-foss-2023b-R-4.3.3
module load R-bundle-CRAN/2023.12-foss-2023b

echo "Running validation split ${SLURM_ARRAY_TASK_ID}"
echo "Host: $(hostname)"
echo "Start: $(date)"

Rscript --vanilla dengue/state/08-validation_forecasts.R "${SLURM_ARRAY_TASK_ID}"

echo "End: $(date)"