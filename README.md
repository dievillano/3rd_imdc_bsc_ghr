<a href='https://www.bsc.es/es'><img src="https://www.cmb.cat/wp-content/uploads/2014/01/BSC-Logo.jpg" align="right" height="100" width="100"/></a>

# Global Health Resilience model for the 3rd Infodengue-Mosqlimate Dengue Challenge (IMDC) 2026

## Team and Contributors

The Global Health Resilience (GHR) group is based at the Barcelona Supercomputing Center (BSC).

[**Carles Milà**](https://www.bsc.es/mila-garcia-carles)<a href="https://orcid.org/0000-0003-0470-0760"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Barcelona Supercomputing Center (BSC), Spain

[**Chloe Fletcher**](https://www.bsc.es/fletcher-chloe)<a href="https://orcid.org/0000-0002-6705-7605"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Barcelona Supercomputing Center (BSC), Spain\
Department of Medicine & Life Sciences, Universitat Pompeu Fabra, Spain

[**Giovenale Moirano**](https://www.bsc.es/moirano-giovenale)<a href="https://orcid.org/0000-0001-8748-3321"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Università degli Studi di Torino, Italy\
Barcelona Supercomputing Center (BSC), Spain

[**Daniela Lührsen**](https://www.bsc.es/es/luhrsen-daniela-sofie)<a href="https://orcid.org/0009-0002-6340-5964"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Barcelona Supercomputing Center (BSC), Spain

[**Diego Villa**](https://www.bsc.es/villa-almeyda-diego-cesar)<a href="https://orcid.org/0000-0001-7832-5890"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Barcelona Supercomputing Center (BSC), Spain

[**Rachel Lowe**](https://www.bsc.es/lowe-rachel)<a href="https://orcid.org/0000-0003-3939-7343"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID"/>\
</a> Barcelona Supercomputing Center (BSC), Spain\
Catalan Institution for Research and Advanced Studies (ICREA), Spain\
London School of Hygiene and Tropical Medicine, United Kingdom

::: {align="justify"}
:::

## Repository Structure

| Directory / File           | Description                                                                                                                                                                                     |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `climate/`                 | Scripts to download, process and merge observed and seasonal climate datasets, including climatological baselines and climate forecast products used in the challenge.                          |
| `data/raw/`                | Raw challenge datasets provided by the organizers.                                                                                                                                              |
| `data/interim/`            | Intermediate processed datasets generated during data preparation.                                                                                                                              |
| `data/processed/`          | Final datasets, cross-validation splits and graph objects used for model fitting.                                                                                                               |
| `dengue/state/`            | Main modelling workflow, including data preparation, predictor screening, model specification, cross-validation, forecast generation, model selection and preparation of challenge submissions. |
| `dengue/state/jobs/`       | HPC job scripts for executing computationally intensive tasks on the cluster.                                                                                                                   |
| `dengue/state/outputs/`    | Output directory containing fitted models, predictions, scores and submission files.                                                                                                            |
| `resources/health_region/` | Auxiliary spatial resources, including shapefiles, neighbourhood graphs and lookup tables for Brazilian health regions.                                                                         |
| `utils/`                   | Reusable utility functions for data processing, scaling, split generation, model fitting, scoring, validation and parallel execution.                                                           |
| `renv/` and `renv.lock`    | Reproducible R environment and package dependency management.                                                                                                                                   |
| `README.md`                | Repository documentation.                                                                                                                                                                       |



