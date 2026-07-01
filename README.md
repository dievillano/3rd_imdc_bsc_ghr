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

### Modelling scripts

| Script                             | Purpose                                                                                            |
| ---------------------------------- | -------------------------------------------------------------------------------------------------- |
| `00-data_preparation.R`            | Prepare dengue datasets, construct validation splits and integrate climate forecast datasets.      |
| `01-merge_datasets.R`              | Merge epidemiological, climate, environmental and demographic datasets into the modelling dataset. |
| `02-exploratory_analysis.R`        | Exploratory analyses of predictors and response variables.                                         |
| `03-predictor_screening.R`         | Screen candidate predictors using cross-validation.                                                |
| `04-model_specs.R`                 | Define and store candidate model specifications.                                                   |
| `05-run_cv_fit.R`                  | Fit candidate models on the internal cross-validation splits.                                      |
| `06-score_state_predictions.R`     | Compute probabilistic forecast scores (WIS, CRPS and coverage).                                    |
| `07-model_selection.R`             | Compare candidate models and select the final forecasting model.                                   |
| `08-validation_forecasts.R`        | Generate forecasts for the official validation splits.                                             |
| `09-validation_submission.R`       | Aggregate predictions and prepare submission files following the challenge format.                 |
| `10-submit_validation_forecasts.R` | Upload validation forecasts to the Mosqlimate platform.                                            |

## Data and Variables

### Data Sources

We used the epidemiological, climate, environmental and demographic datasets provided 
as part of the 3rd IMDC 2026 challenge. The epidemiological dataset consisted of 
weekly dengue case counts aggregated at the Brazilian health-region level. Environmental 
and demographic covariates included Köppen climate classification, biome and annual 
population estimates.

In addition to the official challenge datasets, seasonal climate forecasts were 
incorporated for temperature, precipitation and ENSO conditions. Bias-corrected 
monthly forecasts of near-surface air temperature (tas) and precipitation (prlr) 
were obtained from the ECMWF SEAS5.1 seasonal forecasting system, while monthly 
Niño 3.4 forecasts were obtained from the Copernicus Climate Data Store. These 
forecast datasets were used to construct the validation datasets while respecting 
the challenge data availability constraints.

### Data Preprocessing

Weekly dengue observations were merged with climate, environmental and demographic 
datasets at the health-region level. Climate variables were aggregated, rolling 
summaries and lagged predictors were generated, and standardized climate anomalies 
and drought indices were computed where appropriate. Seasonal forecast datasets 
were merged with the observed climate records to generate the official validation 
datasets, replacing unavailable future observations with forecast values and 
climatological averages when required.

Predictor standardization was performed separately within each training split to 
avoid information leakage. The resulting scaling parameters were subsequently 
applied to the corresponding validation data.

The complete preprocessing workflow is implemented in the `climate/`, 
`dengue/state/00-data_preparation.R` and `dengue/state/01-merge_datasets.R` scripts.

### Predictor Selection

Candidate predictors consisted of lagged and rolling summaries of temperature, 
precipitation, humidity, ENSO indices and environmental variables. Predictors 
were screened using an automated cross-validation pipeline based on internal 
validation splits. For this submission, the final forecasting model retains the 
predictor set selected by the GHR team for the previous IMDC, allowing us to 
reproduce the previously validated modelling framework within the updated 2026 
data processing and forecasting pipeline. Candidate model specifications and 
predictor screening are implemented in `dengue/state/03-predictor_screening.R` and 
`dengue/state/04-model_specs.R`, respectively.

## Model Training

The submitted forecasting model is based on the Bayesian hierarchical framework 
developed by the GHR team for the previous IMDC. The model is implemented using 
the R-INLA package with a negative binomial likelihood to account for overdispersed 
dengue case counts.

The model combines fixed effects describing the relationships between dengue incidence 
and selected climate predictors with spatial and temporal random effects. Spatial 
dependence is modelled using a BYM2 conditional autoregressive model defined over 
the health-region neighbourhood graph, while temporal variation is represented 
through cyclic weekly random walks and yearly random effects. Climate predictors 
are standardized within each training split prior to model fitting.

Model training and forecasting are performed independently for each cross-validation 
split. For every split, predictors are standardized using only the training data, 
the fitted scaler is applied to the corresponding validation data, and posterior 
predictive samples are generated for all validation observations. These posterior 
predictive samples are subsequently aggregated to the state level for evaluation 
and submission.

The complete modelling workflow is implemented in the following scripts:

  - `dengue/state/05-run_cv_fit.R`: fits candidate models on the internal cross-validation splits.
  - `dengue/state/06-score_state_predictions.R`: computes probabilistic forecast scores (WIS, CRPS and prediction interval coverage).
  - `dengue/state/07-model_selection.R`: compares candidate models and selects the final forecasting model.
  - `dengue/state/08-validation_forecasts.R`: fits the selected model to the official validation splits and generates probabilistic forecasts.
  - `dengue/state/09-validation_submission.R`: aggregates health-region forecasts to the state level and formats predictions according to the IMDC submission specifications.

The scripts are intended to be executed sequentially. Computationally intensive 
tasks are parallelized through the SLURM job scripts located in `dengue/state/jobs/`.

## Data Usage Restriction

The challenge requires that forecasts for EW 41 of the current year through EW 40 
of the following year are generated using only information available up to EW 25 
of the current year. To satisfy this requirement, the complete forecasting workflow 
was designed to prevent the use of future epidemiological or climate observations 
during model fitting and prediction.

For each official validation split, the epidemiological training data include only 
observations available up to EW 25 of the corresponding year. Validation observations 
are never used during model fitting and are reserved exclusively for forecast evaluation.

Observed monthly climate variables are used only up to the forecast issuance month 
(June, corresponding to data availability through EW 25). Beyond this point, observed 
climate data are replaced by the corresponding seasonal climate forecasts prepared 
for each validation period. When the forecast horizon extends beyond the available 
seasonal forecasts, climatological averages are used to complete the predictor time 
series. This procedure reproduces the information that would have been available 
operationally at the time each forecast was issued.

To avoid information leakage during predictor preprocessing, all climate predictors 
are standardized independently within each training split. Scaling parameters are 
estimated exclusively from the training data and subsequently applied to the corresponding 
validation data.

The implementation of these restrictions is contained in the climate preprocessing 
scripts (`climate/05-*` to `climate/08-*`), the data preparation workflow 
(`dengue/state/00-data_preparation.R` and `dengue/state/01-merge_datasets.R`), and 
the split-specific scaling utilities (`utils/fit_scaler.R` and `utils/scale_split.R`).

## Prediction Uncertainty 

Predictive uncertainty is quantified through the posterior predictive distribution 
obtained from the Bayesian hierarchical model fitted using R-INLA. For each 
validation split, posterior predictive samples are generated for every health-region-week 
combination, accounting for uncertainty in both the model parameters and the negative 
binomial observation process.

Posterior predictive samples are subsequently aggregated from the health-region 
level to the state level by summing the corresponding samples across health regions. 
Prediction intervals are then obtained empirically from the aggregated posterior 
predictive distribution by computing the required quantiles (2.5%, 5%, 10%, 25%, 
50%, 75%, 90%, 95% and 97.5%), corresponding to the 95%, 90%, 80% and 50% prediction 
intervals requested by the challenge.

The posterior predictive samples are also used to compute the probabilistic 
evaluation metrics employed during model assessment, including the Weighted 
Interval Score (WIS), Continuous Ranked Probability Score (CRPS) and empirical 
coverage of the 95% prediction intervals.

## Reference

- **Araujo EC**, Carvalho LM, Ganem F, Vacaro LB et al. (2025) Leveraging probabilistic forecasts for dengue preparedness and control: the 2024 Dengue Forecasting Sprint in Brazil, medRxiv 2025.05.12.25327419 [Preprint].  
- **CSTools** (2025a) CST_Calibration: Forecast Calibration (WWW), *rdrr.io CSTools (version 5.2.0)*, https://rdrr.io/cran/CSTools/man/CST_Calibration.html [Accessed 30 Jul 2025].  
- **CSTools** (2025b) CST_QuantileMapping: Quantiles Mapping for seasonal or decadal forecast data (WWW), *rdrr.io CSTools (version 5.2.0)*, https://rdrr.io/cran/CSTools/man/CST_QuantileMapping.html [Accessed 30 Jul 2025].  
- **Fletcher C**, Moirano G, Alcayna T, Rollock L et al. (in press) Compound and cascading effects of climatic extremes on dengue outbreak risk in the Caribbean: an impact-based modelling framework with long-lag and short-lag interactions, *The Lancet Planetary Health*.  
- **Fletcher C**, Moirano G, Alcayna T, Rollock L et al. (2025) Data and R code to accompany "Compound and cascading effects of climatic extremes on dengue outbreak risk in the Caribbean: an impact-based modelling framework with long-lag and short-lag interactions" (version v1.0.0), *Zenodo*, https://doi.org/10.5281/zenodo.15731719  
- **Van Schaeybroeck B** and Vannitsem S (2011) Post-processing through linear regression, *Nonlinear Processes in Geophysics*, 18, 147-160.

