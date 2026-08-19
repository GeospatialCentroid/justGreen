# justGreen
Code base for the JustGreen project. This tool assesses the relationship between NDVI (Normalized Difference Vegetation Index) distributions within US cities and various health outcomes, specifically non-accidental mortality, stroke incidence, and dementia.

## Overview
The goal is to analyze the top 200 most populated cities in the United States, calculate their current NDVI values, and estimate the potential health benefits (lives saved/cases prevented) associated with NDVI exposure and hypothetical increases in greenness.

## Workflow Summary
The workflow is orchestrated through `main.R` and consists of the following steps:

1.  **Population Data (`0_downloadPopulationData.R`):** Downloads initial 2020 Census population data.
2.  **City Selection (`1_selectTop200Cities.R`):** Identifies the top 200 cities by population and associates them with their respective counties.
3.  **NDVI Acquisition (`2_downloadNDVI.py` & `5_pullDataFromDrive.R`):** Downloads yearly max NDVI values from Google Earth Engine (GEE).
4.  **Census Geographies (`3_downloadCensusGeographies.R` & `3b_55plusInCities.R`):** Gathers census tract boundaries and detailed demographic data (populations over 20, 35, and 55).
5.  **Water Masking (`4_processWaterFiles.R` & `maskWaterFromNDVI.R`):** Processes and applies water masks (lakes, oceans) to ensure NDVI values only reflect land surface.
6.  **NDVI Aggregation (`6_ndviPerCity.R`):** Calculates mean and standard deviation of NDVI for both city-wide and census tract levels (using 250m and 500m buffers).
7.  **Health Metric Application (`7_applyHealthMetrics.R` calling `metricsToAllTracts.R`):** Applies dose-response functions to relate NDVI exposure to health outcomes.
8.  **Summary Generation (`8_generateCitySummaries.R`):** Produces detailed HTML reports for each city using `citySummary_3.Rmd`.
9.  **Data Transfer (`9_transferDataToShinyRepo.R`):** Packages the processed data and reports for use in the Shiny application.

## Methodology

### Data Sources
- **Population:** US Census 2020 DHC and ACS estimates.
- **NDVI:** Derived from Landsat or Sentinel-2 imagery via Google Earth Engine.
- **Health Rates:** County-level mortality rates and state-level dementia/stroke incidence.

### Health Impact Assessment
The project uses dose-response relationships from **Garber et al. 2024** to estimate the impact of NDVI on health.

#### Key Formulas:
- **Relative Rate (RR):**
  - Mortality: $RR = \text{doseResponse}^{(\Delta NDVI / 0.1)}$ (where $\text{doseResponse} \approx 0.96$)
  - Stroke/Dementia: $RR = \text{doseResponse}^{(\Delta NDVI / 0.12)}$ (where $\text{doseResponse} \approx 0.96 \text{ or } 0.97$)
- **Population Attributable Fraction (PAF):**
  - $PAF = \frac{RR - 1}{RR} = 1 - \frac{1}{RR}$
- **Estimated Health Impact:**
  - $\text{Lives Saved/Cases Prevented} = \text{Expected Incidence} \times PAF$
  - $\text{Expected Incidence} = \text{Target Population} \times \text{Crude Rate}$

#### Parameters:
| Outcome | Population Group | Buffer | doseResponse (per 0.1/0.12 NDVI) |
| :--- | :--- | :--- | :--- |
| Non-accidental Mortality | Age 20+ | 500m | 0.96 (0.94 - 0.97) |
| Stroke Incidence | Age 35+ | 250m | 0.96 (0.95 - 0.98) |
| Dementia Incidence | Age 55+ | 250m | 0.97 (0.96 - 0.98) |

*Note: NDVI null/base value is typically considered 0.1.*

## Project Structure
- `scripts/`: Data processing and analysis scripts.
- `scripts/assets/`: Visual assets for reports (logos, fonts, CSS).
- `functions/`: Reusable R functions for health metrics and data cleaning.
- `data/`: Raw, processed, and final product data (ignored by git).
- `deprecated/`: Older versions of scripts and templates for reference.
