# Use and Costs of P2Y12 Inhibitors After Acute Myocardial Infarction


# Supplementary Analysis Code

This repository contains illustrative R code related to the study of **P2Y12 inhibitor utilization, treatment costs, industry payments, prescribing patterns, and cardiovascular outcomes** among patients treated with **clopidogrel**, **ticagrelor**, or **prasugrel** after acute myocardial infarction in clinical practice.

**ClinicalTrials.gov ID**: [NCT07610577](https://clinicaltrials.gov/study/NCT07610577)

---

## 🧪 About the Analysis

Primary analyses were conducted using claims-based and publicly available data sources from the United States and Germany, including **Optum Clinformatics**, **Merative MarketScan**, **CMS Open Payments**, **Medicare Part D Prescriber Public Use Files**, and **BARMER Krankenkasse Scientific Data Warehouse / Wissenschafts-Data-Warehouse**.

The code provided here is **not sufficient to replicate study results**, as it:
- Does **not include source data** because of privacy, licensing, and data-use restrictions
- Supplements the primary analyses by illustrating downstream steps:
  - Age- and sex-standardization of P2Y12 inhibitor utilization in the US
  - Standardized baseline table generation for the US cohorts
  - US cost plotting and figure generation
  - Description and visualization of US industry payments
  - Construction of provider-level prescribing and payment cohorts in the US
  - Modeling of the association between industry payments and prescribing behavior in the US
  - Modeling of factors associated with prasugrel versus ticagrelor initiation in the US
  - Age- and sex-standardization of P2Y12 inhibitor utilization in Germany
  - Standardized baseline table generation for the German cohort
  - German cost plotting and figure generation
  - Quarterly outcome risk estimation in Germany
  - Segmented trend analyses for German clinical outcomes
  - Modeling of factors associated with prasugrel versus ticagrelor initiation in Germany

> **Outcome focus:** Unless otherwise specified in a given script, MACE refers to **all-cause mortality, acute myocardial infarction, or stroke** (composite). Bleeding outcomes refer to **bleeding-related hospitalizations**.

---

## 📁 Contents

| Script | Purpose |
|--------|---------|
| `US_std_utilization.R` | Calculates age- and sex-standardized utilization of **clopidogrel**, **ticagrelor**, and **prasugrel** in the US cohorts. |
| `US_std_utilization_graph.R` | Plots temporal trends in standardized US P2Y12 inhibitor utilization. |
| `US_drug_cost_graph.R` | Plots inflation-adjusted 12-month treatment costs for **clopidogrel**, **ticagrelor**, and **prasugrel** in the United States. |
| `US_std_baseline_table.R` | Generates standardized baseline characteristics tables for the US P2Y12 inhibitor cohorts. |
| `US_factors_associated.R` | Estimates factors associated with **prasugrel versus ticagrelor** initiation in the United States after 2019. |
| `US_openpayment_descriptive.R` | Summarizes descriptive trends in industry payments associated with **ticagrelor** and **prasugrel** using CMS Open Payments data. |
| `US_openpayment_graph.R` | Plots temporal trends in industry payments associated with **ticagrelor** and **prasugrel**. |
| `US_prescription_payments_association_provider_cohort.R` | Constructs the provider-level cohort linking Open Payments and Medicare Part D prescribing data. |
| `US_prescription_payments_association_regression.R` | Estimates the association between ticagrelor-associated industry payments and subsequent ticagrelor versus prasugrel prescribing patterns. |
| `GER_std_utilization.R` | Calculates age- and sex-standardized utilization of **clopidogrel**, **ticagrelor**, and **prasugrel** in the German cohort. |
| `GER_std_utilization_graph.R` | Plots temporal trends in standardized German P2Y12 inhibitor utilization. |
| `GER_drug_cost_graph.R` | Plots inflation-adjusted 12-month treatment costs for **clopidogrel**, **ticagrelor**, and **prasugrel** in Germany. |
| `GER_outcome_calc.R` | Calculates quarterly age- and sex-standardized 1-year risks for German outcomes, including **MACE**, **bleeding-related hospitalization**, and mortality as applicable. |
| `GER_outcome_graph.R` | Plots segmented temporal trends in standardized 1-year German outcome risks. |
| `GER_outcome_trend_metrics.R` | Estimates segmented regression metrics for German outcome trends before and after the prespecified transition period. |
| `GER_std_baseline_table.R` | Generates standardized baseline characteristics tables for the German P2Y12 inhibitor cohort. |
| `GER_factors_associated_calc.R` | Estimates factors associated with **prasugrel versus ticagrelor** initiation in Germany after 2019. |

---

## 📦 Dependencies

These scripts require R and commonly used R packages, including:

- `tidyverse`
- `dplyr`
- `data.table`
- `ggplot2`
- `lubridate`
- `survival`
- `broom`
- `sandwich`
- `lmtest`
- `nnet`
- `scales`

Install via:

```r
install.packages(c(
  "tidyverse",
  "dplyr",
  "data.table",
  "ggplot2",
  "lubridate",
  "survival",
  "broom",
  "sandwich",
  "lmtest",
  "nnet",
  "scales"
))
