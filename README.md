Use and Costs of P2Y12 Inhibitors After Acute Myocardial Infarction

Supplementary Analysis Code

This repository contains illustrative code related to the study of P2Y12 inhibitor utilization, treatment costs, cardiovascular outcomes, and prescribing patterns among patients treated with clopidogrel, ticagrelor, or prasugrel after acute myocardial infarction in clinical practice.

ClinicalTrials.gov ID: [NCT07610577]

⸻

🧪 About the Analysis

Primary analyses were conducted using claims-based and publicly available data sources from Germany and the United States, including:

* BARMER Krankenkasse Scientific Data Warehouse / Wissenschafts-Data-Warehouse (Germany)
* Optum Clinformatics (United States)
* Merative MarketScan (United States)
* CMS Open Payments
* Medicare Part D Prescriber Public Use Files

The code provided here is not sufficient to replicate study results, as it:

* Does not include source data because of privacy, licensing, and data-use restrictions
* Supplements the primary analyses by illustrating downstream steps:
    * Post-processing of exported results
    * Plotting and figure generation
    * Temporal trend analyses
    * Cost calculations and inflation adjustment
    * Drug cost savings scenarios
    * Industry payment and prescribing analyses

Outcome focus: Unless otherwise specified in a given script, MACE refers to a composite of all-cause mortality, acute myocardial infarction, and stroke. Bleeding outcomes refer to bleeding-related hospitalizations.

⸻

📁 Contents

Script	Purpose
P2Y12i_Utilization_Trends.ipynb	Plots temporal trends in age- and sex-standardized use of clopidogrel, ticagrelor, and prasugrel after acute myocardial infarction.
P2Y12i_Cost_Trends.ipynb	Summarizes and plots inflation-adjusted 12-month treatment costs for clopidogrel, ticagrelor, and prasugrel in Germany and the United States.
Adverse_Event_Trends.ipynb	Estimates and plots quarterly age- and sex-standardized 1-year risks of MACE and bleeding-related hospitalizations.
Drug_Cost_Savings_US.ipynb	Estimates potential US drug cost savings under a ticagrelor-to-prasugrel substitution scenario.
Industry_Payments.ipynb	Summarizes temporal trends in industry payments associated with ticagrelor and prasugrel using CMS Open Payments data.
Prescribing_Behavior.ipynb	Examines the association between industry payments and subsequent ticagrelor versus prasugrel prescribing patterns.
Patient_Characteristics_Model.ipynb	Evaluates patient characteristics associated with initiation of ticagrelor versus prasugrel after 2019.

⸻

📦 Dependencies

These scripts require:

* pandas
* numpy
* matplotlib
* lifelines
* statsmodels
* scipy
* seaborn (optional; used in some visualizations)

Install via:

pip install -r requirements.txt
