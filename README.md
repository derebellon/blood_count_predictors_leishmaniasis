# Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia

**Principal Investigator & Script Developer**: David Esteban Rebellón Sánchez  
**Supervisors**: María Adelaida Gómez & Lyda Osorio  
**Institutions**: Centro Internacional de Entrenamiento e Investigaciones Médicas (CIDEIM), Universidad del Valle  
**Co-authors**: David E. Rebellón-Sánchez, Lina Giraldo-Parra, Jimena Jojoa, Jonny A. García-Luna, Lyda Osorio, María Adelaida Gómez  

---

## 📁 Repository structure
```
├── data/                           # Raw and cleaned data in RData, CSV, and STATA formats
│   ├── Clean_data_RData/            # Cleaned datasets in .RData
│   ├── Clean_data_csv/              # Cleaned datasets in .csv
│   ├── Clean_data_STATA_dta/        # Cleaned datasets in .dta (STATA)
│   └── codebook/                    # Data dictionary
├── figures/                        # All figures used in analyses and reports
├── Reports/                        # RMarkdown reports: cleaning, analysis, modeling, etc.
├── Results/                        # Outputs from modeling and machine learning workflows
├── tables/                         # Summary tables and model outputs
├── Dofile_STATA_Used_for_Analysys_Phase1/  # STATA scripts used for Poisson regression modeling
├── 00 Limpieza y analisis exploratorio.R   # Script for cleaning and exploratory data analysis
├── 01 Script Analisis - Final.R            # Statistical and ML analysis script (basic)
├── README.md                      # This file
```

---

## 📊 Project overview
This project aims to evaluate whether hematological parameters obtained before and after systemic treatment can predict therapeutic failure in patients with cutaneous leishmaniasis (CL). Data were retrospectively retrieved from clinical records of studies conducted between 2007 and 2020 at CIDEIM.

---

## ⚒️ Data workflow
### Data sources
All data originated from clinical records and standardized Case Report Forms (CRFs), entered into SQL-backed forms using double data entry. Participants were anonymized and assigned unique study codes.

### Cleaning and curation
Cleaning and exploratory analysis were conducted using the script: `00 Limpieza y analisis exploratorio.R`  
Final curated datasets are saved in three formats: `.RData`, `.csv`, and `.dta`

### Data outputs
Cleaned datasets include:  
`participantes`, `evaluacion_base`, `hemopre`, `hemopos`, `fin_tto`, `sem_8`, `sem_13`, `sem_26`, `visita_adic`, `estado_fin`, `criterios_inclusion`, `especie_parasit`, `gran_base_resumen`, `Big_hemo`, `base_regre_centrada`, `final_df_cleaned`

Each dataset is available in:
- `data/Clean_data_RData/` (.RData)
- `data/Clean_data_csv/` (.csv)
- `data/Clean_data_STATA_dta/` (.dta)

A comprehensive codebook describing all variables is located in `data/codebook/`.

---

## 📘 Project phases
**NOTE**:  
This project was conducted in two analytical phases:

**Phase 1** involved an extensive descriptive and exploratory analysis, fully documented in my MSc thesis titled:  
*“Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia.”*  
This work was awarded meritorious thesis recognition by Universidad del Valle, granting the MSc in Epidemiology.

**Phase 2** focused on validating initial findings using a refined analytical strategy in R.  
A clinically selected subset of variables was integrated with hematological parameters into a comprehensive dataset.  
This phase included:
- Multiple imputation of missing data
- Development and validation of machine learning models
- Comparative performance assessment

Only the validated outputs from Phase 2 were used in the final manuscript submitted for publication.

---

## 🦡 Analysis scripts
### Exploratory & descriptive analysis
Conducted via `00 Limpieza y analisis exploratorio.R`  
All descriptive outputs saved in `Reports/`

### Statistical & ML analysis
Advanced modeling is handled in `01 Script Analisis - Final.R`
- Machine learning models (Random Forest, XGBoost, SuperLearner, Lasso/Ridge) are in development
- Outputs from this phase are saved in:
  - `Results/` for model performance metrics and saved models
  - `tables/` for model coefficients, AUCs, optimal cut-offs
  - `figures/` for ROC curves, variable importance, and PCA/PLSDA projections

### Poisson regression models
Poisson regression models used in Phase 1 were developed in STATA  
STATA commands and do-files are stored in: `Dofile_STATA_Used_for_Analysys_Phase1/`

---

## 🗓️ Reproducibility
All scripts are fully commented and modular to support reproducibility.  
Dependencies are noted in each script.  
Upcoming updates will include modular pipelines for ML and version-controlled outputs for reproducible research.

---

## ✍️ Citation
If you use or adapt this project, please cite:

Rebellón-Sánchez DE, Giraldo-Parra L, Jojoa J, García-Luna JA, Osorio L, Gómez MA. *Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia*. [In preparation].

