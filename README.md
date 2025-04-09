# Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia

**Principal Investigator & Script Developer:** David Esteban Rebellón Sánchez  
**Supervisors:** María Adelaida Gómez & Lyda Osorio  
**Institutions:** Centro Internacional de Entrenamiento e Investigaciones Médicas (CIDEIM), Universidad del Valle  
**Co-authors:** David E. Rebellón-Sánchez, Lina Giraldo-Parra, Jimena Jojoa, Jonny A. García-Luna, Lyda Osorio, María Adelaida Gómez

---

## 📁 Repository structure

```
├── data/                   # Raw and cleaned data in RData, CSV, and STATA formats
│   ├── Clean_data_RData/     # Cleaned datasets in .RData
│   ├── Clean_data_csv/       # Cleaned datasets in .csv
│   ├── Clean_data_STATA_dta/ # Cleaned datasets in .dta (STATA)
│   └── codebook/             # Data dictionary
├── figures/               # All figures used in analyses and reports
├── Reports/               # RMarkdown reports: cleaning, analysis, modeling, etc.
├── Results/               # Outputs from modeling and machine learning workflows
├── tables/                # Summary tables and model outputs
├── 00 Limpieza y analisis exploratorio.R   # Script for cleaning and exploratory data analysis
├── 01 Script Analisis - Final.R            # Statistical and ML analysis script (basic)
├── README.md             # This file
```

---

## 📊 Project overview
This project aims to evaluate whether hematological parameters obtained before and after systemic treatment can predict therapeutic failure in patients with cutaneous leishmaniasis (CL). Data were retrospectively retrieved from clinical records of studies conducted between 2007 and 2020 at CIDEIM.

---

## ⚒️ Data workflow

### Data sources
All data originated from clinical records and standardized Case Report Forms (CRFs), entered into SQL-backed forms using double data entry. Participants were anonymized and assigned unique study codes.

### Cleaning and curation
- Cleaning and exploratory analysis were conducted using the script: `00 Limpieza y analisis exploratorio.R`
- Final curated datasets are saved in three formats: `.RData`, `.csv`, and `.dta`

### Data outputs
Cleaned datasets are saved as:
- `participantes`, `evaluacion_base`, `hemopre`, `hemopos`, `fin_tto`, `sem_8`, `sem_13`, `sem_26`, `visita_adic`, `estado_fin`, `criterios_inclusion`, `especie_parasit`, `gran_base_resumen`, `Big_hemo`, `base_regre_centrada`, `final_df_cleaned`

Each dataset is available in:
- `data/Clean_data_RData/` (.RData)
- `data/Clean_data_csv/` (.csv)
- `data/Clean_data_STATA_dta/` (.dta)

A **codebook** detailing variable definitions is included in `data/codebook/`.

---

## 🪡 Analysis scripts

### Exploratory & descriptive analysis
- Conducted via `00 Limpieza y analisis exploratorio.R`
- Outputs saved in `Reports/`

### Statistical & ML analysis
- Ongoing development in `01 Script Analisis - Final.R`
- Additional ML scripts (Random Forest, XGBoost, SuperLearner, Lasso/Ridge) will be added
- All model outputs will be saved in:
  - `Results/` (performance metrics, ML models)
  - `tables/` (model coefficients, AUCs, cut-offs)
  - `figures/` (ROC curves, variable importance plots, PCA/PLSDA projections)

---

## 📆 Reproducibility

All scripts are documented and structured for reproducibility. Dependencies are managed via comments in each R script. Future updates will include modular ML pipelines and versioned outputs.

---

## ✍️ Citation
If you use or adapt this project, please cite:
> Rebellón-Sánchez DE, Giraldo-Parra L, Jojoa J, García-Luna JA, Osorio L, Gómez MA. Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia. [In preparation].

