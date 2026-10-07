# Blood count parameters as early biomarkers of therapeutic outcome in cutaneous leishmaniasis

A retrospective cohort in Colombia.

**Principal investigator & analyst:** David Esteban Rebellón-Sánchez
**Supervisors:** María Adelaida Gómez, Lyda Osorio
**Co-authors:** D. E. Rebellón-Sánchez, L. Giraldo-Parra, J. Jojoa, J. A. García-Luna, L. Osorio, M. A. Gómez
**Institutions:** Centro Internacional de Entrenamiento e Investigaciones Médicas (CIDEIM) · Universidad del Valle · Universidad Icesi (Cali, Colombia)

---

## What this repository is, and the question behind it

Treating cutaneous leishmaniasis (CL) is hard: the drugs are toxic, failure is common, and — crucially — by definition you cannot *call* a treatment a failure until 3 to 6 months after it started, and sometimes later. In practice most patients in our endemic regions never make it back for those late visits. So the clinical question that drives everything here is simple: **can something as cheap and universally available as a blood count tell us early who is at risk of failing?**

The honest answer we arrived at, and which this repository documents end to end, has three parts:

1. **No single blood count parameter is a good biomarker on its own** — their sensitivity and specificity are only slightly better than a coin flip.
2. **But blood counts *do* add real value when combined with simple clinical variables.** A parsimonious model using clinical data plus a few blood-count parameters improved out-of-sample discrimination of failure from an AUC of 0.68 (clinical only) to 0.74 before treatment (likelihood-ratio *P* = 0.004).
3. **Fancier is not better here.** We put LASSO, random forest, XGBoost and a SuperLearner ensemble up against a well-specified logistic regression, and the regression matched or beat all of them. With only 31 failures, that is exactly what the statistical literature predicts, and it is the more honest, more deployable result.

To make the finding usable at the bedside, we also distilled the regression into a **simple points-based risk score** (0–8 points before treatment) that, at a cut-off of 3 points, flagged every failure in our cohort (100% sensitivity, 100% negative predictive value) — i.e. a low score effectively rules failure out.

Everything below — the data, every script, every table and figure — is here so that **anyone can reproduce any analysis**, and just as importantly, understand *why* each decision was made.

---

## A note on the data (please read before reusing)

- All data are **de-identified**. Each participant appears only as an anonymous study code (e.g. `PH1001`); there are **no names, national IDs, dates of birth or calendar dates** anywhere. Follow-up is expressed as *relative days* from the initial visit. Age is stored in years, not as a birth date.
- Participants were included only if they had, in their original written informed consent, authorized the use of their data in future studies. The study was approved by the IRBs of Universidad del Valle (E 033-021) and CIDEIM (Approval Act 07-2021).
- Even though the data are de-identified, this is still individual-level clinical data. If you reuse it, do so under the same ethical spirit in which it was collected, and cite the work.
- The cleaned dataset is provided in three formats so you are not locked into one tool: `.RData`, `.csv` and STATA `.dta`. A full data dictionary lives in `data/` (`2022-02-02_CODEBOOK_HEMOPREPOST.xlsx`).

---

## The two phases of this project

**Phase 1 — the thesis (descriptive + inferential, in STATA).**
The original work was my MSc in Epidemiology (Universidad del Valle), which received meritorious-thesis recognition. It built the cleaned database, described the cohort, and fit the univariate and multivariate Poisson (relative-risk) models. Those STATA do-files are preserved verbatim in `Dofile_STATA_Used_for_Analysys_Phase1/`, and the thesis report is in `Reports/`.

**Phase 2 — the reanalysis (validation + prediction, in R).**
Phase 2, which is the backbone of the manuscript, re-implemented everything in a clean, modular R pipeline (`R/`), ported the regression from STATA, and added the pieces the thesis did not have: multiple imputation, machine-learning models, out-of-sample validation, interpretability, and the risk score. The reasoning for each step is spelled out below.

---

## The analysis pipeline (`R/`) — what each script does, and *why*

Run from the repository root. Each script sources `R/00_config.R` first and writes its outputs to its own subfolder under `outputs/`. The numbering is the execution order.

| Script | What it does | Why we did it this way |
|---|---|---|
| `00_config.R` | Single source of truth: random seed, paths, and the outcome definition. | Reproducibility. We fix the seed, and we define **failure (the minority class, 31/200) as the positive/event class**, because the clinically important and statistically harder job is predicting *failure*, not cure. |
| `10_impute_and_prepare.R` | Multiple imputation (`mice`, m = 20) with **logical constraints**. | Missing blood counts shouldn't cost us whole patients (complete-case analysis would). Imputation recovers them, but we constrain it so imputed values make clinical sense — e.g. no glucantime dose for a patient who received miltefosine, no ulcer measurement when there is no ulcer. |
| `15_feature_engineering.R` | Builds percentages, 12 cell-population ratios, and pre→EoTx variations. | A count of "2 eosinophils" means something different among 3 cells than among 1000, so we add **percentages** (relative abundance) and **ratios**. X/immature-granulocyte ratios are floored at 0.01 (a limit-of-detection stand-in) **only for the ML models**, to avoid division by zero, and kept out of the inferential models. |
| `20_logistic_univariate.R` | Univariate odds ratios (per 1 SD), MI-pooled, `gtsummary` tables. | Standardizing per SD makes effect sizes comparable across parameters on very different scales. |
| `21_logistic_multivariate_RR.R` | Multivariate **relative risks** via robust Poisson regression, MI-pooled with Rubin's rules; Youden cut-points. | In a cohort we prefer **RR over OR** (more interpretable); robust/sandwich errors reproduce STATA's `vce(robust)`. We dichotomize the key blood counts at Youden-optimal cut-offs so the result is usable at the bedside. This script reproduces the thesis's final model. |
| `22_model_comparison.R`, `23_new_comparisons.R` | Nested-model likelihood-ratio tests; clinical vs. +blood-count vs. blood-count-only. | This is the formal test of the central hypothesis: does adding blood counts to a clinical model *significantly* improve it? (Yes: LR *P* = 0.004 pre-treatment.) |
| `30_lasso.R`, `33_hierarchical_lasso.R` | LASSO selection; a hierarchical LASSO that selects within families first. | A principled, penalized way to let the data pick variables, and to check whether anything emerges that the biology-driven model missed. |
| `31_random_forest.R`, `32_xgboost.R` | Random forest and gradient boosting. | The flexible, non-linear contenders — to test fairly whether ML beats regression. |
| `34_rf_families.R` | Random forests fit to **separate families** (counts / percentages / ratios). | David's idea: does a parameter emerge in one representation that the others hide? (The count family did best and kept surfacing erythroid indices — haematocrit, haemoglobin, red cells — a hypothesis-generating signal, not a confirmed biomarker.) |
| `50_models_split_validation.R` | The **master out-of-sample comparison** of all models. | The single most important methodological choice: we **split first, then balance only the training fold**, and evaluate on the untouched fold that keeps the real (imbalanced) outcome distribution. Balancing before splitting leaks information and inflates performance; we don't do that. |
| `60_superlearner.R` | SuperLearner ensemble, validated with **nested** CV (`CV.SuperLearner`), weights chosen to maximize AUC. | The ensemble is the strongest "ML can't lose" test. Nested CV is what makes its reported performance honest (it validates the weight-learning too). It still didn't beat the single best model. |
| `70_interpretability_shap.R`, `71_rf_shap.R` | SHAP values for XGBoost (native, exact) and random forest (`treeshap`). | If ML and regression disagree, who's right? SHAP shows the ML models leaned on the *same* biology-informed parameters the regression selected — reassuring, not contradictory. |
| `80_final_models_export.R` | Fits the final models on all data (unweighted, calibrated) and exports them for a browser tool. | For deployment, calibration matters more than class weighting; the exported `models.json` powers the point-of-care calculator. |
| `91_master_metrics.R` | The master performance table (Table 5): every model × scenario, with accuracy, AUC + CI, failure-focused sensitivity/specificity, PPV/NPV, F1, Brier, at the Youden threshold. | One honest, comparable scoreboard. CIs come from the repeated-CV distribution (base learners) or bootstrap of the out-of-fold predictions (SuperLearner). |
| `92_figures.R`, `95_figures_final.R`, `96`, `97` | All manuscript figures (AUC comparison, ROC curves, SHAP, forest plot, blood-count dynamics). | Consistent styling, failure in one colour throughout, pre-treatment always shown before end-of-treatment. |
| `93_paper_tables.R` | Tables 1–4 (baseline + overall, univariate OR, multivariate RR in three nested columns, LASSO coefficients). | The LASSO table uses `lambda.1se` because `lambda.min` was numerically unstable on the full feature set (too many correlated features, too few events) — a deliberate, documented choice. |
| `98_risk_score.R` | Turns the pre- and post-treatment models into an **integer points score**, with the Youden cut-off and AUC. | A regression equation is not something you use in a rural clinic; a 0–8 points card is. |

### The validation philosophy, in one paragraph

We only have **31 failures**. That single fact shapes every methodological decision: we validate out of sample with **repeated stratified cross-validation** and bootstrap optimism correction rather than trusting apparent performance; we handle class imbalance **inside the training folds only**; and we report **failure-focused metrics** (PR-AUC, sensitivity/recall for failure, F-scores, Brier) instead of accuracy, because with a 15.5% failure rate a model that predicts "cure" for everyone is 84.5% accurate and useless. It is also why regression wins: flexible algorithms are *data-hungry* and need many more events per variable to pay off (Christodoulou 2019; van der Ploeg 2014; Riley 2020).

---

## Key results (saved in `paper_results/`)

- **`paper_results/tables/`** — the manuscript tables (T1–T6) as Markdown, plus the raw metric CSVs (master metrics, risk-score points and performance, random-forest-by-family).
- **`paper_results/figures/`** — the final figures (Figures 3–7): AUC of all models, ROC curves, SHAP, adjusted-RR forest plot, and blood-count dynamics by outcome.

Headline numbers: failure incidence 15.5%; adding pre-treatment blood counts lifted the logistic model's out-of-sample AUC from 0.68 to 0.74 (LR *P* = 0.004); no ML model beat it; the pre-treatment risk score reached an AUC of 0.79 and, at ≥ 3 points, identified all failures (sensitivity 100%, NPV 100%).

---

## How to reproduce

```bash
# from the repository root, with R >= 4.5
Rscript R/10_impute_and_prepare.R      # multiple imputation -> data/derived/
Rscript R/15_feature_engineering.R
Rscript R/20_logistic_univariate.R
Rscript R/21_logistic_multivariate_RR.R
Rscript R/22_model_comparison.R
Rscript R/23_new_comparisons.R
Rscript R/30_lasso.R ; Rscript R/33_hierarchical_lasso.R
Rscript R/31_random_forest.R ; Rscript R/32_xgboost.R ; Rscript R/34_rf_families.R
Rscript R/50_models_split_validation.R
Rscript R/60_superlearner.R
Rscript R/70_interpretability_shap.R ; Rscript R/71_rf_shap.R
Rscript R/80_final_models_export.R
Rscript R/91_master_metrics.R          # Table 5
Rscript R/93_paper_tables.R            # Tables 1-4
Rscript R/98_risk_score.R             # Table 6 (risk score)
Rscript R/95_figures_final.R           # Figures 3-7
Rscript R/96_roc_and_theme.R ; Rscript R/97_roc_ci.R
```

Required R packages: `mice`, `sandwich`, `cutpointr`, `glmnet`, `ranger`, `xgboost`, `SuperLearner`, `treeshap`, `pROC`, `PRROC`, `gtsummary`, `ggplot2`, `patchwork`, `dplyr`, `gt`, `knitr`. Intermediate outputs (`outputs/`, `data/derived/`) are regenerable and therefore git-ignored.

---

## Repository structure

```
R/                      # Phase-2 reproducible pipeline (00 -> 98); utils/ holds shared helpers
data/                   # De-identified cleaned data (.RData / .csv / .dta) + codebook
paper_results/          # Final manuscript tables (T1-T6) and figures (3-7) — committed
Dofile_STATA_.../       # Phase-1 STATA do-files (thesis)
legacy/                 # Original loose exploration scripts, superseded by R/ (kept for provenance)
Reports/                # Thesis report and cleaning reports
Results/                # Phase-1 descriptive outputs and the enrolment flow diagram
tables/                 # Phase-1 summary tables
outputs/                # (git-ignored) regenerable Phase-2 outputs
```

---

## Citation, funding, ethics, license

**Citation:** Rebellón-Sánchez DE, Giraldo-Parra L, Jojoa J, García-Luna JA, Osorio L, Gómez MA. *Blood count parameters as early biomarkers of therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia.* (Manuscript in preparation.)

**Funding:** Wellcome Trust 107595/Z/15/Z and NIAID-NIH U19AI129910. D.E.R.-S. was supported by the Global Infectious Diseases Research Training Program of the Fogarty International Center (NIH) under award D43 TW006589.

**Ethics:** IRBs of Universidad del Valle (E 033-021) and CIDEIM (Approval Act 07-2021); retrospective analysis of data from consenting participants.

**License:** see `LICENSE`.
