**Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia**

David E. Rebellón-Sánchez^1,2,3,4\*^, Lina Giraldo-Parra^1,4^, Jimena Jojoa^1,4^, Jonny A. García-Luna^1,4^, Lyda Osorio^2^, María Adelaida Gómez^1,3,4\*^

^1^ Centro Internacional de Entrenamiento e Investigaciones Médicas-CIDEIM, Cali-Colombia.

^2^ Escuela de Salud Pública, Universidad del Valle, Calle 4B \# 36-00, Cali-Colombia

^3^ London School of Hygiene and Tropical Medicine, Keppel Street, London-UK.

^4^ Universidad Icesi, Calle 18, 122-135, Cali-Colombia.

Running Head: Blood count parameters as biomarkers of therapeutic outcome in CL.

\* Address correspondence to:

David Rebellón-Sánchez

david.rebellon-sanchez@lshtm.ac.uk

Co-corresponding:

María Adelaida Gómez

mgomez@cideim.org.co

Tel: +57 (602) 5552164

**ABSTRACT**

**Background:** Cutaneous leishmaniasis (CL) carries high treatment failure rates, and failure is typically diagnosed only 3 to 6 months after treatment. Accessible early biomarkers could improve care. We investigated whether blood count parameters are early biomarkers of therapeutic failure and whether they improve its prediction beyond sociodemographic and clinical variables, in CL patients in Colombia.

**Methods:** A retrospective cohort study analyzed CL patients who consulted CIDEIM clinics between years 2007-2020. We examined blood counts alongside socio-demographic and clinical data, using bivariate analysis and multivariate regression to evaluate associations with failure. We additionally compared the regression with machine-learning models (LASSO, random forest, XGBoost and a SuperLearner ensemble) by repeated cross-validation, and derived a points-based risk score.

**Results:** Analyzing 200 out of 1225 records, the incidence of treatment failure was 15.5%. Lower pre-treatment platelet and immature granulocyte counts (≤ 0.01x10^3^ cells/µL), an end-of-treatment eosinophil percentage ≥ 14%, and a monocyte variation ratio ≥ 1.07 were associated with a higher risk of treatment failure. As single markers their sensitivity and specificity were suboptimal, but adding blood counts to a clinical model improved prediction of failure (ROC-AUC 0.68 to 0.74 before treatment; *P* = 0.004); machine-learning models did not outperform the regression. A pre-treatment score separated low-, intermediate- and high-risk groups (cross-validated failure 0%, 11%, 27%); a low score ruled out failure.

**Conclusions:** Combined with clinical variables in a parsimonious model or an easy risk score, blood count parameters can serve as early predictors of treatment failure in CL, supporting personalized follow-up. External validation is needed.

**Key words:** Cutaneous Leishmaniasis, Blood Count Parameters, Hemogram, Biomarkers, Predictive Biomarkers, Treatment Outcome, Therapeutic Failure, Antileishmanial Treatment, Colombia.

**BACKGROUND**

Cutaneous leishmaniasis (CL) is a major public health concern in the Americas, accounting for 95-97% of all leishmaniasis cases in the region. Colombia is the second Latin American country, after Brazil, with the highest number of cases reporting 8000-12,000 cases per year \[1,2\]. The control of CL faces a number of limitations, including geographical, economic, sociocultural, health system barriers and the use of highly toxic drugs \[3,4\]. Approximately 80% of patients live in isolated rural areas with limited access to healthcare facilities, which hampers timely diagnosis, treatment, and follow-up \[5\]. Therapeutic failure (TF) is also common, with clinical studies reporting TF rates up to 40% \[6--12\], plausibly with higher frequencies outside of controlled clinical studies and trials.

The clinical definition of treatment outcome for CL demands strict and lengthy patient follow-up, including evaluations conducted at week 8 after treatment initiation to evaluate initial response (clinical improvement or TF), at week 13 to determine apparent cure or TF, and at week 26 to establish definitive cure or TF \[13--15\]. Furthermore, in a cohort from our reference centre, approximately 60% of patients do not attend follow-up visits, 20% only return at end of treatment, 5% at day 45, 7% at week 13, and only 8% at week 26 or more \[16\]. The complexity of CL treatment and, clinical follow-up demands strategies tailored to the needs of patients in remote and rural areas, where the majority of CL cases concentrate \[17,18\].

A complete blood count (CBC) is an easily accessible and readily available test, which provides real-time insights into nutritional status, oxygen-carrying capacity (reflected in haemoglobin concentration and red-cell count), coagulation capacity, and the patient\'s immune response \[19\]. While CBC are commonly used in clinical practice for patient monitoring, interpreting hematologic findings their clinical significance during treatment can be challenging. For instance, an abnormal count of neutrophils or monocytes may indicate induction or inhibition of innate immune responses, which are necessary for resolution of infections and directly associated with clinical outcomes such as treatment response or mortality \[20\]. Likewise, an uncontrolled pro-inflammatory response over time may lead to chronic infections, as is the case of CL, where heightened inflammation can sustain ulcerated lesions despite appropriate antileishmanial therapy \[21\]. Given the central role of immunocompetence in the efficacy of antileishmanial chemotherapy and the feasibility of obtaining complete blood counts (CBCs), even in rural healthcare settings, we sought to address two questions: (1) whether CBC parameters could serve as early biomarkers of treatment failure and (2) whether incorporating these parameters into a predictive model based on sociodemographic and clinical characteristics could improve the identification of patients at risk of treatment failure among Colombian patients receiving Glucantime® (GLUC) or miltefosine (MLF), ultimately supporting the development of a simple, clinically applicable risk score.

**METHODS**

**Study design and subjects.** A retrospective cohort study was conducted using secondary information from adults and children with CL, who participated in previous studies at the Centro Internacional de Entrenamiento e Investigaciones Médicas (CIDEIM) — a reference centre for the treatment of leishmaniasis and neglected tropical diseases in south-western Colombia and a Pan American Health Organization (PAHO/OPS) collaborating centre — between years 2007 and 2020, and who received systemic treatment with GLUC or MLF according to the guidelines of the Colombian Ministry of Health and Social Protection \[22\]. Sociodemographic, clinical and laboratory information was retrieved from clinical records ([Figure 1A]{.mark}) at five follow-up time points: before treatment (Pre-Tx), end of treatment (EoTx), and at week 8, week 13 and week 26 after initiation of treatment. The inclusion criteria where: 1) Parasitological confirmation of CL; 2) Systemic antileishmanial treatment with adherence of at least 90%; 3) At least one CBC available at either Pre-Tx or EoTx assesments; 4) Documented therapeutic response; and 5) Patients who, in their written informed consent, authorized the use of data in future studies. The records of patients with the following characteristics were excluded: 1) Mucosal, visceral, diffuse or disseminated leishmaniasis; and 2) Participants with immunosuppressive diseases or immunomodulatory therapy.

**Definition of study variables.** The outcome variable was the therapeutic response (cure or TF) defined by the physician in the original clinical studies in which the patients participated. Cure was defined, at day 90 or more, as complete re-epithelialization with absence of inflammatory signs in all skin lesions, without the appearance of new lesions or relapses. TF was defined at any time during follow-up if it met any of the following criteria: new skin lesions after EoTx, or an increase in lesion area \> 50% from baseline, a decrease in area by less than 50% at week 8 of follow-up; incomplete re-epithelialization and/or the presence of hardening, raised borders or redness of the lesion after week 13 of follow-up; patients with reactivation of lesions after initial cure (within the available follow-up, which extended up to 245 days from the initial visit). Because these criteria require sustained clinical observation, the diagnosis of therapeutic failure is, by definition, usually established between 3 and 6 months (weeks 13 to 26) after treatment initiation, and in many cases even later, which underscores the value of early predictors available at or before the end of treatment.

The exposure variables were the CBC parameters before and after treatment, which included: 1) Absolute counts and percentages of cell populations: platelets, red blood cells (normoblasts) and leukocytes (neutrophils, lymphocytes, monocytes, eosinophils, basophils and immature granulocytes); 2) Hematocrit (Hto) and hemoglobin (Hb) quantification; 3) Ratios comparing cell populations at each moment of measurement (Pre-Tx and EoTx); 4) Ratios of cell populations comparing the variations between EoTx vs. Pre-Tx. Co-variables included: sociodemographic data (age, sex, ethnicity), clinical data (weight, height, body mass index, time of evolution of CL lesions, presence and number of comorbidities, concomitant infections, previous history of leishmaniasis, number and type of lesions, regional lymphadenopathy, type and adherence to treatment), and *Leishmania* species. Concomitant infection referred predominantly to infection of the CL lesion; a small number of patients with a systemic infectious syndrome were also classified as having a concomitant infection.

**Data sources**

All data were obtained from medical records and case report forms (CRF) from previous studies conducted at CIDEIM. Additional information on data handling and quality assurance can be found in the Supplementary material section (Supplementary methods, Data sources).

**Sampling and power calculation**

Sampling was done by convenience and data from all participants who met the inclusion criteria were included. Power calculations were performed using STATA 16.0 software, taking into account: 1) expected incidences of TF in the range of 9 and 22% \[23\], 2) expected ratios between unexposed (without the clinical variables of interest, eg. Non concomitant coinfection) and exposed (with the clinical variable of interest, eg. with concomitant coinfection), and 3) different expected risk ratios of TF. A sample size of 200 participants provided a statistical power of ≥ 80% to identify factors with relative risks (RR) ≥ 2.35, with a ≥ 15% incidence of TF, and an unexposed/exposed ratio of 3 to 1 or lower.

**Data preparation**

The same prepared dataset underpinned both the statistical analysis and the predictive modelling. Missing predictor data were handled with multiple imputation by chained equations (`mice`, m = 20). Logical (deterministic) constraints were imposed so that imputed values were clinically coherent — for example, lesion-size variables were not imputed when no lesion was present, and drug-specific dosing was constrained to the drug actually received (a miltefosine patient could never receive an imputed glucantime dose). From the raw cell counts we engineered cell percentages (relative abundance), 12 pairwise cell-population ratios, and their Pre-Tx to EoTx variations; ratios with an immature-granulocyte denominator were floored at 0.01 (a limit-of-detection surrogate) to avoid division by zero and were used only in the machine-learning models.

**Statistical analysis**

A descriptive analysis was performed. Qualitative variables were described as frequencies and percentages, and quantitative variables with measures of central tendency and dispersion. Normality was assessed with the Shapiro-Wilk test. The presence of collinearity was explored using scatterplots. Incidence of TF was estimated with a 95% confidence interval (CI) for each follow-up time point.

Bivariate analysis was performed using chi^2^ or exact Fisher test to compare categorical variables and t-test or Wilcoxon-Mann-Whitney for numerical variables. To reduce bias due to multiple testing, *p* and *q* values were calculated \[24\]. Blood count parameters chosen for inclusion in multivariate regression models predicting therapeutic response were those that showed a difference between cures and TF in bivariate analyses (*p*-value \< 0.1 and/or *q*-value ≤ 0.3).

Cut-off points for selected blood count parameters were established through a combination of the Youden index \[25\] and bootstrap methods \[26\]. The best cut-off was chosen based on the sensitivity, specificity, area under the curve (AUC) and clinical interpretability.

Adjusted RR with 95% confidence intervals (CI) were estimated using generalized linear models with a log link (log-binomial, or robust Poisson regression as the pre-specified estimator to ensure convergence), together with contingency tables. Separate models for Pre-Tx and EoTx blood count parameters were constructed using the cut-off points previously estimated, in addition those clinical and sociodemographic variables associated with the outcome in this and previous studies were also included \[23,27\]. Multicollinearity among candidate predictor variables was assessed using variance inflation factors (VIFs), with values greater than 10 considered indicative of substantial multicollinearity. Backward and forward strategies were combined to obtain the most parsimonious model by removing variables with Wald test *p* \>0.2 and then reintroducing them one by one. The Bayesian Information Criteria (BIC) was used to compare each new model with the previous one. Models with a balance between the lower BIC and a higher number of observations contributing to the model estimation were preferred.

Goodness of fit was assessed using the deviance and Pearson chi-square statistics. To assess the model specification, we performed a link test by including the predicted values and their squared terms in an auxiliary regression. A non-significant squared term indicated a well-specified model, while a significant result suggested potential misspecification, such as omitted variables or an incorrect functional form.

**Predictive modelling and internal validation.** To evaluate whether blood count parameters serve as early predictors of therapeutic failure, and whether adding them to a model built only from sociodemographic and clinical variables improves the prediction of failure, we compared regression and machine-learning models in a pre-specified predictive analysis using the prepared dataset described above.

*Models and choice of base learner.* For the predictive comparison we used logistic regression rather than the log-binomial/robust-Poisson model used above for risk-ratio estimation: logistic regression directly yields individual predicted probabilities and is the standard basis for discrimination (ROC) and for clinical risk scores \[58,59\]. The final biology-informed logistic model (parsimonious model) was compared with penalised logistic regression (LASSO), random forest, extreme gradient boosting (XGBoost), and a SuperLearner ensemble stacking these learners. Each algorithm was evaluated using four nested predictor sets: (1) clinical variables only; (2) clinical variables combined with pre-treatment (Pre-Tx) blood counts; (3) clinical variables combined with end-of-treatment (EoTx) blood counts; and (4) clinical variables combined with both Pre-Tx and EoTx blood counts. As a sensitivity analysis, models were also trained using the complete, unselected feature set to assess the impact of biology-informed feature selection on predictive performance.

*Validation, partitioning and class imbalance.* Discrimination was estimated out of sample by repeated stratified cross-validation (10 folds × 15 repetitions), with a stratified 70/30 hold-out as an additional check and 500-resample bootstrap optimism correction. To address the class imbalance without introducing information leakage, balancing was applied inside each training fold only and never to held-out data: the data were split first, the training fold was balanced, and performance was evaluated on the untouched fold that preserves the real outcome distribution. Balancing used inverse-frequency class weights (the failure class weighted up), with synthetic minority oversampling (SMOTE) assessed as a sensitivity analysis. In-fold imputation was refitted on training data only and without the outcome, so the pipeline mimics what happens when a new patient arrives.

*Metrics.* Because the outcome was imbalanced, we pre-specified failure-focused metrics as the primary outcomes: PR-AUC, sensitivity/recall for failure, specificity, positive and negative predictive values, and F-scores at the Youden threshold, together with the Brier score (which captures calibration — whether a predicted 30% risk corresponds to an observed failure rate near 30%) and ROC-AUC with 95% confidence intervals. The incremental value of blood counts was tested with likelihood-ratio tests between nested models.

*Interpretability and risk score.* Model interpretability was examined with SHAP (Shapley additive explanation) values, using the exact tree-based implementation for XGBoost and random forest, to identify the parameters that most influenced the machine-learning predictions. Finally, to obtain a bedside tool, we translated the multivariable models into an integer points score using the method of Sullivan et al. \[57\]: each predictor is reduced to integer points by dividing its regression coefficient by a reference coefficient (here the smallest) and rounding, so that a patient's total score maps monotonically to predicted risk. In the regression models age was continuous; for the score only, age was entered in bands (< 25, 25-39, >= 40 years, assigned 2, 1 and 0 points) to keep the tool bedside-friendly. Rather than a single threshold, patients were grouped into low-, intermediate- and high-risk strata; both the score's discrimination (AUC) and the observed failure rate within each stratum were validated by the same repeated cross-validation, re-deriving the points within each training fold (reported as the cross-validated, optimism-corrected estimates). Beyond the three risk strata, and in addition to the Sullivan points method, the optimal single operating cut-off was chosen with the Youden index, that is, the score value that maximised the sum of sensitivity and specificity.

Phase-1 analyses (descriptive statistics and the final risk-ratio models) were performed in STATA 16; the Phase-2 predictive reanalysis was performed in R 4.5. All code is openly available (see *Data and code availability*).

**Ethical considerations**

The study was conducted in compliance with the Declaration of Helsinki and the Council for International Organizations of Medical Sciences recommendations. It was approved by two institutional review boards, the Comité de Revisión de ética Humana of the Universidad del Valle (code number: E 033-021) and the Comité de Ética en Investigación (CIEIH) of CIDEIM (Approval Act 07-2021). Due to the retrospective nature of the study, the IRBs approved an exemption for the requirement of informed consent for the current analysis. However, only data from subjects whom accepted the use of their information in future studies were included in this analysis.

**Data and code availability**

All analysis code, the de-identified analysis dataset, the data dictionary, and the scripts that regenerate every table and figure are openly available at https://github.com/derebellon/blood_count_predictors_leishmaniasis, so that any reader can reproduce or extend any of the analyses reported here. Participants are represented only by anonymous study codes; no names, identification numbers or calendar dates are included. An online calculator that returns, for an individual patient, both the model-based predicted probability of failure and the points-based risk score with its interpretation is available at https://smartepi.co/scars-leishmaniasis. The calculator is provided for research and transparency and has not yet been externally validated for clinical decision-making.

**RESULTS**

**Characteristics of study participants and therapeutic outcome**

A total of 1225 patient records were reviewed, of which 200 met the inclusion criteria and were included in the analysis ([Figure 1B]{.mark}).

Participants were mostly male (79%), afro-colombian (61%) and with a median age of 32 years (range 2 - 77) ([Table 1]{.mark}). A total of 21 patients, approximately 10.5% of the sample, were under the age of 18. More than half of the participants with available data had some alteration in body mass index (of the 192 participants with recorded BMI, 38.54% were overweight or obese and 13.54% underweight). Approximately half of the participants consulted before four weeks of evolution of their CL lesions. Fifty percent of the cases had a single lesion in the baseline evaluation, being most commonly an ulcer (85%), with 19% also presenting lymphadenopathy. Only one participant reported prior history of CL. The prevalence of concomitant infection in the baseline assessment was 3.5%. Five percent of the participants had some type of comorbidity, being the most frequent those of cardiovascular origin. *Leishmania* isolates were obtained from 153 participants, and the most frequent species isolated was *L. (V.) panamensis* (85%) followed by *L. (V.) braziliensis* (11.8%). Sixty-five percent of the participants received Glucantime (n=131) while 34.5% (n=69) received Miltefosine ([Table 1]{.mark}).

The incidence of TF was 15.50% (95% CI: 11.14 - 21.16%), with 31 patients classified as TF by the end of follow-up (week 26). The cumulative incidence rose from 1.0% at end of treatment and 2.0% by week 8 to 11.5% by week 13 and 15.5% by week 26 ([Table S1]{.mark}); thus most failures (about three-quarters) were identified by week 13, while roughly a quarter occurred later, underscoring the value of follow-up through week 26.

The incidence of TF was higher in men (18.3% vs. 4.8% in women; RR 3.85 CI95% 0.95-15.5) ([Table 1]{.mark}). Interestingly, patients who had ulcerated lesions and who persisted with ulceration by EoTx, had higher incidence of TF than those who progressed to plaque (32% vs. 11%; RR 2,92 CI95%: 1.41 -- 6.06). In addition, those who had concomitant infection at baseline failed more frequently than those without (43% vs. 14%; RR 2.95 CI95% 1.17 - 7.42). Furthermore, higher rates of TF were found among patients infected with *L. (V.) braziliensis* than with *L. (V.) panamensis* (33% vs 15%; RR 2.16; CI95%: 1.00 -- 4.66). No significant differences were found by the type of antileishmanial treatment used.

**Pre and EoTx blood count characteristics**

Pre-Tx CBC were requested in 98% (n=196) of the 200 participants, while EoTx blood counts in 86% (n=171). A total of 167 participants (83.5%) had Pre-Tx and EoTx blood counts. Based on Pre-Tx blood tests, approximately 10% of participants had leukopenia (\< 5x10^3^ cells/µL), and 10% had leukocytosis (\> 10x10^3^ cells/µL) ([Table S2]{.mark}). Although 75% of patients had neutropenia (\< 55% neutrophils), less than 5% of these were within clinically significant ranges (\< 1x10^3^ cells/µL). Eosinophilia (\> 0.5x10^3^ cells/µL or \> 4% eosinophils) was found in 73% of patients. Approximately 80% had mild (between 0.5 and 1.5x10^3^ cells/µL) and 20% moderate eosinophilia (≥ 1.5 and \< 5x10^3^ cells/µL). In addition, \> 25% of the participants had monocytosis (\> 0.7x10^3^ cells/µL) and this estimation was close to 50% when measured by percentage (\>8% monocytes). The other Pre-Tx CBC parameters were within the normal ranges ([Table S2]{.mark}). The proportion of patients with leucocytosis, leukopenia, neutropenia, neutrophilia or monocyte abnormalities at EoTx was similar to those at Pre-Tx, except for eosinophilia which showed a statistically significant increase (*P* \< 0.001) by EoTx, observed in 123 patients ([Table S2]{.mark}, [Figure 7]{.mark}).

When analysing cell population ratios, and their variations between Pre-Tx and EoTx measurements, the largest changes were found in the eosinophil/basophil (Me 1.81, IQR 1.01 - 2.92), eosinophil/lymphocyte (Me 1.71 (IQR 0, 96 - 1.97), eosinophils/monocyte (Me 1.45 (IQR 0.99 - 2.27) and monocyte/lymphocyte (Me 1.33 IQR 0.97 - 1.81) ratios ([Table S2]{.mark}). These results suggest that the most significant changes during the course of treatment occurred in eosinophil counts.

**Selection of blood count parameters for multivariate models**

Blood count parameters were screened by their bivariate association with therapeutic outcome. Seven blood count parameters that showed a statistically significant difference between cures and TF (*p*-value \< 0.1 and/or *q*-value ≤ 0.3) were selected for construction of the multivariate models ([Table 2]{.mark}).

**Table 2. Univariate associations of clinical and sociodemographic variables with therapeutic failure.** OR, odds ratio (logistic regression); RR, relative risk (robust Poisson). For continuous variables (age, body mass index) both ratios are per 1-SD increase (multiplicative change in odds/risk per 1 standard deviation); e.g. each 1-SD increase in age was associated with a 29% lower risk (RR 0.71). *Underdose: no failures observed, relative risk not estimable.

| Clinical and sociodemographic variables | OR | 95% CI | RR (95% CI) | p-value |
|---|---|---|---|---|
| Age | 0.67 | 0.44, 1.01 | 0.71 (0.52-0.98) | 0.053 |
| Sex |   |   |   |   |
| Female | — | — | 1.00 (ref) |   |
| Male | 4.50 | 1.02, 19.9 | 3.85 (0.96-15.50) | 0.047 |
| Ethnicity |   |   |   |   |
| Afro-Colombian | — | — | 1.00 (ref) |   |
| Indigenous | 0.76 | 0.16, 3.68 | 0.79 (0.21-3.03) | 0.732 |
| Mestizo | 0.56 | 0.23, 1.41 | 0.61 (0.28-1.35) | 0.219 |
| Time of lesion evolution |   |   |   |   |
| 4 weeks or less | — | — | 1.00 (ref) |   |
| More than 4 weeks | 0.59 | 0.25, 1.39 | 0.65 (0.32-1.31) | 0.230 |
| Body mass index | 0.72 | 0.48, 1.06 | 0.76 (0.58-0.99) | 0.095 |
| Number of lesions |   |   |   |   |
| One lesion | — | — | 1.00 (ref) |   |
| Three or more lesions | 0.92 | 0.35, 2.44 | 0.93 (0.41-2.14) | 0.868 |
| Two lesions | 1.28 | 0.51, 3.19 | 1.23 (0.58-2.61) | 0.595 |
| Lesion type (baseline) |   |   |   |   |
| Non-ulcerated lesion | — | — | 1.00 (ref) |   |
| Ulcer | 0.63 | 0.23, 1.71 | 0.68 (0.31-1.52) | 0.362 |
| Regional lymphadenopathy |   |   |   |   |
| No | — | — | 1.00 (ref) |   |
| Yes | 1.96 | 0.81, 4.71 | 1.73 (0.87-3.46) | 0.133 |
| Concomitant infection (pre-Tx) |   |   |   |   |
| No | — | — | 1.00 (ref) |   |
| Yes | 4.42 | 0.93, 21.0 | 2.95 (1.18-7.42) | 0.062 |
| Comorbidities |   |   |   |   |
| No | — | — | 1.00 (ref) |   |
| Yes | 1.39 | 0.28, 6.94 | 1.31 (0.36-4.73) | 0.688 |
| Leishmania species |   |   |   |   |
| L. braziliensis | — | — | 1.00 (ref) |   |
| L. panamensis and other L. viannia spp | 0.35 | 0.12, 1.04 | 0.44 (0.21-0.96) | 0.059 |
| Not isolated/Unknown | 0.24 | 0.06, 0.93 | 0.32 (0.11-0.92) | 0.038 |
| Treatment |   |   |   |   |
| Glucantime | — | — | 1.00 (ref) |   |
| Miltefosine | 0.62 | 0.26, 1.47 | 0.66 (0.31-1.40) | 0.273 |
| Medication dose range |   |   |   |   |
| Normal range | — | — | 1.00 (ref) |   |
| Overdose | 1.50 | 0.51, 4.39 | 1.39 (0.59-3.28) | 0.460 |
| Underdose | 0.00 | 0.00, Inf | Not estimable* | 0.989 |



In Pre-Tx parameters, lower platelet (RR 0.99 CI95% 0.98 - 0.99) and immature granulocyte counts were associated with TF, while eosinophils and the eosinophil/granulocyte ratio were not (*P* \>0.05 and *q* \>0.3) [(Table 2)]{.mark}. For immature granulocytes, the RR confidence interval marginally crossed the null value (RR 0.06 CI95% 0.003-1.06), but the Wald test (*P* =0.05) suggested a potential predictive capability, so the parameter was retained. No collinearity was found between the Pre-Tx parameters ([Figure S2]{.mark}). Consequently, Pre-Tx immature granulocyte count and Pre-Tx platelet count were carried forward to the multivariate models.

In EoTx parameters, significant associations with TF were found for eosinophil percentage, absolute eosinophil count, the eosinophil/neutrophil ratio and the eosinophil/granulocyte ratio. Notably, the EoTx eosinophil/neutrophil ratio showed the highest RR for TF (RR 3.72, CI95% 1.60-8.62). Although the association between the monocyte variation ratio and TF crossed the null value when analysing the confidence interval (RR 2.10 CI95% 0.90 - 4.89), the *p*-value of the bivariate analysis was 0.04 and thus was kept as a variable for construction of the multivariate models. Other blood count parameters initially suggested by PLS-DA showed no statistically significant differences or associations ([Table S2]{.mark}).

Several EoTx parameters showed strong correlations: absolute eosinophil count and eosinophil percentage (r=0.80), eosinophil count or percentage with the eosinophil/neutrophil index (r=0.68 and r=0.87, respectively), and eosinophils with the exponentiated eosinophil/immature granulocyte index (r=0.99 and r=0.80, respectively). There was no correlation between the variation in monocytes and other EoTx blood count parameters. Consequently, in the construction of multivariate models, we opted to include only the variation in monocytes and the percentage of eosinophils to avoid multicollinearity.

**Definition of cut-off points for blood count parameters**

To enhance the clinical utility of the identified biomarkers in distinguishing between cure and TF, and to improve their interpretability, cutoff points were defined ([Table S4]{.mark}). All parameters, with the exception of the EoTx absolute eosinophil count, maintained statistically significant associations with TF upon categorization according to the established cut-off points ([Table S5]{.mark}). The distribution of differential blood count parameters by outcome and the ROC areas for the selected cut-off points are shown in [Figure 2]{.mark}.

**Multivariate analysis of factors associated with TF**

Multivariate model analyses showed that the presence of concomitant infection was associated with an increased risk of TF by about 2.7 times (adjusted-RR (aRR) 2.74 CI95%: 1.30 - 5.80) ([Table 3]{.mark}, [Figure 6]{.mark}), while age acted as a protective factor: each additional year of life was associated with a 4% decrease in the probability of TF (aRR 0.96 CI95%: 0.94 - 0.98). Additionally, the presence of Pre-Tx lymphadenopathy (aRR 2.06 CI95% 1.07 - 3.97) and having co-infection at EoTx were associated with an increased risk of TF (aRR 6.57 CI95% 1.03 - 42.07). Of the Pre-Tx blood count parameters, immature granulocyte count and platelet count were independently associated with TF. Patients with a Pre-Tx granulocyte count ≤ 0.01 x 10^3^ cells/μL had 2 times the risk of TF (aRR 2.03 95% CI 1.05 - 3.91). Low Pre-Tx platelet counts were also independently associated with TF, with approximately a 3-fold increased risk (aRR 2.86, 95% CI 1.39 - 5.86; *P* = 0.004).

Regarding EoTx blood count parameters, a percentage of eosinophils ≥ 14% was associated with a 2-fold increased risk of TF (aRR 2.22 CI95% 1.16 - 4.24) and the Pre- to EoTx monocyte variation ratio ≥ 1.07 was associated with a 3-fold increased risk of TF (aRR 3.15 CI95%: 1.66 - 5.96). When the % eosinophils were replaced with their correlated parameters (such as the absolute eosinophil count or ratios that included eosinophils in the numerator or denominator), no improvement in the multivariate model performance was observed, and none of the parameters were significantly associated with TF. These results suggest that the % eosinophils may be a better biomarker of TF than the composite indices constructed using the correlated parameters.

The goodness-of-fit of the models was assessed, and a good fit was found (D-statistic \>0.4 and Pearson\'s X^2^ \> 0.7). The residuals and leverage analysis identified 4 outliers, corresponding to patients with a high number of risk factors for TF. When they were removed from the model, no variation \>10% in the estimates of association strengths was found, ruling out that the explanatory models could be affected by these patients.

**Predictive modelling of therapeutic failure**

Returning to the study's second question, we quantified how much blood count parameters add to the prediction of TF, and whether machine-learning algorithms outperform our parsimonious regression ([Table 5]{.mark}). Adding Pre-Tx blood counts (immature granulocytes and platelets) to the clinical model raised the out-of-sample discrimination of the parsimonious logistic model from a ROC-AUC of 0.68 (95% CI 0.67-0.69; clinical only) to 0.74 (95% CI 0.73-0.75), a statistically significant improvement (likelihood-ratio test *P* = 0.004) ([Figure 3]{.mark}, [Figure 4]{.mark}). At EoTx, adding blood counts increased the AUC from 0.62 (95% CI 0.59-0.64) to 0.67 (95% CI 0.65-0.69; *P* = 0.003). Combining Pre-Tx and EoTx parameters did not improve discrimination over the Pre-Tx model (AUC 0.68, 95% CI 0.66-0.71; likelihood-ratio *P* = 0.20). At the Youden threshold, the best model (Pre-Tx clinical + blood count) reached a sensitivity for failure of 72% and a specificity of 73%.

The machine-learning algorithms did not exceed the parsimonious logistic regression. In the Pre-Tx clinical + blood count scenario the ROC-AUC was 0.74 (95% CI 0.73-0.75) for the logistic model, 0.74 (95% CI 0.71-0.75) for LASSO, 0.69 (95% CI 0.65-0.72) for random forest, 0.72 (95% CI 0.69-0.74) for XGBoost, and 0.72 (95% CI 0.64-0.80) for the SuperLearner ensemble ([Figure 3]{.mark}, [Table 5]{.mark}). With the full, unselected feature set, discrimination was lower (random forest ROC-AUC 0.53 and XGBoost 0.59 for the Pre-Tx blood counts). SHAP analysis identified platelets and immature granulocytes before treatment, and the eosinophil percentage, eosinophil change and monocyte ratio at EoTx, as the parameters that contributed most to the machine-learning predictions ([Figure 5]{.mark}). In an exploratory analysis fitting random forests to separate families of parameters (absolute counts, percentages and ratios) ([Table S6]{.mark}), the count-based family reached the highest discrimination (ROC-AUC 0.61), with erythroid parameters (haematocrit, haemoglobin and red-cell count) among its top contributors; none of these sets exceeded the clinical + selected-biomarker model.

To translate these models into a bedside tool, we derived a points-based risk score for TF, which we named the SCARS score, after the predictor families it combines, namely Sex, blood Counts, Age, Regional adenopathy and Superinfection (concomitant infection), assigning each predictor points proportional to its regression coefficient following the method of Sullivan et al. \[57\], with age entered in bands (< 25, 25-39, >= 40 years) ([Table 6]{.mark}). Rather than a single binary threshold, we grouped patients into three risk strata. For the pre-treatment score, the low-risk stratum (33 patients) had no failures (0%, cross-validated 0%), the intermediate stratum 11.0% failures (cross-validated 10.6%) and the high-risk stratum 32.8% (cross-validated 27.4%); discrimination was a cross-validated AUC of 0.69 (95% CI 0.65-0.72; apparent 0.77). In practice this gives the clinician a direct read-off: a patient scoring 0-2 points before treatment has essentially no predicted risk of failure (no failures observed), a score of 3-5 points an intermediate risk (about one in nine, 11%), and a score of 6 or more points a high risk (about one in three, 33%), which can be used to tailor the intensity of follow-up. Used as a single cut-off, the score discriminated best at ≥ 5 points (the Youden-optimal threshold): a sensitivity of 84% and specificity of 57% for the pre-treatment score, and 75% and 61% at end of treatment ([Table 6]{.mark}). The score is designed to be read in both directions rather than as a stand-alone test: a low score rules failure out (negative predictive value 0.95 below the optimal cut-off), whereas a high score flags high risk (specificity rising to 77% at ≥ 6 points). Both operating points are informative, a lower threshold (≥ 4) maximising sensitivity for ruling failure out and a higher one (≥ 6) maximising specificity for flagging high risk, but we suggest the Youden-optimal cut-off of ≥ 5 as the single operating point for now, pending future external validation. The end-of-treatment score behaved similarly (low-risk 0%, intermediate 11.3%, high-risk 27.3%; cross-validated AUC 0.68). These estimates are internally validated by cross-validation; external validation in an independent cohort is still required before the SCARS score or its online calculator are used in clinical practice; both are presented as provisional, biology-informed decision-support tools that have been constructed but not yet externally validated.

Because treatment choice and age may modify the risk of failure, we explored the observed failure rate across risk strata by treatment and by age. These analyses are exploratory and the study is not powered for them: there were only 23 failures among 131 patients treated with meglumine antimoniate and 8 among 69 treated with miltefosine, and treatment was not randomised. Treatment was moreover strongly tied to age, as all children under 18 received miltefosine and none received antimony, so per-drug and per-age estimates cannot be disentangled (confounding by indication). With these caveats, the pre-treatment failure rate fell with age (21.7% below 25 years, 16.0% at 25-39 and 9.2% at 40 years or older, mirroring the protective adjusted effect of age), and within each SCARS risk stratum the observed failure rate was broadly similar for the two drugs; after adjusting for the score, the two treatments carried a comparable risk of failure (adjusted odds ratio for miltefosine versus antimony 0.92) ([Table S7]{.mark}). Much larger, ideally multicentre and/or randomised, samples will be required to develop a treatment- or age-specific failure score.

**DISCUSSION**

In this study we evaluated the utility of haematological parameters as early biomarkers of therapeutic outcomes in CL. Of the 32 Pre-Tx parameters, 31 EoTx parameters and 31 parameter ratios analyzed, Pre-Tx immature granulocyte count ≤ 0.01x10^3^ cells/µL, EoTx eosinophil percentage ≥ 14% and monocyte change ratio ≥ 1.07 were associated with TF in CL patients in central and southwestern Colombia. As isolated markers, however, their sensitivity and specificity were suboptimal, and they should not be used on their own to classify risk of TF. Their value lies instead in complementing clinical variables: when added to a clinical model, blood count parameters improved the prediction of TF (see below). At its best operating point the combined model behaved as a screening tool that flags patients for closer follow-up (sensitivity 72%, specificity 73%) rather than as a stand-alone diagnostic test.

The incidence of the TF observed in this study, at 15.50% (95% CI: 11.14% - 21.16%), aligns with previous research in the region \[23,28\], underscoring the stability of these rates over time. Week 13 was an informative time point for assessing therapeutic response: about three-quarters of failures had been detected by then (cumulative incidence 11.5% of 15.5%), consistent with the follow-up windows proposed by Olliaro et al. \[13,15\]. Nonetheless, roughly a quarter of failures occurred after week 13, so week 13 is best interpreted as a useful trend indicator rather than a definitive endpoint, and follow-up through week 26 remains necessary. Findings in clinical subgroups highlight a higher incidence of TF among men and younger populations, potentially mediated by differences in immune response \[29\] and the pharmacokinetics of antileishmanial drugs \[30,31\]. Furthermore, the presence of concomitant infections at baseline and EoTx was associated with a higher risk of TF. As these were predominantly infections of the CL lesion itself, they may directly impair local wound healing and sustain inflammation, in addition to any systemic interaction with the immune response to the parasite. This study also highlights the association of Pre-Tx lymphadenopathy, a possible indicator of infection severity, with an increased risk of TF. These systemic haematological signals are consistent with the innate immune biosignature of treatment failure recently characterised in this population \[60\].

Among Pre-Tx CBC parameters, low levels of immature granulocytes (≤ 0.01x10^3^ cells/µL) were significantly associated with an increased risk of TF in multivariate analyses (adjusted RR 2.03, 95% CI 1.05-3.91) \[32\]. Immature granulocytes include metamyelocytes, myelocytes, and promyelocytes, with the latter being divided into precursors of neutrophils (most abundant), eosinophils, and basophils \[32\]. Recent studies have identified that immature granulocyte levels measured in the CBC are directly proportional to the degree of systemic inflammation predominantly mediated by increased neutrophil functions \[33--35\]. Neutrophils contribute to *Leishmania* killing in the early stages of infection, by production of neutrophil extracellular traps \[36,37\] and activation of dendritic cells \[38\]. Therefore, having higher levels of immature granulocytes in patients who cured suggests higher neutrophil activity, and potential early control of infection. In contrast, higher risk of TF in CL patients with low immature granulocyte counts could be related to a decrease in the effector functions of neutrophils due to low rates of turnover in the bone marrow (manifested as a decrease in systemic granulocyte counts in TF patients) \[32\].

Lower platelet counts were associated with an increased risk of TF both in univariate analysis and, when dichotomised, in the multivariable pre-treatment model (adjusted RR 2.86, 95% CI 1.39-5.86); the role of platelets in the immune response to *Leishmania spp.* infection nonetheless remains unknown. Previous studies comparing CL patients with healthy individuals did not find differences in platelet counts at either Pre-Tx or EoTx (*P* \> 0.2) \[39,40\]. Platelets can nonetheless participate in antiparasitic immunity: in human malaria, platelets bind infected erythrocytes and kill the intracellular *Plasmodium* parasites they contain, and platelet counts and platelet-associated parasite killing correlate inversely with parasite load \[41\]. An increase in platelet factor 4 has also been reported in parasitic infection, suggesting platelet involvement even in the absence of direct contact with the parasite \[42\]. Whether platelets play an analogous role in *Leishmania* infection warrants further research.

Among EoTx parameters, eosinophils were significantly associated with therapeutic outcome. Although eosinophil absolute counts, percentages, and certain cellular indices involving eosinophils showed associations with TF, the percentage of eosinophils was the most succinct indicator of the systemic immune response. In the multivariate model, an EoTx eosinophil percentage ≥ 14% was associated with a two-fold increased risk of TF. In murine models of *Leishmania* infection, eosinophils contribute to parasite destruction in the early stages of infection, with a marked increase in eosinophil levels within the first 3-24 hours \[43\]. Eosinophils also participate in wound healing and epithelial remodelling, and their role appears to be context-dependent, with different functions co-occurring in distinct injury- and repair-related tissue compartments \[56\]. This duality may explain the paradox observed in our data: whereas a robust early eosinophil response might aid infection control and healing, the persistent elevation of eosinophil percentages at EoTx identified in our study may reflect a prolonged pro-inflammatory state that delays re-epithelialization and favours TF \[21\].

Eosinophils contribute in shaping the Th1/Th2 immune balance via positive feedback loops \[43\]. Production of IL-4 by eosinophils promotes differentiation of CD4+ T cells into a Th2 phenotype; in turn production of IL-4 by Th2 cells leads to recruitment of more eosinophils\[43--46\]. This mechanism could help explain the findings of this and other studies reporting higher EoTx eosinophil levels compared to Pre-Tx levels \[39,47\]. The fact that the associated risk of TF is observed at EoTx, but not Pre-Tx, suggests that this is a drug-dependent/drug-induced event.

An increase in monocytes at EoTx (≥ 1.07-fold change), correlated with a higher risk of TF.While monocyte activation and recruitment are initially crucial for combating the parasite \[48\], subsequent downregulation is necessary to promote wound healing and cure \[49\]. Continuous monocyte accumulation in tissues can lead to chronic inflammation and impaired wound healing. Therefore, the observed increase in monocytes at EoTx likely reflects not just an active immune response but also a failure to transition towards resolution and healing, thereby exacerbating the risk of treatment failure \[49,50\]. Consistent with this, a molecular signature of healing in cutaneous leishmaniasis is consolidated within the first 10 days of treatment, and its failure to establish characterises non-healing lesions \[61\].

Beyond individual associations, we compared a parsimonious, biology-informed logistic regression with penalised regression (LASSO), random forest, gradient boosting (XGBoost) and a SuperLearner ensemble. The parsimonious regression matched or exceeded the flexible machine-learning algorithms in out-of-sample discrimination, and the ensemble did not improve on the best single model. This is consistent with a growing body of evidence that machine-learning methods offer no systematic performance benefit over logistic regression for clinical prediction \[51\], and that flexible algorithms are \"data-hungry\", requiring many more events per variable to reach a stable performance and low optimism \[52\]. With only 31 treatment failures, our dataset falls well below the size at which such methods tend to outperform regression \[53\], favouring a parsimonious model for both discrimination and clinical interpretability. Reassuringly, SHAP analysis showed that the machine-learning models relied on the same biology-informed parameters selected by the regression \[54\], and the SuperLearner ensemble \[55\] did not surpass the best single learner. An exploratory analysis by parameter family suggested that erythroid indices (haematocrit, haemoglobin and red-cell count) recurrently contributed to the random-forest predictions without improving discrimination, a signal that may warrant exploration in larger cohorts. Expressed as a simple points score, these variables stratified patients into low-, intermediate- and high-risk groups; the score's main value is to rule out failure — a low score identifies patients at negligible risk — and to flag the high-risk stratum for closer follow-up, rather than to diagnose failure in any individual.

This study offers several strengths that contribute to the understanding of the association between hematological parameters and therapeutic outcomes in CL. The cohort design employed in this study allows for the assessment of directionality and potential causal relationships, providing a more robust foundation for interpreting the findings. Additionally, the standardized data collection procedures implemented in a single research center, along with regular quality control checks, ensure the reliability and validity of the collected data, minimizing the risk of information bias. Moreover, the adherence to recommended guidelines for defining therapeutic response and the processing of hematological parameters in a single laboratory following international standards further enhance the consistency and comparability of the results. Lastly, the inclusion of patients from different healthcare centers in endemic areas for CL increases the external validity of the study, allowing for the generalization of the findings to the broader population in the southern and central regions of Colombia.

Despite the strengths, several limitations should be acknowledged. Firstly, the study may have had insufficient statistical power to detect weak associations, highlighting the need for larger sample sizes to explore associations within specific clinical subgroups or parasite species. Additionally, the imputation of missing data resulted in the loss of statistical significance for some associations, indicating a potential impact on the performance of certain models due to missing data. Future studies should validate these models using independent patient cohorts. We also did not measure drug exposure or parasite drug susceptibility: both the pharmacokinetic–pharmacodynamic relationship of antimony, in which immune gene-expression profiles behave as measurable pharmacodynamic endpoints \[62\], and natural parasite resistance to meglumine antimoniate \[63\] are recognised determinants of therapeutic outcome that the complete blood count cannot capture. Integrating such host and parasite determinants, for example host genetic susceptibility expressed as polygenic risk scores together with parasite drug-resistance markers, with clinical and haematological scores could further improve predictive performance and help move the management of neglected tropical diseases towards personalized medicine.

**CONCLUSIONS**

This study establishes Pre-Tx immature granulocyte count ≤ 0.01x10^3^ cells/µL, EoTx eosinophil percentage ≥ 14%, and monocyte change ratio ≥ 1.07 as putative biomarkers for predicting TF in CL patients in central and southwestern Colombia. These blood count indicators, alongside clinical variables such as younger age, lymphadenopathy, and co-infections, underscore the feasibility of a personalized patient monitoring strategy for this neglected infectious disease. The findings advocate for a multifaceted approach in the clinical management of CL, suggesting that both CBC alterations and specific patient characteristics should guide a tailored therapeutic decision making, and clinical follow-up.

**FUNDINGS**

Research reported in this publication was supported by Wellcome Trust 107595/Z/15/Z and NIAID-NIH award number U19AI129910. D.E.R.-S. was supported by the Global Infectious Diseases Research Training Program of the Fogarty International Center of the National Institutes of Health under award number D43 TW006589. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.

**ACKNOWLEDGMENTS**

We gratefully acknowledge the members of the clinical and biostatistics groups of CIDEIM in Cali, Colombia, and Tumaco, Colombia, for their support in the implementation and conduct of the research protocol.

**AUTHOR CONTRIBUTIONS** [borrador --- ajustar según corresponda]{.mark}

D.E.R.-S. and M.A.G. conceived and designed the study. D.E.R.-S. curated the data, performed the statistical analysis and drafted the manuscript. L.G.-P. and J.J. contributed to data collection, clinical data interpretation and critical revision. J.A.G.-L. contributed to the statistical analysis and critical revision. L.O. supervised the epidemiological and biostatistical analysis. M.A.G. supervised the study, provided resources and critically revised the manuscript. All authors read and approved the final version of the manuscript.

**DISCLOSURE OF USE OF GENERATIVE AI AND AI-ASSISTED TECHNOLOGIES IN MANUSCRIPT PREPARATION**

In the course of composing this manuscript and preparing its figures, the authors employed generative AI tools --- including recent versions of Claude (Opus), ChatGPT and Gemini for language editing and analytical support, and GPT-Image for refining the graphical study-design figure (Figure 1A) from an author-created base. Subsequent to the use of these technologies, the authors meticulously reviewed and refined all AI-assisted content and assume complete responsibility for the final publication\'s content.

**SUPPLEMENTARY METHODS**

*Exploratory multivariate projection.* As an exploratory step, principal component analysis and partial least squares discriminant analysis (PLS-DA) were performed. The first two PLS-DA components did not clearly discriminate cures from TF (AUC 0.68); variable selection for the multivariate models therefore relied on the bivariate associations with therapeutic outcome, and the PLS-DA was not used further.

**REFERENCES**

1\. Padilla JC, Lizarazo E, Murillo OL, Mendigaña FA, Pachón E, Vera MJ. Transmisión de las ETV en Colombia, 1990-2016 ARTÍCULO ORIGINAL. Biomédica **2017**; 37:27--40.

2\. Organizacion Panamericana de la Salud/, Organizacion Mundial de la Salud. Plan of action to strengthen the surveillance and control of leishmaniasis in the Americas 2017-2022. Http://Www2PahoOrg **2017**; :70. Available at: http://iris.paho.org/xmlui/handle/123456789/34147.

3\. Weigle KA, Santrich C, Martinez F, Valderrama L, Saravia NG. Epidemiology of cutaneous leishmaniasis in Colombia: a longitudinal study of the natural history, prevalence, and incidence of infection and clinical manifestations. J Infect Dis **1993**; 168:699--708. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=8354912.

4\. Figueroa RA, Lozano LE, Romero IC, et al. Detection of leishmania in unaffected mucosal tissues of patients with cutaneous leishmaniasis caused by leishmania (viannia) species. J Infect Dis **2009**; 200:638--646. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=19569974.

5\. Agudelo Chivatá JN. Leishmaniasis Cutánea, Mucosa Y Visceral, Colombia 2018. Inst Nac Salud **2019**; 05:28.

6\. Soto J, Toledo J, Vega J, Berman J. Short report: efficacy of pentavalent antimony for treatment of colombian cutaneous leishmaniasis. Am J Trop Med Hyg **2005**; 72:421--422. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=15827279.

7\. Soto J, Rea J, Balderrama M, et al. Short report: Efficacy of miltefosine for Bolivian cutaneous leishmaniasis. Am J Trop Med Hyg **2008**; 78:210--211. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=18256415.

8\. Navin TR, Arana BA, Arana FE, Berman JD, Chajon JF. Placebo-controlled clinical trial of sodium stibogluconate (Pentostam) versus ketoconazole for treating cutaneous leishmaniasis in Guatemala. J Infect Dis **1992**; 165:528--534. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=1311351.

9\. Navin TR, Arana BA, Arana FE, de Merida AM, Castillo AL, Pozuelos JL. Placebo-controlled clinical trial of meglumine antimonate (glucantime) vs. localized controlled heat in the treatment of cutaneous leishmaniasis in Guatemala. Am J Trop Med Hyg **1990**; 42:43--50. Available at: http://www.ncbi.nlm.nih.gov/pubmed/2405727.

10\. Velez I, Lopez L, Sanchez X, Mestra L, Rojas C, Rodriguez E. Efficacy of miltefosine for the treatment of American cutaneous leishmaniasis. Am J Trop Med Hyg **2010**; 83:351--356. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=20682881.

11\. López L, Vélez I, Asela C, et al. A phase II study to evaluate the safety and efficacy of topical 3% amphotericin B cream (Anfoleish) for the treatment of uncomplicated cutaneous leishmaniasis in Colombia. PLoS Negl Trop Dis **2018**; 12:e0006653. Available at: https://dx.plos.org/10.1371/journal.pntd.0006653.

12\. Grajalew LF, Ochoa MT, Palacios R, Osorio LE. Treatment failure in children in a randomized clinical trial with 10 and 20 days of meglumine antimonate for cutaneous leishmaniasis due to Leishmania viannia species. Am J Trop Med Hyg **2001**; 64:187--193. Available at: https://ajtmh.org/doi/10.4269/ajtmh.2001.64.187.

13\. Olliaro P, Grogl M, Boni M, et al. Harmonized clinical trial methodologies for localized cutaneous leishmaniasis and potential for extensive network with capacities for clinical evaluation. PLoS Negl Trop Dis **2018**; 12:e0006141. Available at: http://www.ncbi.nlm.nih.gov/pubmed/29329311.

14\. Directrices para el tratamiento de las leishmaniasis en la Región de las Américas. Segunda edición. Pan American Health Organization, 2022. Available at: https://iris.paho.org/handle/10665.2/56121.

15\. Olliaro P, Vaillant M, Arana B, et al. Methodology of clinical trials aimed at assessing interventions for cutaneous leishmaniasis. PLoS Negl Trop Dis **2013**; 7:e2130. Available at: http://www.ncbi.nlm.nih.gov/pubmed/23556016.

16\. Castro Noriega M del M, Martínez Valencia ÁJ, Cossio Duque A, Jojoa SJ. Seguimiento de pacientes con leishmaniasis tegumentaria: una experiencia en un centro de referencia. 2014: 71.

17\. Rijal S, Sundar S, Mondal D, Das P, Alvar J, Boelaert M. Eliminating visceral leishmaniasis in South Asia: the road ahead. BMJ **2019**; 364:k5224. Available at: https://www.bmj.com/lookup/doi/10.1136/bmj.k5224.

18\. Matlashewski G, Arana B, Kroeger A, et al. Research priorities for elimination of visceral leishmaniasis. Lancet Glob Heal **2014**; 2:e683--e684. Available at: https://linkinghub.elsevier.com/retrieve/pii/S2214109X14703183.

19\. Tefferi A, Hanson CA, Inwards DJ. How to Interpret and Pursue an Abnormal Complete Blood Cell Count in Adults. Mayo Clin Proc **2005**; 80:923--936. Available at: https://linkinghub.elsevier.com/retrieve/pii/S0025619611615681.

20\. Zhang Y, Peng W, Zheng X. The prognostic value of the combined neutrophil-to-lymphocyte ratio (NLR) and neutrophil-to-platelet ratio (NPR) in sepsis. Sci Rep **2024**; 14:15075. Available at: https://www.nature.com/articles/s41598-024-64469-8.

21\. Giraldo-Parra L, Rebellón-Sánchez DE, Navas A, Belew AT, El-Sayed NM, Gómez MA. Consolidation of a Molecular Signature of Healing in Cutaneous Leishmaniasis Is Achieved during the First 10 Days of Treatment. J Immunol **2024**; 212:894--903. Available at: https://journals.aai.org/jimmunol/article/212/5/894/266619/Consolidation-of-a-Molecular-Signature-of-Healing.

22\. Social M de S y P. Lineamientos de atención clínica integral para Leishmaniasis en Colombia. 2023: 1--42. Available at: https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/PP/PAI/Lineamientos-leishmaniasis.pdf.

23\. Castro MDM, Cossio A, Velasco C, Osorio L. Risk factors for therapeutic failure to meglumine antimoniate and miltefosine in adults and children with cutaneous leishmaniasis in Colombia: A cohort study. PLoS Negl Trop Dis **2017**; 11:e0005515. Available at: http://www.ncbi.nlm.nih.gov/pubmed/28379954.

24\. Lai Y. A statistical method for the conservative adjustment of false discovery rate (q-value). BMC Bioinformatics **2017**; 18:69. Available at: http://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-017-1474-6.

25\. Fluss R, Faraggi D, Reiser B. Estimation of the Youden Index and its Associated Cutoff Point. Biometrical J **2005**; 47:458--472. Available at: https://onlinelibrary.wiley.com/doi/10.1002/bimj.200410135.

26\. Thiele C, Hirschfeld G. cutpointr: Improved Estimation and Validation of Optimal Cutpoints in R. J Stat Softw **2020**; :1--27. Available at: http://arxiv.org/abs/2002.09209.

27\. Valencia C, Arévalo J, Dujardin JC, Llanos-Cuentas A, Chappuis F, Zimic M. Prediction score for antimony treatment failure in patients with ulcerative leishmaniasis lesions. PLoS Negl Trop Dis **2012**; 6:1--6.

28\. Castro-Noriega M del M. Factores Asociados a Falla Terapéutica en Niños y Adultos con Leishmaniasis cutánea en tres zonas endémicas de Colombia 2007-2013. 2015;

29\. Lockard RD, Wilson ME, Rodríguez NE. Sex-Related Differences in Immune Response and Symptomatic Manifestations to Infection with Leishmania Species. J Immunol Res **2019**; 2019:1--14. Available at: https://www.hindawi.com/journals/jir/2019/4103819/.

30\. Castro MD, Gomez MA, Kip AE, et al. Pharmacokinetics of Miltefosine in Children and Adults with Cutaneous Leishmaniasis. Antimicrob Agents Chemother **2017**; 61. Available at: http://www.ncbi.nlm.nih.gov/pubmed/27956421.

31\. Cruz A, Rainey PM, Herwaldt BL, et al. Pharmacokinetics of antimony in children treated for leishmaniasis with meglumine antimoniate. J Infect Dis **2007**; 195:602--608. Available at: http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=17230422.

32\. Farrell AP. BLOOD \| Cellular Composition of the Blood. In: Encyclopedia of Fish Physiology. Elsevier, 2011: 984--991. Available at: https://linkinghub.elsevier.com/retrieve/pii/B9780123745538001258.

33\. INCIR S. The Role of Immature Granulocytes and Inflammatory Hemogram Indices in the Inflammation. Int J Med Biochem **2020**; Available at: http://www.internationalbiochemistry.com/jvi.aspx?un=IJMB-02986&volume=.

34\. Georgakopoulou V, Makrodimitri S, Triantafyllou M, et al. Immature granulocytes: Innovative biomarker for SARS‑CoV‑2 infection. Mol Med Rep **2022**; 26:217. Available at: http://www.spandidos-publications.com/10.3892/mmr.2022.12733.

35\. Lipiński M, Rydzewska G. Immature granulocytes predict severe acute pancreatitis independently of systemic inflammatory response syndrome. Gastroenterol Rev **2017**; 2:140--144. Available at: https://www.termedia.pl/doi/10.5114/pg.2017.68116.

36\. Rochael NC, Guimaraes-Costa AB, Nascimento MT, et al. Classical ROS-dependent and early/rapid ROS-independent release of Neutrophil Extracellular Traps triggered by Leishmania parasites. Sci Rep **2015**; 5:18302. Available at: http://www.ncbi.nlm.nih.gov/pubmed/26673780.

37\. Guimarães-Costa AB, Nascimento MTC, Froment GS, et al. Leishmania amazonensis promastigotes induce and are killed by neutrophil extracellular traps. Proc Natl Acad Sci U S A **2009**; 106:6748--6753.

38\. Charmoy M, Brunner-Agten S, Aebischer D, et al. Neutrophil-derived CCL3 is essential for the rapid recruitment of dendritic cells to the site of Leishmania major inoculation in resistant mice. PLoS Pathog **2010**; 6.

39\. Sula B, Tekin R. Use of hematological parameters in evaluation of treatment efficacy in cutaneous leishmaniasis. J Microbiol Infect Dis **2015**; 5:167--172. Available at: https://www.jmidonline.org/?mno=302656988.

40\. An I, Ayhan E, Aksoy M, Ozturk M, Erat T, Doni NY. Evaluation of inflammatory parameters in patients with cutaneous leishmaniasis. Dermatol Ther **2021**; 34. Available at: https://onlinelibrary.wiley.com/doi/10.1111/dth.14603.

41\. Kho S, Barber BE, Johar E, et al. Platelets kill circulating parasites of all major Plasmodium species in human malaria. Blood **2018**; 132:1332--1344. Available at: https://ashpublications.org/blood/article/132/12/1332/39622/Platelets-kill-circulating-parasites-of-all-major.

42\. J Matowicka-Karna, Kemona H. Does parasitic infection affect platelet factor 4 concentration? Rocz Akad Med Bialymst **2001**; 46:126--32.

43\. Rodríguez NE, Wilson ME. Eosinophils and mast cells in leishmaniasis. Immunol Res **2014**; 59:129--141. Available at: http://link.springer.com/10.1007/s12026-014-8536-x.

44\. Blanchard C, Rothenberg ME. Chapter 3 Biology of the Eosinophil. 2009: 81--121. Available at: https://linkinghub.elsevier.com/retrieve/pii/S0065277608010031.

45\. Jacobsen EA, Taranova AG, Lee NA, Lee JJ. Eosinophils: Singularly destructive effector cells or purveyors of immunoregulation? J Allergy Clin Immunol **2007**; 119:1313--1320. Available at: https://linkinghub.elsevier.com/retrieve/pii/S0091674907006422.

46\. Akuthota P, Wang H, Weller PF. Eosinophils as antigen-presenting cells in allergic upper airway disease. Curr Opin Allergy Clin Immunol **2010**; 10:14--19. Available at: https://journals.lww.com/00130832-201002000-00004.

47\. Gómez MA, Navas A, Prieto MD, et al. Immuno-pharmacokinetics of Meglumine Antimoniate in Patients with Cutaneous Leishmaniasis Caused by Leishmania (Viannia). Clin Infect Dis **2021**; 72:E484--E492.

48\. Scott P, Novais FO. Cutaneous leishmaniasis: Immune responses in protection and pathogenesis. Nat Rev Immunol **2016**; 16:581--592.

49\. Navas A, Fernandez O, Gallego-Marin C, et al. Profiles of Local and Systemic Inflammation in the Outcome of Treatment of Human Cutaneous Leishmaniasis Caused by Leishmania (Viannia). Infect Immun **2020**; 88. Available at: http://www.ncbi.nlm.nih.gov/pubmed/31818959.

50\. Pham M-HT, Bonello GB, Castiblanco J, et al. The rs1024611 Regulatory Region Polymorphism Is Associated with CCL2 Allelic Expression Imbalance. PLoS One **2012**; 7:e49498. Available at: https://dx.plos.org/10.1371/journal.pone.0049498.

51\. Christodoulou E, Ma J, Collins GS, Steyerberg EW, Verbakel JY, van Calster B. A systematic review shows no performance benefit of machine learning over logistic regression for clinical prediction models. J Clin Epidemiol **2019**; 110:12--22. Available at: https://doi.org/10.1016/j.jclinepi.2019.02.004.

52\. van der Ploeg T, Austin PC, Steyerberg EW. Modern modelling techniques are data hungry: a simulation study for predicting dichotomous endpoints. BMC Med Res Methodol **2014**; 14:137. Available at: https://doi.org/10.1186/1471-2288-14-137.

53\. Riley RD, Ensor J, Snell KIE, et al. Calculating the sample size required for developing a clinical prediction model. BMJ **2020**; 368:m441. Available at: https://doi.org/10.1136/bmj.m441.

54\. Lundberg SM, Lee S-I. A unified approach to interpreting model predictions. Adv Neural Inf Process Syst **2017**; 30:4765--4774. Available at: https://papers.nips.cc/paper/7062-a-unified-approach-to-interpreting-model-predictions.

55\. van der Laan MJ, Polley EC, Hubbard AE. Super Learner. Stat Appl Genet Mol Biol **2007**; 6:Article25. Available at: https://doi.org/10.2202/1544-6115.1309.

56\. Coden ME, Berdnikovs S. Eosinophils in wound healing and epithelial remodeling: is coagulation a missing link? J Leukoc Biol **2020**; 108:93--103. Available at: https://doi.org/10.1002/JLB.3MR0120-390R.

57\. Sullivan LM, Massaro JM, D'Agostino RB Sr. Presentation of multivariate data for clinical use: the Framingham Study risk score functions. Stat Med **2004**; 23:1631--1660. Available at: https://doi.org/10.1002/sim.1742.

58\. Steyerberg EW. Clinical Prediction Models: A Practical Approach to Development, Validation, and Updating. 2nd ed. Cham: Springer, **2019**.

59\. Collins GS, Reitsma JB, Altman DG, Moons KGM. Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): the TRIPOD statement. Ann Intern Med **2015**; 162:55--63. Available at: https://doi.org/10.7326/M14-0697.

60\. Gómez MA, Belew AT, Vargas DA, Giraldo-Parra L, Alexander N, Rebellón-Sánchez DE, Alexander TA, El-Sayed NM. Innate biosignature of treatment failure in human cutaneous leishmaniasis. Nat Commun **2025**; 16:3235. Available at: https://doi.org/10.1038/s41467-025-58330-3.

61\. Giraldo-Parra L, Rebellón-Sánchez DE, Navas A, Belew AT, El-Sayed NM, Gómez MA. Consolidation of a molecular signature of healing in cutaneous leishmaniasis is achieved during the first 10 days of treatment. J Immunol **2024**; 212:894--903. Available at: https://doi.org/10.4049/jimmunol.2300576.

62\. Alexander N, Giraldo-Parra L, Rebellón-Sánchez DE, Gómez MA. Modelling immune gene expression profiles as pharmacodynamic endpoints of antileishmanial treatment. Br J Clin Pharmacol **2026**; 92:3666--3675. Available at: https://doi.org/10.1002/bcp.70675.

63\. Fernández OL, Rosales-Chilama M, Sánchez-Hidalgo A, Gómez P, Rebellón-Sánchez DE, Regli IB, Díaz-Varela M, Tacchini-Cottier F, Saravia NG. Natural resistance to meglumine antimoniate is associated with treatment failure in cutaneous leishmaniasis caused by Leishmania (Viannia) panamensis. PLoS Negl Trop Dis **2024**; 18:e0012156. Available at: https://doi.org/10.1371/journal.pntd.0012156.

ewpage

