## Table 6. Points-based risk score for therapeutic failure, with risk strata

Points were assigned to each predictor in proportion to its regression coefficient (method of Sullivan et al.); age was categorised into bands only for the score (the regression models use continuous age). Risk strata show the observed failure rate (apparent) and the cross-validated (optimism-corrected) failure rate. External validation in an independent cohort is still required.


**Pre-treatment — score points**

| Predictor | Points |
|---|---|
| Age < 25 years | 2 |
| Age 25-39 years | 1 |
| Age >= 40 years (reference) | 0 |
| Male sex | 2 |
| Regional lymphadenopathy | 1 |
| Concomitant infection (pre-Tx) | 2 |
| Immature granulocytes <= 0.01 x10^3/uL | 1 |
| Platelets <= 269 x10^3/uL | 2 |

**Pre-treatment — risk strata**  (cross-validated AUC 0.685, 95% CI 0.646-0.722; apparent 0.773)

| Risk stratum | Score | N | Failures | Observed failure % (95% CI) | Cross-validated failure % |
|---|---|---|---|---|---|
| Low | 0-2 | 33 | 0 | 0.0 (0.0-10.4) | 0.0 |
| Intermediate | 3-5 | 109 | 12 | 11.0 (6.4-18.3) | 10.6 |
| High | 6+ | 58 | 19 | 32.8 (22.1-45.6) | 27.4 |

**End of treatment — score points**

| Predictor | Points |
|---|---|
| Age < 25 years | 2 |
| Age 25-39 years | 1 |
| Age >= 40 years (reference) | 0 |
| Male sex | 2 |
| Regional lymphadenopathy | 2 |
| Concomitant infection (EoTx) | 3 |
| Eosinophils >= 14% | 2 |
| Monocyte ratio (EoTx/pre) >= 1.07 | 2 |

**End of treatment — risk strata**  (cross-validated AUC 0.682, 95% CI 0.636-0.725; apparent 0.78)

| Risk stratum | Score | N | Failures | Observed failure % (95% CI) | Cross-validated failure % |
|---|---|---|---|---|---|
| Low | 0-2 | 32 | 0 | 0.0 (0.0-10.7) | 0.0 |
| Intermediate | 3-4 | 62 | 7 | 11.3 (5.6-21.5) | 16.7 |
| High | 5+ | 77 | 21 | 27.3 (18.6-38.1) | 21.7 |

**Pre-treatment score — operating characteristics (false positives / false negatives)**

| Threshold | Sensitivity | Specificity | PPV | NPV | False positives | False negatives |
|---|---|---|---|---|---|---|
| Rule-out (score >= 4) | 94% | 33% | 0.20 | 0.96 | 114 | 2 |
| Best balance (score >= 5, Youden-optimal) | 84% | 57% | 0.26 | 0.95 | 73 | 5 |
| Rule-in (score >= 6) | 61% | 77% | 0.33 | 0.92 | 39 | 12 |

The score reaches its best overall balance at the Youden-optimal cut-off (score >= 5): 84% sensitivity and 57% specificity for the pre-treatment score (75%/61% at end of treatment). It is meant to be read in both directions rather than as a stand-alone diagnostic test: a low score rules failure out (high negative predictive value; a lower cut-off such as >= 4 maximises sensitivity), while a high score (>= 6) raises specificity and flags the high-risk stratum that warrants closer follow-up.
