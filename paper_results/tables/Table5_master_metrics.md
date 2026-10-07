## Table 5. Out-of-sample predictive performance of all models (failure-focused, Youden threshold)

Repeated stratified cross-validation. AUC with 95% CI. Sensitivity/specificity, PPV/NPV for the FAILURE class.


**PRE — clinical only**

| Model | Accuracy | ROC-AUC (95% CI) | Sens. (failure) | Spec. (failure) | PPV | NPV | F1 | Brier |
|---|---|---|---|---|---|---|---|---|
| Parsimonious logistic | 56% | 0.68 (0.67–0.69) | 81% | 52% | 0.24 | 0.94 | 0.36 | 0.12 |
| LASSO | 60% | 0.67 (0.64–0.68) | 73% | 57% | 0.24 | 0.92 | 0.36 | 0.13 |
| Random Forest | 59% | 0.65 (0.61–0.67) | 75% | 56% | 0.24 | 0.93 | 0.36 | 0.13 |
| XGBoost | 65% | 0.56 (0.53–0.58) | 68% | 64% | 0.26 | 0.92 | 0.38 | 0.13 |
| SuperLearner | 66% | 0.65 (0.55–0.74) | 64% | 66% | 0.26 | 0.91 | 0.37 | 0.12 |

**PRE — clinical + blood count**

| Model | Accuracy | ROC-AUC (95% CI) | Sens. (failure) | Spec. (failure) | PPV | NPV | F1 | Brier |
|---|---|---|---|---|---|---|---|---|
| Parsimonious logistic | 73% | 0.74 (0.73–0.75) | 72% | 73% | 0.33 | 0.93 | 0.45 | 0.12 |
| LASSO | 71% | 0.74 (0.71–0.75) | 73% | 70% | 0.32 | 0.94 | 0.44 | 0.12 |
| Random Forest | 61% | 0.69 (0.65–0.72) | 77% | 58% | 0.26 | 0.94 | 0.38 | 0.13 |
| XGBoost | 66% | 0.71 (0.69–0.74) | 78% | 63% | 0.28 | 0.94 | 0.41 | 0.12 |
| SuperLearner | 67% | 0.72 (0.64–0.80) | 71% | 66% | 0.28 | 0.93 | 0.40 | 0.12 |

**EoTx — clinical only**

| Model | Accuracy | ROC-AUC (95% CI) | Sens. (failure) | Spec. (failure) | PPV | NPV | F1 | Brier |
|---|---|---|---|---|---|---|---|---|
| Parsimonious logistic | 60% | 0.62 (0.59–0.64) | 70% | 58% | 0.25 | 0.91 | 0.37 | 0.14 |
| LASSO | 57% | 0.62 (0.59–0.66) | 73% | 54% | 0.24 | 0.91 | 0.36 | 0.14 |
| Random Forest | 61% | 0.63 (0.61–0.65) | 70% | 60% | 0.26 | 0.91 | 0.37 | 0.14 |
| XGBoost | 63% | 0.56 (0.55–0.59) | 67% | 62% | 0.26 | 0.91 | 0.37 | 0.13 |
| SuperLearner | 54% | 0.56 (0.45–0.67) | 71% | 50% | 0.22 | 0.90 | 0.34 | 0.14 |

**EoTx — clinical + blood count**

| Model | Accuracy | ROC-AUC (95% CI) | Sens. (failure) | Spec. (failure) | PPV | NPV | F1 | Brier |
|---|---|---|---|---|---|---|---|---|
| Parsimonious logistic | 62% | 0.67 (0.65–0.69) | 72% | 60% | 0.26 | 0.92 | 0.38 | 0.14 |
| LASSO | 62% | 0.67 (0.66–0.69) | 72% | 60% | 0.26 | 0.92 | 0.38 | 0.13 |
| Random Forest | 66% | 0.66 (0.62–0.68) | 60% | 68% | 0.28 | 0.90 | 0.37 | 0.13 |
| XGBoost | 70% | 0.66 (0.62–0.68) | 59% | 72% | 0.29 | 0.90 | 0.39 | 0.13 |
| SuperLearner | 42% | 0.67 (0.57–0.77) | 96% | 31% | 0.21 | 0.98 | 0.35 | 0.13 |

**Combined PRE + EoTx blood count**

| Model | Accuracy | ROC-AUC (95% CI) | Sens. (failure) | Spec. (failure) | PPV | NPV | F1 | Brier |
|---|---|---|---|---|---|---|---|---|
| Parsimonious logistic | 69% | 0.68 (0.66–0.71) | 67% | 69% | 0.30 | 0.91 | 0.41 | 0.13 |
| LASSO | 71% | 0.69 (0.68–0.73) | 64% | 72% | 0.31 | 0.91 | 0.42 | 0.13 |
| Random Forest | 73% | 0.67 (0.65–0.69) | 57% | 77% | 0.33 | 0.90 | 0.41 | 0.13 |
| XGBoost | 75% | 0.67 (0.63–0.70) | 56% | 78% | 0.35 | 0.90 | 0.42 | 0.13 |
| SuperLearner | 82% | 0.69 (0.58–0.80) | 43% | 90% | 0.46 | 0.89 | 0.44 | 0.13 |
