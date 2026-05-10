# Logistic Regression in R

Three datasets, three problems, one algorithm — predicting income, diabetes, and churn using logistic regression with base R, caret, and tidymodels.

## Problem

Logistic regression is the backbone of binary classification. But the gap between "fit a model" and "understand what it's telling you" is where most practitioners get stuck. This project works through three real datasets to show the full picture: data prep, model building, coefficient interpretation, probability calculation, and evaluation.

## What's Inside

| Script | Dataset | Target | Rows | What It Covers |
|--------|---------|--------|------|---------------|
| `adult - logistic regression.R` | Census Income | Income >50K | 48,842 | Full EDA, feature engineering, multiple model iterations, GLM + tidymodels |
| `churn - logistic regression.R` | Census Income | Income >50K | 48,842 | Alternative approach with different feature engineering |
| `diabetes - logistic regression.R` | Pima Indians | Diabetes pos/neg | 392 | Single-variable model, manual probability calculation, visualisation |
| `calculate probability.R` | — | — | — | Probability ↔ odds ↔ log-odds conversions |

## Results

### Diabetes (Single Variable — Glucose Only)

| Metric | Value |
|--------|-------|
| Intercept | -6.159 |
| Glucose coefficient | 0.043 |
| A glucose of 180 → P(diabetes) | 83.6% |
| A glucose of 20 → P(diabetes) | 0.5% |

### Adult Income (Full Model)

The adult dataset includes 48K rows with features like age, workclass, education, marital status, race, gender, and hours per week. The model predicts whether income exceeds $50K.

Key features:
- **education.num** — strongest predictor (higher education → higher income)
- **age** — positive relationship with income
- **hours.per.week** — more hours = more likely to earn >50K
- **gender** — significant coefficient (reflecting wage gap in the data)

## How Logistic Regression Works

**Linear model** → **sigmoid function** → **probability**:

```
log-odds = β₀ + β₁x₁ + β₂x₂ + ...
probability = exp(log-odds) / (1 + exp(log-odds))
```

### Probability ↔ Odds ↔ Log-Odds

| Probability | Odds | Log-Odds |
|------------|------|----------|
| 0.25 | 0.333 | -1.099 |
| 0.50 | 1.000 | 0.000 |
| 0.75 | 3.000 | 1.099 |

```r
# probability → odds
odds <- p / (1 - p)

# odds → log-odds
log_odds <- log(odds)

# log-odds → probability
p <- exp(log_odds) / (1 + exp(log_odds))
```

### Interpreting Coefficients

A coefficient of 0.043 for glucose means:
- Each unit increase in glucose → odds multiply by exp(0.043) = 1.044
- A 10-unit increase → odds multiply by exp(0.43) = 1.54 (54% higher odds)

## Setup

```bash
git clone https://github.com/wsamuelw/logistic-regression-in-r.git
cd logistic-regression-in-r
```

```r
install.packages(c("tidyverse", "tidymodels", "caret", "vip", "car", "naniar", "mlbench"))
source("diabetes - logistic regression.R")
```

## Data Sources

| Dataset | Source | Description |
|---------|--------|-------------|
| Census Income / Adult | [UCI ML Repository](https://archive.ics.uci.edu/ml/datasets/Adult) | Predict income from demographics |
| Pima Indians Diabetes | `mlbench::PimaIndiansDiabetes2` | Predict diabetes from medical indicators |

## Tech Stack

- **base R** — `glm()` for logistic regression
- **tidymodels** — modern modelling workflow
- **caret** — train/test splitting and evaluation
- **vip** — variable importance plots
- **naniar** — missing data visualisation
- **car** — diagnostic plots

## References

- [GLM documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html)
- [Interpreting logistic regression coefficients](https://www.displayr.com/how-to-interpret-logistic-regression-coefficients/)
- [R Generalized Linear Model](https://www.guru99.com/r-generalized-linear-model.html)

## License

MIT
