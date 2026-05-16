# Data

Place the Our World in Data CO₂ and Energy Dataset in this folder.

Expected filename:

```text
owid-co2-data.csv
```

Dataset source: Our World in Data CO₂ and Energy Dataset.

The dataset is not included in this repository by default. Download it from the official OWID source and place it in this folder before running the analysis script.

# Methodology

This project follows a structured environmental data analytics workflow.

## Workflow

1. Data import and cleaning
2. Missing value assessment
3. Feature engineering
4. Descriptive statistics
5. Exploratory data analysis
6. Probability-based risk analysis
7. Hypothesis testing
8. Correlation analysis
9. Regression modeling
10. Model diagnostics
11. Output generation and visualization

## Statistical Methods

- Descriptive statistics
- Quantile-based emission categorization
- Wilcoxon rank-sum test
- Effect size estimation
- Spearman correlation
- Log-transformed multiple linear regression

## Modeling Formula

```text
log(CO₂) ~ log(GDP per capita) + log(Energy per capita) + log(Population)
```

This model is used to evaluate how economic development, energy consumption, and population size explain variation in total CO₂ emissions.
