# Email Marketing Response Analysis - Statistical Modelling

## Project Overview

This project explores how statistical modelling can help businesses identify customers who are most likely to respond to email marketing campaigns.

Using the Customer Personality Analysis dataset, we investigated customer characteristics, purchasing behaviour, and campaign responses to develop data-driven customer targeting strategies.

The project was completed as a consultancy assignment for IT3081 – Statistical Modelling, BSc (Hons) Data Science, Sri Lanka Institute of Information Technology (SLIIT).

## Business Problem

Broad email marketing campaigns can result in low response rates, inefficient marketing expenditure, and missed business opportunities.

The objective was to analyse customer data, identify factors associated with campaign responses, and investigate predictive modelling techniques to support more effective customer targeting.

## Dataset

- Source: Kaggle – Customer Personality Analysis
- Initial records: 2,240 customers
- Variables: 29
- Records after cleaning: 2,238
- Campaign response rate: 14.9%

The dataset contains customer demographic information, spending patterns, purchasing behaviour, and campaign response information.

## Statistical Analysis and Methodology

### 1. Exploratory Data Analysis (EDA)
- Analysed customer demographics and income.
- Investigated spending and purchasing patterns.
- Explored differences between responders and non-responders.

### 2. Statistical Inference
Applied statistical tests to investigate relationships and differences:

- Welch's t-test
- Chi-square test
- ANOVA
- F-test

### 3. Predictive Modelling
Developed and evaluated classification models:

- Logistic Regression
- Ridge Regression
- LASSO
- Naïve Bayes

### 4. Advanced Statistical Techniques
- Principal Component Analysis (PCA)
- Bayesian decision-making
- Experimental design
- Time series feasibility analysis

### 5. Model Evaluation
Evaluated models using:

- Accuracy
- Sensitivity
- Specificity
- F1-score
- ROC-AUC

## Key Findings

### Customer Characteristics

- Responders had an average income of approximately $60,183, compared with $50,500 for non-responders.
- Income differences were statistically significant (p < 0.001).
- Education level was significantly associated with campaign response (p < 0.001).

### Predictive Performance

| Model | Accuracy | Sensitivity | ROC-AUC |
|---|---:|---:|---:|
| Logistic Regression | 86.3% | 21.9% | 0.812 |
| Naïve Bayes | Not specified | 51.2% | Not specified |

Although Logistic Regression achieved good overall accuracy and ROC-AUC, its sensitivity highlighted the challenge of identifying actual responders in an imbalanced dataset.

Naïve Bayes achieved higher sensitivity, demonstrating a different trade-off in identifying potential responders.

### PCA Analysis

Three principal components retained 79.88% of the variance. However, the resulting model's ROC-AUC decreased from 0.812 to 0.696.

## Business Recommendation: Customer Response Prediction Dashboard

We proposed a customer segmentation dashboard to support probability-based marketing decisions.

| Customer Segment | Customers | Recommended Strategy |
|---|---:|---|
| High Response | 118 | Priority campaigns |
| Medium Response | 529 | Selective targeting |
| Low Response | 1,591 | Lower-cost communication |

This proposed solution aims to help businesses prioritise marketing resources and improve campaign efficiency.

**Note:** The dashboard is presented as a proposed business solution, not as a fully deployed production system.

## Key Learning Outcomes

- Applying statistical inference to real-world business questions.
- Analysing imbalanced classification datasets.
- Comparing predictive models using multiple evaluation metrics.
- Understanding the trade-offs between accuracy and sensitivity.
- Using statistical evidence to support business recommendations.
- Translating analytical findings into practical decision-making strategies.

## Technologies and Techniques

- Python
- Statistical Modelling
- Exploratory Data Analysis
- Logistic Regression
- Ridge and LASSO
- Naïve Bayes
- PCA
- Bayesian Decision-Making
- Statistical Hypothesis Testing

## Project Information

**Module:** IT3081 – Statistical Modelling  
**Project Type:** Academic Consultancy Group Project  
**Degree:** BSc (Hons) Information Technology – Data Science Specialisation  
**Institution:** Sri Lanka Institute of Information Technology (SLIIT)
