#Load the dataset
data <- read.delim(
  "C:/Users/vaish/Desktop/SLIIT/3Y1S/Stastical Modeling/SM Project/marketing_campaign.csv",
  header = TRUE,
  sep = "\t"
)
dim(data)
head(data)
str(data)

#Check missing values
colSums(is.na(data))

#Check duplicate records
sum(duplicated(data))

data[is.na(data$Income), ]

median(data$Income, na.rm = TRUE)

data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)
colSums(is.na(data))

#Check unsusual values
table(data$Education)

#Check Martial Status
table(data$Marital_Status)

summary(data$Year_Birth)

summary(data$Income)

table(data$Marital_Status)

summary(data$Year_Birth)

summary(data$Income)

data[data$Year_Birth < 1900, ]

data[data$Income > 200000, ]

min(data$Year_Birth)

data[data$Year_Birth < 1900, c("ID", "Year_Birth", "Education", "Marital_Status", "Income", "Response")]

data <- data[data$Year_Birth >= 1900, ]
nrow(data)

min(data$Year_Birth)

#Checking Income Anomaly
data[data$Income > 200000, c("ID", "Income", "Education", "Marital_Status", "Response")]

data$Income[data$Income == 666666] <- NA

data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)

summary(data$Income)

sum(is.na(data$Income))

data[data$Marital_Status %in% c("Absurd", "YOLO"),
     c("ID", "Marital_Status", "Income", "Education", "Response")]

data$Marital_Status[data$Marital_Status %in% c("Absurd", "YOLO")] <- "Other"

table(data$Marital_Status)

# Check Income for outliers
boxplot(data$Income,
        main = "Boxplot of Customer Income",
        ylab = "Income")

# Create total spending
data$Total_Spend <- data$MntWines +
  data$MntFruits +
  data$MntMeatProducts +
  data$MntFishProducts +
  data$MntSweetProducts +
  data$MntGoldProds

# Check total spending for outliers
boxplot(data$Total_Spend,
        main = "Boxplot of Total Customer Spending",
        ylab = "Total Spending")

table(data$Response)

prop.table(table(data$Response)) * 100

# Response by Education
table(data$Education, data$Response)

# Response percentage within each Education group
prop.table(table(data$Education, data$Response), margin = 1) * 100

education_response <- prop.table(
  table(data$Education, data$Response),
  margin = 1
) * 100

barplot(
  education_response[, "1"],
  main = "Campaign Response Rate by Education",
  ylab = "Response Rate (%)",
  xlab = "Education",
  ylim = c(0, 25)
)
table(data$Marital_Status, data$Response)

marital_response <- prop.table(
  table(data$Marital_Status, data$Response),
  margin = 1
) * 100

barplot(
  marital_response[, "1"],
  main = "Campaign Response Rate by Marital Status",
  ylab = "Response Rate (%)",
  xlab = "Marital Status",
  ylim = c(0, 25)
)

# Create total number of children
data$TotalChildren <- data$Kidhome + data$Teenhome

# Check the number of customers in each group
table(data$TotalChildren)

# Response by number of children
table(data$TotalChildren, data$Response)

# Response percentage within each children group
children_response <- prop.table(
  table(data$TotalChildren, data$Response),
  margin = 1
) * 100

children_response

barplot(
  children_response[, "1"],
  main = "Campaign Response Rate by Number of Children",
  ylab = "Response Rate (%)",
  xlab = "Number of Children",
  ylim = c(0, 30)
)

# Select spending variables
spending_data <- data[, c(
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts",
  "MntGoldProds"
)]

# Calculate correlation matrix
spending_cor <- cor(spending_data)

# Display correlation matrix
round(spending_cor, 2)

install.packages("corrplot")

library(corrplot)

corrplot(
  spending_cor,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Heatmap of Customer Spending",
  mar = c(0, 0, 2, 0)
)

# Histogram of Recency
hist(
  data$Recency,
  main = "Distribution of Customer Recency",
  xlab = "Recency (Days)",
  ylab = "Number of Customers",
  breaks = 20
)

# Histogram of Website Visits
hist(
  data$NumWebVisitsMonth,
  main = "Distribution of Monthly Website Visits",
  xlab = "Number of Web Visits per Month",
  ylab = "Number of Customers",
  breaks = 15
)

# Compare income between responders and non-responders

t.test(Income ~ Response, data = data)

# Chi-square test: Education vs Campaign Response

education_table <- table(data$Education, data$Response)

education_table

chisq.test(education_table)

# ANOVA: Total Spending vs Marital Status

anova_model <- aov(Total_Spend ~ Marital_Status, data = data)
summary(anova_model)

# F-test: Income variance by Campaign Response

var_test <- var.test(Income ~ Response, data = data)

var_test

# Logistic Regression

logistic_model <- glm(
  Response ~ Income + Recency + MntWines + MntFruits +
    MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = data,
  family = binomial
)

summary(logistic_model)

# Predicted probabilities

data$predicted_prob <- predict(
  logistic_model,
  type = "response"
)

head(data$predicted_prob)

# Convert predicted probabilities into predicted classes
data$predicted_class <- ifelse(data$predicted_prob >= 0.5, 1, 0)

head(data[, c("Response", "predicted_prob", "predicted_class")])

# Create data used for prediction
model_data <- na.omit(data[, c(
  "Response",
  "Income",
  "Recency",
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts",
  "MntGoldProds",
  "NumWebPurchases",
  "NumCatalogPurchases",
  "NumStorePurchases",
  "NumWebVisitsMonth"
)])

# Predicted probabilities
model_data$predicted_prob <- predict(
  logistic_model,
  newdata = model_data,
  type = "response"
)

# Convert probabilities into classes
model_data$predicted_class <- ifelse(
  model_data$predicted_prob >= 0.5,
  1,
  0
)

# View actual vs predicted
head(model_data[, c(
  "Response",
  "predicted_prob",
  "predicted_class"
)])

# Confusion Matrix
confusion_matrix <- table(
  Actual = model_data$Response,
  Predicted = model_data$predicted_class
)

confusion_matrix
# Accuracy
logistic_accuracy <- mean(
  model_data$predicted_class == model_data$Response
)

logistic_accuracy

# Sensitivity / Recall
logistic_sensitivity <- sum(
  model_data$predicted_class == 1 &
    model_data$Response == 1
) / sum(model_data$Response == 1)

logistic_sensitivity

# Specificity
logistic_specificity <- sum(
  model_data$predicted_class == 0 &
    model_data$Response == 0
) / sum(model_data$Response == 0)

logistic_specificity

library(pROC)

roc_model <- roc(
  model_data$Response,
  model_data$predicted_prob
)

auc(roc_model)

plot(
  roc_model,
  main = "ROC Curve - Logistic Regression"
)

install.packages("glmnet")
library(glmnet)

# Predictor variables
x <- model.matrix(
  Response ~ Income + Recency + MntWines + MntFruits +
    MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = model_data
)[, -1]

# Response variable
y <- model_data$Response

ridge_model <- cv.glmnet(
  x,
  y,
  family = "binomial",
  alpha = 0
)

ridge_model

ridge_model$lambda.min

ridge_prob <- predict(
  ridge_model,
  newx = x,
  s = "lambda.min",
  type = "response"
)

head(ridge_prob)

ridge_class <- ifelse(ridge_prob >= 0.5, 1, 0)

ridge_confusion <- table(
  Actual = y,
  Predicted = ridge_class
)

ridge_confusion

ridge_accuracy <- mean(ridge_class == y)

ridge_accuracy
# Ridge Precision
ridge_precision <- sum(
  ridge_class == 1 & y == 1
) / sum(ridge_class == 1)

ridge_precision

# Ridge F1-score
ridge_sensitivity <- sum(
  ridge_class == 1 & y == 1
) / sum(y == 1)

ridge_f1 <- 2 * (
  ridge_precision * ridge_sensitivity
) / (
  ridge_precision + ridge_sensitivity
)

ridge_f1

library(pROC)

ridge_roc <- roc(y, as.numeric(ridge_prob))

auc(ridge_roc)

# LASSO Logistic Regression
lasso_model <- cv.glmnet(
  x,
  y,
  family = "binomial",
  alpha = 1
)

lasso_model

lasso_model$lambda.min

lasso_prob <- predict(
  lasso_model,
  newx = x,
  s = "lambda.min",
  type = "response"
)

head(lasso_prob)

lasso_class <- ifelse(lasso_prob >= 0.5, 1, 0)

head(lasso_class)

lasso_confusion <- table(
  Actual = y,
  Predicted = lasso_class
)

lasso_confusion

lasso_accuracy <- mean(lasso_class == y)

lasso_accuracy

# LASSO Precision
lasso_precision <- sum(
  lasso_class == 1 & y == 1
) / sum(lasso_class == 1)

lasso_precision

# LASSO F1-score
lasso_sensitivity <- sum(
  lasso_class == 1 & y == 1
) / sum(y == 1)

lasso_f1 <- 2 * (
  lasso_precision * lasso_sensitivity
) / (
  lasso_precision + lasso_sensitivity
)

lasso_f1

lasso_roc <- roc(
  y,
  as.numeric(lasso_prob)
)

auc(lasso_roc)

plot(
  lasso_roc,
  main = "ROC Curve - LASSO Logistic Regression"
)

install.packages("e1071")
library(e1071)

# Naïve Bayes model
nb_model <- naiveBayes(
  factor(Response) ~ Income + Recency + MntWines +
    MntFruits + MntMeatProducts + MntFishProducts +
    MntSweetProducts + MntGoldProds +
    NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = model_data
)

nb_model

# Naive Bayes predicted probabilities
nb_prob <- predict(
  nb_model,
  newdata = model_data,
  type = "raw"
)

# Probability of Response = 1
nb_prob_1 <- nb_prob[, "1"]

head(nb_prob_1)

# Convert probabilities into predicted classes
nb_class <- ifelse(nb_prob_1 >= 0.5, 1, 0)

head(nb_class)

# Confusion Matrix
nb_confusion <- table(
  Actual = y,
  Predicted = nb_class
)

nb_confusion

# Accuracy
nb_accuracy <- mean(nb_class == y)

nb_accuracy

# Sensitivity / Recall
nb_sensitivity <- sum(nb_class == 1 & y == 1) / sum(y == 1)

nb_sensitivity

# Specificity
nb_specificity <- sum(nb_class == 0 & y == 0) / sum(y == 0)

nb_specificity

# Naive Bayes Precision
nb_precision <- sum(
  nb_class == 1 & y == 1
) / sum(nb_class == 1)

nb_precision

# Naive Bayes F1-score
nb_f1 <- 2 * (
  nb_precision * nb_sensitivity
) / (
  nb_precision + nb_sensitivity
)

nb_f1

# ROC Curve and AUC
nb_roc <- roc(
  y,
  nb_prob_1
)

auc(nb_roc)

plot(
  nb_roc,
  main = "ROC Curve - Naive Bayes"
)
# McFadden's Pseudo R-squared

null_model <- glm(
  Response ~ 1,
  data = model_data,
  family = binomial
)

mcfadden_r2 <- 1 - logLik(logistic_model) / logLik(null_model)

mcfadden_r2

library(corrplot)
library(glmnet)
library(e1071)
library(car)
library(pROC)

# Check multicollinearity using VIF
vif(logistic_model)

# PCA variables
pca_data <- na.omit(data[, c(
  "MntWines",
  "MntFruits",
  "MntMeatProducts",
  "MntFishProducts",
  "MntSweetProducts",
  "MntGoldProds",
  "NumWebPurchases",
  "NumCatalogPurchases",
  "NumStorePurchases"
)])

head(pca_data)

# Standardize variables
pca_scaled <- scale(pca_data)

# Perform PCA
pca_model <- prcomp(
  pca_scaled,
  center = FALSE,
  scale. = FALSE
)

summary(pca_model)

# Scree plot
plot(
  pca_model,
  type = "l",
  main = "Scree Plot - Spending and Purchase Channel PCA"
)

# PCA loadings
pca_model$rotation

# Keep first 3 principal components
pca_scores <- as.data.frame(pca_model$x[, 1:3])

# Check dimensions
nrow(pca_scores)
nrow(model_data)

# Add Response
pca_scores$Response <- model_data$Response

head(pca_scores)

table(pca_scores$Response)

nrow(pca_scores)
nrow(model_data)

pca_logistic <- glm(
  Response ~ PC1 + PC2 + PC3,
  data = pca_scores,
  family = binomial
)

summary(pca_logistic)

pca_prob <- predict(
  pca_logistic,
  type = "response"
)

pca_class <- ifelse(
  pca_prob >= 0.5,
  1,
  0
)

pca_confusion <- table(
  Actual = pca_scores$Response,
  Predicted = pca_class
)

pca_confusion

pca_accuracy <- mean(
  pca_class == pca_scores$Response
)

pca_accuracy

# Create ROC object
pca_roc <- roc(
  pca_scores$Response,
  pca_prob
)

# AUC
auc(pca_roc)

install.packages("pROC")
library(pROC)

pca_roc <- pROC::roc(
  pca_scores$Response,
  pca_prob
)
pca_roc

pROC::auc(pca_roc)

plot(
  pca_roc,
  main = "ROC Curve - PCA Logistic Regression"
)
# PCA Logistic Regression - Sensitivity
pca_sensitivity <- sum(pca_class == 1 & pca_scores$Response == 1) /
  sum(pca_scores$Response == 1)

pca_sensitivity

# PCA Logistic Regression - Specificity
pca_specificity <- sum(pca_class == 0 & pca_scores$Response == 0) /
  sum(pca_scores$Response == 0)

pca_specificity

# Original Logistic Regression probabilities
logistic_prob <- predict(
  logistic_model,
  type = "response"
)

# Convert probabilities into classes
logistic_class <- ifelse(
  logistic_prob >= 0.5,
  1,
  0
)

logistic_confusion <- table(
  Actual = y,
  Predicted = logistic_class
)

logistic_confusion

logistic_accuracy <- mean(logistic_class == y)

logistic_accuracy

logistic_sensitivity <- sum(
  logistic_class == 1 & y == 1
) / sum(y == 1)

logistic_sensitivity

logistic_specificity <- sum(
  logistic_class == 0 & y == 0
) / sum(y == 0)

logistic_specificity

# Precision
logistic_precision <- sum(
  logistic_class == 1 & y == 1
) / sum(logistic_class == 1)

logistic_precision

# F1-score
logistic_f1 <- 2 * (
  logistic_precision * logistic_sensitivity
) / (
  logistic_precision + logistic_sensitivity
)

logistic_f1


logistic_roc <- pROC::roc(
  y,
  logistic_prob
)

pROC::auc(logistic_roc)

# ==========================================
# TASK 5 - MODEL COMPARISON
# ==========================================

task5_model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "Ridge Logistic",
    "LASSO Logistic",
    "Naive Bayes"
  ),
  
  Accuracy = c(
    logistic_accuracy,
    ridge_accuracy,
    lasso_accuracy,
    nb_accuracy
  ),
  
  Sensitivity = c(
    logistic_sensitivity,
    ridge_sensitivity,
    lasso_sensitivity,
    nb_sensitivity
  ),
  
  Specificity = c(
    logistic_specificity,
    sum(ridge_class == 0 & y == 0) / sum(y == 0),
    sum(lasso_class == 0 & y == 0) / sum(y == 0),
    nb_specificity
  ),
  
  Precision = c(
    logistic_precision,
    ridge_precision,
    lasso_precision,
    nb_precision
  ),
  
  F1_Score = c(
    logistic_f1,
    ridge_f1,
    lasso_f1,
    nb_f1
  ),
  
  ROC_AUC = c(
    as.numeric(pROC::auc(logistic_roc)),
    as.numeric(pROC::auc(ridge_roc)),
    as.numeric(pROC::auc(lasso_roc)),
    as.numeric(pROC::auc(nb_roc))
  )
)

task5_model_comparison

# ============================================================
# TASK 5 - TRAIN / TEST VALIDATION
# ============================================================

library(glmnet)
set.seed(123)

train_index <- sample(
  1:nrow(model_data),
  size = 0.80 * nrow(model_data)
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Logistic Regression
logistic_test_model <- glm(
  Response ~ Income + Recency + MntWines + MntFruits +
    MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = train_data,
  family = binomial
)

logistic_test_prob <- predict(
  logistic_test_model,
  newdata = test_data,
  type = "response"
)

logistic_test_class <- ifelse(logistic_test_prob >= 0.5, 1, 0)

# Ridge Logistic Regression
x_train <- model.matrix(
  Response ~ Income + Recency + MntWines + MntFruits +
    MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = train_data
)[, -1]

x_test <- model.matrix(
  Response ~ Income + Recency + MntWines + MntFruits +
    MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = test_data
)[, -1]

y_train <- train_data$Response
y_test <- test_data$Response

ridge_test_model <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 0
)

ridge_test_prob <- as.numeric(
  predict(
    ridge_test_model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )
)

ridge_test_class <- ifelse(ridge_test_prob >= 0.5, 1, 0)

# LASSO Logistic Regression
lasso_test_model <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1
)

lasso_test_prob <- as.numeric(
  predict(
    lasso_test_model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )
)

lasso_test_class <- ifelse(lasso_test_prob >= 0.5, 1, 0)

# Naive Bayes
nb_test_model <- naiveBayes(
  factor(Response) ~ Income + Recency + MntWines +
    MntFruits + MntMeatProducts + MntFishProducts +
    MntSweetProducts + MntGoldProds +
    NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth,
  data = train_data
)

nb_test_prob <- predict(
  nb_test_model,
  newdata = test_data,
  type = "raw"
)[, "1"]

nb_test_class <- ifelse(nb_test_prob >= 0.5, 1, 0)

# Validation Metrics
calculate_metrics <- function(actual, predicted, probability) {
  
  accuracy <- mean(predicted == actual)
  
  sensitivity <- sum(predicted == 1 & actual == 1) /
    sum(actual == 1)
  
  specificity <- sum(predicted == 0 & actual == 0) /
    sum(actual == 0)
  
  precision <- ifelse(
    sum(predicted == 1) == 0,
    0,
    sum(predicted == 1 & actual == 1) /
      sum(predicted == 1)
  )
  
  f1 <- ifelse(
    precision + sensitivity == 0,
    0,
    2 * precision * sensitivity /
      (precision + sensitivity)
  )
  
  roc_auc <- as.numeric(
    pROC::auc(
      pROC::roc(actual, probability, quiet = TRUE)
    )
  )
  
  c(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1_Score = f1,
    ROC_AUC = roc_auc
  )
}

# Final validation comparison
validation_comparison <- rbind(
  
  Logistic_Regression =
    calculate_metrics(
      y_test,
      logistic_test_class,
      logistic_test_prob
    ),
  
  Ridge_Logistic =
    calculate_metrics(
      y_test,
      ridge_test_class,
      ridge_test_prob
    ),
  
  LASSO_Logistic =
    calculate_metrics(
      y_test,
      lasso_test_class,
      lasso_test_prob
    ),
  
  Naive_Bayes =
    calculate_metrics(
      y_test,
      nb_test_class,
      nb_test_prob
    )
)

validation_comparison

# ==========================================
# TASK 7 - PCA MODEL COMPARISON
# ==========================================

# Compare Original Logistic Regression vs PCA Logistic Regression

model_comparison <- data.frame(
  Metric = c(
    "Accuracy",
    "Sensitivity",
    "Specificity",
    "ROC-AUC"
  ),
  
  Original_Logistic = c(
    logistic_accuracy,
    logistic_sensitivity,
    logistic_specificity,
    as.numeric(pROC::auc(logistic_roc))
  ),
  
  PCA_Logistic = c(
    pca_accuracy,
    pca_sensitivity,
    pca_specificity,
    as.numeric(pROC::auc(pca_roc))
  )
)

model_comparison

# Convert data into matrix for bar plot

comparison_matrix <- rbind(
  model_comparison$Original_Logistic,
  model_comparison$PCA_Logistic
)

colnames(comparison_matrix) <- model_comparison$Metric

barplot(
  comparison_matrix,
  beside = TRUE,
  names.arg = model_comparison$Metric,
  ylim = c(0, 1),
  main = "Original Logistic Regression vs PCA Logistic Regression",
  ylab = "Score",
  legend.text = c("Original Logistic", "PCA Logistic")
)
# ==========================================
# TASK 8 - BAYESIAN METHODS
# Bayesian Decision Making
# ==========================================

# Cost of emailing a non-responder
cost_false_positive <- 1

# Cost of missing a potential responder
cost_false_negative <- 5

# Calculate Bayesian decision threshold
bayesian_threshold <- cost_false_positive /
  (cost_false_positive + cost_false_negative)

bayesian_threshold

# Check that actual responses and probabilities have same length
length(y)
length(nb_prob_1)

# Classify customers using Bayesian decision threshold
nb_bayesian_class <- ifelse(
  nb_prob_1 >= bayesian_threshold,
  1,
  0
)

# Confusion matrix
nb_bayesian_confusion <- table(
  Actual = y,
  Predicted = nb_bayesian_class
)

nb_bayesian_confusion

# Bayesian Decision Performance

# Accuracy
bayesian_accuracy <- mean(nb_bayesian_class == y)

bayesian_accuracy

# Sensitivity / Recall
bayesian_sensitivity <- sum(
  nb_bayesian_class == 1 & y == 1
) / sum(y == 1)

bayesian_sensitivity

# Specificity
bayesian_specificity <- sum(
  nb_bayesian_class == 0 & y == 0
) / sum(y == 0)

bayesian_specificity

# Compare Standard and Bayesian Thresholds

threshold_comparison <- data.frame(
  Threshold = c(0.5, bayesian_threshold),
  
  Accuracy = c(
    nb_accuracy,
    bayesian_accuracy
  ),
  
  Sensitivity = c(
    nb_sensitivity,
    bayesian_sensitivity
  ),
  
  Specificity = c(
    nb_specificity,
    bayesian_specificity
  )
)

threshold_comparison

# ==========================================
# Compare Business Costs
# ==========================================

# Standard threshold confusion matrix
nb_standard_confusion <- table(
  Actual = y,
  Predicted = nb_class
)

# Bayesian threshold confusion matrix
nb_bayesian_confusion

# Calculate total business cost
standard_cost <- 
  nb_standard_confusion[1,2] * cost_false_positive +
  
  nb_standard_confusion[2,1] * cost_false_negative

bayesian_cost <- 
  nb_bayesian_confusion[1,2] * cost_false_positive +
  nb_bayesian_confusion[2,1] * cost_false_negative

standard_cost
bayesian_cost

# ==========================================
# TASK 9 - TIME SERIES EXTENSION
# ==========================================

# Time-related variables available in the dataset
str(data$Dt_Customer)
summary(data$Dt_Customer)

# Convert Dt_Customer to Date format

data$Dt_Customer <- as.Date(
  data$Dt_Customer,
  format = "%d-%m-%Y"
)

summary(data$Dt_Customer)

min(data$Dt_Customer, na.rm = TRUE)
max(data$Dt_Customer, na.rm = TRUE)

# ==========================================
# TASK 10 - INDUSTRY INNOVATION PROPOSAL
# Customer Response Prediction Dashboard
# ==========================================

# Generate response probabilities using Logistic Regression

customer_response_probability <- predict(
  logistic_model,
  newdata = model_data,
  type = "response"
)

head(customer_response_probability)

# Create customer segments

customer_segment <- ifelse(
  customer_response_probability >= 0.50,
  "High",
  ifelse(
    customer_response_probability >= bayesian_threshold,
    "Medium",
    "Low"
  )
)

table(customer_segment)

# Create customer response dashboard data

customer_dashboard <- data.frame(
  Customer_ID = 1:nrow(model_data),
  Response_Probability = round(
    customer_response_probability,
    3
  ),
  Segment = customer_segment
)

head(customer_dashboard)

# Customer segment distribution

segment_counts <- table(customer_dashboard$Segment)

segment_counts

# Customer Response Prediction Segments

barplot(
  segment_counts,
  main = "Customer Response Prediction Segments",
  xlab = "Customer Segment",
  ylab = "Number of Customers"
)