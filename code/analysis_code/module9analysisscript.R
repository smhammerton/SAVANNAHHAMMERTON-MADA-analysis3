# Setup-------------------------------------------------------------
#load packages 
library(tidymodels) #for model fitting 
library(tidyverse) #for data processing 
library(rsample) #for data splitting 

#load data 
mydata <- readRDS('data/processed_data/processeddata.rds')


# Data Splitting------------------------------------------------------
#set seed to fix random numbers 
set.seed(123)

#put 3/4 of data into training set 
data_split <- initial_split(mydata, prop = 3/4)

#create data frames for training and testing sets 
train_data <- training(data_split)
test_data <- testing(data_split)

# Workflow Creation and Model Fitting--------------------------------
#create recipe fitting nausea to all predictors 
nausea <- recipes::recipe(Nausea ~ ., data = train_data)

#logistic regression model specification, computational engine glm
log_mod <- parsnip::logistic_reg() %>% 
  set_engine('glm')

#create simple workflow fitting logistic model to all predictors using glm function
nausea_workflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(nausea)

#view workflow
nausea_workflow

#prepare recipe and train model from resulting predictors 
nausea_fit <- nausea_workflow %>% 
  fit(data = train_data)

#pull fitted model object and get tibble of model coefficients 
nausea_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

# Model 1 Evaluation-------------------------------------------------
#use trained workflow to predict with unseen test data 
predict(nausea_fit, test_data)

#predict probabilities for each case 
nausea_aug <- augment(nausea_fit, test_data)

#view the data 
nausea_aug %>% 
  select(Nausea, .pred_class, .pred_No)

#create ROC curve
nausea_aug %>% roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()

#calculate AUC 
nausea_aug %>% 
  roc_auc(truth = Nausea, .pred_No)

# Alternative Model--------------------------------------------------

# Fitting only main predictor (runny nose) to nausea 

## Workflow Creation and Model Fitting-------------------------------

#create recipe fitting nausea to all predictors 
nausea_run <- recipes::recipe(Nausea ~ RunnyNose, data = train_data)


#create simple workflow fitting logistic model to all predictors using glm function
nausea_run_workflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(nausea_run)

#view workflow
nausea_run_workflow

#prepare recipe and train model from resulting predictors 
nausea_run_fit <- nausea_run_workflow %>% 
  fit(data = train_data)

#pull fitted model object and get tibble of model coefficients 
nausea_run_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

## Model 2 Evaluation-------------------------------------------------
#use trained workflow to predict with unseen test data 
predict(nausea_run_fit, test_data)

#predict probabilities for each case 
nausea_run_aug <- augment(nausea_run_fit, test_data)

#view the data 
nausea_run_aug %>% 
  select(Nausea, .pred_class, .pred_Yes)

#create ROC curve
nausea_run_aug %>% roc_curve(truth = Nausea, .pred_Yes) %>% 
  autoplot()

#calculate AUC 
nausea_run_aug %>% 
  roc_auc(truth = Nausea, .pred_Yes)

