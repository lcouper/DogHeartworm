#### Dog Heartworm Question 2 Qnalysis ####

# Q1: What are the key environmental predictors of mosquito abundance?

# approach: model trapped abundance as a function of several climate & land cover predictors
# Note this script uses data files cleaned using "DogHeartworm_Q2analysis_DataPreProcessing.R"

#### Step 1a. Read in data sets and load libraries #####

library(xgboost)
library(caret)
library(mltools)
library(vip)
library(pdp)
library(rBayesianOptimization)
library(data.table)

#AeVexans = read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/AeVexans_Q2_AllPredictors.csv", header = T)[,-1]
#AeAlbo = read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/AeAlbopictus_Q2_AllPredictors.csv", header = T)[,-1]
Species =read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_AllSpeciesPresAbs_AllPredictors.csv", header = T)[,-1]

# repeat for other species later 


#### Step 1c: Perform one-hot encoding ####

# necessary to reclassify categorical variables as dummy variables
# first = convert the county name & trap type predictors (confounds) to factors
# then convert df to data table for one_hot function to work

AeVexans$name = as.factor(AeVexans$name)
AeVexans$trap = as.factor(AeVexans$trap)
AeVexansDT = data.table(AeVexans)
AeVexans2 = one_hot(AeVexansDT)

#### Step 2: Split data into training (80%) and testing set (20%) 

set.seed(1234)
parts = sort(sample(nrow(AeVexans2), nrow(AeVexans2)*.80))
train = data.matrix(AeVexans2[parts, ])
test = data.matrix(AeVexans2[-parts, ])

#### Step 3: Define predictors & response variables in testing & training data sets ####
# predictors = all climate & land cover variables, 
# trap features (type, # of nights, vector control agency), and
# potential spatial & temporal confounders (lat / long, surveillance year)
# response = abundance in trap ('num_count')

train_x = train[, -c(1,30,33,34,40:44,47:51)]
train_y = train[,43]
test_x = test[, -c(1,30,33,34,40:44,47:51)]
test_y = test[, 43]

# Create matrix for use in XGBoost #

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#### Step 4: Specify model ##### 

minitial_xgb = xgboost(data = xgb_train, nrounds = 40, objective = "reg:squarederror",
                 early_stopping_rounds = 3, # trains until train_rmse hasn't improved in 3 rounds
                 max.depth = 4, # max tree depth (# of nodes)
                 eta = 0.23) # controls learning rate. Default is 3. Smaller = more robust to overfitting but slower to compute

#### Step 5: Tune hyperparameters  #####
# Two main options for hyperparmater tuning: grid search (uninformed, slower) 
# and Bayesian optimization (faster, 'informed' based on prior results)

#### 5a: Tune using grid search #####
# using example here: https://www.r-bloggers.com/2020/11/r-xgboost-regression/

# Set up grid with range of hyperparameters to try
hyper_grid <- expand.grid(max_depth = seq(3, 6, 1), eta = seq(.2, .35, .01))  

xgb_train_rmse = vector()
xgb_test_rmse = vector()

for (j in 1:nrow(hyper_grid)) {
  set.seed(1234) 
  m_xgb_untuned <- xgb.cv(
    data = train_x,
    label = train_y,
    nrounds = 10, # INCREASE LATER
   metrics = "rmse",
    early_stopping_rounds = 3,
    nfold = 5, # 5-fold cross validation
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  # if you want to store all the RMSE somewhere (not necessary for rest of loop to work)
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  cat(j, "\n")
}    

# Find the best parameters from the grid seach above

df = cbind(xgb_train_rmse, xgb_test_rmse, 1:64) # 64 is based on grid size (4 depth values x 16 eta values)
which.min(df[,2]) 
hyper_grid[which.min(df[,2]),] # suggests max_depth of 4 and eta of 0.35 is best (note this will change with each run)


#### 5b: Tune using Bayesian Optimization #####
# note this was 200 for Caroline@
ntrees.max = 3
xgb_cv_bayes <- function(eta, max.depth, min.child.weight, subsample, colsample_bytree, gamma) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = eta,
                             max_depth = max.depth, min_child_weight = min.child.weight,
                             subsample = subsample, colsample_bytree = colsample_bytree,
                             gamma = gamma,
                             objective = "reg:squarederror",
                             eval_metric = "rmse", seed = 25),
               data = xgb_train,
               nrounds = ntrees.max,
               nfold = 5, #this then uses 5 fold CV within this function
               early_stopping_rounds = 3, # change to 10 later
               scale_pos_weight = 4,
               verbose = T)
  list(Score = -unlist(cv$evaluation_log[cv$best_iteration, "test_rmse_mean"]), # Ensure score is negative, since optimization maximizes
       Pred = cv$pred,
       cb.print.evaluation(period = 1))
}

best_params <- BayesianOptimization(xgb_cv_bayes,
                 bounds = list(eta = c(0.1, 0.3),
                   max.depth = c(2L, 10L),
                   min.child.weight = c(10L, 25L),
                   subsample = c(0.5, 0.8),
                   colsample_bytree = c(0.5, 0.8),
                   gamma = c(10, 20)),
                  init_grid_dt = NULL, init_points = 10,
                  n_iter = 3, acq = "ucb", kappa = 3,
                  eps = 1.5, verbose = T)

#### Step 6 : Run model with optimal hyper parameters ####

watchlist <- list(train = xgb_train, test = xgb_test)
mfit_xgb = xgboost(data = xgb_train, 
                   nrounds = 10, # change nrounds later
                   eta = best_params$Best_Par[1],
                   max_depth = best_params$Best_Par[2],
                   min_child_weight = best_params$Best_Par[3],
                   subsample = best_params$Best_Par[4],
                   colsample_bytree = best_params$Best_Par[5],
                   gamma = best_params$Best_Par[6],
                 objective = "reg:squarederror",
                 scale_pos_weight = 4)

# notes:
# can repeat this step (bootstrap) 100 times to calculate CIs for each of the predictors 

#### Step 7: Evaluate model performance on test data (SKIPPING FOR NOW) #####

pred_xgb = predict(mfit_xgb, test_x) # Use model to predict abundance on test data 
y = test[, 43] # compare to actual abundance
postResample(pred_xgb, y) # calculate RMSE and R2
# plot
plot(y, pred_xgb, xlab = "actual", ylab = "predicted") # plot fit
abline(lm(pred_xgb ~ y))

# look at model residuals
residuals = y - pred_xgb
plot(residuals, ylab = "residuals") # plot residuals 


#### Step 8: Examine feature importance ####

# Determine variable importance
importance_matrixFit <- xgb.importance(model = mfit_xgb)

# calculates a few metrics for each predictor:
# 1) gain : how much the tree improves by adding a split based on a given variable
# 2) cover : how many observations related to this feature? 
# 3) frequency : relative % of times a features was used in trees 

# Plot "Importance" based on Gain
xgb.plot.importance(importance_matrixFit, xlab = "Feature Importance")

# Similar to above but only plot top 10 features 
vip(mfit_xgb, num_features = 10)

#### Step 9: Partial dependence plots (not yet done) ####




