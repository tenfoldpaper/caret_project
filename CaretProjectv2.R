# Code originally obtained from Amazon AWS on Wine data set
# Modified by Seongjin Bien, Jacobs University Bremen @ TquanT 2018

# these first 2 lines are for setting up parallel processing and can be omitted
library(doParallel)
registerDoParallel(cores = detectCores() - 1)

set.seed(10)
library(caret)
library(corrplot)
library(kknn)
library(randomForest)
library(kernlab)

#white.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
white.url <- "/home/whitebox/Documents/TquanT/CaretProject/winequality-white.csv"
white.raw <- read.csv(white.url, header = TRUE, sep = ";")
white <- white.raw
str(white)

#Remove outlier
max.sug <- which(white$residual.sugar == max(white$residual.sugar))
white <- white[-max.sug, ]

#Model building data formatting
white$quality <- as.factor(white$quality)
inTrain <- createDataPartition(white$quality, p = 0.8, list = F)
#train.white <- white[inTrain,]
#test.white <- white[-inTrain,]

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# The permutations of size x input parameter training models are necessary for making "interactive" plots of the data 
#   without making the users wait for a long time to compute the values each time. 
#   PLEASE use the saved RData; the code below is commented out for a good reason! 

# Size Partitions
eightTwo <- createDataPartition(white$quality, p = 0.8, list = F)
eightTwoTrain.white <- white[eightTwo,]
eightTwoTest.white <- white[-eightTwo,]

oneThird <- createDataPartition(white$quality, p = 2/3, list = F)
oneThirdTrain.white <- white[oneThird,]
oneThirdTest.white <- white[-oneThird,]

halfHalf <- createDataPartition(white$quality, p = 0.5, list = F)
halfHalfTrain.white <- white[halfHalf,]
halfHalfTest.white <- white[-halfHalf,]

# # --- KNN grid permutations --- #
# #
# kknn.grid <- expand.grid(kmax = seq(1,30,2), distance = c(1, 2),
#                         kernel = c("rectangular", "gaussian", "cos"))
# #
#  
# # --- kknn train permutations --- #
# # # format: kknn <size> <kernel>, e.g. 
# kknnET.train <- train(quality ~ ., data = eightTwoTrain.white, method = "kknn", trControl = t.ctrl, tuneGrid = kknn.grid, prerocess = c("center", "scale"))
# kknnOT.train <- train(quality ~ ., data = oneThirdTrain.white, method = "kknn", trControl = t.ctrl, tuneGrid = kknn.grid, prerocess = c("center", "scale"))
# kknnHH.train <- train(quality ~ ., data = halfHalfTrain.white, method = "kknn", trControl = t.ctrl, tuneGrid = kknn.grid, prerocess = c("center", "scale"))

# --- KKNN model result dataframes examples
  # Plot x axis = max # neighbours , plot y axis = accuracy
  # 80/20 size, kernel querying example at distance == 1 (Distance specifies the type used, either Manhattan (1) or Euclidean (2).)
#kknnET.train$results[kknnET.train$results$kernel=='rectangular' & kknnET.train$results$distance==1,]

  # 0.66 / 0.33 proportion model, multiple-kernel querying example 
#kknnOT.train$results[kknnOT.train$results$kernel=='rectangular' & kknnOT.train$results$kernel=='gaussian' & kknnOT.train$results$distance==2]
  #... and so on. 


# # --- rf grid (there is only one) --- #
# rf.grid <- expand.grid(mtry = 1:15)
# 
# # --- rf train permutations --- #
# rfET.train <- train(quality ~ ., data = eightTwoTrain.white, method = "rf",
#                   trControl = t.ctrl, tuneGrid = rf.grid,
#                   preProcess = c("center", "scale"))
# rfOT.train <- train(quality ~ ., data = oneThirdTrain.white, method = "rf",
#                   trControl = t.ctrl, tuneGrid = rf.grid,
#                   preProcess = c("center", "scale"))
# rfHH.train <- train(quality ~ ., data = halfHalfTrain.white, method = "rf",
#                   trControl = t.ctrl, tuneGrid = rf.grid,
#                   preProcess = c("center", "scale"))

# --- rf model result dataframes examples
  # Plot x axis = mtry, y axis = accuracy
  # 80/20 proportion; no further querying needed 
#rfET.train$results
  # 0.66/0.33 proportion; no further querying needed
#rfOT.train$results

# # --- svm grid permutations --- #
# svm.grid <- expand.grid(C = 2*(1:5), sigma = seq(0.25, 2, length = 8))
# 
# # --- svm train permutations --- #
# svmET.train <- train(quality ~ ., data = eightTwoTrain.white, method = "svmRadial",
#                    trControl = t.ctrl, tuneGrid = svm.grid,
#                    preProcess = c("center", "scale"))
# svmOT.train <- train(quality ~ ., data = oneThirdTrain.white, method = "svmRadial",
#                    trControl = t.ctrl, tuneGrid = svm.grid,
#                    preProcess = c("center", "scale"))
# svmHH.train <- train(quality ~ ., data = halfHalfTrain.white, method = "svmRadial",
#                    trControl = t.ctrl, tuneGrid = svm.grid,
#                    preProcess = c("center", "scale"))

# --- svm model dataframes example
  # Plot x axis = sigma, y axis = accuracy
  # Each C value (2, 4, 6, 8, 10) has its own curve, which can be selected. 
#svmET.train$results[svmET.train$results$C==2,]
#svmET.train$results[svmET.train$results$C==4,]
#svmET.train$results[svmET.train$results$C==6,]

# --- Multi-Layer Perceptron Neural network grid --- #

mlp.grid <- expand.grid(layer1 = c(5, 6, 7, 8, 9, 10, 11, 12), 
                        layer2 = c(5, 6, 7, 8, 9, 10, 11, 12), 
                        layer3 = c(5, 6, 7, 8, 9, 10, 11, 12))

# --- MLP model permutations --- #
mlpET.train <- train(quality ~ ., data = eightTwoTrain.white, method = "mlpML",
                     trControl = t.ctrl, tuneGrid = mlp.grid,
                     preProcess = c("center", "scale"))
# mlpOT.train <- train(quality ~ ., data = oneThirdTrain.white, method = "mlpML", 
#                      trControl = t.ctrl, tuneGrid = mlp.grid, 
#                      preProcess = c("center", "scale"))
# mlpHH.train <- train(quality ~ ., data = halfHalfTrain.white, method = "mlpML", 
#                      trControl = t.ctrl, tuneGrid = mlp.grid, 
#                      preProcess = c("center", "scale"))
