# these first 2 lines are for setting up parallel processing and can be omitted
library(doParallel)
registerDoParallel(cores = detectCores() - 1)

set.seed(10)
library(caret)
library(corrplot)
library(kknn)
library(randomForest)
library(kernlab)

white.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
white.raw <- read.csv(white.url, header = TRUE, sep = ";")
white <- white.raw
str(white)

#Remove outlier
max.sug <- which(white$residual.sugar == max(white$residual.sugar))
white <- white[-max.sug, ]

#Model building data formatting
white$quality <- as.factor(white$quality)
inTrain <- createDataPartition(white$quality, p = 0.8, list = F)
train.white <- white[inTrain,]
test.white <- white[-inTrain,]

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

#The below commented-out codes take a longggggg time to run, so be careful! 

# KNN permutations
# 
# 3 kernels
# 3 Data sizes 
# = 18 training models 
# 
# 
# 

# kknn.grid <- expand.grid(kmax = seq(1,30,2), distance = c(1, 2),
#                          kernel = c("rectangular", "gaussian", "cos"))
# kknn.train <- train(quality ~ ., data = train.white, method = "kknn",
#                     trControl = t.ctrl, tuneGrid = kknn.grid,
#                     preProcess = c("center", "scale"))
# plot(kknn.train)
# 
# rf.grid <- expand.grid(mtry = 1:15)
# rf.train <- train(quality ~ ., data = train.white, method = "rf", 
#                   trControl = t.ctrl, tuneGrid = rf.grid,
#                   preProcess = c("center", "scale"))
# 
# plot(rf.train)
# 
# svm.grid <- expand.grid(C = 2^(1:3), sigma = seq(0.25, 2, length = 8))
# svm.train <- train(quality ~ ., data = train.white, method = "svmRadial",
#                    trControl = t.ctrl, tuneGrid = svm.grid,
#                    preProcess = c("center", "scale"))
# plot(svm.train)
