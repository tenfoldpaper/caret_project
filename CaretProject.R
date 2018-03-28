set.seed(1)

library(caret)
library(parallel)
library(doParallel)

cluster <- makeCluster(4)
registerDoParallel(cluster)

raw.data <- read.csv2("/home/whitebox/Documents/TquanT/CaretProject/winequality-white.csv",
 sep = ";")

#Data doesn't have any missing variables
#cat(sapply(raw.data, anyNA))

#Data has some duplicated entries
#cat(any(duplicated(raw.data)))

#Remove the dupes, store it into a new variable 
nodupl.data <- unique(raw.data)

#quality <- as.factor(raw.data$quality)

#selected.data <- subset(nodupl.data, select = c("fixed acidity","volatile acidity",
#    "citric acid","residual sugar","chlorides",
#    "free sulfur dioxide","total sulfur dioxide",
#    "density","pH","sulphates","alcohol"))

train.model.ind <- createDataPartition(nodupl.data$quality, p = 0.5, list = FALSE)
train.model <- nodupl.data[train.model.ind,]
test.model <- nodupl.data[-train.model.ind,]
cat("Done to here\n")
tc <- trainControl(allowParallel = FALSE, method = 'repeatedcv', number = 4, repeats = 2)
system.time({
    knnModel1 <- train(quality ~ ., data = train.model, method = 'kknn', trainControl = tc)
    knnModel2 <- train(quality ~ ., data = train.model, method = 'kknn', k = 3, l = 2, trainControl=tc)
})
stopCluster(cluster)
registerDoSEQ()