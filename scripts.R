########################################################
# Variable                                             #
########################################################

train_file = "eclipse_data/eclipse3.0.csv"
testing_file = "eclipse_data/eclipse3.1.csv"
predict_file = "eclipse_data/eclipse3.2_without_POST.csv"

result_file1 = "result/file1.csv"
result_file2 = "result/file2.csv"

# predict_method = "Rborist"
predict_method = "rda"

########################################################
# Metrics                                              #
########################################################

# read a dataset
data <- read.csv(train_file)
# shrink data
shrinked_data <- data[,c("TLOC","VG_total","CHURN","AGE","BFC","PRE","POST")]

########################################################
# Model Building                                       #
########################################################

# active the caret package
library(caret)

# convert the dependent variable to buggy or not
idx <- (shrinked_data[,"POST"] > 0)
shrinked_data[idx, "POST"] <- "Buggy"
shrinked_data[!idx, "POST"] <- "Free"
shrinked_data[,"POST"] <- as.factor(shrinked_data[,"POST"])

# change experimental settings
# fitControl <- trainControl(
#       ## 10-fold CV
#       method = "cv",
#       number = 10,
#       ## For ROC
#       classProbs = TRUE,
#       summaryFunction = twoClassSummary)

fitControl <- trainControl(
      method = "repeatedcv",
      number = 9,
      repeats = 4)

model.lda <- train(POST ~ .,
      data = shrinked_data,
      method = "rf",
      tuneGrid = data.frame(mtry = 2),
      trControl = fitControl)

########################################################
# Performance Evaluation                               #
########################################################

data.testing <- read.csv(testing_file)
pred.lda <- predict(model.lda, data.testing, type="prob") # you can put other models instead of model.lda

# if you want to save your prediciton
write.csv(pred.lda, file=result_file1)

# caculate ROC between prediction results and files in the version 3.1
library(pROC) 
idx <- (data.testing[,"POST"] > 0)
data.testing[idx, "POST"] <- "Buggy"
data.testing[!idx, "POST"] <- "Free"
data.testing[,"POST"] <- as.factor(data.testing[,"POST"])
roc.lda <- roc(predictor=pred.lda$Buggy, response=data.testing$POST)
print(roc.lda$auc)

### [Tips]
### to better understand variable importance for objects produced by train
# varImp(model.lda)

########################################################
# Predict Data                                         #
########################################################

data.v32 <- read.csv(predict_file)

# sum(data.v32$TLOC)
# 
# sum(data.v32$CHURN)
# sum(data.v32$ADD)
# sum(data.v32$DEL)
# sum(data.v32$MLOC_total)
# 
# print(summary(data.v32))

pred.v32 <- predict(model.lda, data.v32, type="prob") # you can put other models instead of model.lda

idx <- (pred.v32[,"Buggy"] > pred.v32[,"Free"])
data.v32[idx, "POST"] <- 1
data.v32[!idx, "POST"] <- 0
# data.v32[,"POST"] <- as.factor(data.v32[,"POST"])

length(data.v32$POST) # total list
sum(data.v32$POST)    # bug number

sum(data.v32$TLOC) / sum(data.v32$POST)

# if you want to save your prediciton
write.csv(pred.v32, file=result_file2)
