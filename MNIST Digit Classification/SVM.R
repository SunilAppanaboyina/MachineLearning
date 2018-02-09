
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------DATA UNDERSTANDING, PREPARATION & EDA----------------------------------------------------------------

# Loading data
mnist_train <- read.csv("mnist_train.csv",stringsAsFactors = F,header = F) # train
mnist_test <- read.csv("mnist_test.csv",stringsAsFactors = F,header = F) # test

# Dimension of train data
dim(mnist_train)

# Checking class of all columns
summary(factor(sapply(mnist_train,class))) # train - all are integers
summary(factor(sapply(mnist_test,class))) # test - all are integers

# since all columns are integers lets check if there are any non-numerical or special values (NaN, Inf, NULL)
sum(sapply(mnist_train,function(x) sum(!is.finite(x)))) # train - none
sum(sapply(mnist_test,function(x) sum(!is.finite(x)))) # test - none

# Scaling train & test data by 255(max pixel value)
mnist_train <- cbind(mnist_train[,1],mnist_train[,-1]/255)
mnist_test <- cbind(mnist_test[,1],mnist_test[,-1]/255)

# Removing columns in train with all zeros
mnist_train <- mnist_train[,-(which(colSums(mnist_train)==0))]

# Scaling and removing column with all zeros led to improvement in accuracy and speed of execution of cross validation step

# Naming first column as Label and converting to factor
# train
colnames(mnist_train)[1] <- "Label"
mnist_train$Label <- factor(mnist_train$Label)
# test
colnames(mnist_test)[1] <- "Label"
mnist_test$Label <- factor(mnist_test$Label)

# EDA dosent make sense for this data set as all columns are pixel values

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------MODELING & EVALUATION----------------------------------------------------------------------

# Sampling only 20% of the original train data set for model building
library(caTools)
set.seed(100)
indices = sample.split(mnist_train$Label, SplitRatio = 0.2)
train = mnist_train[indices,]

# Taking whole test data set for evaluation
test <- mnist_test

# Linear kernal modeling
library(kernlab)
model_linear <- ksvm(Label~ ., data = train, scale = FALSE, kernel = "vanilladot")
model_linear

# Linear kernal model evaluation
eval_linear <- predict(model_linear,test)
library(caret)
confusionMatrix(eval_linear,test$Label) # Accuracy 92%

# Polynomial kernal modeling
model_poly <- ksvm(Label~ ., data = train, scale = FALSE, kernel = "polydot",kpar = list(degree = 2))
model_poly

# Polynomial kernal model evaluation
eval_poly <- predict(model_poly,test)
confusionMatrix(eval_poly,test$Label) # Accuracy 97%

# RBF kernal modeling
model_rbf <- ksvm(Label~ ., data = train, scale = FALSE, kernel = "rbfdot")
model_rbf

# RBF kernal model evaluation
eval_rbf <- predict(model_rbf,test)
confusionMatrix(eval_rbf,test$Label) # Accuracy 96%

# Kernal   Accuracy
#----------------------
# Linear      92%
# Polynomial  97%
# RBF         96%
#
# since accuracy of polynomial kernal is highest, it will be used for cross-validation model building

# Cross-validation using polynomial kernal
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
set.seed(100)
grid <- expand.grid(degree=c(2,3), scale=c(0.1,0.3), C=c(0.1,0.5) )
exec_start <- Sys.time()
fit.svm <- train(Label~., data=train, method="svmPoly", metric=metric, trControl=trainControl,tuneGrid=grid) # 22 min execution time
Sys.time() - exec_start
fit.svm
# Final model values:
# degree = 3
# scale = 0.1
# C = 0.1
# Accuracy 96%

plot(fit.svm)

# Evaluation
evaluate_non_linear<- predict(fit.svm, test)
confusionMatrix(evaluate_non_linear, test$Label) # Accuracy 97%

