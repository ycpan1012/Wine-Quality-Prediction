#model packages
library(class)
library(glmnet)
library(caret)
library(nnet)
library(tree)
library(e1071)

#plot packages
library(rpart.plot)
library(rattle)

#import data
red_wine <- read.csv(file = 'winequality-red.csv' ,sep = ';')
white_wine <- read.csv(file = 'winequality-white.csv' ,sep = ';')
set.seed(5)
gp <- runif(nrow(red_wine))
red_wine <- red_wine[order(gp),]

#remove quality and add rating -- red_wine
rating <- ifelse(red_wine$quality < 5, 0, ifelse(red_wine$quality < 7, 1, 2))
red_wine = subset(red_wine, select= -quality)
red_wine$rating <- rating
red_wine$rating <- factor(red_wine$rating)

############################# SAMPLING #############################

#split data into training/test set
#sample
set.seed(10)
train = sample(nrow(red_wine), nrow(red_wine)*0.8)

#traing/test set
red_wine_train = red_wine[train,]
red_wine_test = red_wine[-train,]

#target variable
rating_train = red_wine$rating[train]
rating_test  = red_wine$rating[-train]

# set the 10-fold, traincontrol
set.seed(100)

myControl <- trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 5,
    savePredictions = TRUE)

############################# TRAINING MODELS #############################

#fit model: logistic regression
red_wine_train$rating <- relevel(red_wine_train$rating, ref = '0')
lr_fit <- train(rating ~ .,
             method     = "multinom",
             trace      = FALSE,
             trControl  = myControl,
             metric     = "Accuracy",
             data       = red_wine_train)

#fit model: KNN
knn_fit <- train(rating ~ .,
             method     = "knn",
             preProc    = c("center","scale"),
             tuneGrid   = expand.grid(k = seq(1,50,2)),
             #tuneLength = 25,
             trControl  = myControl,
             metric     = "Accuracy",
             data       = red_wine_train)

#fit model: decision tree (unpruned)
#dt_fit1 <- tree(rating~., data = red_wine, subset=train)
#dt_fit1 <- rpart( rating ~., data = red_wine_train, method = "class")
dt_fit1 <- train(rating ~ .,
             method     = "rpart",
             tuneGrid   = expand.grid(cp = 0),
             #trControl  = myControl,
             metric     = "Accuracy",
             data       = red_wine_train)
             
#fit model: decision tree (pruned)
dt_fit2 <- train(rating ~ .,
             method     = "rpart",
             trControl  = myControl,
             metric     = "Accuracy",
             data       = red_wine_train)

#fit model: random forest
rf_fit <- train(rating ~ .,
             method     = "ranger",
             trControl  = myControl,
             metric    = "Accuracy",
             data       = red_wine_train)

############################# GRAPHING #############################

#interpretation: logistic regression
summary(lr_fit)

#interpretation: KNN
plot(knn_fit)

#interpretation: decision tree-1
fancyRpartPlot(dt_fit1$finalModel)

#interpretation: decision tree-2
fancyRpartPlot(dt_fit2$finalModel)

#interpretation: random forest
plot(rf_fit)

############################# TESTING MODELS #############################

#test: logistic regression
lr_pred <- predict(lr_fit, newdata = red_wine_test)
table(lr_pred, rating_test)
mean(lr_pred != rating_test)

#test: KNN
knn_pred <- predict(knn_fit, newdata = red_wine_test)
table(knn_pred, rating_test)
mean(knn_pred != rating_test)

#test: decision tree (unpruned)
dt_pred1 <- predict(dt_fit1, newdata = red_wine_test)
table(dt_pred1, rating_test)
mean(dt_pred1 != rating_test)

#test: decision tree (pruned)
dt_pred2 <- predict(dt_fit2, newdata = red_wine_test)
table(dt_pred2, rating_test)
mean(dt_pred2 != rating_test)

#test: random forest
rf_pred <- predict(rf_fit, newdata = red_wine_test)
table(rf_pred, rating_test)
mean(rf_pred != rating_test)
