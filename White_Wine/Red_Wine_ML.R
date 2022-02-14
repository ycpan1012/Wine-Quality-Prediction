#model package
library(class)
library(glmnet)
library(caret)
library(nnet)
library(tree)
library(e1071)

#plot
library(rpart.plot)
library(rattle)

white_wine <- read.csv(file = 'winequality-white.csv' ,sep = ';')
set.seed(5)
gp <- runif(nrow(white_wine))
white_wine <- white_wine[order(gp),]

#remove quality and add rating -- white_wine
rating <- ifelse(white_wine$quality < 5, 0, ifelse(white_wine$quality < 7, 1, 2))
white_wine = subset(white_wine, select= -quality)
white_wine$rating <- rating
white_wine$rating <- factor(white_wine$rating

#split data into training/test set
#sample
set.seed(10)
train = sample(nrow(white_wine), nrow(white_wine)*0.8)

#traing/test set
white_wine_train = white_wine[train,]
white_wine_test = white_wine[-train,]

#target variable
rating_train = white_wine$rating[train]
rating_test  = white_wine$rating[-train]

# set the 10-fold, traincontrol
set.seed(100)

myControl <- trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 5,
    savePredictions = TRUE)
    
#fit model: logistic regression
white_wine_train$rating <- relevel(white_wine_train$rating, ref = '0')
lr_fit <- train(rating ~ .,
             method     = "multinom",
             trace      = FALSE,
             trControl  = myControl,
             metric     = "Accuracy",
             data       = white_wine_train)
             
#check the p-value
z <- (summary(lr_fit)$coefficients) / (summary(lr_fit)$standard.errors)
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#fit model: KNN
knn_fit <- train(rating ~ .,
             method     = "knn",
             preProc    = c("center","scale"),
             tuneGrid   = expand.grid(k = seq(5,100,5)),
             #tuneLength = 25,
             trControl  = myControl,
             metric     = "Accuracy",
             data       = white_wine_train) 

#fit model: decision tree (unpruned)
dt_fit1 <- train(rating ~ .,
             method     = "rpart",
             tuneGrid   = expand.grid(cp = 0),
             #trControl  = myControl,
             metric     = "Accuracy",
             data       = white_wine_train)

#fit model: decision tree (pruned)
dt_fit2 <- train(rating ~ .,
             method     = "rpart",
             trControl  = myControl,
             metric     = "Accuracy",
             data       = white_wine_train)

#fit model: random forest
rf_fit <- train(rating ~ .,
             method     = "ranger",
             trControl  = myControl,
             metric    = "Accuracy",
             data       = white_wine_train)

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

#test: logistic regression
lr_pred <- predict(lr_fit, newdata = white_wine_test)
table(lr_pred, rating_test)
mean(lr_pred != rating_test)

#test: KNN
knn_pred <- predict(knn_fit, newdata = white_wine_test)
table(knn_pred, rating_test)
mean(knn_pred != rating_test)

#test: decision tree (unpruned)
dt_pred1 <- predict(dt_fit1, newdata = white_wine_test)
table(dt_pred1, rating_test)
mean(dt_pred1 != rating_test)

#test: decision tree (pruned)
dt_pred2 <- predict(dt_fit2, newdata = white_wine_test)
table(dt_pred2, rating_test)
mean(dt_pred2 != rating_test)

#test: random forest
rf_pred <- predict(rf_fit, newdata = white_wine_test)
table(rf_pred, rating_test)
mean(rf_pred != rating_test)
