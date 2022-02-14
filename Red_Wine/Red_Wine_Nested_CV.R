#model package
library(class)
library(glmnet)
library(caret)
library(nnet)
library(tree)
library(rpart)

#import data from CSV
red_wine <- read.csv(file = 'winequality-red.csv' ,sep = ';')
set.seed(1)
gp <- runif(nrow(red_wine))
red_wine <- red_wine[order(gp),]

#remove quality and add rating -- red_wine
rating <- ifelse(red_wine$quality < 5, 0, ifelse(red_wine$quality < 7, 1, 2))
red_wine = subset(red_wine, select= -quality)
red_wine$rating <- rating
red_wine$rating <- factor(red_wine$rating)

#build ML models
lr_te  = c()
knn_te = c()
dt1_te = c()
dt2_te = c()
rf_te  = c()
svm_te = c()

for (i in 0:9) {
    test = c(1:(nrow(red_wine)*0.1)) + floor(i*nrow(red_wine)*0.1)
    
    #traing/test set
    red_wine_train = red_wine[-test,]
    red_wine_test = red_wine[test,]
   
    #target variable
    rating_train = red_wine$rating[-test]
    rating_test  = red_wine$rating[test]
    
    # set the 10-fold, traincontrol
    set.seed(100)
    myFolds <- createFolds(rating_train, k = 10)
    myControl <- trainControl(
        savePredictions = "all",
        index = myFolds)
    
    
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
                    trControl  = myControl,
                    metric     = "Accuracy",
                    data       = red_wine_train)
    
    
    #fit model: decision tree (unpruned)
    dt_fit1 <- rpart( rating ~., data = red_wine_train, method = "class")
    
    
    #fit model: decision tree (pruned)
    dt_fit2 <- train(rating ~ .,
                    method     = "rpart",
                    #trControl  = myControl,
                    metric     = "Accuracy",
                    data       = red_wine_train)
    
    
    #fit model: random forest
    rf_fit <- train(rating ~ .,
                    method     = "ranger",
                    trControl  = myControl,
                    metric     = "Accuracy",
                    data       = red_wine_train)
    
    
    #fit model: SVM
    svm_fit <- train(rating ~ .,
                    method     = "svmLinear",
                    preProc    = c("center","scale"),
                    tuneGrid   = expand.grid(C = seq(1,20,2)),
                    trControl  = myControl,
                    metric     = "Accuracy",
                    data       = red_wine_train)
    
    
    #test
    lr_pred <- predict(lr_fit, newdata = red_wine_test)
    a <- mean(lr_pred != rating_test)
    lr_te = c(lr_te, a)
    
    
    knn_pred <- predict(knn_fit, newdata = red_wine_test)
    b <- mean(knn_pred != rating_test)
    knn_te = c(knn_te, b)
    
    
    dt_pred1 <- predict(dt_fit1, newdata = red_wine_test, type='class')
    c <- mean(dt_pred1 != rating_test)
    dt1_te = c(dt1_te, c)

    
    dt_pred2 <- predict(dt_fit2, newdata = red_wine_test)
    d <- mean(dt_pred2 != rating_test)
    dt2_te = c(dt2_te, d)

    
    rf_pred <- predict(rf_fit, newdata = red_wine_test)
    e <- mean(rf_pred != rating_test)
    rf_te = c(rf_te, e)

    
    svm_pred <- predict(svm_fit, newdata = red_wine_test)
    f <- mean(svm_pred != rating_test)
    svm_te = c(svm_te, f)

}

#print out results
cat('logistic regression: 10-fold CV error rate ='   , mean(lr_te) , '\n')
cat('KNN: 10-fold CV error rate ='                   , mean(knn_te), '\n')
cat('unpruned decision tree: 10-fold CV error rate =', mean(dt1_te), '\n')
cat('pruned decision tree: 10-fold CV error rate ='  , mean(dt2_te), '\n')
cat('ranfom forest: 10-fold CV error rate ='         , mean(rf_te) , '\n')
cat('SVM: 10-fold CV error rate ='                   , mean(svm_te), '\n')

#Output:
#logistic regression: 10-fold CV error rate = 0.1584906 
#KNN: 10-fold CV error rate = 0.1666667 
#unpruned decision tree: 10-fold CV error rate = 0.1566038 
#pruned decision tree: 10-fold CV error rate = 0.1672956 
#ranfom forest: 10-fold CV error rate = 0.1295597 
#SVM: 10-fold CV error rate = 0.1754717 
