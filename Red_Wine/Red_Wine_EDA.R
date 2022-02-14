rm(list=ls())
library(ggplot2)
library(ggExtra)
library(viridis)
library(dplyr)
library(corrplot)
library(e1071)

red_wine <- read.csv(file = 'winequality-red.csv' ,sep = ';')
str(red_wine)
head(red_wine)
is.null(red_wine)

#(1-1) Histogram of target variable “quality”
hist(red_wine$quality, freq = TRUE, xlab = 'quality', ylab = 'count', main ='', col ='#8FB98D')

#(1-2) skewness
for (i in 1:11) {
    distribution = red_wine[,i]
    cat('col', i, 'skewness=', skewness(distribution),'\n')
}

#(2) comparison_histogram
hist(red_wine$fixed.acidity, freq = TRUE, xlab = '(1) fixed acidity', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$volatile.acidity, freq = TRUE, xlab = '(2) volatile acidity', ylab = 'count', main ='', xlim = c(0,1.5), col ='#8FB98D')
hist(red_wine$citric.acid, freq = TRUE, xlab = '(3) citric acid', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$residual.sugar, freq = TRUE, xlab = '(4) residual sugar', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$chlorides, freq = TRUE, xlab = '(5) chlorides', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$free.sulfur.dioxide, freq = TRUE, xlab = '(6) free sulfur dioxide', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$total.sulfur.dioxide, freq = TRUE, xlab = '(7) total sulfur dioxide', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$density, freq = TRUE, xlab = '(8) density', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$pH, freq = TRUE, xlab = '(9) pH', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$sulphates, freq = TRUE, xlab = '(10) sulphates', ylab = 'count', main ='', col ='#8FB98D')
hist(red_wine$alcohol, freq = TRUE, xlab = '(11) alcohol', ylab = 'count', main ='', col ='#8FB98D')

#(3) comparison_boxplot
fixacidity_box         <- boxplot(red_wine$fixed.acidity ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'fixed acidity')
title('(1)')
volatileacidity_box    <- boxplot(red_wine$volatile.acidity ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'volatile acidity')
title('(2)')
citricacid_box         <- boxplot(red_wine$citric.acid ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'citric acid')
title('(3)')
residualsugar_box      <- boxplot(red_wine$residual.sugar ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'residual sugar')
title('(4)')
chlorides_box          <- boxplot(red_wine$chlorides ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'chlorides')
title('(5)')
freesulfurdioxide_box  <- boxplot(red_wine$free.sulfur.dioxide ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'free sulfur dioxide')
title('(6)')
totalsulfurdioxide_box <- boxplot(red_wine$total.sulfur.dioxide ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'total sulfur dioxide')
title('(7)')
density_box            <- boxplot(red_wine$density ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'density')
title('(8)')
pH_box                 <- boxplot(red_wine$pH ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'pH')
title('(9)')
sulphates_box          <- boxplot(red_wine$sulphates ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'sulphates')
title('(10)')
alcohol_box            <- boxplot(red_wine$alcohol ~ red_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'alcohol')
title('(11)')

#(4) corrplot
M = cor(red_wine)
corr <- corrplot(M, method='color', addCoef.col = 'black',number.cex=0.8, tl.cex=0.9, tl.col='black', tl.srt = 50)
