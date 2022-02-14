#import library
library(ggplot2)
library(ggExtra)
library(viridis)
library(dplyr)
library(corrplot)
library(hrbrthemes)

#import data
white_wine <- read.csv('winequality-white.csv', sep = ';')
str(white_wine)
is.null(white_wine)

#histogram for quality feature
hist(white_wine$quality, freq = TRUE, xlab = 'quality', ylab = 'count', main ='', col ='#8FB98D')

#comparison_boxplot
fixacidity_box         <- boxplot(white_wine$fixed.acidity ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'fixed acidity')
title('(1)')
volatileacidity_box    <- boxplot(white_wine$volatile.acidity ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'volatile acidity')
title('(2)')
citricacid_box         <- boxplot(white_wine$citric.acid ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'citric acid')
title('(3)')
residualsugar_box      <- boxplot(white_wine$residual.sugar ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'residual sugar')
title('(4)')
chlorides_box          <- boxplot(white_wine$chlorides ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'chlorides')
title('(5)')
freesulfurdioxide_box  <- boxplot(white_wine$free.sulfur.dioxide ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'free sulfur dioxide')
title('(6)')
totalsulfurdioxide_box <- boxplot(white_wine$total.sulfur.dioxide ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'total sulfur dioxide')
title('(7)')
density_box            <- boxplot(white_wine$density ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'density')
title('(8)')
pH_box                 <- boxplot(white_wine$pH ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'pH')
title('(9)')
sulphates_box          <- boxplot(white_wine$sulphates ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'sulphates')
title('(10)')
alcohol_box            <- boxplot(white_wine$alcohol ~ white_wine$quality, col="#69b3a2", xlab = 'quality', ylab = 'alcohol')
title('(11)')

#comparison_histogram
hist(white_wine$fixed.acidity, freq = TRUE, xlab = '(1) fixed acidity', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$volatile.acidity, freq = TRUE, xlab = '(2) volatile acidity', ylab = 'count', main ='', xlim = c(0,1.5), col ='#8FB98D')
hist(white_wine$citric.acid, freq = TRUE, xlab = '(3) citric acid', ylab = 'count', main ='',col ='#8FB98D')
hist(white_wine$residual.sugar, freq = TRUE, xlab = '(4) residual sugar', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$chlorides, freq = TRUE, xlab = '(5) chlorides', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$free.sulfur.dioxide, freq = TRUE, xlab = '(6) free sulfur dioxide', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$total.sulfur.dioxide, freq = TRUE, xlab = '(7) total sulfur dioxide', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$density, freq = TRUE, xlab = '(8) density', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$pH, freq = TRUE, xlab = '(9) pH', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$sulphates, freq = TRUE, xlab = '(10) sulphates', ylab = 'count', main ='', col ='#8FB98D')
hist(white_wine$alcohol, freq = TRUE, xlab = '(11) alcohol', ylab = 'count', main ='', col ='#8FB98D')

#corrplot
M = cor(white_wine)
corr <- corrplot(M, method='color', addCoef.col = 'black',number.cex=0.8, tl.cex=0.9, tl.col='black', tl.srt = 50)
