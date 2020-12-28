# code is in exactly the same order as in notebook

datasett<-read.csv('impacts.csv')
dataset<-datasett[,4:10]
head(dataset)

d<-density(dataset$Cumulative.Palermo.Scale)
plot(d, main="Cumulative Palermo Scale")
polygon(d, col="green", border="blue")

d<-density(dataset$Maximum.Palermo.Scale)
plot(d, main="Maximum Palermo Scale")
polygon(d, col="green", border="blue")

d<-density(dataset$Asteroid.Velocity)
plot(d, main="Asteroid Velocity")
polygon(d, col="green", border="blue")

d<-density(dataset$Asteroid.Magnitude)
plot(d, main="Asteroid Magnitude")
polygon(d, col="green", border="blue")

d<-density(dataset$Cumulative.Impact.Probability)
plot(d, main="Cumulative Impact Probability")

head(dataset$Cumulative.Impact.Probability)

d<-density(dataset$Cumulative.Impact.Probability)
plot(d, xlim=c(1.1e-10,1.1e-3), main="Cumulative Impact Probability")

d<-density(dataset$Asteroid.Diameter..km.)
plot(d, main="Asteroid Diameter (km)")
polygon(d, col="red", border="blue")

d<-density(dataset$Possible.Impacts)
plot(d, main="Possible Impacts")
polygon(d, col="red", border="blue")

str(dataset)
summary(dataset)

getmode <- function(m) {
  uniqv <- unique(m)
  uniqv[which.max(tabulate(match(m, uniqv)))]
}

d<-density(dataset$Cumulative.Palermo.Scale)
plot(d, main="Cumulative Palermo Scale")
polygon(d, col="green", border="chocolate3")
abline(v = mean(dataset$Cumulative.Palermo.Scale),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Cumulative.Palermo.Scale),
       col = "red",
       lwd = 2)
abline(v = getmode(dataset$Cumulative.Palermo.Scale),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

d<-density(dataset$Maximum.Palermo.Scale)
plot(d, main="Maximum Palermo Scale")
polygon(d, col="green", border="chocolate3")
abline(v = mean(dataset$Maximum.Palermo.Scale),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Maximum.Palermo.Scale),
       col = "red",
       lwd = 2)
abline(v = getmode(dataset$Maximum.Palermo.Scale),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

d<-density(dataset$Asteroid.Velocity)
plot(d, main="Asteroid Velocity")
polygon(d, col="green", border="chocolate3")
abline(v = mean(dataset$Asteroid.Velocity),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Asteroid.Velocity),
       col = "red",
       lwd = 2)
abline(v = getmode(dataset$Asteroid.Velocity),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

d<-density(dataset$Asteroid.Magnitude)
plot(d, main="Asteroid Magnitude")
polygon(d, col="green", border="chocolate3")
abline(v = mean(dataset$Asteroid.Magnitude),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Asteroid.Magnitude),
       col = "red",
       lwd = 2)
abline(v = getmode(dataset$Asteroid.Velocity),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

d<-density(dataset$Possible.Impacts)
plot(d, main="Possible Impacts")
polygon(d, col="red", border="chocolate3")
abline(v = mean(dataset$Possible.Impacts),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Possible.Impacts),
       col = "green",
       lwd = 2)
abline(v = getmode(dataset$Possible.Impacts),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

d<-density(dataset$Asteroid.Diameter..km.)
plot(d, main="Asteroid Diameter (km)")
polygon(d, col="red", border="chocolate3")
abline(v = mean(dataset$Asteroid.Diameter..km.),
       col = "royalblue",
       lwd = 2)
abline(v = median(dataset$Asteroid.Diameter..km.),
       col = "green",
       lwd = 2)
abline(v = getmode(dataset$Asteroid.Diameter..km.),
       col = "purple",
       lwd = 2)
legend(x = "topright",
       c("Density plot", "Mean", "Median", "Mode"),
       col = c("chocolate3", "royalblue", "red", "purple"),
       lwd = c(2, 2, 2, 2))

print("H-Spread, InterQuartile Range per variable:")
intq <- lapply(dataset, IQR)
intq

print("min value/max value per variable:")
rng <- lapply(dataset, range)
rng

print("variance of each variable:")
vrc <- lapply(dataset, var)
vrc

variances <- c(var(dataset$Maximum.Palermo.Scale), var(dataset$Asteroid.Magnitude),
               var(dataset$Asteroid.Velocity))
barplot(variances,
        main = "variances of different variables",
        xlab = "variable",
        ylab = "variance",
        col = c("red","green", "blue"))
legend("topleft",
       c("palermo","magnitude", "velocity"),
       fill = c("red","green", "blue")
)

print("standard deviation of each variable:")
sds <- lapply(dataset, sd)
sds

sdvs <- c(sd(dataset$Maximum.Palermo.Scale), sd(dataset$Asteroid.Magnitude),
          sd(dataset$Asteroid.Velocity))
barplot(sdvs,
        main = "SDs of different variables",
        xlab = "variable",
        ylab = "Standard Deviation",
        col = c("red","green", "blue"))
legend("topleft",
       c("palermo","magnitude", "velocity"),
       fill = c("red","green", "blue")
)

getskew <- function(x){
  3*(mean(x)-median(x))/sd(x)
}

getskew(dataset$Asteroid.Diameter..km.)

print("Measure of how skewed distribution is")
sk <- lapply(dataset, getskew)
sk

kurt <- function(x){
  
  j=0
  for (z in x) {
    j=j+( ((z-mean(x))^4)/length(x) )
  }
  j=j/(sd(x)^4)-3
  return (j)
}

print("kurtosis of different distributions:")

krt <- lapply(dataset, kurt)
krt

kurt2 <- function(x){
  k = 0
  for (z in x) {
    k=k+((z-mean(x))^4)/(sd(x)^4)
  }
  return (k-3)
}
print("kurtosis of different distributions way 2:")

krt <- lapply(dataset, kurt2)
krt

plot(dataset)

data.cor = cor(dataset, method = "spearman")
data.cor

#install.packages("corrplot")
library(corrplot)

corrplot(data.cor)

# second way
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE)

plot(dataset$Maximum.Palermo.Scale, dataset$Cumulative.Palermo.Scale,
     xlab="Maximum Palermo", ylab="Cumulative Palermo")

plot(dataset$Cumulative.Palermo.Scale, dataset$Cumulative.Impact.Probability,
     xlab="Impact Probability", ylab="Cumulative Palermo")

plot(dataset$Cumulative.Palermo.Scale, dataset$Possible.Impacts,
     xlab="Possible Impacts", ylab="Cumulative Palermo")
plot(dataset$Cumulative.Palermo.Scale, dataset$Asteroid.Diameter..km.,
     xlab="Asteroid Diameter", ylab="Cumulative Palermo")

# gets the outliers and substracts their entire row from the copy of a dataset, from now on, dataframe is x

outliers <- boxplot(dataset$Cumulative.Palermo.Scale, plot=FALSE)$out
x<-dataset
x<- x[-which(x$Cumulative.Palermo.Scale %in% outliers),]

outliers1 <- boxplot(x$Cumulative.Impact.Probability, plot=FALSE)$out
x<- x[-which(x$Cumulative.Impact.Probability %in% outliers1),]

# we can verify that this worked by comparing the number of entires in initial and final dataframe
print("initial dataframe entry count:")
dim(dataset)
print("processed dataframe entry count:")
dim(x)

print("original, with outliers")

plot(dataset$Cumulative.Palermo.Scale, dataset$Cumulative.Impact.Probability)
abline(lm(dataset$Cumulative.Impact.Probability~dataset$Cumulative.Palermo.Scale), col="red")


print("cleaned, without outliers")

plot(x$Cumulative.Palermo.Scale, x$Cumulative.Impact.Probability)
abline(lm(x$Cumulative.Impact.Probability~x$Cumulative.Palermo.Scale), col="red")

# create linear model with cleaned data

y<-x$Cumulative.Palermo.Scale
xx<-x$Cumulative.Impact.Probability
lmMod <- lm (y ~ xx)
lmMod

summary(lmMod)

plot(lmMod$residuals, col="blue", lwd=2)
abline(h=0)

summary(lmMod)

#now lets try multilinear regression with 2 independent variables, with dependent variable this
#time being impact probability

fit <- lm(x$Cumulative.Impact.Probability ~ x$Cumulative.Palermo.Scale + x$Asteroid.Diameter..km.)
summary(fit) # show results

install.packages('caTools')
library(caTools)

install.packages('ggplot2')
library(ggplot2)

install.packages('MLmetrics')
library(MLmetrics)

#randomly split dataset into testing and training, create linear model

set.seed(123)
split=sample.split(x$Cumulative.Palermo.Scale, SplitRatio = 0.8)

training_set = subset(x, split == TRUE)
test_set = subset(x, split == FALSE)

regressor = lm(training_set$Cumulative.Palermo.Scale ~ training_set$Maximum.Palermo.Scale)

summary(regressor)

plot(regressor$residuals, col="blue", lwd=2)
abline(h=0, col='red')

# visualising prediction line mapped to actual results
y_pred = predict(regressor, newdata=test_set)


ggplot() +
  geom_point(aes(x=training_set$Maximum.Palermo.Scale,
                 y=training_set$Cumulative.Palermo.Scale), colour='red')+
  geom_line(aes(x=training_set$Maximum.Palermo.Scale,
                y=predict(regressor, newdata=training_set)), colour='blue') +
  ggtitle('Cumulative Palermo vs Maximum Palermo') +
  xlab('Maximum Palermo') +
  ylab('Cumulative Palermo')

# evaluating model accuracy with different metrics

library(MLmetrics)
print("Root Mean Square Error:")
RMSE(test_set$Cumulative.Palermo.Scale, y_pred)
print("mean absolute percentage error:")
MAPE(test_set$Cumulative.Palermo.Scale, y_pred)
print("mean absolute error:")
MAE(test_set$Cumulative.Palermo.Scale, y_pred)

# testing on test set

ggplot() +
  geom_point(aes(x=test_set$Maximum.Palermo.Scale,
                 y=test_set$Cumulative.Palermo.Scale), colour='red')+
  geom_line(aes(x=training_set$Maximum.Palermo.Scale,
                y=predict(regressor, newdata=training_set)), colour='blue') +
  ggtitle('Cumulative Palermo vs Maximum Palermo') +
  xlab('Maximum Palermo') +
  ylab('Cumulative Palermo')

# multilinear

regressor = lm(formula = Cumulative.Palermo.Scale ~ ., data=training_set)
summary(regressor)

regressor = lm(formula = Cumulative.Palermo.Scale ~ Possible.Impacts + 
                 Asteroid.Velocity + Maximum.Palermo.Scale, data=training_set)
summary(regressor)

# different model evaluation metrics:

print("Root Mean Square Error:")
RMSE(predict(regressor, newdata=test_set), test_set$Cumulative.Palermo.Scale)
print("mean absolute percentage error:")
MAPE(predict(regressor, newdata=test_set), test_set$Cumulative.Palermo.Scale)
print("mean absolute error:")
MAE(predict(regressor, newdata=test_set), test_set$Cumulative.Palermo.Scale)

# SVR regression

# install.packages('e1071')
# library(e1071)

regressor = svm(formula = Cumulative.Palermo.Scale ~ Maximum.Palermo.Scale, data=x,
                type = 'eps-regression')

# summary and different model evaluation metrics:

summary(regressor)

print("Root Mean Square Error:")
RMSE(predict(regressor, newdata=x), x$Cumulative.Palermo.Scale)
print("mean absolute percentage error:")
MAPE(predict(regressor, newdata=x), x$Cumulative.Palermo.Scale)
print("mean absolute error:")
MAE(predict(regressor, newdata=x), x$Cumulative.Palermo.Scale)

# residual visualisation

plot(regressor$residuals, col="blue", lwd=2)
abline(h=0, col='red')

# visualising accuracy

ggplot() +
  geom_point(aes(x=x$Maximum.Palermo.Scale,
                 y=x$Cumulative.Palermo.Scale), colour='red')+
  geom_line(aes(x=x$Maximum.Palermo.Scale,
                y=predict(regressor, newdata=x)), colour='blue') +
  ggtitle('Cumulative Palermo vs Maximum Palermo') +
  xlab('Maximum Palermo') +
  ylab('Cumulative Palermo')

