# codes are written in R studio
```r
# installing the loading the packages
install.packages("kernlab")
install.packages("ggplot2")
library("kernlab")
library("ggplot2")
```
```r
# reading the data
forest <- read.csv(file.choose())
summary(forest)
attach(forest)
forest1 <- forest[,1:10] #taking only required columns
forest1 <- cbind(forest1,size_category)
attach(forest1)
str(forest1)
```
```r
# converting strings into categories
forest1$month <- as.factor(forest1$month)
forest1$day <- as.factor(forest1$day)
forest1$size_category <- as.factor(forest1$size_category)
```
```r
# training and testing data and loading the package
library(caret)
partdata <- createDataPartition(forest1$size_category, p=.70, list = F)
train1 <- forest1[partdata,]
test1 <- forest1[-partdata,]
```
```r
# building the model
model1 <- ksvm(size_category~. , data = train1, kernel = "vanilladot") #Vanilladot - Linear kernel
model1 # checking error
# checking the accuracy
pred1 <- predict(model1, test1[,-11])
tab1 <- table(test1[,11], pred1)
equals <- test1[,11] == pred1
prop.table(table(equals))
```
```r
# buidling the model with different kernel and checking its accuracy

# model2: radial basis function kernel
model2 <- ksvm(size_category~. , data =train1, kernel = "rbfdot") 
model2 
pred2 <- predict(model2, test1[-11])
tab2 <- table(test1[,11], pred2)
equals2 <-test1[,11] == pred2
prop.table(table(equals2))

# model3 : polynomial kernel
model3 <- ksvm(size_category~. , data =train1, kernel = "polydot", kpar = list(degree = 2, scale = 2)) 
model3 
pred3 <- predict(model3, test1[-11])
tab3 <- table(test1[,11], pred3)
equals3 <-test1[,11] == pred3
prop.table(table(equals3))

# model4 : hyperbolic tangent kernel
model4 <- ksvm(size_category~. , data = train1, kernel = "tanhdot") 
model4 
pred4 <- predict(model4,test1[-11])
tab4 <- table(test1[,11], pred4)
equals4 <- test1[,11] == pred4
prop.table(table(equals4))

# model5: Laplacian kernel
model5 <- ksvm(size_category~. , data = train1, kernel = "laplacedot", kpar = list(sigma =2), C = 8) 
model5 
pred5 <- predict(model5, test1[-11])
tab5 <- table(test1[,11], pred5)
equals5 <- test1[,11] == pred5
prop.table(table(equals5))
```
# choose which gives the least training error







