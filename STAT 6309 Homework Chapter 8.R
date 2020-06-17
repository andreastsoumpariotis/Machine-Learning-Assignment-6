
packages = c('ISLR', 'caret', 'tidyverse', 'rpart', 'rpart.plot', 'ranger', 'ggthemes',
              'knitr', 'kableExtra', 'dplyr')

# Question 8

car = read.csv("Documents/SMU/2019-20/Spring/STAT 6309/Homework/Homework Chapter 8/Carseats.csv")
View(car)

set.seed(1)

# Part a
train = sample(1:nrow(car), nrow(car)/2)
car_train = car[train, ]
car_test = car[-train,]

# Part b
library(tree)
regression_tree = tree(Sales~., data = car_train)
# Summary
summary(regression_tree)
# Plot Regression Tree
plot(regression_tree)
text(regression_tree, pretty =0)
# MSE
yhat = predict(regression_tree, newdata = car_test)
mean((yhat - car_test$Sales)^2) #4.148897

# Part c
# Cross-Validation
cv = cv.tree(regression_tree)
plot(cv$size, cv$dev, type = "b", xlab="Size", ylab="Dev")
# Prune tree
prune = prune.tree(regression_tree, best = 8)
plot(prune)
text(prune, pretty=0)
# MSE
yhat2 = predict(prune, newdata = car_test)
mean((yhat2 - car_test$Sales)^2) #5.09085

# Part d
# Bagging Approach
library(randomForest)
bag = randomForest(Sales ~ ., data = car_train, mtry = 10, importance = TRUE)
# MSE
yhat3 = predict(bag, newdata = car_test)
mean((yhat3 - car_test$Sales)^2) #2.565661
# Importance
importance(bag)
varImpPlot(bag)

# Part e
rf = randomForest(Sales ~ ., data = car_train, mtry = 3, importance = TRUE)
# MSE
yhat4 = predict(rf, newdata = car_test)
mean((yhat4 - car_test$Sales)^2) #3.277562
# Importance
importance(rf)
varImpPlot(rf)

# Question 9

oj = read.csv("Documents/SMU/2019-20/Spring/STAT 6309/Homework/Homework Chapter 8/OJ.csv")
View(oj)

set.seed(1)

# Part a
train = sample(dim(oj)[1],800)
oj_train = oj[train,]
oj_test = oj[-train,]

# Part b
oj_tree = tree(Purchase ~ ., data = oj_train)
summary(oj_tree)

# Part c
print(oj_tree)

# Part d
plot(oj_tree)
text(oj_tree, pretty=TRUE)

# Part e
tree = predict(oj_tree, newdata = oj_test, type = "class")
table(tree, oj_test$Purchase)
(154+65)/270 #0.8111111

# Part f
cv = cv.tree(oj_tree, FUN = prune.misclass)
print(cv)

# Part g
plot(cv$size, cv$dev,type='b', xlab = "Tree Size", ylab = "Error Rate")

# Part i
prune = prune.misclass(oj_tree, best = 5)
plot(prune)
text(prune, pretty=0)

# Part j
# Unpruned Tree
tree = predict(oj_tree, newdata = oj_train, type = "class")
table(tree, oj_train$Purchase)
(439+235)/800 #0.8425
# Pruned Tree
tree2 = predict(prune, newdata = oj_train, type = "class")
table(tree2, oj_train$Purchase)
(442+221)/800 #0.82875

# Part k
# Unpruned Tree
tree = predict(oj_tree, newdata = oj_test, type = "class")
table(tree, oj_test$Purchase)
(154+65)/270 #0.8111111
# Pruned Tree
tree2 = predict(prune, newdata = oj_test, type = "class")
table(tree2, oj_test$Purchase)
(154+59)/270 #0.7888889

# Question 10
### CODE DOESNT WORK AFTER PART C- SEE OTHER CODE IN FOLDER ###

library(gbm)
library(glmnet)
set.seed(1)

hitters = read.csv("Documents/SMU/2019-20/Spring/STAT 6309/Homework/Homework Chapter 8/Hitters.csv")

# Part a
hitters = na.omit(hitters)
hitters$Salary = log(hitters$Salary)

# Part b
train = 1:200
hitters_train = Hitters[train,]
hitters_test = Hitters[-train,]

# Part c
p = seq(-10, -0.2, by = 0.1)
lambda = 10^p
train_error = rep(NA, length(lambda))
for (n in 1:length(lambda)) {boost = gbm(Salary ~ ., data = hitters_train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[n])
  pred_train = predict(boost, hitters_train, n.trees = 1000)
  train_error[n] = mean((pred_train - hitters_train$Salary)^2)}
plot(lambda, train_error, type = "b", xlab = "Shrinkage Values", ylab = "Training MSE")

# Part d
test_error <- rep(NA, length(lambda))
for (n in 1:length(lambda)) {boost = gbm(Salary ~ ., data = hitters_train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[n])
  yhat = predict(boost, hitters_test, n.trees = 1000)
  test_error[n] = mean((yhat - hitters_test$Salary)^2)}
plot(lambda, test_error, type = "b", xlab = "Shrinkage Values", ylab = "Test MSE")

# Part e
fit = lm(Salary ~ ., data = hitters_train)
pred = predict(fit, hitters_test)
mean((pred - hitters_test$Salary)^2) #0.4917959
x = model.matrix(Salary ~ ., data = hitters_train)
x_test = model.matrix(Salary ~ ., data = hitters_test)
y = hitters_train$Salary
fit2 = glmnet(x, y, alpha = 0)
pred2 = predict(fit2, s = 0.01, new = x_test)
mean((pred2 - hitters_test$Salary)^2) #0.4570283

# Part f
boost = gbm(Salary ~ ., data = hitters_train, distribution = "gaussian", n.trees = 1000, shrinkage = lambda[which.min(test_error)])
summary(boost)

