library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]



set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]




y <- iris$Species



'///////////////////////////////////////////////////////////////////////////////'


x <- iris$Petal.Length


cutoff <- seq(0.0, 7.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
'//////////////////////////////////////////////////////////////////////////////'

x <- iris$Petal.Width


cutoff <- seq(0.0, 7.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
'////////////////////////////////////////////////////////////////////////////////////'


x <- iris$Sepal.Length


cutoff <- seq(0.0, 10.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
'///////////////////////////////////////////////////////////////////////////////////'


x <- iris$Sepal.Width


cutoff <- seq(0.0, 10.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
'////////////////////// Q4 ///////////////////////////////////////////'

x <- iris$Petal.Length


cutoff <- seq(0.0, 7.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

x <- iris$Petal.Width


cutoff <- seq(0.0, 7.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

x <- iris$Sepal.Length


cutoff <- seq(0.0, 10.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

x <- iris$Sepal.Width


cutoff <- seq(0.0, 10.0)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == t$Species)
})
max(accuracy)

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

'La respuesta es que se coje aquel que tiene una diferencia menor entre modelos porque sigue una salida coherente dentro de un intervalo y no modifica su accuracy mucho'

'//////////////////////////////////// Q5 /////////////////////////////////////////'


y_hat <- ifelse(test$Petal.Length > 4.7 & test$Petal.Width > 1.5 , "virginica", "versicolor")%>% factor(levels = levels(test$Species))
mean(y_hat == test$Species)

