'VIDEOS'

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

'ML MODEL LOGISTIC REGRESSION'
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall["Accuracy"]


'ML MODEL K-NEAREST NEIGHBORHOOD OR KNN MODEL'
knn_fit <- knn3(y ~ ., data = mnist_27$train)

'OTRA MANERA DE HACER LO MISMO CON UNA MATRIZ TRAIN Y UN VECTOR Y'

class(mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)


knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

'Las manchas azules que deberian ser rojas en la zona rojas (2) es debido al overtraining'

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]


y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]




'OverSmoothing'

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]



ks <- seq(3, 251, 2)


library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})
accuracy


'///////////////// Q1 /////////////////////////'

data("heights")
head(heights)

set.seed(1)
ks <- seq(1, 101, 3)


y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
train_set <- heights[test_index,]
test_set <- heights[-test_index,]
class(heights$sex)
class(heights$height)
'OJO EN EL MODELO KNN SE CREA ESTE CON TRAIN PERO PARA LA FUNCIÓN PREDICT SE PONE
TEST_SET,FACTOR CON TEST SET TAMBIEN ASÍ COMO F_MEAS TAMBIEN'
f1scored <- function(k){

knn_fit <- knn3(sex ~ height, data = train_set, k = k)   
y_hat <- predict(knn_fit, test_set, type = "class") %>% factor(levels = levels(test_set$sex))   
F_meas(data = y_hat, reference = factor(test_set$sex))
}

f1s <- sapply(ks,f1scored)


plot(ks,f1s)
max(f1s)
k_max <- ks[which.max(f1s)]
k_max


'//////////////////// Q2 ////////////////////'

'////////////////////////////////////////////////////////////////////'
'
y <- as.matrix(tissue_gene_expression$y)
x <- as.matrix(tissue_gene_expression$x)

x
y
class(y)
d

train_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
train_index
class(train_index)

train_set_x <- as.matrix(tissue_gene_expression$x)[-train_index,]
train_set_x
test_set_x <- as.matrix(tissue_gene_expression$x)[train_index,]
test_set_x

train_set_y <- as.matrix(tissue_gene_expression$y)[-train_index,]
train_set_y
test_set_y <- as.matrix(tissue_gene_expression$y)[train_index,]
test_set_y

train_set <- list('x'= train_set_x,'y'=as.factor(train_set_y))
train_set

test_set <- list('x'= test_set_x,'y'=as.factor(test_set_y))
test_set


class(tissue_gene_expression)
class(train_set)

test_set$y
class(test_set_y)
factor(test_set$y)
class(factor(test_set$y))
levels((factor(test_set$y)))


accuracyQ2 <-function(k){ 
  
knn_fitq2 <- knn3(y~x , data=train_set, k=k)
y_hat <- predict(knn_fitq2, test_set,type="class")%>% factor(levels = levels(test_set$y))
cm<- confusionMatrix(data = y_hat, reference = factor(test_set$y))
Accuracy_k <- cm$overall["Accuracy"]

}

kq2 <- seq(1,11,1)
accu <- sapply(kq2,accuracyQ2)
accu
data.frame(k=kq2,ac=accu)
'



'/////////////////////Esta es la que funciona para Q2///////////////////////////////////'
data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1)

ind <- createDataPartition(y, times = 1, list = FALSE)
ind
train_y <- y[ind]
class(train_y)
test_y <- y[-ind]

train_x <- x[ind,]
class(train_x)
test_x <- x[-ind,]

ks <- seq(1,11,2)
'/////////////////////////////////////////////////////////////'
train_set <- data.frame(x=as.numeric(train_x),y=train_y)
train_set
test_set <- data.frame(x=test_x,y=test_y)
test_set


class(train_set$y)
class(train_set$x)
'/////////////////////////////AQUI PUEDO HACERLO UNO A UNO QUE ES COMO SALE, CON LA FUNCIÓN K=5 QUE ES EL MAXIMO NO ME SALE LO MISMO(SOLO ESE PUNTO)/////////////////////////////////////'
knn_fitq2 <- knn3(train_x,train_y,k=11)
y_hat <- predict(knn_fitq2, test_x,type="class")
y_hat
mean(y_hat==test_y)

'////////////////////////////////////////////////////////////////////////'
accuracyQ22 <- function(x) {
  knn_fitq2 <- knn3(train_x,train_y,k=x)
  y_hat <- predict(knn_fitq2, test_x,type="class")
  mean(y_hat==test_y)
}
sapply(ks,accuracyQ22)
'////////////////////////////////////////////////////////////////'