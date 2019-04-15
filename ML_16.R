
'/////////////////////////////// Q1 /////////////////////////////////'

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
class(models)
library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
names(fits)
fits
class(fits)


y_hat <- predict(fits$knn,mnist_27$test)
y_hat

'/////////////////////////////// Q2 /////////////////////////////////'

prediction <- function(x){
 
  y_hat <- predict(x, mnist_27$test)
}

y_hat_models <- as.matrix(sapply(fits,prediction))
y_hat_models

'/////////////////////////////// Q3 /////////////////////////////////'

'PARA 1 MODELO SOLO, ES DECIR, UNA SOLA COLUMNA(la ultima en este caso)'

mean(y_hat_models[,23]==mnist_27$test$y)

'FOR LOOP / SE DEBERIA HACER CON UNA FP (FUNCTINAL PROGRAMMING) YA QUE CONOCEMOS LAS DIMENSIONES DE LA SALIDA'

i<-1

j<- 1

accuracy <- vector("double",length(models))


for (i in 1:length(models)) {
  
  accuracy[i]=mean(y_hat_models[,i]==mnist_27$test$y)
}

accuracy

mean(accuracy)


'/////////////////////////////// Q4 /////////////////////////////////'

'TEORICAMENTE HABRIA QUE HACERLO CON LAS PROBABILIDADES PERO AQUI LO HACE POR VOTACION ES DECIR CONTAR CUANTOS 7 Y 2 HAY Y EL QUE MAS HAY ESE SE COGE'

pr_rf <- predict(fits$rf, mnist_27$test,type="prob")
pr_glm <- predict(fits$glm, mnist_27$test,type="prob")
'..........'
p <- ( pr_rf + pr_glm ) / 2
'......'
p
class(p)

'esto habria que revisarlo'

factor(apply(p, 1, whick.max))


'//////////////////////////ESTA ES LA SOLUCION:ATENCION A COMO SACA LA MAYORIA ABSOLUTA, NO ES NADA SENCILLO, EL EXAMINADOR LO HACE CON rowMeans y coge el que supera 0.5//////////////////////////////////////'

i<-1
count <- 0

 for(i in 1:200){
   
  count[i] <- names(which.max(table(y_hat_models[i, ])))
  
 }

count

ensemble <- mean(count==mnist_27$test$y)

ensemble



' mira lo que hace table por separado primero, luego saca which.max que te da el numero de la columna, si quieres sacar el nombre o level hay que usar funcion Names'
table(y_hat_models)

names(which.max(table(y_hat_models[1, ])))

p <- table(y_hat_models[1, ])

p

class(p)

summary(y_hat_models)

'con esto no he podido hacerlo'

y_pred <- apply(y_hat_models,1,which.max(table))

y_pred

class(y_pred)
'/////////////////////////////// Q5 /////////////////////////////////'

index <- which(accuracy>=0.8)
index
models[index]
'/////////////////////////////// Q6 y Q7 jugando con el Q5/////////////////////////////////'

fits_tuned <- lapply(models[index], function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
names(fits)
fits





prediction <- function(x){
  
  y_hat <- predict(x, mnist_27$test)
}

y_hat<- as.matrix(sapply(fits_tuned,prediction))
y_hat







i<-1

j<- 1

accuracy <- vector("double",length(models[index]))


for (i in 1:length(models)) {
  
  accuracy[i]=mean(y_hat[,i]==mnist_27$test$y)
}

accuracy
mean(accuracy)

'/////////////////////////////// Q7 /////////////////////////////////'

fits_tuned <- lapply(models[index], function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
names(fits)
fits





prediction <- function(x){
  
  y_hat <- predict(x, mnist_27$test)
}

y_hat<- as.matrix(sapply(fits_tuned,prediction))
y_hat







i<-1

j<- 1

accuracy <- vector("double",length(models[index]))


for (i in 1:length(models)) {
  
  accuracy[i]=mean(y_hat[,i]==mnist_27$test$y)
}

accuracy
mean(accuracy)
