'ENUNCIADO'
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat
xo <- dat$x
yo <- dat$y
xo
yo
'Q1'

library(caret)
y <-dat$y
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- dat %>% slice(test_index)
train <- dat %>% slice(-test_index)
test
train
test_index
'GUESSING'
avg <- mean(train$y)
sd <- sd(train$y)
mean((avg-test$y)^2)
'MODELO MACHINE LEARING REGRESION LINEAL'
fit <- lm(y~x,data = train)
summary(fit)
y_hat <- predict(fit,test)
y_hat
mean((y_hat-test$y)^2)



'voy a intentar el replicate porque lo requiere el ejercicio
pero en principio esto bastaria, se mide el error entre modelos con el RMSD 
no la variabilidad entre conjuntos de datos de muestra (Funciona!)'

set.seed(1)
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
  
})

RMSDs
mean(RMSDs)
sd(RMSDs)

'Q2'


n <- c(100, 500, 1000, 5000, 10000)

RMSD_mean <- function(x){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n=x, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  y <- dat$y
  RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))})
  return(mean(RMSDs))}

RMSD_sd <- function(x){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n=x, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  y <- dat$y
  RMSDs <- replicate(100,{
    test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
    test <- dat %>% slice(test_index)
    train <- dat %>% slice(-test_index)
    fit <- lm(y~x,data = train)
    y_hat <- predict(fit,test)
    sqrt(mean((y_hat-test$y)^2))})
  return(sd(RMSDs))}

'PARA LAS RESPUESTAS DEL EXAMINADOR SOLO ES CORRECTO PONER SET.SEED 
ANTES DE APLICAR CADA FUNCION, SI NO SE PONE ANTES DE CADA UNA 
AL APLICARLA DESPUES A LA SIGUIENTE FUNCION NO SE OBTIENEN LAS RESPUESTAS 
CORRECTAS'
set.seed(1)
means <- sapply(n,RMSD_mean)
means
set.seed(1)
sds <- sapply(n,RMSD_sd)
sds

'la de 100 los dos mal, la de 500 los dos bien, 
la de 1000 los dos mal,la de 5000 solo el sd y la 10000 la media (voy a hacerlos manualmente).Despues de hacerlo 
no me sirve para obtener las respuestas correctas'
n <- 1000
set.seed(1)
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 1000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat
y <- dat$y
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
                         })
RMSDs
mean(RMSDs)
sd(RMSDs)

'Q4'

n <- 100
set.seed(1)
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
y <- dat$y
set.seed(1)
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
})
mean(RMSDs)
sd(RMSDs)

'Q6'

'y~x_1'

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

y <- dat$y
set.seed(1)
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_1,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
})
mean(RMSDs)
sd(RMSDs)

'y~x_2'


set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

y <- dat$y
set.seed(1)
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_2,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
})
mean(RMSDs)
sd(RMSDs)

'y~x_1~x_2'


set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

y <- dat$y
set.seed(1)
RMSDs <- replicate(100,{
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_1+x_2,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
})
mean(RMSDs)
sd(RMSDs)


' no replication of data / n<-1000 no sirve de nada'
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
y <- dat$y
set.seed(1)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- dat %>% slice(test_index)
train <- dat %>% slice(-test_index)
fit <- lm(y~x_1,data = train)
y_hat <- predict(fit,test)
sqrt(mean((y_hat-test$y)^2))





set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
y <- dat$y
set.seed(1)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- dat %>% slice(test_index)
train <- dat %>% slice(-test_index)
fit <- lm(y~x_2,data = train)
y_hat <- predict(fit,test)
sqrt(mean((y_hat-test$y)^2))






set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

  y <- dat$y
  set.seed(1)
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_1+x_2,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))

  'Q8'
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  y <- dat$y
  cor(dat)
  set.seed(1)
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_1,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
  
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  y <- dat$y
  set.seed(1)
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_2,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
  
  
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  y <- dat$y
  set.seed(1)
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- dat %>% slice(test_index)
  train <- dat %>% slice(-test_index)
  fit <- lm(y~x_1+x_2,data = train)
  y_hat <- predict(fit,test)
  sqrt(mean((y_hat-test$y)^2))
