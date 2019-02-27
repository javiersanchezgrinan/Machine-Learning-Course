'Enunciado'

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()


'se trata de crear una diferencia numerica entre un conjunto de datos y otro
e ir viendo como el accuracy va creciendo a medidad que se van 
distanciando ambas muestras normales (no hay intersección de datos)'


delta <- seq(0, 3, len=25)
accuracy <- function(x){
  
  y <- rbinom(1000, 1, 0.5)
  f_0 <- rnorm(1000, 0, 1)
  f_1 <- rnorm(1000, x, 1)
  x <- ifelse(y == 1, f_1, f_0)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train0 <- data.frame(x = x, y = as.factor(y))
  train <- train0 %>% slice(-test_index)
  test0 <- data.frame(x = x, y = as.factor(y))
  test <- test0 %>% slice(test_index)
  glm_fit <- train %>% glm(y~x,data=.,family = "binomial")
  glm_fit
  p_hat_logit <- predict(glm_fit, newdata = test, type = "response")
  p_hat_logit
  y_hat <- factor(ifelse(p_hat_logit > 0.5, 1, 0))
  y_hat
  return(mean(y_hat==test$y))}
  accur <- sapply(delta,accuracy)
  accur
  data.frame(delta, accur) %>% 
    ggplot(aes(delta, accur)) + 
    geom_point() + geom_line() 
