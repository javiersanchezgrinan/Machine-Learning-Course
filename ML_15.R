



getModelInfo("Rborist")



n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

dat

set.seed(1)
grid <- expand.grid(minNode=seq(25, 100, 25),predFixed=1)
grid

train_Rborist <- train(y~.,method="Rborist",tuneGrid=grid, data = dat)
ggplot(train_Rborist,highlight=TRUE)
train_Rborist$bestTune
train_Rborist$finalModel
train_Rborist$modelInfo



library(caret)
dat %>% 
  mutate(y_hat = predict(train_Rborist)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  geom_step(aes(x, y), col = 2)
  geom_smooth(aes(y_hat, x), col = 2)
  geom_smooth(aes(x, y_hat), col = 2)


