



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




data("tissue_gene_expression")

'HASTA AHORA AS?, PERO QUEREMOS OPTIMIZAR PARAMETROS POR LO QUE FUNCION TRAIN'
set.seed(1991)

x <- tissue_gene_expression$x
class(x)
y <- tissue_gene_expression$y
class(y)
grid <-expand.grid(cp=seq(0, 0.1, 0.01))
class(grid)
grid
fit <- train(x, y, method="rpart", tuneGrid = grid) 
fit$bestTune
plot(fit)
confusionMatrix(fit)





set.seed(1991)

x <- tissue_gene_expression$x
class(x)
y <- tissue_gene_expression$y
class(y)
grid <-expand.grid(cp=seq(0, 0.1, 0.01))
class(grid)
grid
fit <- train(x, y, method="rpart", tuneGrid = grid,control= rpart.control(minsplit = 0)) 
fit$bestTune
plot(fit)
confusionMatrix(fit)
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)



