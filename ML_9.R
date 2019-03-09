'Coger 500 muestras cuyos resultados son 2 o 5 en el labels'
set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images [ind,]
y <- mnist$train$labels[ind]


y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

'Calculo de la distancia'
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

'Se puede hacer así tambien'

sqrt(crossprod(x_1-x_2))

'Todavia más sencillo'

d<- dist(x)
d

'Es de la clase dist, para tratarla necesitamos convertirla en matriz'

as.matrix(d)[1:3,1:3]

'Podemos verlo en forma de imagen'
image(as.matrix(d))

'Las ordenamos por valor de labels 0-9 ya que estamos calculando distancias por registros'

image(as.matrix(d)[order(y),order(y)])

'si queremos calcular distancias por entradas transponemos la matrix'

d <- dist(t(x))
dim(as.matrix(d))


'Inicio Lab Distance'
'///////////////////////////// Q1 ///////////////////////////////////////'

library(dslabs)
data("tissue_gene_expression")


dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x) 
d <- as.matrix(d)
d

x_1 <- tissue_gene_expression$x[1,]
x_2 <- tissue_gene_expression$x[2,]
d <- sqrt(crossprod(x_1-x_2))
d


x_39 <- tissue_gene_expression$x[39,]
x_40 <- tissue_gene_expression$x[40,]
d <- sqrt(crossprod(x_39-x_40))
d


x_73 <- tissue_gene_expression$x[73,]
x_74 <- tissue_gene_expression$x[74,]
d <- sqrt(crossprod(x_73-x_74))
d


image(as.matrix(d))
image(as.matrix(d)[order(tissue_gene_expression$y),order(tissue_gene_expression$y)])
