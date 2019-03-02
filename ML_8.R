'Codigo Clases'

mnist <- read_mnist()

head(mnist)
class(mnist)

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

x
class(x)
y

x1 <- 1:5
x2 <- 6:10
matrix <- cbind(x1,x2)
class(matrix)
dim(matrix)

dim(x1)
dim(as.matrix(x1))


z <- 1:15
matrix_z <- matrix(z,5,3)
matrix_z
matrix_z <- matrix(z,5,3,byrow=TRUE)
matrix_z

'Q2'
nrow((matrix_z))
ncol((matrix_z))

'Codigo clases de vuelta'
'Hacer transpuesta de una matriz'
t(matrix_z)

'Volvemos al ejemplo de los digitos'

grid <- matrix(x[3,],28,28)

'Para crear una imagen'

'image((1:28,1:28,grid)'
      
'La imagen aparece tumbada, si queremos darle la vuelta a la enumeración de R'

'image((1:28,1:28,grid[,28:1])'
      
'Seguimos'      
library(matrixStats)
colSds(mnist$train$images)
      
'Extraer columnas o filas de un vector'

mnist$train$images[,c(351,352)]
mnist$train$images[c(351,352),]
      
'si seleccionas una sola columna de una matriz se convierte en vector dicha variable, si no 
quieres que suceda esto deben poner el argumento Drop'


class(x[,1,drop=FALSE])

'Convertir matrices en vectores'

mat <- matrix(1:15,5,3)
mat
as.vector(mat)


as.vector(x)
qplot(as.vector(x),bins=30,color=I("Black"))

'Q6'

mnist <- read_mnist()
mnist
class(mnist)
MNIST <- matrix(mnist$train$images,60000,784)
class(MNIST)

sds <- colSds(MNIST)
new_MNIST <- MNIST[,sds > 60]
new_MNIST
dim(new_MNIST)
avgs <- rowMeans((new_MNIST))
avgs
class(avgs)

qplot(as.vector(MNIST),bins=30,color=I("Black"))
new_Bin <- as.vector(MNIST)
new_Bin[new_Bin<50]<- 0
new_Bin[new_Bin>205]<- 0
new_Bin[new_Bin>=50 & new_Bin<=205]<- 1
mean(new_Bin)


AVGS <- as.matrix(avgs)
AVGS
LABELS <- as.matrix(mnist$train$labels)
LABELS
AVGS_LABELS <- cbind(AVGS,LABELS) 



AVGS_LABELS %>% ggplot(aes(x=[,2],y=[,1]))


