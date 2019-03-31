set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x
x_subset <- x[ ,sample(p, 100)]


fit <- train(x_subset, y, method = "glm")
fit$results


library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)  

'//Solucion para obtener genefilter package//'


tt <- colttests(x, y)
tt
nrow(tt)
ind <- tt[tt$p.value<=0.01,]
nrow(ind)
ind <- which(tt$p.value<=0.01)
ind

x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

'////////////////////////////////    Q7  ////////////////////'

data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

fit <- train(x,y,method="knn",tuneGrid = data.frame(k = seq(1, 301, 25)))
fit$results
ggplot(fit)
