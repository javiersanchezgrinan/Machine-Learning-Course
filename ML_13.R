

'// Q1 y Q2 /////////////'


set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
class(ind)
y <- droplevels(tissue_gene_expression$y[ind])
class(y)
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
class(x)
x<- data.frame(x)
x
class(x)

train_LDA <- train(x,y,method = "lda")
train_LDA$results
train_LDA$finalModel


'///////////////////////////////////////// PRUEBAS Q2 CON TODOS LOS GENES/////////////////////////////////////////'
dotplot(t(train_LDA$finalModel$means))

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
class(ind)
y <- droplevels(tissue_gene_expression$y[ind])
class(y)
x <- tissue_gene_expression$x[ind, ]
x <- data.frame(x)
x


'////////'
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
class(ind)
y <- droplevels(tissue_gene_expression$y[ind])
class(y)
x <- tissue_gene_expression$x[ind, ]
x <- data.frame(x)
x
'/////////'
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- x %>% slice(-test_index)
test_set <- x %>% slice(test_index)

train_LDA <- train(x,y,method = "lda")
t(train_LDA$finalModel$means)
train_LDA$results
train_LDA$finalModel
train_LDA$finalModel$scaling
class(train_LDA$finalModel$scaling)


prueba <- data.frame(train_LDA$finalModel$scaling)
prueba <- prueba %>% mutate(genes = as.factor(rownames(prueba)))
max(prueba$LD1)
prueba[which(prueba$LD1>0.40),]
'//////////'
means <- data.frame(t(train_LDA$finalModel$means))
means
means <- means %>% mutate(gene=as.factor(rownames(means)))
means
means[which(means$hippocampus>13),]

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  geom_point() +
  geom_label_repel(aes(label=gene),size=0.5)+
  theme(legend.position="none")  
'//////////'


'//////////////////////////// Q3 Q4 ////////////////////////////////////'

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
x<- data.frame(x)
x
class(x)

train_LDA <- train(x,y,method = "qda")
train_LDA$results
train_LDA$finalModel

'//////////////////////////// Q5 ////////////////////////////////////'

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
x<- data.frame(x)
x
class(x)

train_LDA <- train(x,y,method = "lda",preProcess = "center")
train_LDA$results
train_LDA$finalModel

'//////////// Q6 ///////////////////'

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
x<- data.frame(x)
x
class(x)

train_LDA <- train(x,y,method = "lda",preProcessing = "scale")
train_LDA$results
train_LDA$finalModel
