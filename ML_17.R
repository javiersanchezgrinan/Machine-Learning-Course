

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

'/////////////////////////////// Q1 /////////////////////////////////'


x <- as.matrix(tissue_gene_expression$x)
d <- dist(x)
plot(d)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))
pca <- prcomp(x)
summary(pca)


data.frame(pca$x[,1:2], Gens=tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = tissue_gene_expression$y))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)


'/////////////////////////////// Q2 /////////////////////////////////'

as.data.frame(tissue_gene_expression$x)

correlation<- cor(as.data.frame(tissue_gene_expression$x))
max(correlation)
max(correlation)

'/////////////////////////////// Q2 Preanalisis (Faltaria por optimizar un poco el grafico, separación entre valores de Means) /////////////////////////////////'

means <- rowMeans(as.data.frame(tissue_gene_expression$x))

means

q3 <- data.frame(means=means, PC=pca$x[,1],Tissue=tissue_gene_expression$y)

q3 %>% ggplot(aes(means,PC, fill = Tissue))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

cor(means,pca$x[,1])

'/////////////////////////////// Q3 /////////////////////////////////'
x <- as.data.frame(tissue_gene_expression$x)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
x

pca <- prcomp(x)
data.frame(pca_1 = pca$x[,1], pca_2 = pca$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pca_1, pca_2, color = tissue)) +
  geom_point()



'/////////////////////////////// Q4 /////////////////////////////////'

'solucion profesor'

for(i in 1:10){
  boxplot(pca$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}


'segunda forma
'
q4 <- data.frame(PC=pca$x[,1:10],Tissue=tissue_gene_expression$y)
q4

q4 %>% ggplot(aes(x=Tissue,y=pca$x[,7]))+
  geom_boxplot()



'/////////////////////////////// Q5 /////////////////////////////////'
class(summary(pca)$importance)

importance_df <- data.frame(summary(pca)$importance)
importance_df
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))


importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()








