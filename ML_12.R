
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)


indexes$Resample01[indexes$Resample01==3]

indexes$Resample01[indexes$Resample01==4]

indexes$Resample01[indexes$Resample01==7]


indexes$Resample01[indexes$Resample01==3]
indexes$Resample01[indexes$Resample02==3]
indexes$Resample01[indexes$Resample03==3]
indexes$Resample01[indexes$Resample04==3]
indexes$Resample01[indexes$Resample05==3]
indexes$Resample01[indexes$Resample06==3]
indexes$Resample01[indexes$Resample07==3]
indexes$Resample01[indexes$Resample08==3]
indexes$Resample01[indexes$Resample09==3]
indexes$Resample01[indexes$Resample10==3]



set.seed(1)
y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

B <- 10000
quantiles <- replicate(B,{
  
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
  
})

mean(quantiles)
sd(quantiles)

'////// Q4 //////'


N <- 100
set.seed(1)
y <- rnorm(N, 0, 1)
set.seed(1)
ind10 <- createResample(y,10,list=FALSE)
ind10
class(ind10)

df10 <- as.data.frame(as.table(ind10))
df10
df10 <- df10 %>% mutate(y = y[ind10])
df10
Q_stars <- df10 %>% group_by(Var2) %>% summarize(Q_star = quantile(y, 0.75))
Q_stars                                                 
mean(Q_stars$Q_star)
sd(Q_stars$Q_star)



'////// Q5 //////'



N <- 100


set.seed(1)
y <- rnorm(N, 0, 1)
set.seed(1)


ind10 <- createResample(y,10000,list=FALSE)
ind10
class(ind10)
df10 <- as.data.frame(as.table(ind10))
df10
df10 <- df10 %>% mutate(y = y[ind10])
df10
Q_stars <- df10 %>% group_by(Var2) %>% summarize(Q_star = quantile(y, 0.75))
Q_stars                                                 
mean(Q_stars$Q_star)
sd(Q_stars$Q_star)