library(caret)
library(dslabs)
y <- heights$sex
x <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
plot((accuracy))
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

ncol(read_mnist())


'//////////////////////////////'
  
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
dat
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

'QUESTION 1'

inclass_females <- ifelse(y=='Female'& x =='inclass',TRUE,FALSE)
sum(inclass_females)

total_inclass <- ifelse(x =='inclass',TRUE,FALSE)
sum(total_inclass)

online_females <- ifelse(y=='Female'& x =='online',TRUE,FALSE)
sum(online_females)

total_online <- ifelse(x =='online',TRUE,FALSE)
sum(total_online)

proportion_inclass <- sum(inclass_females)/sum(total_inclass)

proportion_online <- sum(online_females)/sum(total_online)

proportion_inclass

proportion_online

'QUESTION 2'

'///////////////COMPARACION GUESSING COMO CLASS CHARACTER////////////////////////'

library(caret
        )
set.seed(2)

test_index <- createDataPartition(dat$sex, times = 1, p = 0.5, list = FALSE)

test_index
class(test_index)

test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

test_set
train_set

class(test_set)
class(train_set)

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat
class(y_hat)
mean(y_hat == test_set$sex)
class(test_set$sex)



'////////////////////COMPARACION GUESSIONG CON CLASS FACTOR//////////////////////'


library(caret
)
set.seed(2)

y <- factor(dat$sex, c("Female", "Male"))

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_index
class(test_index)

test_set <- y[test_index]
train_set <- y[-test_index]

test_set
train_set

class(test_set)
class(train_set)

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)%>%factor(levels = levels(test_set))
y_hat
class(y_hat)
mean(y_hat == test_set)
class(test_set$sex)