## Input the training set
train <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
## Size of the training set is ~19 MB.
# library(pryr)
# object_size(train)

## Minor exploration of the data
str(train)
summary(train)
sum(complete.cases(train))
sum(complete.cases(train[train$new_window=="yes",]))
# table(train$user_name, train$classe)
## It can be seen from the summary results that there are certain columns which
## include an unreasonable amount of NA values. Moreover, the NA values are all
## limited to the rows for which variable new_window's value corresponds to yes.
## And none of the columns apart from these ones contain NA values.

## Minor preprocessing
na_vec <- lapply(train, function(x) any(is.na(x)))
sum(unlist(na_vec))
train <- train[!unlist(na_vec)]
## We obtain the columns containing the NA values and remove them.

## Splitting the provided training dataset to create cross-validation and
## training datasets.
set.seed(23)
library(caret)
split <- createDataPartition(train$classe, p = 0.7, list = F)
x_train <- train[split,]
x_test <- train[-split,]

## Further exploration, now using the training data set created in the previous
## step.
library(ggplot2)
qplot(user_name, roll_belt, data=x_train, fill=user_name, geom = c("boxplot"))
qplot(user_name, pitch_belt, data=x_train, fill=user_name, geom = c("boxplot"))
## We discover there is a lot of variation in the numerical variables. It would
## be useful to perform standardization when preprocessing.
qplot(roll_belt, data=x_train, colour=user_name, geom = c("density"))
## The data is skewed; possibly due to the ??

train_sd <- unlist((sapply(x_train, function(x) {if (is.numeric(x) | is.integer((x))) sd(x)})))
# which.min(train_sd)
qplot(gyros_belt_y, data=x_train, colour=user_name, geom = c("density"))
qplot(user_name, gyros_belt_y, data=x_train, fill=user_name, geom = c("boxplot"))

sort(train_sd)
# x_train <- x_train[, !names(x_train) %in% rem_names]
# x_test <- x_test[, !names(x_test) %in% rem_names]

## gyros_* variables' covariance
cor(x_train[c(51, 52, 53, 38, 39, 40)])

std_obj <- preProcess(x_train[,-c(1:6, 60)], method = c("center", "scale"))
x_train_std <- predict(std_obj, x_train[,-c(1:6, 60)])
b <- unlist((sapply(x_train_std, function(x) {if (is.numeric(x) | is.integer((x))) sd(x)})))
x_train_std$user_name <- x_train$user_name

qplot(user_name, roll_belt, data=x_train_std, fill=user_name, geom = c("boxplot"))
qplot(user_name, pitch_belt, data=x_train_std, fill=user_name, geom = c("boxplot"))
qplot(roll_belt, data=x_train_std, colour=user_name, geom = c("density"))

std_names <- names(x_train_std)
x_train[,std_names] <- x_train_std[, std_names]
rm(x_train_std)

x_test_std <- predict(std_obj, x_test[,-c(1:6, 60)])
std_names <- names(x_test_std)
x_test[,std_names] <- x_test_std[,std_names]
rm(x_test_std)

## Removed an outlier row
x_train <- x_train[!x_train$gyros_forearm_y > 10,]

# length(unique(train$raw_timestamp_part_1))
# length(unique(train$raw_timestamp_part_2))

library(randomForest)
rf_mod <- randomForest(classe ~ ., data=x_train[,-c(1:6)], ntree = 20)
pred <- predict(rf_mod, x_test[,-c(1:6)])
confusionMatrix(pred, x_test$classe)
pred_train <- predict(rf_mod)
confusionMatrix(pred_train, x_train$classe)

test <- read.csv(file = "pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
test <- test[!unlist(na_vec)]

test_std <- predict(std_obj, test[,-c(1:6, 60)])
test[,std_names] <- test_std[,std_names]
rm(test_std)

answers <- predict(rf_mod, test)

# require(rpart)
# tree_mod <- train(classe ~ ., data = x_train[,-c(1:6)], method ="rpart")

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
