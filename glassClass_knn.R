#####KNN#####

#Prepare a model for glass classification using KNN

#Load the dataset
glass<- read.csv(file.choose())
View(glass)
attach(glass)

#Standardizing the data
library(DataExplorer)
plot_str(glass)
str(glass)
plot_missing(glass)
head(glass)

#Type of glass table
table(Type)

#Visualizing different features of glass data
library(corrplot)
corrplot(cor(glass))

#Splitting data
library(caTools)
library(class)

set.seed(101)
glass_sample <- sample.split(glass$Type,SplitRatio = 0.70)
glass_train <- subset(glass, glass_sample == TRUE)
glass_test <- subset(glass, glass_sample == FALSE)

#choosing k-value
glass_predict <- NULL
glass_error <- NULL

for (i in 1:9) {
  glass_predict <- knn(glass_train[1:9],glass_test[1:9],glass_train$Type,k=i)
  glass_error[i] <- mean(glass_predict!=glass_test$Type)
  
}

glass_knnerror <- as.data.frame(cbind(k=1:9,error.type =glass_error))

#choosing k-value by visualization
ggplot(glass_knnerror,aes(k,error.type))+ geom_point()+ geom_line() + scale_x_continuous(breaks=1:9)+ 
  theme_bw() +xlab("Value of K") +ylab('Error')
## According to the plot, k=1 or k=2 is the optimum value, we will take the value as 2

#KNN
glass_predict<- knn(glass_train[1:9],glass_test[1:9],glass_train$Type,k=2)
#Error in prediction
error<- mean(glass_predict!=glass_test$Type)
#Confusion Matrix
library(caret)
confusionMatrix(factor(glass_predict, levels = 1:6),factor(glass_test$Type, levels = 1:6))

## The Accuracy measured is 69.09%. 

