#####KNN#####

#Implement a KNN model to classify the animals in to categories. 

#Load the dataset
zoo<- read.csv(file.choose())
View(zoo)
attach(zoo)

#Standardizing the data
library(DataExplorer)
plot_str(zoo)
str(zoo)
plot_missing(zoo)
head(zoo)

#First column in dataset removed as it's not required 
zoo <- zoo[-1]
View(zoo)

#Type of animal table
table(type)

#Visualizing different features of glass data
library(corrplot)
corrplot(cor(zoo))

#Splitting data
library(caTools)

set.seed(101)
zoo_sample <- sample.split(zoo$type,SplitRatio = 0.70)
zoo_train <- subset(zoo, zoo_sample == TRUE)
zoo_test <- subset(zoo, zoo_sample == FALSE)

#choosing k-value
zoo_predict <- NULL
zoo_error <- NULL

for (i in 1:15) {
  zoo_predict <- knn(zoo_train[1:15],zoo_test[1:15],zoo_train$type,k=i)
  zoo_error[i] <- mean(zoo_predict!=zoo_test$type)
  
}

zoo_knnerror <- as.data.frame(cbind(k=1:15,error.type =zoo_error))

#choosing k-value by visualization
ggplot(zoo_knnerror,aes(k,error.type))+ geom_point()+ geom_line() + scale_x_continuous(breaks=1:15)+ 
  theme_bw() +xlab("Value of K") +ylab('Error')
## According to the plot, k=1 is the optimum value.

#KNN
zoo_predict<- knn(zoo_train[1:15],zoo_test[1:15],zoo_train$type,k=1)
#Error in prediction
error<- mean(zoo_predict!=zoo_test$type)
#Confusion Matrix
confusionMatrix(factor(zoo_predict, levels = 1:15),factor(zoo_test$type, levels = 1:15))

## The Accuracy measured is 100%. 

