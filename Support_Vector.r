## Support Vector Machines in R (https://www.youtube.com/watch?v=pS5gXENd3a4&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1&index=2)
data(iris)
str(iris)
library(ggplot2)
qplot(Petal.Length, Petal.Width, data = iris,
      color= Species)
## Support Vector Machines (1:28 : https://www.youtube.com/watch?v=pS5gXENd3a4&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1&index=2)
#install.packages("e1071")
library(e1071)
mymodel<- svm(Species~., data = iris)
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice=list(Sepal.Width = 3, Sepal.Length=4))
#In the vis the data is shown in graph with support vectors highlighted in cross and desicion boundaries. The predicted class regiosn is also identified.

## Confusion Matrix and Misclassification Error
pred<- predict(mymodel, data= iris)
tab<- table(Predicted= pred, Actual= iris$Species)
tab

#Misclassification Error
1- sum(diag(tab))/sum(tab)

#This is the misclassification while using 'radial' kernal, now changing the kernal to see difference in misclassification error.

# Linear kernel
mymodel<- svm(Species~., data = iris,
              kernel= "linear")
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice=list(Sepal.Width = 3, Sepal.Length=4))
#In the vis the data is shown in graph with support vectors highlighted in cross and desicion boundaries. The predicted class regiosn is also identified.

## Confusion Matrix and Misclassification Error
pred<- predict(mymodel, data= iris)
tab<- table(Predicted= pred, Actual= iris$Species)
tab
#Misclassification Error
1- sum(diag(tab))/sum(tab)

# Polynomial kernel
mymodel<- svm(Species~., data = iris,
              kernel= "polynomial")
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice=list(Sepal.Width = 3, Sepal.Length=4))
#In the vis the data is shown in graph with support vectors highlighted in cross and desicion boundaries. The predicted class regiosn is also identified.

## Confusion Matrix and Misclassification Error
pred<- predict(mymodel, data= iris)
tab<- table(Predicted= pred, Actual= iris$Species)
tab
#Misclassification Error
1- sum(diag(tab))/sum(tab)

# Sigmoid kernel
mymodel<- svm(Species~., data = iris,
              kernel= "sigmoid")
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice=list(Sepal.Width = 3, Sepal.Length=4))
#In the vis the data is shown in graph with support vectors highlighted in cross and desicion boundaries. The predicted class regiosn is also identified.

## Confusion Matrix and Misclassification Error
pred<- predict(mymodel, data= iris)
tab<- table(Predicted= pred, Actual= iris$Species)
tab
#Misclassification Error
1- sum(diag(tab))/sum(tab)

#Fine-Tuning the model to get better classification aka (Hyper optimisation of the parameter), also helps in selecting best model
set.seed(123)
tmodel<- tune(svm, Species~., data = iris,
     ranges = list(epsilon = seq(0,1,0.1), cost= 2^(2:7))) # 88 different combinitaions
#So, the parameter epsilon make use of sequence which will start from 0 to 1 with an increment of 0.1
#cost captures cost of contraint violation and default value is 1, if cost is too high then high penelty for non seperable points and what may happen is model may store too many support vectors and model will overfit.. if cost is too less then underfitting. So we cover large range to get optimum value.

plot(tmodel)
## darker region means better result --> Lower value of cost
summary(tmodel)

## Best Model
mymodel<- tmodel$best.model
summary(mymodel)
plot(mymodel, data = iris,
     Petal.Width~Petal.Length,
     slice=list(Sepal.Width = 3, Sepal.Length=4))
pred<- predict(mymodel, data= iris)
tab<- table(Predicted= pred, Actual= iris$Species)
tab
#Misclassification Error
1- sum(diag(tab))/sum(tab)

