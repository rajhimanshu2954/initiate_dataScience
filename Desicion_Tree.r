# Desicion Tree: https://www.youtube.com/watch?v=tU3Adlru1Ng&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1

## We are using the CTG(Cardiotocographic) data which has 21 features and the response feature is NSP --> N(Normal)1, S(Suspect)2, P(Pathalogic)3

## Reading and preparing data from CSV file
data<- Cardiotocographic
str(data)
#Converting the response into factor or categorical variable
data$NSPF<- as.factor(data$NSP)

##Partitoning data into train and test
set.seed(1234)
ind<- sample(2, nrow(data), replace = T, prob= c(0.8, 0.2))
train<- data[ind==1, ]
validate<- data[ind==2, ]

## Desicion Tree with Party Pcakage
library(party)
tree<- ctree(NSPF~LB+AC+FM, data=train)
tree
plot(tree)

## There are 19 nodes in the tree
## This tree is generally upside down, the roots are at the top and the leaves are in bottom, the most imp varible is uaully at the top

##Adding controls to the control parameters

tree<- ctree(NSPF~LB+AC+FM, data=train, control= ctree_control(mincriterion = 0.99, minsplit=500))  ## mincriterion--> Confidence level
																								   ## minsplit --> A branch will split into 2 only when the sample size is 500  These will restrict growth of tree
# After the changes now there are only 9 nodes

																								   
## Predicting in validation dataset
predict(tree, validate, type = "prob")

#For all observations in the validate dataframe we get 3 probablities namely for 1,2,3.

#If we dont want the probablities, just the predicted factors
predict(tree, validate)

## Descition Tree with R-part package
library(rpart)
tree1<- rpart(NSPF~LB+AC+FM, train)
library(rpart.plot)
rpart.plot(tree1)

##If we want extra information
rpart.plot(tree1, extra=1)
rpart.plot(tree1, extra=2)
rpart.plot(tree1, extra=4)

predict(tree, validate)
# By default probablities

## Misclassification Error for train data
tab<- table(predict(tree), train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
0.197 ## based on training data the misclassfication error is 19.7%

	1		2		3
1	1222	70		112

2	126		156		32

3	0		0		0

## Misclassification Error for validate data
testPred<- predict(tree, newdata= validate)
tab<- table(testPred, validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
0.210 ## based on validation data the misclassfication error is 21.0%

	1		2		3
1	274		21		28

2	33		48		4

3	0		0		0
