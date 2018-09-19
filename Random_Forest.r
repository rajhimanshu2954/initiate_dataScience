## Random Forest in R (https://www.youtube.com/watch?v=dJclNIN-TPo&index=3&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1)
## CTG data : 2166 data points: Contains Fetal cardiograms to conclude NSP between normal(1) suspect(2) or pathalogic(3)
##Read Data
data<- read.csv("", header=T)
str(data)
data$NSP<- as.factor(data$NSP)
table(data$NSP)

##Data Partition
set.seed(123)
##Partitoning data into train and test
set.seed(1234)
ind<- sample(2, nrow(data), replace = T, prob= c(0.7, 0.3))
train<- data[ind==1, ]
test<- data[ind==2, ] 
 
## Randon Forest Model
# RF is developed using aggregating desicion trees.
# Can be used for both classificatio and regression model
# It avoids overfitting
# Can deal with large no of features
# Helps in feature selection based on importance
# Very user friendly as it deals with 2 free parameters
	Trees: nTree--> deafault 500,
	Varibles randonmly sampled as candidate at each split - mtry 
	default is square root(p) for classificatio and p/3 for regression
	
## Works in  3-steps (See Video: 4:56)

## Implementing Random Forest
library(randomForest)
set.seed(1234)
rf<- randomForest(NSP~., data= train)
print(rf)
attributes(rf)

# Prediction and confustion matrix
library(caret)
p1<- predict(rf, train)
head(p1)
confusionMatrix(p1,train$NSP)
# randomForest gives you the oob(out of bag) error rate
#	For each bootstrap iteration we calcualte prediction error for data not in bootstrap sample

## Prediction with test data
p2<- predict(rf, test)
confusionMatrix(p2,test$NSP)

## Error rate for randomForest
plot(rf)

## Tune model (mtry)
t<- tuneRF(train[,-22], train[,22], 
		stepFactor= 0.5, 					#m-try is deflated or inflated by this value in every try
		plot = T,
		ntree = 300,
		trace = T,
		improve = 0.05)
# This gives us an idea on what mtry value should we choose
# Now, retrying the rf model
rf2 <- randomForest(NSP~., data= train,
		ntree = 300,
		mtry = 8,
		importance = T,
		proximity = T)
print(rf)
p2<- predict(rf, test)
confusionMatrix(p1,test$NSP)

## Number of nodes for the trees
hist(treesize(rf),
	main = "No of Nodes for the trees",
	col = "green")
	
## Variable Importance
varImpPlot(rf)
# Meandecreaseaccuracy (imp): This graph tells you how worse the model will perform once you remove each variable
# MeandecreaseGini (imp): This graph tells you how pure the nodes are at the end of the tree without each variable

varImpPlot(rf,
			sort = T,
			n.var = 10,
			title = "Top 10 variables")
importance(rf)
varUsed(rf)

## Partial Dependence Plot
partialPlot(rf, train, ASTV, "1")
partialPlot(rf, train, ASTV, "3")
partialPlot(rf, train, ASTV, "2")

## Extract Single tree from Forest
getTree(rf, 1, labelVar = T)

## MDSM (Multi-dimensional Scaling Plot of Proximity matrix)
MDSplot(rf, train$NSP)

