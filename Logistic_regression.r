#Logistic regression (URL:https://www.youtube.com/watch?v=AVx7Wc1CQ7Y&t=905s)

## Reading the data file
mydata<- read.csv(file.choose(), header = T)
str(mydata)

## Converting into factor variables
mydata.admit<- as.factor(mydata.admit)
mydata.rank<- as.factor(mydata.rank)

## Two-way table for factor variation:
xtabs(~admit + rank, data= mydata)

***********
##More on two-way factor table....
##This is an example Code taken from other youtube tutorial (https://www.youtube.com/watch?v=VGKz3Jkx-9I&t=308s) good to find crosstabs and contigency and proportions etc.

sn.tab<- table(sn$Gender, sn$site) ## Both categorical relations
sn.tab

## Marginal Frequencies
margin.table(sn.tab, 1) # Row Marginal Frequencies
margin.table(sn.tab, 2) # Column Marginal Frequencies

##Proportions of categorical values in each other
round(prop.table(sn.tab), 2) 	 # cell-wise%
round(prop.table(sn.tab, 1), 2)  # row-wise%
round(prop.table(sn.tab, 2), 2)	 # col-wise%

## Chi-Squared Test: 
chisq.test(sn.tab)
p-value = 0.02151 # which means <0.05 which means the variations  are there and ignore null hypothesis

###
***********
## Partitioning data - Train(80%) and Test(20%)
set.seed(1234)
ind<- sample(2, nrow(mydata), replace = T, prob= c(0.8, 0.2))
train<- mydata[ind==1, ]
test<- mydata[ind==2, ]

## Logistic Regression model
mymodel1<- glm(admit~ gre + gpa + rank, data= train, family = 'binomial')
summary(mymodel1)

##Less singinficant varibles will be removed after checking the P-Value and confidence level
mymodel<- glm(admit~ gpa + rank, data= train, family = 'binomial')
summary(mymodel1)

##Prediction
p1<- predict(mymodel, train, type = 'response')
head(p1)
head(train)

## Probability Calculation (Refer to the video, very important and informative)

##Misclassification Error - train data
pred1<- ifelse(p1>0.5, 1, 0)
tab1<- table(Predicted= pred1, Actual= train$admit)
tab1
1- sum(diag(tab1))/sum(tab1)  # Misclassification Error on train data
 
##Misclassification Error - test data
p2<- predict(mymodel, test, type = 'response')
pred2<- ifelse(p2>0.5, 1, 0)
tab2<- table(Predicted= pred2, Actual= test$admit)
tab2
1- sum(diag(tab2))/sum(tab2)  # Misclassification Error on train data

##Good-ness of Fit (Calulcate P value using chi-squared)
with(mymodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail =F))
#1.45e-6
#p-value= 0.00000145 which is very small this confidence level very high
*************************************************** 

# ROC and AUC with R (https://www.youtube.com/watch?v=ypO1DPEKYFo)
## Logistic regression model (library(nnet) % need to look into this %)
library(nnet)
mymodel<- multinom(admit~., data =binary)

##Consusion Matrix, Misclassification Error:
p<- predict(mymodel, binary)
tab<- table(p, binary$admit)
tab

p	0		1
0	253		98			## Confusion Matrix
1	20		29

sum(diag(tab))/ sum(tab) 	 # Correct Classification = 0.705
1- sum(diag(tab))/ sum(tab)  # Mis-Classification = 0.295

# Is the classification good enough?
table(binary$admit)
0		1
273		127

## 127 - admitted
## 273 - Not admitted

## 273/400 - If we predict all students will npt be predicted we will be 0.6825 (68.25%) right.
## We have to get a better accuracy than above, as of now we have 0.705 which is 70.5% accuract which is slightly better.
## Further Model Performance Evaluation
library(ROCR)
pred<- predict(mymodel, data= binary, type= 'prob')
head(pred)
head(binary)
## So, here cutoff probability is 0.5
hist(pred)

## Probability varoes between 0 and 0.8 and most of the value are 0.4, so using  cutoff of 0.5 is not justified. Using a different value of cutoff accuracy or misclassification might change.

## Let's change and see what are the new preditions and probabilies

pred<- prediction(pred, binary$admit) 	#using ROCR package (need to read)
eval<- performance(pred, "acc")
plot(eval) 								##Gives you a graph for cutoff versues accuracy

## Using naked eye lets try to see thr accuracy model

abline(h = 0.71, v=0.45)

## Identifying best Values and accuracy
max<- which.max(slot(eval, "y.values")[[1]])
acc<- slot(eval, "y.values")[[1]][max]
0.7175

cut<- slot(eval, "x.values")[[1]][max]
0.468

print(c(Accuracy = acc, Cutoff= cut))
Accuracy   Cutoff
0.7175     0.468

## This is based on the table we have seen earlier, it tells how the model has performed.
## Sometimes we are interested to predict on one group. Like Sensitivity, Specificity
## Let's suppose for below set
p  0      1
0  253    98
1  20     29
tpr --> true positive rate (Sensitivity) --> 29/ (29+98) --> 0.2283
fpr --> false positive rate (1- Specificity) --> 20/(253+20) --> 0.073
roc<- performance(pred, "tpr", "fpr")
plot(roc)

## True positive rate(Sensitivity) vs false positive rate (1- Specificity)
abline(a=0, b=1)
plot(roc,
	colorize = T,
    main= "Roc Curve",
    ylab = "Senstivity",
	xlab= "1-Specificity")

abline(a=0, b=1)

## We find area under curve, if the area is higher the model performance is better. Area should be more than 50%

##Area Under Curve (AUC)
auc<- performance(pred, "auc")
auc<- unlist(slot(auc, "y.values"))
auc
0.69212 ~ 0.70 # which is greated than 0.5 which means this is a better model than neutral model
legend(0.6, 0.2, auc, title = "AUC", cex= 1.2 )

*************************************************** 
# Handling class imbalance problem in R (Random Forest for Eg) (https://www.youtube.com/watch?v=Ho2Klvzjegg&index=11&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1)

## Reading Data
data<- read.csv("~/Desktop/binary.csv", header=True)
str(data)
data$admit <-  as.factor(data$admit)
summary(data)
admit
0: 273
1: 127          }#imbalance dataset

## Class Imbalance Problem
prop.table(table(data$admit))
0			1
0.6825		0.3175

##Can see the barplot for class imbalance

barplot(prop.table(table(data$admit)), 
	col= rainbow(2),
	ylim= c(0, 0.7),
	main= "Class Distribution"
	)

## When we built model using imbalance data the predictive model is domniated by the contribution from data in bigger class, accuracy is better to predict "0" Class to "1" class in above case.

## Now Partitionining the data to see the predictions,
set.seed(1234)
ind<- sample(2, nrow(mydata), replace = T, prob= c(0.8, 0.2))
train<- mydata[ind==1, ]
test<- mydata[ind==2, ]
 
## Data for developing predictive model
table(train$admit)
0		1
188		97

prob.table(table(train$admit))
0		1
0.6596 	0.3403

summary(train)

## Predction Model - random forest
library(randomForest)
rftrain<- randomForest(admit~., data=train)

## Model Evaluation with test data
library(caret)
library(e1071)
confusionMatrix(predict(rftrain, test), test$admit, positive= '1') ## To refer that posotive is one, which means a student getting admitted to college is positive

				Reference
Prediction 		0		1	
		0		71		21
		1		14		9

	Acc: 					0.6957 			## (71+9)/(71+14+21+9)
	95% CI: (				0.6029, 0.778) 	## Calculated based on the binomial distribution 
	No Infformation Rate:	0.7391  		## largest Proportion of observed class (in above lasrgest proportion is '0' i.e. (71+14)/(115))
											## This means that, if we do not develop any model and simpli classfied everything to '0', we will be right 73% times. 
	Sensitivity: 			0.3000			##9/(21+9):  How often we are able to correctly predict class 1
	Specificity: 			0.8325			##71/85: HOw often we are correclty predict class 0
	
	Specificity>Sensitivity ## One of the reason for wide diff becuase of data imbalance. If we wanted to predict only for 0 this was not a bad model, however for predicting 1 this is....the total acc of 0.6957 is misleading
	
## Oversampling for better accuracy

library(ROSE)
## ROSE --> Randomly Over Sampling Examples
table(train$admit)
0		1
188		97
over<- ovun.sample(admit~., data= train, method= "over", N = 376)$data
table(over$admit)
0		1
188		188

## The class 1 dataset are incresed to 188 by random repetition to match the total  	
rfover<- randomForest(admit~., data=over)
confusionMatrix(predict(rfover, test), test$admit, positive= '1')
				Reference
Prediction 		0		1	
		0		50		13
		1		35		17
		
	Acc: 					0.5826
	Sensitivity: 			0.5667 ##Improved Signifcantly
	Specificity: 			0.5882 
	
## Under-sampling
table(train$admit)
0		1
188		97
under<- ovun.sample(admit~., data= train, method= "under", N = 194)$data
table(under$admit)
0		1
97		97
rfunder<- randomForest(admit~., data=under)
confusionMatrix(predict(rfunder, test), test$admit, positive= '1')
				Reference
Prediction 		0		1	
		0		45		10	
		1		40		20
		
	Acc: 					0.5652
	Sensitivity: 			0.6667 ##Improved Signifcantly even from over sampling
	Specificity: 			0.5294 
*************************************************.

There is one more both under and over sampling which creates for both.......you can watch the video for more details

There is another one to create synthetic data and do the both sampling as well
