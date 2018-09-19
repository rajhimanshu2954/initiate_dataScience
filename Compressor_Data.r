#install.packages("caret")
#install.packages("corrplot")
#install.packages("gridExtra")
#install.packages("rmarkdown")
#install.packages("randomForest")

##Loading the libraries
library(xlsx)
library(caret)
library(corrplot)
library(gridExtra)
library(randomForest)
library(dplyr)


#Setting the work directory
setwd("C:\\Users\\himanshu.raj01\\Desktop\\ABB\\Data")

##Reading the pre-cleaned file
datafile<- read.csv("GT_Compressor_Data.csv")
Tag_list<- read.csv("Tag_List.csv")

##Cleaning the data
dim(datafile)

##Removing near Zero variance data
nearZero<- nearZeroVar(datafile, saveMetrics = TRUE)
head(nearZero, 20)
datafile_new <- datafile[, !nearZero$nzv]
dim(datafile_new)

##Removing unwanted columns
datafile_new <- datafile_new[,-which(names(datafile_new) %in% c("Time.Stamp","Status"))]
dim(datafile_new)
summary(datafile_new)

##Finding correlation matrix
corrplot(cor(datafile_new[, -length(names(datafile_new))]), method = "color", tl.cex = 0.3)
##Highly Correlated Data


#Exploratory Analysis 

a<- ggplot(datafile_new, aes(x=RUL, y=AXIAL.COMPRESSOR.INLET.FL)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
b<- ggplot(datafile_new, aes(x=RUL, y=INLT.PLENUM.DP.TRANSMITER)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
c<- ggplot(datafile_new, aes(x=RUL, y=AXIAL.COMPRESSOR.DISCHARG.1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
d<- ggplot(datafile_new, aes(x=RUL, y=IGV.opening.feedback..296CT.102.ÿÿ)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
e<- ggplot(datafile_new, aes(x=RUL, y=X2ST.INJ.COMP.GAS.296CT102.2)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
f<- ggplot(datafile_new, aes(x=RUL, y=T4.INNER.WHEELSPACE.TEMP)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
g<- ggplot(datafile_new, aes(x=RUL, y=T4.OUTER.WHEELSPACE.TEMP.1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
h<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG1.JOURNAL)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
i<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG1.CESMIC.VIB.1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
grid.arrange(a, b, c, d, e, f, g, h, i, ncol=3, nrow=3)


a<- ggplot(datafile_new, aes(x=RUL, y=TURBINE.BEARING.DRAIN..2)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
b<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG4.RADIAL)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
c<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG4.RADIAL.1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
d<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG4.CESMIC.VIB)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
e<- ggplot(datafile_new, aes(x=RUL, y=TURBINE.THRUST.BEARING..1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
f<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG1.AXIAL.DISPLACMENT)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
g<- ggplot(datafile_new, aes(x=RUL, y=HIGH.EFFICIENCY.FLTR)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
h<- ggplot(datafile_new, aes(x=RUL, y=T4.BRG4.AXIAL.DISPLACMENT)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
i<- ggplot(datafile_new, aes(x=RUL, y=DROPLET.FOG.COALESCNG.FLT)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
grid.arrange(a, b, c, d, e, f, g, h, i, ncol=3, nrow=3)


a<- ggplot(datafile_new, aes(x=RUL, y=PULSE.JET.FLTR)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
b<- ggplot(datafile_new, aes(x=RUL, y=AIR.INTAKE.FLTR.1)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
c<- ggplot(datafile_new, aes(x=RUL, y=GAS.FUEL.SPLITTER.VV)) + geom_point() + geom_smooth(method=lm) + theme(text = element_text(size = 7))
grid.arrange(a, b, c, ncol=3, nrow=1)
rm(a,b,c,d,e,f,g,h,i)

##Subsetting the data which will be used in predictive analysis
traindata<- subset(datafile_new, select = c("AXIAL.COMPRESSOR.INLET.FL", "INLT.PLENUM.DP.TRANSMITER", "AXIAL.COMPRESSOR.DISCHARG.1", "IGV.opening.feedback..296CT.102.ÿÿ", "X2ST.INJ.COMP.GAS.296CT102.2", "T4.INNER.WHEELSPACE.TEMP", "T4.OUTER.WHEELSPACE.TEMP.1", "T4.BRG1.JOURNAL", "T4.BRG1.CESMIC.VIB.1", "TURBINE.BEARING.DRAIN..2", "T4.BRG4.RADIAL", "T4.BRG4.RADIAL.1", "T4.BRG4.CESMIC.VIB", "TURBINE.THRUST.BEARING..1", "T4.BRG1.AXIAL.DISPLACMENT", "HIGH.EFFICIENCY.FLTR", "T4.BRG4.AXIAL.DISPLACMENT", "DROPLET.FOG.COALESCNG.FLT", "PULSE.JET.FLTR", "AIR.INTAKE.FLTR.1", "GAS.FUEL.SPLITTER.VV","RUL"))
dim(traindata)

set.seed(12345)
intrain<- createDataPartition(traindata$RUL, p=0.8, list = F)
training<- traindata[intrain, ]
test<- traindata[-intrain, ]

modelRF <- train(RUL ~ ., data = training, method = "rf", trControl = trainControl(method = "cv", 5), ntree=100)
modelRF

predictRF<- predict(modelRF, test)
plot(modelRF)

test$FinalPredicted_RUL = predictRF
SSE_rf=sum((as.integer(test$FinalPredicted_RUL)-as.integer(test$RUL))^2)
SSR_rf=sum(as.integer((test$FinalPredicted_RUL-mean(test$RUL))^2))
SST_rf<-SSR_rf+SSE_rf
R2_rf<- SSR_rf/SST_rf
write.csv(test, file = "Random_Forest.csv", row.names = FALSE)
testNew<- test%>% 
    filter(RUL<=20) %>% 
    
    select(RUL,FinalPredicted_RUL) %>% 
    data.frame()
    

