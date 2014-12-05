# TODO: Add comment
# 
# Author: Hiren Dutta
###############################################################################

## You should create a report describing how you built your model, how you used cross validation,
## what you think the expected out of sample error is, and why you made the choices you did.
## You will also use your prediction model to predict 20 different test cases.

## 1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML
## file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number
## of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages
## branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

## 2. You should also apply your machine learning algorithm to the 20 test cases available in the test data
## above. Please submit your predictions in appropriate format to the programming assignment for automated grading.
## See the programming assignment for additional details.


require(caret)
require(ggplot2)
require(knitr)
require(randomForest)

# List of URLs 
training_URL<-"D:/Workspace/git/PracticalMachineLearning/data/pml-training.csv"
test_URL<-"D:/Workspace/git/PracticalMachineLearning/data/pml-testing.csv"
model_fileName <- "D:/Workspace/git/PracticalMachineLearning/model/rf_model.rda"
output_directory <- "D:/Workspace/git/PracticalMachineLearning/output/"

# Load CSV dataset
training<-read.csv(training_URL,na.strings=c("NA",""))
test<-read.csv(test_URL,na.strings=c("NA",""))

#Preprocess Dataset, removing unnesessary colunms.
noOfCols <- ncol(training);
paste("number of column =>", noOfCols )
training<-training[,7:noOfCols]
test<-test[,7:noOfCols]

noOfTRainingRows <- nrow(training) - 1
paste("number of Rows =>", noOfTRainingRows )
dataOnly<-apply(!is.na(training),2,sum)>noOfTRainingRows
training<-training[,dataOnly]
test<-test[,dataOnly]
dim(training)

# Partitioning training dataset 
InTrain<-createDataPartition(y=training$classe,p=0.6,list=FALSE)
training1<-training[InTrain,]

dim(training1)

# Creating model with trainging data set (used Random Forest)
rf_model<-train(classe~.,data=training1,method="rf",
		trControl=trainControl(method="cv",number=5),
		prox=TRUE,allowParallel=TRUE)
print(rf_model)
print(rf_model$finalModel)

# Recall phase - Predicting test dataset
pml_write_files  <- function(x) {
	n = length(x)
	for(i in 1:n){
		filename = paste0(output_directory,"problem_id_",i,".txt")
		write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
	}
}

pml_write_files(predict(rf_model, newdata = test))

knit2html("D:/Workspace/git/PracticalMachineLearning/project.Rmd", "D:/Workspace/git/PracticalMachineLearning/index.html")