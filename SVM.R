dataset=read.csv(file = "C:/Users/gandixit/Desktop/R tutorial/SVM/Iris.csv",header = T)

nrow(dataset)


View(dataset)

str((dataset))
class(dataset)

normalize=function(x){
  (x-min(x))/(max(x)-min(x))
}

attach(dataset)

dataset$PetalLengthCm=normalize(dataset$PetalLengthCm)
dataset$SepalLengthCm=normalize(dataset$SepalLengthCm)
dataset$SepalWidthCm=normalize(dataset$SepalWidthCm)
dataset$PetalWidthCm=normalize(dataset$PetalWidthCm)

set.seed(111)
rownumber1=sample(1:nrow(dataset),0.7*nrow(dataset))
col=c("PetalLengthCm","PetalWidthCm","Species")
traindata=dataset[rownumber1,col]
testdata=dataset[-rownumber1,col]

nrow(dataset)
nrow(traindata)
nrow(testdata)


str(dataset)
library(e1071)

str(iris)
attach(dataset)
str(dataset)
modelbuildlinear=svm(formula=Species~ .,data = traindata,kernel="linear",cost=0.1,scale = F)
modelbuildpolynomial=svm(formula=Species~ .,data = traindata,kernel="polynomial",cost=0.1,scale = F)
modelbuildradial=svm(formula=Species~ .,data = traindata,kernel="radial",cost=0.1,scale = F)


print(modelbuildlinear)
print(modelbuildpolynomial)
print(modelbuildradial)


predictionlinear=predict(object = modelbuildlinear,newdata = testdata[,col],type="class")

predictionpolynomial=predict(object = modelbuildpolynomial,newdata = testdata[,col],type="class")


predictionradial=predict(object = modelbuildradial,newdata = testdata[,col],type="class")

table(predictedvalues=predictionlinear,actualvalues=testdata[,3])

table(predictedvalues=predictionpolynomial,actualvalues=testdata[,3])

table(predictedvalues=predictionradial,actualvalues=testdata[,3])


plot(modelbuildlinear,testdata[,col])
plot(modelbuildpolynomial,testdata[,col])
plot(modelbuildradial,testdata[,col])