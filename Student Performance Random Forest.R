
#Question 10(A)
##rm(list = ls())## Clears the R code being used
library(ISLR)
library(tree)
names(Student_Performance_Ben_Gonzalez)
library(randomForest)
attach(Student_Performance_Ben_Gonzalez)
fix(Student_Performance_Ben_Gonzalez)

lstMSEs=numeric()
set.seed(1)
maxnumpreds=ncol(Student_Performance_Ben_Gonzalez)-1
maxnumtrees=10

for(numpreds in 1:maxnumpreds){
  for(numtrees in 1:maxnumtrees){
    
    nrow(Student_Performance_Ben_Gonzalez)
    train=sample(1:nrow(Student_Performance_Ben_Gonzalez),nrow(Student_Performance_Ben_Gonzalez)/2)
    
    
    model.bagged=randomForest(G3~.,data = Student_Performance_Ben_Gonzalez,subset = train,mtry=numpreds,ntree=numtrees,importance=TRUE)
    
    
    
    pred.vals.bagged=predict(model.bagged,newdata = Student_Performance_Ben_Gonzalez[-train])
    testvals=Student_Performance_Ben_Gonzalez$G3[-train]
    mse=mean((pred.vals.bagged - testvals)^2)
    lstMSEs=rbind(lstMSEs,mse)
    print(paste("     Processed Trees:",numtrees))
  }
  print(paste("     Processed Predictors:",numpreds))
}

matMSEs=matrix(lstMSEs,nrow = maxnumpreds,ncol=maxnumtrees)


print(paste("The optimal configuration is",loc[1],"predictors and",loc[2], "trees"))
length(lstMSEs)
list(lstMSEs)

min(lstMSEs)
min(matMSEs)
lstMSEs

loc=which(matMSEs==min(matMSEs),arr.ind=TRUE)
print(paste("The optimal configuration is",loc[1],"predictors and",loc[2], "trees"))
length(lstMSEs)
print(paste("        Processed Trees:", numtrees))
print(paste("        Processed Predictors:",numpreds))
matMSEs[loc[1],loc[2]]



which(matMSEs==min(matMSEs),arr.ind = TRUE)
importance(model.bagged)
tree.student1=tree(G3~.,data = Student_Performance_Ben_Gonzalez)
plot(model.bagged)
plot(tree.student1)
text(tree.student1,pretty = 0)
varImpPlot(model.bagged)
model.bagged
min(lstMSEs)

