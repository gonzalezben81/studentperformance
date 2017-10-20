#Question 12(B)

##rm(list=ls())
library(ISLR)
library(randomForest)
library(tree)
attach(Student_Performance_Ben_Gonzalez)
fix(College)
lstMSEs <- numeric()
set.seed(1)
maxnumpreds <- ncol(Student_Performance_Ben_Gonzalez) - 1
maxnumtrees <- 10

samplesize <- dim(Student_Performance_Ben_Gonzalez)[1] 
numfolds <- 10

quotient <- samplesize %/% numfolds 
remainder <- samplesize %% numfolds 


lstsizes <- rep(quotient,numfolds)
if (remainder > 0) {
  for (i in 1:remainder){
    lstsizes[i] <- lstsizes[i]+1
  }
}
start <- 1

for(numpreds in 1:maxnumpreds){
  for(numtrees in 1:maxnumtrees){
    train <- sample(1:nrow(Student_Performance_Ben_Gonzalez), nrow(Student_Performance_Ben_Gonzalez)/2) 
    model.bagged.2 <- randomForest(G3~.,data=Student_Performance_Ben_Gonzalez,subset=train,mtry=numpreds,ntree=numtrees,importance=TRUE) 
    
    pred.vals.bagged <- predict(model.bagged.2, newdata=Student_Performance_Ben_Gonzalez[-train,])
    testvals <- Student_Performance_Ben_Gonzalez$G3[-train]
    mse <- mean((pred.vals.bagged - testvals)^2)
    lstMSEs <- rbind(lstMSEs, mse)
    
    print(paste("    Processed trees:", numtrees))
  }
  print(paste("Processed predictors:", numpreds))
}

matMSEs <- matrix(lstMSEs,nrow=maxnumpreds,ncol=maxnumtrees)
location <- which(matMSEs == min(matMSEs), arr.ind=TRUE)
print(paste("Optimal number of predictors:", location[1],
            "  optimal number of trees:", location[2]))
tree.student2=tree(HOM~.,data = College)
plot(model.bagged.2)
text(tree.student2,pretty = 0)
varImpPlot(model.bagged.2)
importance(model.bagged.2)
model.bagged.2
min(lstMSEs)
