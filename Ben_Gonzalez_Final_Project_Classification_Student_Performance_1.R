## Student Performance Classification Models #############################################

## Logistic Regression Prediction GLM Classification ##########
## Schoolsup Classification Prediction
glm.fit1=glm(schoolsup~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit1,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit1,schoolsup,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,schoolsup)
mean(glm.pred==schoolsup)
mean(glm.pred!=schoolsup)

## Famsup Classification Prediction
glm.fit2=glm(famsup~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit2,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit1,famsup,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,famsup)
mean(glm.pred==famsup)
mean(glm.pred!=famsup)

## Paid Classification Prediction
glm.fit3=glm(paid~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit3,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit3,paid,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,paid)
mean(glm.pred==paid)
mean(glm.pred!=paid)

## Activities Classification Prediction
glm.fit4=glm(activities~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit4,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit4,paid,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,activities)
mean(glm.pred==activities)
mean(glm.pred!=activities)

## Nursery Classification Prediction
glm.fit5=glm(nursery~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit5,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit5,paid,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,nursery)
mean(glm.pred==nursery)
mean(glm.pred!=nursery)

## Higher Classification Prediction
glm.fit6=glm(higher~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit6,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit6,paid,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,higher)
mean(glm.pred==higher)
mean(glm.pred!=higher)

## Internet Classification Prediction
glm.fit7=glm(internet~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit7,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit7,paid,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,internet)
mean(glm.pred==internet)
mean(glm.pred!=internet)

## Romantic Classification Prediction
glm.fit8=glm(romantic~G3, family = binomial, data = Student_Performance_Ben_Gonzalez)
glm.probs=predict(glm.fit8,type = "response")
glm.probs[1:10]
glm.probs=predict(glm.fit8,schoolsup,type = "response")
glm.pred=rep("no",395)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,romantic)
mean(glm.pred==romantic)
mean(glm.pred!=romantic)

## QDA Classification #######################################
library(MASS)
## Schoolsup
qda.fit1=qda(schoolsup~G3,data=Student_Performance_Ben_Gonzalez,subset = train)
qda.fit1
qda.class1=predict(qda.fit1,Student_Performance_Ben_Gonzalez)$class
table(qda.class1,schoolsup)
mean(qda.class1==schoolsup)

## Famsup
qda.fit2=qda(famsup~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit2
qda.class2=predict(qda.fit2,Student_Performance_Ben_Gonzalez)$class
table(qda.class1,famsup)
mean(qda.class1==famsup)
plot(qda.class1,famsup)

## Paid
qda.fit3=qda(paid~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit3
qda.class3=predict(qda.fit3,Student_Performance_Ben_Gonzalez)$class
table(qda.class3,paid)
mean(qda.class3==paid)

## Activities
qda.fit4=qda(activities~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit4
qda.class4=predict(qda.fit4,Student_Performance_Ben_Gonzalez)$class
table(qda.class4,activities)
mean(qda.class4==activities)

## Nursery
qda.fit5=qda(nursery~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit5
qda.class5=predict(qda.fit5,Student_Performance_Ben_Gonzalez)$class
table(qda.class5,nursery)
mean(qda.class5==nursery)

## Higher
qda.fit6=qda(higher~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit6
qda.class6=predict(qda.fit6,Student_Performance_Ben_Gonzalez)$class
table(qda.class6,higher)
mean(qda.class6==higher)

## Internet
qda.fit7=qda(internet~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit7
qda.class7=predict(qda.fit7,Student_Performance_Ben_Gonzalez)$class
table(qda.class7,internet)
mean(qda.class7==internet)

## Romantic
qda.fit8=qda(romantic~G3,data=Student_Performance_Ben_Gonzalez,subset=train)
qda.fit8
qda.class8=predict(qda.fit8,Student_Performance_Ben_Gonzalez)$class
table(qda.class8,romantic)
mean(qda.class8==romantic)

## LDA Classification ######################################

## Schoolsup LDA
lda.fit1=lda(schoolsup~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit1
lda.pred1=predict(lda.fit1,schoolsup)
lda.class1=lda.pred1$class
table(lda.class1,schoolsup)
mean(lda.class1==schoolsup)

## Famsup LDA 
lda.fit2=lda(famsup~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit2
lda.pred2=predict(lda.fit2,famsup)
lda.class2=lda.pred2$class
table(lda.class2,famsup)
mean(lda.class2==famsup)

## Paid LDA
lda.fit3=lda(paid~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit3
lda.pred3=predict(lda.fit3,paid)
lda.class3=lda.pred3$class
table(lda.class3,paid)
mean(lda.class3==paid)

## Activities LDA 
lda.fit4=lda(activities~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit4
lda.pred4=predict(lda.fit4,activities)
lda.class4=lda.pred4$class
table(lda.class4,activities)
mean(lda.class4==activities)

## Nursery LDA
lda.fit5=lda(nursery~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit5
lda.pred5=predict(lda.fit5,nursery)
lda.class5=lda.pred5$class
table(lda.class5,nursery)
mean(lda.class5==nursery)


## Higher LDA
lda.fit6=lda(higher~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit6
lda.pred6=predict(lda.fit6,famsup)
lda.class6=lda.pred6$class
table(lda.class6,higher)
mean(lda.class6==higher)
mean(lda.class6!=higher)
plot(lda.fit6)

## Internet LDA
lda.fit7=lda(internet~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit7
lda.pred7=predict(lda.fit7,internet)
lda.class7=lda.pred7$class
table(lda.class7,internet)
mean(lda.class7==internet)

## Romantic LDA
lda.fit8=lda(romantic~G3,data = Student_Performance_Ben_Gonzalez,subset = train)
lda.fit8
lda.pred8=predict(lda.fit8,romantic)
lda.class8=lda.pred8$class
table(lda.class8,romantic)
mean(lda.class8==romantic)
