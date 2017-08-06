rm(list=ls())
data("Cars93")
View(Cars93)
str(Cars93)

data2=Cars93[c(1,2,27)]
View(data2)

data1=Cars93[c(3,9:11,16,26)]
View(data1)

data4=Cars93[c(18)]
View(data4)
class(data4$Passengers)
data4$Passengers=as.factor(data4$Passengers)
data4_for_dummies=dummy.data.frame(data4)
View(data4_for_dummies)

library(dummies)
data_for_dummies=dummy.data.frame(data1)
View(data_for_dummies)


for(i in 1:ncol(data_for_dummies)){
  if(class(data_for_dummies[,i]) %in% c('integer')){
    data_for_dummies[,i]=as.numeric(as.character(data_for_dummies[,i]))
  }
}
View(data_for_dummies)

data3=Cars93[c(4:8,12:15,17:25)]
View(data3)
for(i in 1:ncol(data3)){
  if(class(data3[,i]) %in% c('numeric','integer')){
    data3[,i]=((data3[,i]-min(data3[,i]))/((max(data3[,i])-min(data3[,i]))))
  }
}
View(data3)
data3=cbind(data3,data_for_dummies)
cor(data3$Weight,data3$Horsepower)
cov(data3$Weight,data3$Horsepower)
data3$Passengers=NULL
data3$Rear.seat.room=NULL
data3$Luggage.room=NULL

data3=cbind(data4_for_dummies,data3)
data5=data3[-c(1,27,30,33,38,41)]
View(data5)
View(data2)
data5$predicted_Price=NULL

names(data5)
fit= lm(formula = horsepower~., data = train)
summary(fit)

nrow(data5)
rows=1:nrow(data5)
set.seed(5000)
train_rows=sample(rows,60)
test_rows=rows[-train_rows]
train=data5[train_rows,]
test=data5[test_rows,]

preds=predict(fit,test)
hist(preds)
plot(preds)

View(Cars93)
distmat1=dist(Cars93)
hireclust=hclust(distmat1)
plot(hireclust)
rect.hclust(hireclust,k=,border='red')
