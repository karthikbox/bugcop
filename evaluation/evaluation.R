thedata <- read.csv("/home/karthik/r/EclipseJDT-CK-OO-DEP.csv" , header=T, sep=",")
set.seed(98052)
x<-as.data.frame(scale(thedata[,-18]))
y<-thedata[,18]
data <- data.frame(y=y,x)

idxs <- sample(1:nrow(data), nrow(data)*2/3, F)
#fit <- lm(y ~ ., data=data[idxs,])
fit<-lm(formula = y ~ cbo + fanIn + fanOut + numberOfAttributes + 
          numberOfLinesOfCode + numberOfMethods + numberOfPublicMethods + 
          rfc + wmc, data = data[idxs, ])
#step(fit,direction="backward")
newdata1=data
newdata=newdata1[-idxs,]
prediction <- predict(fit, newdata)
#quantile(prediction,c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95) )

withPrediction=data.frame(pred=prediction,newdata)
names=as.numeric(row.names(withPrediction))
newdata2=data.frame(names=names,withPrediction)  

ftotal=sum(newdata$y)






n=333
percentileRanksPredicted=rep(NA,n)
percentileRanksActual=rep(NA,n)
for(i in 1:n){
  percentileRanksPredicted[i]=100*((i-0.5)/n)
}
percentileRanksPredicted
percentileRanksActual=percentileRanksPredicted
#percentileRanksActual and percentileRanksPredicted are same
ordered=sort(prediction,decreasing=F)
orderedNewData2=newdata2[order(newdata2$pred),]
finalData=data.frame(percentileRanks=percentileRanksActual,orderedNewData2)
percentiles=c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)*100

k=1
percentileSumPredicted=rep(NA,10)
sum=0
for(i in percentiles){
  sum=0
  for(j in 1:333){
    if(finalData[j,1]>=i){
      sum=sum+finalData[j,4]
    }
  }
  percentileSumPredicted[k]=sum
  k=k+1
}
predicted=percentileSumPredicted/ftotal



orderedNewData3=newdata2[order(newdata2$y),]
finalData1=data.frame(percentileRanks=percentileRanksActual,orderedNewData3)

k=1
percentileSumActual=rep(NA,10)
sum=0
for(i in percentiles){
  sum=0
  for(j in 1:333){
    if(finalData1[j,1]>=i){
      sum=sum+finalData1[j,4]
    }
  }
  percentileSumActual[k]=sum
  k=k+1
}
actual=percentileSumActual/ftotal

predicted
actual

performance=rep(NA,10)
for(i in 1:10){
  performance[i]=(predicted[i]/actual[i])
}

# count=0
# for(i in newdata$y){
#   if(i==0){
#     count=count+1
#   }
# }
# count
# (12*0.5+ 309)/333


#PLOTS BEGIN

#plot=plot(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),list(percentileSumPredicted,percentileSumActual),type="b",xlab="percentile")
# plotActual=plot(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),percentileSumActual,type="b",xlab="percentile")
# plotPredicted=plot(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),percentileSumPredicted,type="b",xlab="percentile")
plotActual=plot(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),actual,type="b",xlab="percentile",ylab="faults",col="red",
                xlim=c(0,0.5),ylim=c(0,1))
lines(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),predicted,type="b",col="blue")
plotPerformance=plot(1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),performance,type="b",xlab="percentile",xlim=c(0,0.5),ylim=c(0,1))

#PLOTS END
