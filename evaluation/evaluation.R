thedata <- read.csv("/home/karthik/r/EclipseJDT-CK-OO-DEP.csv" , header=T, sep=",")
set.seed(98052)
x<-as.data.frame(scale(thedata[,-18]))
y<-thedata[,18]
data <- data.frame(y=y,x)

idxs <- sample(1:nrow(data), nrow(data)*2/3, F)
#fit <- lm(y ~ ., data=data[idxs,])
fit<-lm(formula = y ~ cbo + fanIn + fanOut + numberOfAttributes+numberOfLinesOfCode+ numberOfMethods+numberOfPublicMethods+rfc+wmc,data = data[idxs, ])
#step(fit,direction="backward")
newdata1=data
newdata=newdata1[-idxs,]
prediction <- predict(fit, newdata)
#quantile(prediction,c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95) )

withPrediction=data.frame(pred=prediction,newdata)
names=as.numeric(row.names(withPrediction))
newdata2=data.frame(names=names,withPrediction)  
temp=data.frame(newdata2,loc=thedata[-idxs,9])
newdata2=temp
ftotal=sum(newdata2$y)
locTotal=sum(newdata2$loc)



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
percentiles=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)*100
k=1
percentileSumPredicted=rep(NA,20)
locSumPredicted=rep(NA,20)
sum=0
sumLines=0
for(i in percentiles){
  print(i)
  sum=0
  sumLines=0
  for(j in 1:333){
    if(finalData[j,1]>=i){
      sum=sum+finalData[j,4]
      sumLines=sumLines+finalData[j,22]
    }
  }
  percentileSumPredicted[k]=sum
  locSumPredicted[k]=sumLines
  k=k+1
}
predicted=percentileSumPredicted/ftotal
predictedLoc=locSumPredicted/locTotal

predicted
percentileSumPredicted
predictedLoc



orderedNewData3=newdata2[order(newdata2$y),]
finalData1=data.frame(percentileRanks=percentileRanksActual,orderedNewData3)

k=1
percentileSumActual=rep(NA,20)
locSumActual=rep(NA,20)
sumLines=0
sum=0
for(i in percentiles){
  sum=0
  sumLines=0
  for(j in 1:333){
    if(finalData1[j,1]>=i){
      sum=sum+finalData1[j,4]
      sumLines=sumLines+finalData1[j,22]
    }
  }
  percentileSumActual[k]=sum
  locSumActual[k]=sumLines
  k=k+1
}
actual=percentileSumActual/ftotal
actualLoc=locSumActual/locTotal
predicted
actual


orderedNewData4=newdata2[order(newdata2$numberOfLinesOfCode),]
finalData2=data.frame(percentileRanks=percentileRanksActual,orderedNewData4)
k=1
percentileSumLines=rep(NA,20)
locSumLines=rep(NA,20)
sumLines=0
sum=0
for(i in percentiles){
  sum=0
  sumLines=0
  for(j in 1:333){
    if(finalData2[j,1]>=i){
      sum=sum+finalData2[j,4]
      sumLines=sumLines+finalData2[j,22]
    }
  }
  percentileSumLines[k]=sum
  locSumLines[k]=sumLines
  k=k+1
}
lines=percentileSumLines/ftotal
linesLoc=locSumLines/locTotal


output=data.frame(100-percentiles,actualLoc,predictedLoc,linesLoc)
output=round(output,4)
write.table(output[,1:4], "out.csv", sep=",") 

output=data.frame(100-percentiles,actual,predicted,lines)
output=round(output,4)
write.table(output[,1:4], "out1.csv", sep=",") 

performance=rep(NA,20)
for(i in 1:20){
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
plotActual=plot(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),actual,type="b",xlab="percentile",ylab="faults",col="red",
                xlim=c(0,1),ylim=c(0,1))
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),predicted,type="b",col="blue")
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),lines,type="b",col="green")
plotPerformance=plot(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),performance,type="b",xlab="percentile",xlim=c(0,1),ylim=c(0,1))

#PLOTS END
