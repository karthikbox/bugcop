thedata <- read.csv("/home/karthik/r/lucene.csv" , header=T, sep=",")
set.seed(98052)
thedata1=thedata
for(i in 1:length(thedata[,18])){
  if(thedata[i,18]>0){
    thedata[i,18]=1
  }
  else thedata[i,18]=0
}

# class<-summary(thedata$bugs)
# nn<-as.numeric(class[2])
# np<-as.numeric(class[3])
# n<-np+nn


x<-as.data.frame(scale(thedata[,-18]))
y<-thedata[,18]
data <- data.frame(y=y,x)

# withBugs=data.frame(thedata,bugNumber=thedata1$bugs)
# withLines=data.frame(withBugs,loc=thedata1$numberOfLinesOfCode)


percentiles=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)*100
N=50
actualBugArray=matrix(data=NA,nrow=20,ncol=N)
actualLinesArray=matrix(data=NA,nrow=20,ncol=N)
predictedBugArray=matrix(data=NA,nrow=20,ncol=N)
predictedLinesArray=matrix(data=NA,nrow=20,ncol=N)
locBugArray=matrix(data=NA,nrow=20,ncol=N)
locLinesArray=matrix(data=NA,nrow=20,ncol=N)

for(run in 1:50){

  idxs <- sample(1:nrow(thedata), nrow(thedata)*2/3,F)
  len=length(data[-idxs,18])
  n=len
  percentileRanksPredicted=rep(NA,n)
  percentileRanksActual=rep(NA,n)
  for(i in 1:n){
    percentileRanksPredicted[i]=100*((i-0.5)/n)
  }
  #percentileRanksPredicted
  percentileRanksActual=percentileRanksPredicted
  
  model <- glm(y ~ ., data = data[idxs,], family="binomial")
  
  pred<-predict(model, data[-idxs,-1],type="response")
  pred1=pred>=0.5
  #sorted=sort(pred,decreasing=F)
  #sorted=sorted*100
  
#   newdata=data.frame(pred=pred,test)
#   newdata2=newdata[order(newdata$pred),]
#   newdata3=data.frame(percentile=percentileRanksActual,newdata2)
#   newdata4=data.frame(newdata3,thedata1[-idxs,18])
  withPred=data.frame(pred=pred,data[-idxs,])
  withActualBugs=data.frame(actualBugs=thedata1[-idxs,18],withPred)
  withLoc=data.frame(loc=thedata1[-idxs,9],withActualBugs)

  print(run)
  print("prediction started")
  ##prediction sorted  
  sortedPrediction=withLoc[order(withLoc$pred),]
  
  withPercentile=data.frame(percentile=percentileRanksActual,sortedPrediction)
  finalData=withPercentile
  
  bugsTotal=sum(finalData$actualBugs)
  linesTotal=sum(finalData$loc)
  
  logitSum=rep(NA,20)
  logitLines=rep(NA,20)
  sum=0
  lines=0
  k=1
  for(i in percentiles){
    sum=0
    lines=0
    for(j in 1:len){
      if(finalData[j,1]>=i){
        sum=sum+finalData[j,3]
        lines=lines+finalData[j,2]
      }
    }
    logitSum[k]=sum
    logitLines[k]=lines
    k=k+1
  }
  #logitSum
  #logitLines
  logitSumFraction=logitSum/bugsTotal
  logitLinesFraction=logitLines/linesTotal
  predictedBugArray[,run]=logitSumFraction
  predictedLinesArray[,run]=logitLinesFraction
  
  
  print("actual started")
  ##actual
  
  sortedPrediction=withLoc[order(withLoc$actualBugs),]
  
  withPercentile=data.frame(percentile=percentileRanksActual,sortedPrediction)
  finalData=withPercentile
  
  logitSum=rep(NA,20)
  logitLines=rep(NA,20)
  sum=0
  lines=0
  k=1
  for(i in percentiles){
    sum=0
    lines=0
    for(j in 1:len){
      if(finalData[j,1]>=i){
        sum=sum+finalData[j,3]
        lines=lines+finalData[j,2]
      }
    }
    logitSum[k]=sum
    logitLines[k]=lines
    k=k+1
  }
#   logitSum
#   logitLines
  logitSumFraction=logitSum/bugsTotal
  logitLinesFraction=logitLines/linesTotal
  actualBugArray[,run]=logitSumFraction
  actualLinesArray[,run]=logitLinesFraction
  
  
  print("loc started")
  #loc ordering
  sortedPrediction=withLoc[order(withLoc$loc),]
  
  withPercentile=data.frame(percentile=percentileRanksActual,sortedPrediction)
  finalData=withPercentile
  logitSum=rep(NA,20)
  logitLines=rep(NA,20)
  sum=0
  lines=0
  k=1
  for(i in percentiles){
    sum=0
    lines=0
    for(j in 1:len){
      if(finalData[j,1]>=i){
        sum=sum+finalData[j,3]
        lines=lines+finalData[j,2]
      }
    }
    logitSum[k]=sum
    logitLines[k]=lines
    k=k+1
  }
#   logitSum
#   logitLines
  logitSumFraction=logitSum/bugsTotal
  logitLinesFraction=logitLines/linesTotal
  locBugArray[,run]=logitSumFraction
  locLinesArray[,run]=logitLinesFraction
  
  
  outcome<-table(true=thedata[-idxs,18],pred=pred)
  #print(outcome)
  TN = outcome[1,1]
  FN = outcome[2,1]
  FP = outcome[1,2]
  TP = outcome[2,2]
  
  
  
  precisionPossitive <- if (TP + FP == 0) { 1 } else { TP / (TP + FP) }
  precisionNegative<- if (TN + FN == 0){1} else {TN / (TN +FN)}
  precision[i] <- (np/n)*precisionPossitive + (nn/n)*precisionNegative
  
  recallPossitive <- TP / (TP + FN)
  recallNegative<- TN / (TN + FP)
  recall[i] <- (np/n)*recallPossitive + (nn/n)*recallNegative
  
  accuracy <- (TP + TN) / (TN + FN + FP + TP)
  
}
actual=rep(NA,20)
actualLoc=rep(NA,20)
predicted=rep(NA,20)
predictedLoc=rep(NA,20)
lines=rep(NA,20)
linesLoc=rep(NA,20)
for(i in 1:20){
  actual[i]=mean(actualBugArray[i,])
  actualLoc[i]=mean(actualLinesArray[i,])
  predicted[i]=mean(predictedBugArray[i,])
  predictedLoc[i]=mean(predictedLinesArray[i,])
  lines[i]=mean(locBugArray[i,])
  linesLoc[i]=mean(locLinesArray[i,])
}
output=matrix(data=NA,nrow=20,ncol=3)
output1=matrix(data=NA,nrow=20,ncol=3)

output[,1]=rev(actual)
output[,2]=rev(predicted)
output[,3]=rev(lines)
output1[,1]=rev(actualLoc)
output1[,2]=rev(predictedLoc)
output1[,3]=rev(linesLoc)

output=round(output,4)
write.table(output, "out.csv", sep=",") 

output1=round(output1,4)
write.table(output1, "out1.csv", sep=",") 

plotActual=plot(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),actual,type="b",xlab="percentile",ylab="faults",col="red",
                xlim=c(0,1),ylim=c(0,1))
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),predicted,type="b",col="blue")
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),lines,type="b",col="green")
plotActual=plot(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),actualLoc,type="b",xlab="percentile",ylab="faults",col="red",
                xlim=c(0,1),ylim=c(0,1))
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),predictedLoc,type="b",col="blue")
lines(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),linesLoc,type="b",col="green")
# plotPerformance=plot(1-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),performance,type="b",xlab="percentile",xlim=c(0,1),ylim=c(0,1))
# 
# #PLOTS END
