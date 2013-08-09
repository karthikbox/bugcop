thedata <- read.csv("/home/karthik/r/jdt3.csv" , header=T, sep=",")
set.seed(98052)
thedata1=thedata
for(i in 1:length(thedata[,5])){
  if(thedata[i,5]>0){
    thedata[i,5]=1
  }
  else thedata[i,5]=0
}

x<-as.data.frame(scale(thedata[,1:4]))
y<-thedata[,5]
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

len1=length(thedata[,5])
np=sum(thedata[,5])
nn=len1-np
ntot=np+nn

##individual components
recPos=rep(NA,N)
recNeg=rep(NA,N)
precPos=rep(NA,N)
precNeg=rep(NA,N)
##end

recall=rep(NA,N)
accuracy=rep(NA,N)
precision=rep(NA,N)


bugSum1=rep(NA,N)
locSum1=rep(NA,N)

for(run in 1:50){
  
  idxs <- sample(1:nrow(thedata), nrow(thedata)*2/3,F)
  len=length(data[-idxs,5])
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
  bugPresence=data[-idxs,1]==1
  #sorted=sort(pred,decreasing=F)
  #sorted=sorted*100
  
  #   newdata=data.frame(pred=pred,test)
  #   newdata2=newdata[order(newdata$pred),]
  #   newdata3=data.frame(percentile=percentileRanksActual,newdata2)
  #   newdata4=data.frame(newdata3,thedata1[-idxs,18])
  withPred=data.frame(pred=pred,data[-idxs,])
  withActualBugs=data.frame(actualBugs=thedata1[-idxs,5],withPred)
  withLoc=data.frame(loc=thedata1[-idxs,6],withActualBugs)
  
  print(run)
  print("prediction started")
  ##prediction sorted  
  sortedPrediction=withLoc[order(withLoc$pred),]
  
  withPercentile=data.frame(percentile=percentileRanksActual,sortedPrediction)
  finalData=withPercentile
  
  bugsTotal=sum(finalData$actualBugs)
  linesTotal=sum(finalData$loc)
  
  ###predicted bugs and loc fraction
  pred1=pred>=0.5
  bugPresence=data[-idxs,1]==1
  newdata1=data.frame(pred1=pred1,withLoc)
  newdata2=data.frame(bugPresence=bugPresence,newdata1)
  bugSum=0
  locSum=0
  for(i in 1:len){
    if(newdata2[i,2]==TRUE){
      #print(i)
      bugSum=newdata2[i,4]+bugSum
      locSum=newdata2[i,3]+locSum
    }
  }
  bugSum1[run]=bugSum/bugsTotal
  locSum1[run]=locSum/linesTotal
  
  ##end
  
  
  
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
  
  outcome<-table(true=bugPresence,pred=pred1)
  #print(outcome)
  TN = outcome[1,1]
  FN = outcome[2,1]
  FP = outcome[1,2]
  TP = outcome[2,2]
  #   
  #   
  #   
  precisionPossitive <- if (TP + FP == 0) { 1 } else { TP / (TP + FP) }
  precisionNegative<- if (TN + FN == 0){1} else {TN / (TN +FN)}
  precision[run] <- (np/ntot)*precisionPossitive + (nn/ntot)*precisionNegative
  
  recallPossitive <- TP / (TP + FN)
  recallNegative<- TN / (TN + FP)
  recall[run] <- (np/ntot)*recallPossitive + (nn/ntot)*recallNegative
  
  ##individual components
  recPos[run]=recallPossitive
  recNeg[run]=recallNegative
  precPos[run]=precisionPossitive
  precNeg[run]=precisionNegative
  ##end
  
  accuracy[run] <- (TP + TN) / (TN + FN + FP + TP)
  
  
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

rec=mean(recall)
acc=mean(accuracy)
prec=mean(precision)
rec=round(rec*100,4)
prec=round(prec*100,4)
acc=round(acc*100,4)

##predicted bugs and loc fraction
bugSum=round(mean(bugSum1),4)
locSum=round(mean(locSum1),4)

bugSum
locSum
##end
##individual components
recPosMean=round(mean(recPos),4)
recNegMean=round(mean(recNeg),4)
precPosMean=round(mean(precPos),4)
precNegMean=round(mean(precNeg),4)

recPosMean
recNegMean
precPosMean
precNegMean
ntot
np
nn
##end



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
