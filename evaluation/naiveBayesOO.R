thedata <- read.csv("/home/karthik/r/Equinox-CK-OO-DEP2.csv" , header=T, sep=",")
thedata1=thedata
#library("e1071", lib.loc="/home/karthik/R/i686-pc-linux-gnu-library/2.14")
set.seed(98052)
N<-50
thedata[,12]=thedata[,12]>0 #change--bugs t/f
class1<-summary(thedata$bugs)
nn<-as.numeric(class1[2])
np<-as.numeric(class1[3])
n<-np+nn

# recallPossitive <- TP / (TP + FN)
# recallNegative<- TN / (TN + FP)
# recall <- (np/n)*recallPossitive + (nn/n)*recallNegative

precision<-rep(NA,N)
recall<-rep(NA,N)
accuracy<-rep(NA,N)

len=length(thedata[,1])


##individual components
recPos=rep(NA,N)
recNeg=rep(NA,N)
precPos=rep(NA,N)
precNeg=rep(NA,N)
##end


bugFractionSamples=rep(NA,N)
linesFractionSamples=rep(NA,N)
filesFractionSamples=rep(NA,N)
# for(i in 1:len){
#   if(thedata[i,18]==TRUE){
#     thedata[i,18]="YES"
#   }
#   else{
#     thedata[i,18]="NO"
#   }
# } 


for ( i in 1:N){ 
  print(i)
  idxs <- sample(1:nrow(thedata), nrow(thedata)*2/3)
  train = thedata[ idxs ,]
  test = thedata[-idxs,]
  
  
  
  fit=naiveBayes(as.factor(bugs) ~ . , data = train)
  #fit=naiveBayes(train[,1:17],train[,18])
  #pred1=predict(fit,test[-18],type="raw")
  pred=predict(fit,test[-12]) #change bug column
  finaldata=data.frame(class=pred,bugs=thedata1[-idxs,12]) #change bug column
  finaldata=data.frame(finaldata,loc=thedata1[-idxs,5])#change loc coulmn
  bugTotal=sum(finaldata[,2])
  linesTotal=sum(finaldata[,3])
  sum=0
  lines=0
  files=0
  for(j in 1:length(finaldata[,1])){
    #print(j)
    if(finaldata[j,1]==TRUE){
      sum=sum+finaldata[j,2]
      lines=lines+finaldata[j,3]
      files=files+1
    }
  }
  filesFractionSamples[i]=files/length(test[,12]) #change bug column
  bugFractionSamples[i]=sum/bugTotal
  linesFractionSamples[i]=lines/linesTotal
  outcome<-table(true=thedata[-idxs,12],pred=pred) #change bug column
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
  
  ##individual components
  recPos[i]=recallPossitive
  recNeg[i]=recallNegative
  precPos[i]=precisionPossitive
  precNeg[i]=precisionNegative
  ##end
  
  accuracy <- (TP + TN) / (TN + FN + FP + TP)
}

##individual components
recPosMean=round(mean(recPos),4)
recNegMean=round(mean(recNeg),4)
precPosMean=round(mean(precPos),4)
precNegMean=round(mean(precNeg),4)
#
filesFraction=round(mean(filesFractionSamples),4)
bugFraction=round(mean(bugFractionSamples),4)
locFraction=round(mean(linesFractionSamples),4)
avg_precision=round(mean(precision),4)
avg_recall=round(mean(recall),4)
avg_accuracy=round(mean(accuracy),4)


#print(avg_accuracy)



precPosMean
precNegMean
print(avg_precision)

recPosMean
recNegMean
print(avg_recall)

print(filesFraction*100)
print(bugFraction*100)
#print(locFraction)


# n
# np
# nn
