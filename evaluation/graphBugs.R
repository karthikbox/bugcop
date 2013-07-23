thedata <- read.csv("/home/karthik/r/bugcop/evaluation/equinox.csv" , header=F, sep=",")
percentiles=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)*100
i=3
plotActual=plot(percentiles,
                thedata[,i]*100,type="b",xlab="percentile",ylab="faults",col="red",
                xlim=c(5,100),ylim=c(5,100), main="% of faults vs percentile of files EQUINOX",axes=F)
axis(side=1, at=seq(5,100,by=5))
axis(side=2, at=seq(0, 100, by=10))
box()
lines(percentiles,thedata[,i+2]*100,type="b",col="blue",lwd=2.5)
lines(percentiles,thedata[,i+4]*100,type="b",col="black",lwd=2.5)
lines(percentiles,thedata[,i+6]*100,type="b",col="orange",lwd=2.5)
lines(percentiles,thedata[,i+8]*100,type="b",col="green",lwd=2.5)
lines(percentiles,thedata[,i+10]*100,type="b",col="maroon",lwd=2.5)

legend( "bottomright",c("actual","ck+oo","loc","ck","oo","dep"),
       
       lty=c(1,1),
       
       ,col=c("red","blue","black","orange","green","maroon"))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
      equilogs = F)