smooth.total<-lowess(total, f=10/length(total))
plot(total)
lines(smooth.total, col="red")


find.min<-function(total){
if(total[1+1]<total[1]) slope<- -1 else slope<-1
minimas<-NULL

for(i in 2:length(total)){
	if(total[i]<total[i-1] & slope==1){ slope<- -1 } else if(total[i]>=total[i-1] & slope==-1){
		slope<-1
		minimas<-rbind(minimas, i-1)
	}
}
abline(v=minimas)
}

find.min(smooth.total$y)