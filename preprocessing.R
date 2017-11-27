###author: Liuqing Yang
tt=read.csv('data.csv',header=T)
tt=tt[,-33]
length(which(is.na(tt)=='T'))#check if there is missing value
#> length(which(is.na(tt)=='T'))
#[1] 0  
#there is no missing value

r1=length(which(tt$diagnosis=='M'))/length(tt$diagnosis)#ratio of malignant
r2=1-r1 #ratio of benign

#normalize data
nmlz=function(x){
  return((x-min(x))/(max(x)-min(x)))
}

tt_nmlz=apply(tt[,-c(1,2)],2,nmlz)
tt_nmlz=cbind.data.frame(tt[,c(1,2)],tt_nmlz)

write.csv(tt_nmlz,file = "data_nmlz.csv")#extract normalized data out.

