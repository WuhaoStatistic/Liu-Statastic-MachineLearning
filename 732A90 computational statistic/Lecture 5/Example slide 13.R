mouse<-read.csv("mouse.csv",sep=";",header=TRUE)
B=1000
stat=numeric(B)
n=dim(mouse)[1]
for(b in 1:B){
  Gb=sample(mouse$Group, n)
  stat[b]=mean(mouse$Value[Gb=='z'])-mean(mouse$Value[Gb=='y'])
}
stat0=mean(mouse$Value[mouse$Group=='z'])-mean(mouse$Value[mouse$Group=='y'])
print(c(stat0,mean(stat>stat0)))
## [1] 30.63492  0.12700

