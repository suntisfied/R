rm(list=ls()) # remove all lists existing before

root<-getwd()
setwd(root) # set current directory

FileName<-"HMD_Data" # data file's name
nSubj<-10 # the number of subjects

# Data preparation
SubjVec<-c(paste(1:nSubj))
dataset=NULL
for (ThisSub in 1:nSubj){
  datasetFileName<-paste(FileName, ThisSub, ".txt", sep="")
  temdataset<-read.table(datasetFileName, header=T, sep="\t", na.strings = NaN)
  dataset<-rbind(dataset, temdataset)
}

# write.table(dataset, file="HMD_dataFull_hehav", sep="\t", row.names=F, col.names=T)

# DVs = Correct, RT_Arrow, RT_Scene, HowtoFeel, Discomfort
DV<-dataset$Correct

# Interation plots IVs
IV_Xaxis<-Iv1
IV_Group<-Iv3

attach(dataset)
dataset$Iv1<-as.factor(dataset$Iv1)
dataset$Iv2<-as.factor(dataset$Iv2)
dataset$Iv3<-as.factor(dataset$Iv3)
dataset$Subj<-as.factor(dataset$Sub)

# Descriptive statistics
library("psych")
des.out<-describeBy(x=DV, group=list(Iv1, Iv2, Iv3), mat=T)

# Inferential statistics
aov.out<-aov(DV ~ (Iv1*Iv2*Iv3) + Error(Subj/Iv1*Iv2*Iv3), des.out, na.rm=T)
summary(aov.out)

library("sciplot")
lineplot.CI(IV_Xaxis, DV, group=IV_Group, data=des.out, cex=1, xlab="IVs", ylab="DVs", cex.lab=1.5, x.leg=1, col=c("black","blue","red","green"), pch=c(16,16,16,16))


# repeated ANOVA
#dataset.ANOVA<-melt(dataset, id=c("Iv1", "Iv2", "Iv3"), measure="RT_Arrow", variable.name="aa", value.name="RT")

# Result.ANOVA<-ezANOVA(dataset, dv=.(RT_Arrow), wid=.(Subj), within = .(Iv1, Iv2, Iv3), detailed=T, type=3)
