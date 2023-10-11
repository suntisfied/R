rm(list=ls()) # remove all lists existing before

dataset.bio<-read.table("HMD_Data_bio.txt", header=T, sep="\t", na.strings = NaN)

attach(dataset.bio)

# DVs = HR, LF, HF, HRV, SD_RRI, NSCR, RespRate, RespAmp
aov.out<-aov(dataset.bio$HR ~ (Iv1*Iv2*Iv3) + Error(Subj/(Iv1*Iv2*Iv3)), dataset.bio)
summary(aov.out)

model.tables(aov.out)