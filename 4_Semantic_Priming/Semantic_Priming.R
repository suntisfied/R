rm(list=ls()) # remove all lists existing before

# install required packasges if they didn't been installed
list.of.required.packages <- c("ggplot2", "doBy", "plotrix", "nlme", "lme4", "car", "afex", "ez", "multcomp")
new.packages <- list.of.required.packages[!(list.of.required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)

# function to discriminate odd or even
is.odd <- function(x) x %% 2 != 0
is.even <- function(x) x %% 2 == 0

root<-getwd()
setwd(root) # set current directory

#-data loading starts--------------------------------------------------------
dir<-'data/' # directory

# combinding data
ds=NULL;
for (this in 1:length(list.files(dir))){
  if (grepl(".txt", list.files(dir)[this])==TRUE){ # selet data having name of .txt
    filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
    tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
    ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
  } else if (grepl(".xlsx", list.files(dir)[this])==TRUE){ # selet data having name of .xlsx
    library(xlsx) # for read xlsx directly, need java
    filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
    tmp.ds<-read.xlsx(filename, sheetIndex=1) # filename to data.frame
    ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
  }
}
write.table(ds, file="CombindingAll.txt", sep='\t', row.names = FALSE)
ds$sbj<-ds$group*10 + ds$sbj # combine the index of group & subject number

# select data of only correct response
ds<-ds[ds$CorrectOrNot.0.wrong..1.correct.==1,]

# calculate mean & standard error according to condtions
ds.m.block<-aggregate(ds$ReseponseTime.unit.second., list(sbj=ds$sbj, condition=ds$AssociationTypeIdx.1.high.association..2.low.association..3.non.association..4.non.word.), mean)
names(ds.m.block)[names(ds.m.block)=="x"]<-"rt.m"

# library(reshape2) # for long format to wide (for spss)
# ds.wide <- dcast(ds.m.block, sbj ~ condition, value.var="rt.m")
# write.table(ds.wide, file="CombindingAll_wideFormat.txt", sep='\t', row.names = FALSE)

ds.m.condi<-aggregate(ds.m.block$rt.m, list(condition=ds.m.block$condition), mean)
names(ds.m.condi)[names(ds.m.condi)=="x"]<-"rt.m"

library(plotrix) # package for calculating standard error
ds.m.condi$rt.std<-aggregate(ds.m.block$rt.m, list(condition=ds.m.block$condition), std.error)$x

ds.m <- ds.m.condi

# string value names to character
ds.m[ds.m$condition==1,]$condition<-"high"
ds.m[ds.m$condition==2,]$condition<-"low"
ds.m[ds.m$condition==3,]$condition<-"non"
ds.m[ds.m$condition==4,]$condition<-"non-word"

colors=c("blue", "red", "green", "black")

# draw graph
library(ggplot2)
graph <- ggplot(data = ds.m, aes(x=condition, y=rt.m, color=condition)) +
  geom_bar(fill="white", stat="identity", position=position_dodge(), size=1.5) +
  geom_errorbar(aes(ymin=rt.m-rt.std, ymax=rt.m+rt.std), width=.2, position=position_dodge(.9), size=1.5) +
  xlab("Association Conditions") +
  ylab("Response Time (s)") +
  scale_y_continuous(breaks = round(seq(0, max(ds.m$rt.m) + max(ds.m$rt.std), by = 0.1), 1))+
  theme(legend.position="none") +
  theme(axis.title.y = element_text(angle = 90, vjust=-1)) +
  theme(axis.title.x = element_text(color="black", face="bold", size=20, margin = margin(t=15, r=10, b = 10, l=10))) +
  theme(axis.title.y = element_text(color="black", face="bold", size=20, margin = margin(t=10, r=10, b = 0, l=0))) +
  theme(axis.text = element_text(color="black", face="bold", size=15))
graph

# ANOVA
ds.aov<-aggregate(ds$ReseponseTime.unit.second., by = list(ds$sbj, ds$AssociationTypeIdx.1.high.association..2.low.association..3.non.association..4.non.word.), FUN = "mean")
colnames(ds.aov) <- c("subj", "condition", "rt")

# make values as factor (important part! if undone, result changes)
ds.aov$subj<-factor(ds.aov$subj)
ds.aov$condition<-factor(ds.aov$condition)

# using fuction aov (basic function in R, only type 1 error)
rs.aov.by.aov <- aov(rt ~ condition
                     + Error(subj/(condition)),
                     data = ds.aov)
summary(rs.aov.by.aov)

# using fuction ezAOVA (ezAOVA)
library(ez)
rs.aov.by.ezANOVA <- ezANOVA(data=ds.aov,
                  dv=.(rt),
                  wid.(subj),
                  within=.(condition),
                  detailed=TRUE, type=1)

# using fuction Anova (car)
library(car)
library(afex) # make easy to use function from library car
rs.aov.by.car<-aov_car(rt ~ condition
                + Error(subj/(condition)),
                data = ds.aov, type=3)
summary(rs.aov.by.car)

# post hoc analysis; method Tukey HSD, repeated measure
library(nlme)
rs.postHoc <- lme(rt ~ condition, data=ds.aov, random = ~1|subj)

library(multcomp) # library for post hoc test
summary(glht(rs.postHoc,linfct=mcp(condition="Tukey")))
summary(glht(rs.postHoc,linfct=mcp(condition="Tukey")), test = adjusted("bonferroni")) # options: “single-step”, “Shaffer”, “Westfall”, “free”, “holm”, “hochberg”, “hommel”, “bonferroni”, “BH”, “BY”, “fdr”, “none”

# post hoc analysis; method Tukey HSD, treated as between-subject design
TukeyHSD(aov(rt ~ condition, data = ds.aov))