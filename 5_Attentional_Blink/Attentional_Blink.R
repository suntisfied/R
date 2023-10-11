#===========================================================
# Default Setting===========================================
#===========================================================
rm(list=ls()) # remove all lists existing before
root<-getwd() # get current directory
setwd(root) # set current directory

# install required packasges if they didn't been installed
list.of.required.packages <- c("ggplot2", "doBy", "plotrix", "nlme", "lme4", "car", "afex", "ez", "multcomp", "reshape2")
new.packages <- list.of.required.packages[!(list.of.required.packages %in% installed.packages()[,"Package"])]
# function to discriminate odd or even / result: true or false
is.odd <- function(x) x %% 2 != 0
is.even <- function(x) x %% 2 == 0

# function return input number to set unit
returnNumber <- function(inputNumber, returnUnit){
  result<-inputNumber - (floor(inputNumber/returnUnit)*returnUnit)
  return(result)
}

#=====================
# Key Parameters======
#=====================


#======================================================
# Read Data Text File Whether It Is .txt or .xlsx======
#======================================================
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

# select data which is correct in detecting first target
# ds <- ds[ds$CorrectOrNotInFirst == 1 | ds$AttentionToFirstTarget==1,]

# library(reshape2)
# tmp.ds <- data.frame()
# id.for.long <- names(ds)
# id.for.long <- id.for.long[-which(id.for.long %in% c('ResponseToFirstTarget', 'ResponseToSecondTarget', 'CorrectOrNotInFirst', 'CorrectOrNotSeond', 'ResponseTimeToFirst', 'ResponseTimeToSecond'))]
# tmp.ds.1 <- ds[,c(id.for.long, "ResponseToFirstTarget", "ResponseToSecondTarget")]
# tmp.ds.1 <- melt(tmp.ds.1, id.vars = id.for.long, variable.name = "ResponseToWhichTarget", value.name = "PressedKey")
# tmp.ds.2 <- ds[,c(id.for.long, "CorrectOrNotInFirst", "CorrectOrNotSeond")]
# tmp.ds.2 <- melt(tmp.ds.1, id.vars = id.for.long, variable.name = "ResponseToWhichTarget", value.name = "CorrectOrNot")

# ds[ds$AttentionToFirstTarget==0,]$CorrectOrNotInFirst
# ds[ds$AttentionToFirstTarget==0,]$CorrectOrNotSeond
# ds[ds$AttentionToFirstTarget==1,]$CorrectOrNotSeond

# averaging by blocks
library(doBy)
ds.m.block <- summaryBy(data = ds, CorrectOrNotInFirst + CorrectOrNotSeond  ~ sbj + AttentionToFirstTarget + NumberOfSecondDistractorInterval + SOAList, FUN = c(mean))

library(reshape2)
ds.m.block <- melt(ds.m.block, id.vars = c('sbj', 'AttentionToFirstTarget', 'NumberOfSecondDistractorInterval', 'SOAList'), variable.name = "ResponseToWhichTarget", value.name = "res")
names(ds.m.block) <- c('sbj', 'AttentionToFirstTarget', 'NumberOfSecondDistractorInterval', 'SOAList', 'ResponseToWhichTarget', 'res')
ds.m.block$ResponseToWhichTarget <- as.character(ds.m.block$ResponseToWhichTarget)
ds.m.block[ds.m.block$ResponseToWhichTarget=='CorrectOrNotInFirst.mean',]$ResponseToWhichTarget <- 'First'
ds.m.block[ds.m.block$ResponseToWhichTarget=='CorrectOrNotSeond.mean',]$ResponseToWhichTarget <- 'Second'

ds.m.block$Condition <- -1
ds.m.block[ds.m.block$AttentionToFirstTarget == 0 & ds.m.block$ResponseToWhichTarget == 'First',]$Condition <- 'First'
ds.m.block[ds.m.block$AttentionToFirstTarget == 0 & ds.m.block$ResponseToWhichTarget == 'Second',]$Condition <- 'Second'
ds.m.block[ds.m.block$AttentionToFirstTarget == 1 & ds.m.block$ResponseToWhichTarget == 'Second',]$Condition <- 'Second Only'

# averaging according to condition by merged trials
library(doBy)
library(plotrix)
ds.m <- summaryBy(data = ds.m.block, res ~ Condition + NumberOfSecondDistractorInterval + SOAList, FUN = c(mean, std.error))
names(ds.m) <- c('Condition', 'NumberOfSecondDistractorInterval', 'SOAList','res.m', 'res.std')

ds.m <- ds.m[ds.m$Condition != -1,]

# # string value names to character
# ds.m[ds.m$AttentionToFirstTarget==0,]$AttentionToFirstTarget <- 'yes'
# ds.m[ds.m$AttentionToFirstTarget==1,]$AttentionToFirstTarget <- 'no'

ds.m$NumberOfSecondDistractorInterval <- as.character(ds.m$NumberOfSecondDistractorInterval)
ds.m$SOAList <- as.character(ds.m$SOAList)

# colors=c("blue", "red", "green", "black")

# draw graph
library(ggplot2)
graph <- ggplot(data = ds.m, aes(x=NumberOfSecondDistractorInterval, y=res.m, group=Condition, color=Condition)) +
  geom_line(size = 1.5, aes(color=Condition)) + geom_point(size = 3, shape=21, fill = "white", aes(color=Condition)) +
  geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), width=.2, size=1.2) +
  facet_grid(. ~ SOAList, scales="fixed") +
  ylim(0,1) +
  xlab("Lag") +
  ylab("Accuracy Rate")
  # scale_y_continuous(breaks = round(seq(0, max(ds.m$rt.m) + max(ds.m$rt.std), by = 0.1), 1))+
  # theme(legend.position="none")
  # theme(axis.title.y = element_text(angle = 90, vjust=-1)) +
  # theme(axis.title.x = element_text(color="black", face="bold", size=20, margin = margin(t=15, r=10, b = 10, l=10))) +
  # theme(axis.title.y = element_text(color="black", face="bold", size=20, margin = margin(t=10, r=10, b = 0, l=0))) +
  # theme(axis.text = element_text(color="black", face="bold", size=15))
graph
