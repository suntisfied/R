rm(list=ls()) # remove all lists existing before

# function to discriminate odd or even
is.odd <- function(x) x %% 2 != 0
is.even <- function(x) x %% 2 == 0

# subset(ds, is.odd(ds$subj)==TRUE)
# subset(ds, is.even(ds$subj)==TRUE)

root<-getwd()
setwd(root) # set current directory

#-data loading starts--------------------------------------------------------
dir<-'data/' # directory
ds=NULL;
for (this in 1:length(list.files(dir))){
  filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
  tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NaN) # filename to data.frame
  
  # labeling of response type
  if (agrepl("Familiarity", filename)==TRUE){ # string matching discrimination
    tmp.ds$restype<-"Familiarity" # add new column
  }else if (agrepl("Horrifying", filename)==TRUE){
    tmp.ds$restype<-"Horrifying"
  }else if (agrepl("Human_Likeness", filename)==TRUE){
    tmp.ds$restype<-"Human_Likeness"
  }
  
  ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
}
#-data loading ends--------------------------------------------------------

#-manipulation of dataset for analysis starts--------------------------------------------------------
ds$img<-paste(ds$imageType, ds$imageNum, sep="") # concetenate strings to one
ds$img<-as.factor(ds$img)
ds$imgsizevh<-paste(ds$imageSizeV, "x", ds$imageSizeH)
ds$imgsize<-ds$imageSizeV * ds$imageSizeH

tmp<-subset(ds, ds$subj==1)
ds.imgsizevh<-tmp[order(tmp$img),]$imgsizevh
ds.imgsize<-tmp[order(tmp$img),]$imgsize
ds.imgsizeh<-tmp[order(tmp$img),]$imageSizeH
ds.imgsizev<-tmp[order(tmp$img),]$imageSizeV

ds.fam<-subset(ds, ds$restype=="Familiarity")
ds.hor<-subset(ds, ds$restype=="Horrifying")
ds.hum<-subset(ds, ds$restype=="Human_Likeness")
#-manipulation of dataset for analysis ends--------------------------------------------------------

#-ready for reliability among participants starts--------------------------------------------------------
s2.hor<-subset(ds, ds$subj==2 & ds$restype=="Horrifying")
s2.hum<-subset(ds, ds$subj==2 & ds$restype=="Human_Likeness")

setA.img<-levels(factor(s2.hor$img))
setB.img<-levels(factor(s2.hum$img))

# RR9: setA, RR70: setB
subj.setA.hor<-subset(ds, ds$img=="RR9" & ds$restype=="Horrifying")$subj
subj.setB.hor<-subset(ds, ds$img=="RR70" & ds$restype=="Horrifying")$subj
subj.setA.fam<-subset(ds, ds$img=="RR9" & ds$restype=="Familiarity")$subj
subj.setB.fam<-subset(ds, ds$img=="RR70" & ds$restype=="Familiarity")$subj
subj.setA.hum<-subset(ds, ds$img=="RR9" & ds$restype=="Human_Likeness")$subj
subj.setB.hum<-subset(ds, ds$img=="RR70" & ds$restype=="Human_Likeness")$subj

setA.hor<-subset(ds, ds$subj %in% subj.setA.hor & ds$restype=="Horrifying")
setB.hor<-subset(ds, ds$subj %in% subj.setB.hor & ds$restype=="Horrifying")
setA.fam<-subset(ds, ds$subj %in% subj.setA.fam & ds$restype=="Familiarity")
setB.fam<-subset(ds, ds$subj %in% subj.setB.fam & ds$restype=="Familiarity")
setA.hum<-subset(ds, ds$subj %in% subj.setA.hum & ds$restype=="Human_Likeness")
setB.hum<-subset(ds, ds$subj %in% subj.setB.hum & ds$restype=="Human_Likeness")

library(reshape2)
ds.emo<-melt(ds[,9:141], id.vars = NULL)

setA.fam.tmp<-setA.fam[,c("subj","res", "img")]
setB.fam.tmp<-setB.fam[,c("subj","res", "img")]
setA.hor.tmp<-setA.hor[,c("subj","res", "img")]
setB.hor.tmp<-setB.hor[,c("subj","res", "img")]
setA.hum.tmp<-setA.hum[,c("subj","res", "img")]
setB.hum.tmp<-setB.hum[,c("subj","res", "img")]

setA.fam.wide<-reshape(setA.fam.tmp, timevar = "subj", idvar = c("img"), direction = "wide")
setB.fam.wide<-reshape(setB.fam.tmp, timevar = "subj", idvar = c("img"), direction = "wide")
setA.hor.wide<-reshape(setA.hor.tmp, timevar = "subj", idvar = c("img"), direction = "wide")
setB.hor.wide<-reshape(setB.hor.tmp, timevar = "subj", idvar = c("img"), direction = "wide")
setA.hum.wide<-reshape(setA.hum.tmp, timevar = "subj", idvar = c("img"), direction = "wide")
setB.hum.wide<-reshape(setB.hum.tmp, timevar = "subj", idvar = c("img"), direction = "wide")

setA.fam.cor<-cor(setA.fam.wide[,2:ncol(setA.fam.wide)])
setB.fam.cor<-cor(setB.fam.wide[,2:ncol(setB.fam.wide)])
setA.hor.cor<-cor(setA.hor.wide[,2:ncol(setA.hor.wide)])
setB.hor.cor<-cor(setB.hor.wide[,2:ncol(setB.hor.wide)])
setA.hum.cor<-cor(setA.hum.wide[,2:ncol(setA.hum.wide)])
setB.hum.cor<-cor(setB.hum.wide[,2:ncol(setB.hum.wide)])

setA.fam.alpha<-alpha(setA.fam.cor)
capture.output(print(setA.fam.alpha, prmsd=TRUE, digits=2), file = paste("setA.fam.alpha",".txt",sep="")) # output the result

setB.fam.alpha<-alpha(setB.fam.cor)
capture.output(print(setB.fam.alpha, prmsd=TRUE, digits=2), file = paste("setB.fam.alpha",".txt",sep="")) # output the result

setA.hor.alpha<-alpha(setA.hor.cor)
capture.output(print(setA.hor.alpha, prmsd=TRUE, digits=2), file = paste("setA.hor.alpha",".txt",sep="")) # output the result

setB.hor.alpha<-alpha(setB.hor.cor)
capture.output(print(setB.hor.alpha, prmsd=TRUE, digits=2), file = paste("setB.hor.alpha",".txt",sep="")) # output the result

setA.hum.alpha<-alpha(setA.hum.cor)
capture.output(print(setA.hum.alpha, prmsd=TRUE, digits=2), file = paste("setA.hum.alpha",".txt",sep="")) # output the result

setB.hum.alpha<-alpha(setB.hum.cor)
capture.output(print(setB.hum.alpha, prmsd=TRUE, digits=2), file = paste("setB.hum.alpha",".txt",sep="")) # output the result

# cor.setAB.hor<-cor.test(setA.hor$res, setB.hor$res)
# cor.setAB.fam<-cor.test(setA.fam$res, setB.fam$res)
# cor.setAB.hum<-cor.test(setA.hum$res, setB.hum$res)
#-ready for reliability among participants ends--------------------------------------------------------

#-standardization starts--------------------------------------------------------
# # Z-transform (total)
# ds.fam$res<-(ds.fam$res - mean(subset(ds, ds$restype=="Familiarity", select = res)$res)) / sd(subset(ds, ds$restype=="Familiarity", select = res)$res)
# ds.hor$res<-(ds.hor$res - mean(subset(ds, ds$restype=="Horrifying", select = res)$res)) / sd(subset(ds, ds$restype=="Horrifying", select = res)$res)
# ds.hum$res<-(ds.hum$res - mean(subset(ds, ds$restype=="Human_Likeness", select = res)$res)) / sd(subset(ds, ds$restype=="Human_Likeness", select = res)$res)

# Z-transform ver2 (among participants)
for (i.fam in levels(factor(ds.fam$subj))){
  tmp.fam<-c(mean(subset(ds, ds$restype=="Familiarity" & ds$subj==i.fam, select = "res")$res), sd((subset(ds, ds$restype=="Familiarity" & ds$subj==i.fam, select = "res")$res)))
  ds.fam[ds.fam$subj==i.fam,]$res<-(ds.fam[ds.fam$subj==i.fam,]$res - tmp.fam[1]) / tmp.fam[2]
}
for (i.hor in levels(factor(ds.hor$subj))){
  tmp.hor<-c(mean(subset(ds, ds$restype=="Horrifying" & ds$subj==i.hor, select = "res")$res), sd((subset(ds, ds$restype=="Horrifying" & ds$subj==i.hor, select = "res")$res)))
  ds.hor[ds.hor$subj==i.hor,]$res<-(ds.hor[ds.hor$subj==i.hor,]$res - tmp.hor[1]) / tmp.hor[2]
}
for (i.hum in levels(factor(ds.hum$subj))){
  tmp.hum<-c(mean(subset(ds, ds$restype=="Human_Likeness" & ds$subj==i.hum, select = "res")$res), sd((subset(ds, ds$restype=="Human_Likeness" & ds$subj==i.hum, select = "res")$res)))
  ds.hum[ds.hum$subj==i.hum,]$res<-(ds.hum[ds.hum$subj==i.hum,]$res - tmp.hum[1]) / tmp.hum[2]
}
#-standardization ends--------------------------------------------------------

#-mean & sd of dataset starts--------------------------------------------------------
ds.fam.m<-data.frame(tapply(X=ds.fam$res, INDEX=ds.fam$img, FUN=mean))
ds.hor.m<-data.frame(tapply(X=ds.hor$res, INDEX=ds.hor$img, FUN=mean))
ds.hum.m<-data.frame(tapply(X=ds.hum$res, INDEX=ds.hum$img, FUN=mean))

names(ds.fam.m)[1]="res_mean"
names(ds.hor.m)[1]="res_mean"
names(ds.hum.m)[1]="res_mean"

ds.fam.m$img<-levels(ds$img)
ds.hor.m$img<-levels(ds$img)
ds.hum.m$img<-levels(ds$img)

ds.fam.m$type<-"fam"
ds.hor.m$type<-"hor"
ds.hum.m$type<-"hum"

ds.m<-cbind(ds.fam.m, ds.hor.m$res_mean, ds.hum.m$res_mean)
names(ds.m)[1]="res.fam.m"
names(ds.m)[4]="res.hor.m"
names(ds.m)[5]="res.hum.m"

# sd
ds.m$res.fam.sd<-aggregate(x = ds.fam$res, list(img=ds.fam$img), sd)$x
ds.m$res.hor.sd<-aggregate(x = ds.hor$res, list(img=ds.hor$img), sd)$x
ds.m$res.hum.sd<-aggregate(x = ds.hum$res, list(img=ds.hum$img), sd)$x
#-mean & sd of dataset ends--------------------------------------------------------

#-add image size column starts--------------------------------------------------------
ds.m<-ds.m[order(ds.m$img),]
ds.m$imagesizevh<-ds.imgsizevh
ds.m$imagesize<-ds.imgsize
ds.m$imagesizeh<-ds.imgsizeh
ds.m$imagesizev<-ds.imgsizev
#-add image size column ends--------------------------------------------------------

# #-graph for reliability among participant (cronbach alpha) starts--------------------------------------------------------
# library(plotrix) # library for standard error
# res.constancy.as.sd<-as.data.frame(c("hum", "fam", "hor"))
# names(res.constancy.as.sd)[1]<-"restype"
# res.constancy.as.sd$meanofsd<-c(mean(ds.m$res.hum.sd), mean(ds.m$res.fam.sd), mean(ds.m$res.hor.sd))
# res.constancy.as.sd$stdofsd<-c(std.error(ds.m$res.hum.sd), std.error(ds.m$res.fam.sd), std.error(ds.m$res.hor.sd))
# res.constancy.as.sd$ord<-1:3
# 
# ppp <- ggplot(res.constancy.as.sd, aes(x=restype , y=meanofsd, fill=restype)) +
#   geom_bar(color="black", width=0.8, stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=meanofsd-stdofsd, ymax=meanofsd+stdofsd), width=.2, position=position_dodge(.9)) +
#   xlab("Response types") +
#   ylab("Mean of sd scores") +
#   theme(axis.title.x = element_text(color="black", face="bold", size=20, margin = margin(t=15, r=10, b = 10, l=10))) +
#   theme(axis.title.y = element_text(color="black", face="bold", size=20, margin = margin(t=10, r=10, b = 0, l=0))) +
#   theme(axis.text = element_text(color="black", face="bold", size=15))
# ppp
# #-graph for reliability among participant (cronbach alpha) ends--------------------------------------------------------

# #-correlation according to image sizes starts----------------------------
# cor.hum<-cor.test(ds.m$imagesize, ds.m$res.hum.m)
# cor.fam<-cor.test(ds.m$imagesize, ds.m$res.fam.m)
# cor.hor<-cor.test(ds.m$imagesize, ds.m$res.hor.m)
# 
# # capture.output(print(cor.hor, digits = 2), file = paste("cor_output_", "imagesize & horribleness", ".txt", sep = ""))
# #-correlation according to image sizes ends----------------------------

#-extract data of images which are used in Uv emotion rating research starts--------------------------------------------------------
emo.list<-list.files('D:/OneDrive/Program/R Statistics/2016.06.22_Data_UV_emotion_rating/실험이미지160615_rerabeling')
emo.list<-gsub(".png","",emo.list)
emo.list.res<-subset(ds.m, ds.m$img %in% emo.list)
# write.table(emo.list.res, file="responses_of_imgs_used_in_emotion_reaserch.txt", sep = "\t")
#-extract data of images which are used in Uv emotion rating research ends--------------------------------------------------------

#-remove columns near boundary between divion for same number of data in each divion starts----------
ntype<-10 # how many human_likeness score are divided as order scale

if (is.integer(nrow(ds.m) / ntype)==FALSE) {
  rev<-NULL
  tmp.start<-ceiling(nrow(ds.m) / ntype)
  for (tmp.exci in (1:(ceiling(nrow(ds.m)/ntype) - (nrow(ds.m)-((ceiling(nrow(ds.m)/ntype)*(floor(nrow(ds.m)/(ceiling(nrow(ds.m)/ntype)))))))))) {
    tmp.rev<-seq(tmp.start, nrow(ds.m), by = ceiling(nrow(ds.m) / ntype))
    rev<-append(rev,tmp.rev)
    tmp.start<-tmp.start-1
  }
  
  ds.m <- ds.m[-rev, ]
}
#-remove columns near boundary between divion for same number of data in each divion ends----------

#-ready for order scale starts--------------------------------------------------------
ds.m$type<-ceiling(ds.m$res.hum.m/(100/ntype)) # calculate order by absolute human_likeness socre

ds.m<-ds.m[order(ds.m$res.hum.m),]
ds.m$ord<-1:nrow(ds.m)
ds.m$type.o<-ceiling(ds.m$ord/(ceiling(nrow(ds.m)/ntype))) # calculate order by relative human_likeness socre (order scale)

ds.m<-ds.m[order(ds.m$imagesize),]
ds.m$ord.size<-1:nrow(ds.m)
ds.m$type.size<-ceiling(ds.m$ord.size/(ceiling(nrow(ds.m)/ntype)))
#-ready for order scale ends--------------------------------------------------------

# #-optimization starts------------------------------------------------
# library(optimx) # one of library for optimization
# library(ROI) # one of library for optimization
# #-optimization ends------------------------------------------------

#-remove columns near boundary between divion for same number of data in each divion ends----------

#-rabel for each animation, doll, robot images starts-------------------------------------
ds.m<-ds.m[order(ds.m$img),]
ds.m.ra<-subset(ds.m, agrepl("RA", ds.fam.m$img, max.distance = 0) == TRUE)
ds.m.ha<-subset(ds.m, agrepl("HA", ds.fam.m$img, max.distance = 0) == TRUE)
ds.m.rd<-subset(ds.m, agrepl("RD", ds.fam.m$img, max.distance = 0) == TRUE)
ds.m.hd<-subset(ds.m, agrepl("HD", ds.fam.m$img, max.distance = 0) == TRUE)
ds.m.rr<-subset(ds.m, agrepl("RR", ds.fam.m$img, max.distance = 0) == TRUE)
ds.m.hr<-subset(ds.m, agrepl("HR", ds.fam.m$img, max.distance = 0) == TRUE)

ds.m.a<-rbind(ds.m.ra, ds.m.ha)
ds.m.d<-rbind(ds.m.rd, ds.m.hd)
ds.m.r<-rbind(ds.m.rr, ds.m.hr)
ds.m.dr<-rbind(ds.m.d, ds.m.r)

ds.m.4cate<-cbind(
  data.frame(tapply(ds.m$res.fam.m, ds.m$type.o, mean)),
  data.frame(tapply(ds.m$res.hor.m, ds.m$type.o, mean)),
  data.frame(tapply(ds.m.a$res.fam.m, ds.m.a$type.o, mean)),
  data.frame(tapply(ds.m.a$res.hor.m, ds.m.a$type.o, mean)),
  data.frame(tapply(ds.m.d$res.fam.m, ds.m.d$type.o, mean)),
  data.frame(tapply(ds.m.d$res.hor.m, ds.m.d$type.o, mean)),
  data.frame(tapply(ds.m.r$res.fam.m, ds.m.r$type.o, mean)),
  data.frame(tapply(ds.m.r$res.hor.m, ds.m.r$type.o, mean)),
  data.frame(tapply(ds.m.dr$res.fam.m, ds.m.dr$type.o, mean)),
  data.frame(tapply(ds.m.dr$res.hor.m, ds.m.dr$type.o, mean))
)
names(ds.m.4cate)[1]="ds.m.fam"
names(ds.m.4cate)[2]="ds.m.hor"
names(ds.m.4cate)[3]="ds.m.fam.a"
names(ds.m.4cate)[4]="ds.m.hor.a"
names(ds.m.4cate)[5]="ds.m.fam.d"
names(ds.m.4cate)[6]="ds.m.hor.d"
names(ds.m.4cate)[7]="ds.m.fam.r"
names(ds.m.4cate)[8]="ds.m.hor.r"
names(ds.m.4cate)[9]="ds.m.fam.dr"
names(ds.m.4cate)[10]="ds.m.hor.dr"
#-rabel for each animation, doll, robot images ends-------------------------------------

# ds.m$fam_hor<-ds.m$res.fam.m-ds.m$res.hor.m
# write.table(ds.m, file="data_UV_ratings_sumarized.txt", sep="\t")

#-graphs drawing starts-------------------------------------
#-ready for bargraph starts-------------------------------------
library(plotrix)
ds.m$fam_hor<-ds.m$res.fam.m - ds.m$res.hor.m
ds.m<-ds.m[order(ds.m$type.o),]
type.o.sd<-aggregate(ds.m$fam_hor, list(img=ds.m$type.o), std.error)
type.o.sd.rep<-rep(type.o.sd$x, each = length(ds.m$type.o)/length(unique(ds.m$type.o)))
ds.m$fam_hor_sd<-ds.m$fam_hor/type.o.sd.rep

tg.bar<-ds.m
# tg.bar.restype<-tg.bar$res.fam.m
# tg.bar.restype<-tg.bar$res.hor.m
tg.bar.restype<-tg.bar$fam_hor
tg.bar.ordtype<-tg.bar$type.o # type.o & type.size

# calculate mean & std
barmeans<-tapply(tg.bar.restype, tg.bar.ordtype, mean)
df.mstd<-aggregate(x = tg.bar.restype, list(type.o=tg.bar.ordtype), mean)
colnames(df.mstd)[match("x",colnames(df.mstd))] <- "mean"

df.mstd$std<-aggregate(x = tg.bar.restype, list(type.o=tg.bar.ordtype), std.error)$x
#-ready for bargraph ends-------------------------------------

#-draw barplot between human_likness scores and familiarity or horrifying scores starts---------------
library(ggplot2)
library(colortools)
# seqcolor<-sequential("lightgreen", 8, fun = "log")
seqcolor<-sequential("lightblue", 8, fun = "log")
# seqcolor<-sequential("orange", 8, fun = "log")
seqcolor<-seqcolor[-1]
seqcolor<-seqcolor[floor((length(seqcolor)/2)-(ntype/2)):floor((length(seqcolor)/2)+(ntype/2))]

# pp <- ggplot(ds.m, aes(x=ds.m$res.hum.m, y=ds.m$imagesize)) +
#   geom_point(shape=1) +
#   ylim(0, 1500000) +
#   xlab("human_likeness score") +
#   ylab("image resolution") +
#   theme(axis.title.x = element_text(color="black", face="bold", size=30, margin = margin(t=15, r=10, b = 10, l=10))) +
#   theme(axis.title.y = element_text(color="black", face="bold", size=30, margin = margin(t=10, r=10, b = 0, l=0))) +
#   theme(axis.text = element_text(color="black", face="bold", size=15))
# pp

p <- ggplot(df.mstd, aes(x=type.o, y=mean)) +
  # p <- ggplot(df.mstd, aes(x=type.o, y=std)) +
  geom_bar(fill=c(seqcolor[1:ntype]), color="black", stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=.2, position=position_dodge(.9)) +
  # Xlim() +
  # ylim(0, 100) +
  # scale_y_reverse(lim=c(100, 0)) +
  ylim(-1.05, 1.0) +
  # ylim(0, 0.2) +
  # ggtitle("Familiarity score") +
  # ggtitle("Horrifying score") +
  # ggtitle("standard score") +
  # xlab("human_likeness score") +
  xlab("인간과 닮은 정도") +
  # ylab("human_likeness score") +
  # xlab("image resolution") +
  # ylab("familiarity score") +
  # ylab("horribleness score") +
  # ylab("familiarity - horribleness") +
  ylab("친근한 정도 - 섬뜩한 정도") +
  theme(axis.title.y = element_text(angle = 270, vjust=-1)) +
  # ylab("SD") +
  # theme(plot.title = element_text(color="black", face="bold", size=35)) +
  theme(axis.title.x = element_text(color="black", face="bold", size=20, margin = margin(t=15, r=10, b = 10, l=10))) +
  # theme(axis.title.y = element_text(color="black", face="bold", size=30, margin = margin(t=10, r=10, b = 0, l=0))) +
  theme(axis.title.y = element_text(color="black", face="bold", size=20, margin = margin(t=10, r=10, b = 0, l=0))) +
  theme(axis.text = element_text(color="black", face="bold", size=15)) +
  scale_x_continuous(breaks = 1:ntype)
p
table(tg.bar.ordtype)
#-draw barplot between human_likness scores and familiarity or horrifying scores ends---------------

#-draw scatter & fitting graph between human_likness scores and familiarity or horrifying scores starts---------------
## plot values setting
# option 1: ds.m, ds.m.a, ds.m.d, ds.m.r, ds.m.dr
# option 2: res.fam.m, res.hor.m
tg.sc<-ds.m
plotY=c(tg.sc$res.fam.m-tg.sc$res.hor.m)
# plotY=c(tg.sc$res.fam.m)
# plotY=c(tg.sc$res.hor.m)
plotX=c(tg.sc$res.hum.m)
# hist(plotX, breaks=101)

# satterplot draw
library(car)
scatterplot(plotX, plotY,
            # col=c("black", "red", "blue"),
            # col=append("green",color.scatterplot),
            main="Standard score",
            # main="Familiarity score",
            # main="Horrifying score",
            # xlim = range(-1, 2),
            # ylim = range(-3, 2),
            # ylim = range(0, 80),
            # ylim=rev(c(0,80)),
            xlab="Human_likeness",
            ylab="fam.z-hor.z",
            # ylab="Familiarity",
            # ylab="Horrifying",
            # legend.title = "Category",
            reg.line=FALSE, smooth=FALSE)

scatterplot(ds.m$res.hum.m, ds.m$imagesize,
            ylim = c(0, 1000000),
            xlab="image resolution",
            ylab="human likeness",
            cex.lab = 1,
            reg.line=FALSE, smooth=FALSE)


# draw fitting curve
# lines(lowess(plotX,plotY), col="blue")
linear<-lm(plotY~plotX)
quadratic<-lm(plotY~plotX+I(plotX^2))
cubic<-lm(plotY~plotX+I(plotX^2)+I(plotX^3))

cq = coef(quadratic) 
cc = coef(cubic)

# linear
abline(linear, col="black")

# quadratic
newenv<-seq(min(plotX), max(plotX), by = (max(plotX) - min(plotX))/500) 
sp.quad = cq[1] + cq[2]*newenv +cq[3]*newenv^2 
lines(newenv,sp.quad, col='red')

# cubic
sp.cubic = cc[1] + cc[2]*newenv +cc[3]*newenv^2 +cc[4]*newenv^3 
lines(newenv, sp.cubic, col='blue', lty=2) 
#-draw scatter & fitting graph between human_likness scores and familiarity or horrifying scores ends---------------
#-graphs drawing starts-------------------------------------

# cor.test(ds.m$res.hor.m, (ds.m$res.hum.m)^2, method = "pearson")

# p-values of fitting graphs
summary(linear)
summary(quadratic)
summary(cubic)
