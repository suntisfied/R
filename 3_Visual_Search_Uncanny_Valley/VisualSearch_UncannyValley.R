rm(list=ls()) # remove all lists existing before

# function to discriminate odd or even
is.odd <- function(x) x %% 2 != 0
is.even <- function(x) x %% 2 == 0

root<-getwd()
setwd(root) # set current directory

#-data loading starts--------------------------------------------------------
dir<-'data/' # directory
ds=NULL;
for (this in 1:length(list.files(dir))){
  filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
  tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NaN) # filename to data.frame
  ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
}
#-data loading ends--------------------------------------------------------
# delete data which have reaction time bigger than 5s
ds<-ds[!(ds$tRtR>10),] # selecting data which have reaction time lower than 5s
# ds<-ds[(ds$tAccR)==1,]

# separate data according to having rating session or not 
# ds<-ds[!(ds$RunIdx==6),] # selecting data of 1-5 repeat, no rating response, only setSize & targetType
# ds.rating<-ds[(ds$RunIdx==6),] # selecting data of 6 repeat, rating response

ds.robot_uncanny <- ds[is.odd(ds$sbj) == FALSE, ]
ds.robot_uncanny[ds.robot_uncanny$tType == 1,]$tType <- 1.1
ds.robot_uncanny[ds.robot_uncanny$tType == 3,]$tType <- 3.1

ds.uncanny_human <- ds[is.odd(ds$sbj) == TRUE, ]
# ds.uncanny_human <- ds[ds$sbj==2, ]
ds.uncanny_human[ds.uncanny_human$tType == 1,]$tType <- 1.2
ds.uncanny_human[ds.uncanny_human$tType == 3,]$tType <- 3.2

# calculate the mean & se according to ivs
library(plotrix) # library for standard error
# data of having no rating session
ds.mse<-aggregate(x = ds.robot_uncanny$tRtR,
                           list(targetType=ds.robot_uncanny$tType,
                                setSize=ds.robot_uncanny$SetSize),
                           mean)
names(ds.mse)[3]<-'mean'
ds.mse$se<-aggregate(x = ds.robot_uncanny$tRtR,
                              list(targetType=ds.robot_uncanny$tType,
                                   setSize=ds.robot_uncanny$SetSize),
                              std.error)$x

ds.mse.tmp<-aggregate(x = ds.uncanny_human$tRtR,
                               list(targetType=ds.uncanny_human$tType,
                                    setSize=ds.uncanny_human$SetSize),
                               mean)
names(ds.mse.tmp)[3]<-'mean'
ds.mse.tmp$se<-aggregate(x = ds.uncanny_human$tRtR,
                                  list(targetType=ds.uncanny_human$tType,
                                       setSize=ds.uncanny_human$SetSize),
                                  std.error)$x

ds.mse <- rbind(ds.mse, ds.mse.tmp)

# ready for graph - make scale value scalar to string
ds.mse[1:9,]$targetType <- rep(c('robot', 'uncanny', 'notarget'))
ds.mse[10:18,]$targetType <- rep(c('uncanny', 'human', 'notarget'))
ds.mse$setSize <- as.character(ds.mse$setSize)

# graph draw - accoring to conditions, line graph
library(ggplot2)
ggplot(data = ds.mse[1:9,],
       aes(x = setSize, y = mean, group = targetType, colour = targetType)) +
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = .1, size = 0.7) +
  scale_x_discrete(limit = c("3","6","12"), labels = c("3","6","12")) +
  scale_color_manual(breaks = rev(c("robot", "uncanny", "notarget")),
                     labels = rev(c("robot", "uncanny", "notarget")),
                     values = rev(c("#3690c0", "#fb6a4a", "#999999"))) +
  theme_grey(base_size = 15) +
  xlab("Setsize of test screen") +
  ylab("Mean of RT") +
  scale_y_continuous(breaks = seq(0.5, 2.6, by = 0.2), limits = c(0.5, 2.6))

ggplot(data = ds.mse[10:18,],
       aes(x = setSize, y = mean, group = targetType, colour = targetType)) +
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = .1, size = 0.7) +
  scale_x_discrete(limit = c("3","6","12"), labels = c("3","6","12")) +
  scale_color_manual(breaks = rev(c("uncanny", "human", "notarget")),
                     labels = rev(c("uncanny", "human", "notarget")),
                     values = rev(c("#3690c0", "#999999", "#41ab5d"))) +
  theme_grey(base_size = 15) +
  xlab("Setsize of test screen") +
  ylab("Mean of RT") +
  scale_y_continuous(breaks = seq(0.5, 2.6, by = 0.2), limits = c(0.5, 2.6))

# ANOVA
# aggregate values of repition
ds.aov<-aggregate(ds$tRtR, by = list(ds$sbj, ds$tType, ds$SetSize, ds$tloc), FUN = "mean")
colnames(ds.aov) <- c("subj", "targetType", "setSize", "targetLocation", "reactionTime")

# show the result
ds.aov.t<-ds.aov[ds.aov$subj %in% seq(2,max(ds$sbj), by=2),]
# ds.aov.t<-ds.aov

# make values as factor (important part! if undone, result changes)
ds.aov.t$subj<-factor(ds.aov.t$subj)
ds.aov.t$targetType<-factor(ds.aov.t$targetType)
ds.aov.t$setSize<-factor(ds.aov.t$setSize)
ds.aov.t$targetLocation<-factor(ds.aov.t$targetLocation)

# using fuction aov (basic function in R, only type 1 error)
summary(aov(reactionTime ~ targetType*setSize*targetLocation
            + Error(subj/(targetType*setSize*targetLocation)),
            data = ds.aov.t))

# using fuction ezANOVA (ezANOVA)
library(ez)
ezANOVA(data=ds.aov.t,
        dv=.(reactionTime),
        wid.(subj),
        within=.(targetType,setSize,targetLocation),
        # between=.(),
        detailed=TRUE, type=1)

# using fuction Anova (car)
library(car)
library(afex) # make easy to use function from library car
summary(aov_car(reactionTime ~ targetType*setSize*targetLocation
        + Error(subj/(targetType*setSize*targetLocation)),
        data = ds.aov.t, type=3))