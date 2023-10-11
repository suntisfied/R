  #===========================================================
  # Default Setting===========================================
  #===========================================================
  rm(list=ls()) # remove all lists existing before
  root<-getwd() # get current directory
  setwd(root) # set current directory
  
  # install required packasges if they didn't been installed
  list.of.required.packages <- c("ggplot2", "cowplot", "ggpubr", "doBy", "plotrix")
  new.packages <- list.of.required.packages[!(list.of.required.packages %in% installed.packages()[,"Package"])]
  # install.packages(new.packages)
  
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
  targetData <- c('1a', '1b')
  # targetData <- c('2a', '2b')
  # targetData <- c('2a', '2c')
  # targetData <- c('2a', '2b', '2c')
  # targetData <- c('2a', '3b')
  # targetData <- c('2a', '3c')
  # targetData <- c('2a', '3b', '3c')
  # 1a : measuring orientation bias without binocular rivalry
  # 1b : measuring tilt illusion without binocular rivalry
  # 2a: measuring orientation bias with binocular rivalry
  # 2b: measuring tilt illusion with binocular rivarly
  # 2c: measuring tilt illusion in binocular rivalry sitmuli without binocular rivarly
  # 3b : measuring tilt illusion with interocular grouping
  # 3c : measuring tilt illusion in interocular grouping stimuli without binocular rivalry
  
  analysisType.condition <- 2
  # 1: Dividing Every Experimental Conditions (Including Counterbalancing)
  # 2: divide by vertical & horizontal
  # 3: combinding all
  
  reOrganizeAxis <- 1 # select this option to 1 only when you want to combine all counterbalancing conditions
  # 0: No
  # 1: Yes
  
  analysisType.relativity <- 0
  # 0: absolute y-axis values, standard line & experimental line
  # 1: relative y-axis values, difference line between stad ard & experimental
  
  plotType <- 1
  # 1: line plot
  # 2: bar plot
  if (analysisType.relativity==1){reOrganizeAxis<-1; plotType<-2;}
  
  drawOnlyZeroDegree <- 0
  if (drawOnlyZeroDegree==1){analysisType.relativity <- 0; plotType <- 2;}
  # 0: NO (use all degree coditions)
  # 1: Yes
  
  drawWithoutRagardingRotatingVariance <- 0
  # 0: NO (use all degree coditions)
  # 1: Yes
  if (drawWithoutRagardingRotatingVariance==1){reOrganizeAxis<-1; analysisType.relativity<-1; plotType<-2; drawOnlyZeroDegree <- 0}
  
  drawOnlyTotal <- 0
  # 0: NO
  # 1: Yes
  
  drawOnlyStandard <- 0
  # 0: NO
  # 1: Yes
  
  eraseConsciousResponse <- 0
  # including subjects: c(3,4)
  # 0: use all data
  # 1: use only not consciously perceive grounping stimuli
  # 2: use only consciously perceive grounping stimuli
  
  selectPartialGroup <- 0 # select some subjects' data
  # 0: NO (use all subjects' data)
  # 1: Yes
  
  selectedParticipantsGroup <- c(3,4,5,994) # 1a & 1b
  # selectedParticipantsGroup <- c(3,4,5) # 1a & 1b
  # selectedParticipantsGroup <- c(1,3,4,7,9,12,13,14,15,994) # N:10, 2a & 2b & 2c except participants who do not see binocular rivalry
  # selectedParticipantsGroup <- c(5,11) # participants who do not see binocular rivalry in 2a & 2b & 2c
  # selectedParticipantsGroup <- c(1,2,3,4,8,10,12,14,15,994) #N:10,3b & 3c except participants who do not see binocular rivalry
  # selectedParticipantsGroup <- c(5,6) # participants who do not see binocular rivalry in 3b & 3c
  
  # selectedParticipantsGroup <- c(1,3,4,5,7,9,11,12,13,14,15,994) # 2a & 2b
  # selectedParticipantsGroup <- c(1,4,7,11,13,15) # 2a & 2b
  # selectedParticipantsGroup <- c(3,5,9,12,14,994) # 2a & 2b
  # selectedParticipantsGroup <- c(1,2,3,4,5,6,8,10,12,14,15,994) # 3a & 3b
  # selectedParticipantsGroup <- c(1,3,5,8,12,15) # 3a & 3b
  # selectedParticipantsGroup <- c(2,4,6,10,14,994) # 3a & 3b
  # selectedParticipantsGroup <- c(2, 6, 8, 10, 12) # the selected subject numbers
  # selectedParticipantsGroup <- c(1, 3, 4, 5, 7, 9, 11, 13, 994) # the selected subject numbers
  # selectedParticipantsGroup <- c(12, 13, 14, 994) # the selected subject numbers
  # selectedParticipantsGroup <- c(5, 11) # the selected subject numbers
  # selectedParticipantsGroup <- c(6, 8, 10, 12) # the selected subject numbers
  # selectedParticipantsGroup <- c(6) # the selected subject numbers
  # selectedParticipantsGroup <- c(14, 15, 994) # the selected subject numbers
  # selectedParticipantsGroup <- c(4,14,15,994) # the selected subject numbers
  # among data from 2017.10.18 to 2017.10.22
  # binocular rivalry: c(1, 3, 4, 5, 7, 9, 11, 13, 994)
  # interocular grouping: c(2, 3, 4, 6, 8, 10, 12)
  if ((length(intersect(c('1a', '1b'), targetData)) > 1) & (selectPartialGroup==0)){selectPartialGroup<-1;selectedParticipantsGroup<-c(1,2,3,4,5,14,15,994);}
  
  selectedEffectType <-c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 99)
  # selectedEffectType <-c(0, 1, 2, 6)
  # selectedEffectType <- c(3, 4, 5, 6)
  # selectedEffectType <- c(4, 5, 6)
  # effectType 0: no rivalry, small orientation differece between center & surround (repulsion)
  # effectType 1: no rivalry, big orientation differece between center & surround (attraction)
  # effectType 2: no rivalry, standard (no surround, experimental)
  # effectType 3: rivalry, standard (no surround, experimental)
  # effectType 4: rivalry, small orientation differece between center & surround (repulsion)
  # effectType 5: rivalry, big orientation differece between center & surround (attraction)
  # effectType 6: grouping, small orientation differece between center & adjacent surround (repulsion)
  # effectType 7: grouping, big orientation differece between center & adjacent surround (attraction)
  # effectType 8: no rivalry, grouping stimuli, small orientation differece between center & surround (repulsion)
  # effectType 9: no rivalry, grouping stimuli, big orientation differece between center & surround (attraction)
  # effectType 10: no rivalry, rivalry sitmuli, small orientation differece between center & surround (repulsion)
  # effectType 11: no rivalry, rivalry sitmuli, big orientation differece between center & surround (attraction)
  # effectType 99: standard (no surround, theoretical)
  
  # #=============================================================
  # # raw data with considering counterbalancing==================
  # #=============================================================
  # analysisType.condition <- 1; reOrganizeAxis <- 0; analysisType.relativity <- 0; plotType <- 1
  
  list.targetData <- list() # empty list
  #===========================================================================================
  # Exp 1a / combinding data of exp for "orientation bias without binocular riavalry"==========
  #===========================================================================================
  if ('1a' %in% targetData){
    dir<-'Data_Exp1a_For_OrientationBias_withoutBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        if (c('WichEyeFixation') %in% names(tmp.ds) == FALSE){
          tmp.ds$WichEyeFixation <- -1
        }
        
        if (c('targetGratingOrientatioin') %in% names(tmp.ds) == FALSE){
          tmp.ds$targetGratingOrientatioin <- tmp.ds$whichEye
        }
        tmp.ds$whichEye <- tmp.ds$targetGratingOrientatioin
        
        if (c('resVertList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resVertList <- as.character(tmp.ds$tiltResVert)
        }
        
        if (c('resHorzList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resHorzList <- as.character(tmp.ds$tiltResHorz)
        }
        
        if (c('resList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resList <- paste(tmp.ds$resHorzList, tmp.ds$resVertList, sep=',')
        }
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
        
        ds<-ds[,c('sbj','serial','BlockIdx','TrialIdx','whichEye','WichEyeFixation','targetGratingOrientatioin','targetGratingOrientatioinChange','effectDirection','midoutSameOri','stimSize','stimOrientationDifference','tiltResVert','tiltResHorz','resVertList','resHorzList','resList', 'perceiveGroupingVert',	'perceiveGroupingHorz', 'perceiveGrouping')]
      }
    }
    ds.oriBias<-ds
    # coding new index because these indexes are not in exp for orientation bias
    ds.oriBias$effectDirection<-0
    ds.oriBias$stimOrientationDifference<-0
    ds.oriBias$expVersion<-1
    
    list.targetData <- append(list.targetData, list(ds.oriBias)) # add oriBias data.frame to empty default data.frame
  }
  
  #===========================================================================================
  # Exp 1b / combinding data of exp for "orientation bias without binocular riavalry"==========
  #===========================================================================================
  if ('1b' %in% targetData){
    dir<-'Data_Exp1b_For_TiltIllusion_withoutBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        if (c('WichEyeFixation') %in% names(tmp.ds) == FALSE){
          tmp.ds$WichEyeFixation <- -1
        }
        
        if (c('targetGratingOrientatioin') %in% names(tmp.ds) == FALSE){
          tmp.ds$targetGratingOrientatioin <- tmp.ds$whichEye
        }
        tmp.ds$whichEye <- tmp.ds$targetGratingOrientatioin
        
        if (c('resVertList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resVertList <- as.character(tmp.ds$tiltResVert)
        }
        
        if (c('resHorzList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resHorzList <- as.character(tmp.ds$tiltResHorz)
        }
        
        if (c('resList') %in% names(tmp.ds) == FALSE){
          tmp.ds$resList <- paste(tmp.ds$resHorzList, tmp.ds$resVertList, sep=',')
        }
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
        
        ds<-ds[,c('sbj','serial','BlockIdx','TrialIdx','whichEye','WichEyeFixation','targetGratingOrientatioin','targetGratingOrientatioinChange','effectDirection','midoutSameOri','stimSize','stimOrientationDifference','tiltResVert','tiltResHorz','resVertList','resHorzList','resList', 'perceiveGroupingVert',	'perceiveGroupingHorz', 'perceiveGrouping')]
      }
    }
    ds.tilt<-ds
    ds.tilt[ds.tilt$effectDirection==1,]$effectDirection<-2
    ds.tilt[ds.tilt$effectDirection==0,]$effectDirection<-1
    ds.tilt[ds.tilt$stimOrientationDifference==60,]$stimOrientationDifference<-1
    if (240 %in% ds.tilt$stimOrientationDifference){
      ds.tilt[ds.tilt$stimOrientationDifference==240,]$stimOrientationDifference<-2
    }
    if (120 %in% ds.tilt$stimOrientationDifference){
      ds.tilt[ds.tilt$stimOrientationDifference==120,]$stimOrientationDifference<-2
    }
    
    
    ds.tilt$expVersion<-2
    
    list.targetData <- append(list.targetData, list(ds.tilt)) # add oriBias data.frame to empty default data.frame
  }
  
  #===========================================================================================
  # Exp 2a / combinding data of exp for "orientation bias with binocular rivalry"=============
  #===========================================================================================
  if ('2a' %in% targetData){
    dir<-'Data_Exp2a_For_OrientationBias_withBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        # if ((mean(tmp.ds$targetGratingOrientatioin)==-1)==TRUE){
        #   tmp.ds$targetGratingOrientatioin<-NULL
        # }
        tmp.ds$targetGratingOrientatioin<-33
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
      }
    }
    ds.oriBias.br<-ds
    ds.oriBias.br$effectDirection<-3
    ds.oriBias.br$stimOrientationDifference<-3
    ds.oriBias.br$expVersion<-3
    
    list.targetData <- append(list.targetData, list(ds.oriBias.br))
  }
  
  #============================================================================================
  # Exp 2b / combinding data of exp for existence of "tilt illusion with binocular rivalry"====
  #============================================================================================
  if ('2b' %in% targetData){
    dir<-'Data_Exp2b_For_TiltIllusion_withBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        # if ((mean(tmp.ds$targetGratingOrientatioin)==-1)==TRUE){
        #   tmp.ds$targetGratingOrientatioin<-NULL
        # }
        # tmp.ds$targetGratingOrientatioin<-NULL
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
      }
    }
    ds.tilt.br<-ds
    ds.tilt.br[ds.tilt.br$effectDirection==0,]$effectDirection<-4
    ds.tilt.br[ds.tilt.br$effectDirection==1,]$effectDirection<-5
    ds.tilt.br[ds.tilt.br$stimOrientationDifference==60,]$stimOrientationDifference<-4
    if (240 %in% ds.tilt.br$stimOrientationDifference){
      ds.tilt.br[ds.tilt.br$stimOrientationDifference==240,]$stimOrientationDifference<-5
    }
    if (120 %in% ds.tilt.br$stimOrientationDifference){
      ds.tilt.br[ds.tilt.br$stimOrientationDifference==120,]$stimOrientationDifference<-5
    }
    ds.tilt.br$expVersion<-4
    
    list.targetData <- append(list.targetData, list(ds.tilt.br))
  }
  
  #============================================================================================
  # Exp 2c / combinding data of exp for existence of "tilt illusion with binocular rivalry"====
  #============================================================================================
  if ('2c' %in% targetData){
    dir<-'Data_Exp2c_For_TiltIllusion_InBRStimuli_withoutBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        # if ((mean(tmp.ds$targetGratingOrientatioin)==-1)==TRUE){
        #   tmp.ds$targetGratingOrientatioin<-NULL
        # }
        # tmp.ds$targetGratingOrientatioin<-NULL
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
      }
    }
    ds.tilt.br.withoutBR<-ds
    ds.tilt.br.withoutBR[ds.tilt.br.withoutBR$effectDirection==0,]$effectDirection<-10
    ds.tilt.br.withoutBR[ds.tilt.br.withoutBR$effectDirection==1,]$effectDirection<-11
    ds.tilt.br.withoutBR[ds.tilt.br.withoutBR$stimOrientationDifference==60,]$stimOrientationDifference<-10
    if (240 %in% ds.tilt.br.withoutBR$stimOrientationDifference){
      ds.tilt.br.withoutBR[ds.tilt.br.withoutBR$stimOrientationDifference==240,]$stimOrientationDifference<-11
    }
    if (120 %in% ds.tilt.br.withoutBR$stimOrientationDifference){
      ds.tilt.br.withoutBR[ds.tilt.br.withoutBR$stimOrientationDifference==120,]$stimOrientationDifference<-11
    }
    ds.tilt.br.withoutBR$expVersion<-7
    
    list.targetData <- append(list.targetData, list(ds.tilt.br.withoutBR))
  }
  
  #================================================================================================
  # Exp 3b / combinding data of exp for existence of "tilt illusion with interocular grouping"======
  #================================================================================================
  if ('3b' %in% targetData){
    dir<-'Data_Exp3b_For_TiltIllusion_withIO/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        # if ((mean(tmp.ds$targetGratingOrientatioin)==-1)==TRUE){
        #   tmp.ds$targetGratingOrientatioin<-NULL
        # }
        # tmp.ds$targetGratingOrientatioin<-NULL
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        tmp.ds <- tmp.ds
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
      }
    }
    
    ds.tilt.oi<-ds
    ds.tilt.oi[ds.tilt.oi$effectDirection==0,]$effectDirection<-7 # 0: samll orientation difference between center & distant surround (hypothesized repulsive effect condition in psychopy program) -> 5: big orientation difference between center & adjacent surround (hypothesized attractive effect condition in thesis)
    ds.tilt.oi[ds.tilt.oi$effectDirection==1,]$effectDirection<-6 # 1: big orientation difference between center & distant surround (hypothesized attractive effect condition in psychopy program) -> 5: small orientation difference between center & adjacent surround (hypothesized repulsive effect condition in thesis)
    ds.tilt.oi[ds.tilt.oi$stimOrientationDifference==60,]$stimOrientationDifference<-6
    if (240 %in% ds.tilt.oi$stimOrientationDifference){
      ds.tilt.oi[ds.tilt.oi$stimOrientationDifference==240,]$stimOrientationDifference<-7
    }
    if (120 %in% ds.tilt.oi$stimOrientationDifference){
      ds.tilt.oi[ds.tilt.oi$stimOrientationDifference==120,]$stimOrientationDifference<-7
    }
    ds.tilt.oi$expVersion<-5
    
    list.targetData <- append(list.targetData, list(ds.tilt.oi))
  }
  
  #=======================================================================================================================
  # Exp 3c / combinding data of exp for existence of "tilt illusion in interocular grouping without binocualr rivalry=====
  #=======================================================================================================================
  if ('3c' %in% targetData){
    dir<-'Data_Exp3c_For_TiltIllusion_InIOStimuli_withoutBR/' # directory
    ds=NULL;
    for (this in 1:length(list.files(dir))){
      if (grepl(".txt", list.files(dir)[this])==TRUE){
        filename<-paste(dir, list.files(dir)[this], sep="") # read the filenames in directory
        tmp.ds<-read.table(filename, header=T, sep="\t", na.strings = NA) # filename to data.frame
        
        # if ((mean(tmp.ds$targetGratingOrientatioin)==-1)==TRUE){
        #   tmp.ds$targetGratingOrientatioin<-NULL
        # }
        # tmp.ds$targetGratingOrientatioin<-NULL
        
        if (c('perceiveGrouping') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGrouping <- -1
        }
        
        if (c('perceiveGroupingVert') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingVert <- -1
        }
        
        if (c('perceiveGroupingHorz') %in% names(tmp.ds) == FALSE){
          tmp.ds$perceiveGroupingHorz <- -1
        }
        
        tmp.ds <- tmp.ds
        
        ds<-rbind(ds, tmp.ds) # concatenate data.frame in row
      }
    }
    
    ds.tilt.oi.withoutBR<-ds
    ds.tilt.oi.withoutBR[ds.tilt.oi.withoutBR$effectDirection==0,]$effectDirection<-9 # 0: samll orientation difference between center & distant surround (hypothesized repulsive effect condition in psychopy program) -> 5: big orientation difference between center & adjacent surround (hypothesized attractive effect condition in thesis)
    ds.tilt.oi.withoutBR[ds.tilt.oi.withoutBR$effectDirection==1,]$effectDirection<-8 # 1: big orientation difference between center & distant surround (hypothesized attractive effect condition in psychopy program) -> 5: small orientation difference between center & adjacent surround (hypothesized repulsive effect condition in thesis)
    ds.tilt.oi.withoutBR[ds.tilt.oi.withoutBR$stimOrientationDifference==60,]$stimOrientationDifference<-8
    if (240 %in% ds.tilt.oi.withoutBR$stimOrientationDifference){
      ds.tilt.oi.withoutBR[ds.tilt.oi.withoutBR$stimOrientationDifference==240,]$stimOrientationDifference<-9
    }
    if (120 %in% ds.tilt.oi.withoutBR$stimOrientationDifference){
      ds.tilt.oi.withoutBR[ds.tilt.oi.withoutBR$stimOrientationDifference==120,]$stimOrientationDifference<-9
    }
    ds.tilt.oi.withoutBR$expVersion<-6
    
    list.targetData <- append(list.targetData, list(ds.tilt.oi.withoutBR))
  }
  
  # combinding data of exp for orientation bias & effect of tilt illusion
  ds <- data.frame()
  for (i in 1:length(list.targetData)){ds <- rbind(ds, list.targetData[[i]])}
  
  #========================
  # Select Subjects========
  #========================
  if (selectPartialGroup==0){
    if (c('2b') %in% targetData){
      selectPartialGroup <- 1
      selectedParticipantsGroup <- unique(ds.tilt.br$sbj)
    }
    if (c('3b') %in% targetData){
      selectPartialGroup <- 1
      selectedParticipantsGroup <- unique(ds.tilt.oi$sbj)
    }
    if (c('3c') %in% targetData){
      selectPartialGroup <- 1
      selectedParticipantsGroup <- unique(ds.tilt.oi.withoutBR$sbj)
    }
    if (c('2c') %in% targetData){
      selectPartialGroup <- 1
      selectedParticipantsGroup <- unique(ds.tilt.br.withoutBR$sbj)
    }
  }
  
  if (selectPartialGroup==1){
    ds<-ds[ds$sbj %in% selectedParticipantsGroup,]
  }
  
  if (drawOnlyStandard==1){
    if (length(intersect(c('2a', '2b','2c', '3b', '3c'), targetData))>0){
      ds <- ds[ds$effectDirection %in% c(3),]
    }
    if (length(intersect(c('1a', '1b'), targetData))>0)
      ds <- ds[ds$effectDirection %in% c(0),]
  }
  
  # #===============================================================
  # # Select Responses Only Unconscious To Grouping (Exp. 3)========
  # #===============================================================
  # if (eraseConsciousResponse==1){
  #   ds <- ds[ds$effectDirection %in% c(3) | ds$perceiveGrouping == 0,]
  # }
  # if (eraseConsciousResponse==2){
  #   ds <- ds[ds$effectDirection %in% c(3) | ds$perceiveGrouping == 1,]
  # }
  
  #======================================================================================
  # Use Mean Of Responses List In A Trial As A Representative Value For A Trial==========
  #======================================================================================
  if (length(intersect(c('1a', '1b', '2a', '2b', '2c', '3b', '3c'), targetData)) > 0){
    for (i in 1:nrow(ds)){
      if (is.na(ds[i,]$resVertList)==FALSE){ # replace value of resVert only when resVertList has values
        # for response to vertical target grating
        compoundingTexts.vert<-array(ds[i,]$resVertList)
        splitTexts.vert<-strsplit(compoundingTexts.vert, ",")
        splitTexts.vert<-splitTexts.vert[[1]]
        numbericTexts.vert<-as.numeric(splitTexts.vert)
        ds[i,]$tiltResVert<-mean(numbericTexts.vert)
      }
      
      if (is.na(ds[i,]$resHorzList)==FALSE){ # replace value of resHorz only when resHorzList has values
        # for response to horizontal target grating
        compoundingTexts.horz<-array(ds[i,]$resHorzList)
        splitTexts.horz<-strsplit(compoundingTexts.horz, ",")
        splitTexts.horz<-splitTexts.horz[[1]]
        numbericTexts.horz<-as.numeric(splitTexts.horz)
        ds[i,]$tiltResHorz<-mean(numbericTexts.horz)
      }
    }
    ds[is.na(ds$tiltResVert)==TRUE,]$tiltResVert <- -1 # recode value of na (missing) to -1
    ds[is.na(ds$tiltResHorz)==TRUE,]$tiltResHorz <- -1
  }
  
  # ds[ds$sbj==3 & ds$resType==0, c('res')]
  # 
  # if (unique(ds$sbj==3)){
  #   
  # }
  
  #=========================================================================================
  # Transform Range Of Response To Horizontal From 2~3 To 0~1 (-1: No Response)=============
  #=========================================================================================
  if (length(intersect(c('1a', '1b', '2a', '2b','2c', '3b', '3c'), targetData))>0){
    ds[ds$tiltResHorz>=2 & ds$tiltResHorz<=3,]$tiltResHorz <- ds[ds$tiltResHorz>=2 & ds$tiltResHorz<=3,]$tiltResHorz - 2
  }
  
  #========================================
  # Select Blocks (Repetition)=============
  #========================================
  if (length(intersect(c('1a', '1b'), targetData))>1){
    # ind <- which(with(ds,sbj==994 & BlockIdx %in% 6:10))
    ind <- which(with(ds, BlockIdx %in% 6:10))
    ds<-ds[-ind,]
  }
  
  #=====================================================================================
  # Make Response For Vertical & Horizontal From Wide Format To Long Format=============
  #=====================================================================================
  #=============================
  # For Response To Vetical=====
  #=============================
  if ("WichEyeFixation" %in% names(ds)){
    ds.vert<-ds[, c("sbj", "serial", "BlockIdx", "TrialIdx", "whichEye", "WichEyeFixation", "targetGratingOrientatioin", "targetGratingOrientatioinChange", "effectDirection", "midoutSameOri", "stimSize", "stimOrientationDifference", "tiltResVert", "perceiveGrouping", "perceiveGroupingVert", "perceiveGroupingHorz")]
  } else {
    ds.vert<-ds[, c("sbj", "serial", "BlockIdx", "TrialIdx", "whichEye", "targetGratingOrientatioinChange", "effectDirection", "midoutSameOri", "stimSize", "stimOrientationDifference", "tiltResVert", "perceiveGrouping", "perceiveGroupingVert", "perceiveGroupingHorz")]
  }
  # ds.vert<-ds[,c(1:12)]
  names(ds.vert)[names(ds.vert)=="tiltResVert"]<-"res"
  ds.vert<-cbind(ds.vert, resType=1) # 0: horizontal, 1: vertical
  if (length(intersect(c('2a', '2c'), targetData))>1){
    ds.vert<-ds.vert[ds.vert$targetGratingOrientatioin %in% c(33, 1),] # 33: standard (2a) / 1: vertical
  }
  if (length(intersect(c('1a', '1b'), targetData))>0){
    ds.vert<-ds.vert[ds.vert$whichEye == 1,]
  }
  # erase the missing data
  ds.vert<-ds.vert[ds.vert$res != -1,]
  
  # #===============================================================
  # # Select Responses Only Unconscious To Grouping (Exp. 3)========
  # #===============================================================
  # if (eraseConsciousResponse==1){
  #   ds.vert <- ds.vert[ds.vert$effectDirection %in% c(3) | ds.vert$perceiveGrouping == 0 | ds.vert$perceiveGroupingVert == 0,]
  # }
  # if (eraseConsciousResponse==2){
  #   ds.vert <- ds.vert[ds.vert$effectDirection %in% c(3) | ds.vert$perceiveGrouping == 1 | ds.vert$perceiveGroupingHorz == 1,]
  # }
  
  #===============================
  # For Response To Horizontal====
  #===============================
  if ("WichEyeFixation" %in% names(ds)){
    ds.horz<-ds[, c("sbj", "serial", "BlockIdx", "TrialIdx", "whichEye", "WichEyeFixation", "targetGratingOrientatioin", "targetGratingOrientatioinChange", "effectDirection", "midoutSameOri", "stimSize", "stimOrientationDifference", "tiltResHorz", "perceiveGrouping", "perceiveGroupingVert", "perceiveGroupingHorz")]
  } else {
    ds.horz<-ds[, c("sbj", "serial", "BlockIdx", "TrialIdx", "whichEye", "targetGratingOrientatioinChange", "effectDirection", "midoutSameOri", "stimSize", "stimOrientationDifference", "tiltResHorz", "perceiveGrouping", "perceiveGroupingVert", "perceiveGroupingHorz")]
  }
  # ds.horz<-ds[,c(1:11,13)]
  names(ds.horz)[names(ds.horz)=="tiltResHorz"]<-"res"
  ds.horz<-cbind(ds.horz, resType=0) # 0: horizontal, 1: vertical
  if (length(intersect(c('2a', '2c'), targetData))>1){
    ds.horz<-ds.horz[ds.horz$targetGratingOrientatioin%in% c(33, 0),] # 33: standard (2a) / 0: horizontal
  }
  if (length(intersect(c('1a', '1b'), targetData))>1){
    ds.horz<-ds.horz[ds.horz$whichEye == 0,]
  }
  # erase the missing data
  ds.horz<-ds.horz[ds.horz$res != -1,]
  # ds.horz$res<-ds.horz$res-2
  
  #===============================================================
  # Select Responses Only Unconscious To Grouping (Exp. 3)========
  #===============================================================
  if (eraseConsciousResponse==1){
    ds.horz <- ds.horz[ds.horz$effectDirection %in% c(3) | ds.horz$perceiveGrouping == 0 | ds.horz$perceiveGroupingVert == 0,]
  }
  if (eraseConsciousResponse==2){
    ds.horz <- ds.horz[ds.horz$effectDirection %in% c(3) | ds.horz$perceiveGrouping == 1 | ds.horz$perceiveGroupingHoz == 1,]
  }
  
  ds<-rbind(ds.vert, ds.horz)
      
  if (reOrganizeAxis %in% c(1)){
    #================================================
    # Manipulate For Graph Of Combinding All=========
    #================================================
    if (length(intersect(c('2a', '2b', '2c', '3b', '3c'), targetData))>0){
      # Method 1 (~ 2017.11.06)=========================================================================
      # symmetric transposition by x-axis, y-axis, orgin for matching the shape of all divided graphs
      ds[ds$stimOrientationDifference %in% c(3,4,6,8,10) & ds$resType==1,]$res <- 1 - ds[ds$stimOrientationDifference %in% c(3,4,6,8,10) & ds$resType==1,]$res
      ds[ds$stimOrientationDifference %in% c(3,5,7,9,11) & ds$resType==0,]$res <- 1 - ds[ds$stimOrientationDifference %in% c(3,5,7,9,11) & ds$resType==0,]$res
      ds[ds$stimOrientationDifference %in% c(3,4,6,8,10) & ds$resType==1,]$targetGratingOrientatioinChange<-ds[ds$stimOrientationDifference %in% c(3,4,6,8,10) & ds$resType==1,]$targetGratingOrientatioinChange*(-1)
      ds[ds$stimOrientationDifference %in% c(3,5,7,9,11) & ds$resType==0,]$targetGratingOrientatioinChange<-ds[ds$stimOrientationDifference %in% c(3,5,7,9,11) & ds$resType==0,]$targetGratingOrientatioinChange*(-1)
      
      # # Method 2 (2017.11.06 ~)========================================================================
      # # clockwise surround, horizontal, repulsion
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res
      # # clockwise surround, horizontal, attraction
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange*(-1)
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res
      # # clockwise surround, vertical, repulsion
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange*(-1)
      # # clockwise surround, vertical, attraction
      # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res <- 2 - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res
      # 
      # # counterclockwise surround, horizontal, repulsion
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange*(-1)
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res
      # # counterclockwise surround, horizontal, attraction
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res
      # # counterclockwise surround, vertical, repulsion
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res
      # # counterclockwise surround, vertical, attraction
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange*(-1)
      # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res
    }
    if (length(intersect(c('1a', '1b'), targetData))>0){
      # symmetric transposition by x-axis, y-axis, orgin for matching the shape of all divided graphs
      ds[ds$stimOrientationDifference==1 & ds$resType==1,]$res <- 1 - ds[ds$stimOrientationDifference==1 & ds$resType==1,]$res
      ds[ds$stimOrientationDifference==2 & ds$resType==0,]$res <- 1 - ds[ds$stimOrientationDifference==2 & ds$resType==0,]$res
      ds[ds$stimOrientationDifference==1 & ds$resType==1,]$targetGratingOrientatioinChange<-ds[ds$stimOrientationDifference==1 & ds$resType==1,]$targetGratingOrientatioinChange*(-1)
      ds[ds$stimOrientationDifference==2 & ds$resType==0,]$targetGratingOrientatioinChange<-ds[ds$stimOrientationDifference==2 & ds$resType==0,]$targetGratingOrientatioinChange*(-1)
    }
  }
  
  #==============================================================
  # Combinding Data By Mean Without Response Separation==========
  #==============================================================
  # analysisType.condition <- 2 # 1: Dividing Every Experimental Conditions (Including Counterbalancing), 2: divide by vertical & horizontal, 3: combinding all
  list.ds.res.m <- list()
  library(doBy) # package for applying function per group; function: summaryBy()
  library(plotrix) # package for calculating standard error; function: std.error()
  #=============================================================================================
  # Condition 1: Dividing Every Experimental Conditions (Including Counterbalancing)============
  #=============================================================================================
  # calculate mean & se of responses according to individuals
  ds.res <- summaryBy(data = ds, res ~ sbj + targetGratingOrientatioinChange + whichEye + effectDirection + stimOrientationDifference + resType, FUN = c(mean, std.error))
  # names(ds.res) <- c(sbj + targetGratingOrientatioinChange + whichEye + effectDirection + stimOrientationDifference + resType, 'res.m', 'res.std')
  
  # calculate mean & se of responses in whole group
  ds.res.m <- summaryBy(data = ds, res ~  targetGratingOrientatioinChange + whichEye + effectDirection + stimOrientationDifference + resType, FUN = c(mean, std.error))
  ds.res.m <- cbind(sbj=0,ds.res.m)
  ds.res.m <- rbind(ds.res, ds.res.m)
  names(ds.res.m) <- c('sbj', 'OriRange', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
  list.ds.res.m <- append(list.ds.res.m, list(ds.res.m))
  rm(ds.res); rm(ds.res.m); # erase variable
  
  #==============================================================================
  # Condition 2: Dividing Only Response Types (Vertical & Horizontal)============
  #==============================================================================
  ds.res <- summaryBy(data = ds, res ~ sbj + targetGratingOrientatioinChange + effectDirection + resType, FUN = c(mean, std.error))
  # names(ds.res) <- c('sbj', 'OriRange', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
  
  # calculate mean & se of responses in whole group
  ds.res.m <- summaryBy(data = ds, res ~ targetGratingOrientatioinChange + effectDirection + resType, FUN = c(mean, std.error))
  ds.res.m<-cbind(sbj=0,ds.res.m)
  ds.res.m <- rbind(ds.res, ds.res.m)
  names(ds.res.m) <- c('sbj', 'OriRange', 'effectType', 'resType', 'res.m', 'res.std')
  list.ds.res.m <- append(list.ds.res.m, list(ds.res.m))
  rm(ds.res); rm(ds.res.m);
  
  #====================================================================
  # Condition 3: Combinding All Regrading Rotating Variance============
  #====================================================================
  ds.res <- summaryBy(data = ds, res ~ sbj + targetGratingOrientatioinChange + effectDirection, FUN = c(mean, std.error))
  # names(ds.res) <- c('sbj', 'OriRange', 'effectType', 'res.m', 'res.std')
  
  # calculate mean & se of responses in whole group
  ds.res.m <- summaryBy(data = ds, res ~ targetGratingOrientatioinChange + effectDirection, FUN = c(mean, std.error))
  ds.res.m<-cbind(sbj=0,ds.res.m)
  ds.res.m <- rbind(ds.res, ds.res.m)
  names(ds.res.m) <- c('sbj', 'OriRange', 'effectType', 'res.m', 'res.std')
  list.ds.res.m <- append(list.ds.res.m, list(ds.res.m))
  rm(ds.res); rm(ds.res.m);
  
  # #=============================================================
  # # Condition 4: Without Regarding Rotating Variance============
  # #=============================================================
  # ds.res <- summaryBy(data = ds, res ~ sbj + effectDirection + resType, FUN = c(mean, std.error))
  # # names(ds.res) <- c('sbj', 'OriRange', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
  # 
  # # calculate mean & se of responses in whole group
  # ds.res.m <- summaryBy(data = ds, res ~ effectDirection + resType, FUN = c(mean, std.error))
  # ds.res.m<-cbind(sbj=0,ds.res.m)
  # ds.res.m <- rbind(ds.res, ds.res.m)
  # names(ds.res.m) <- c('sbj', 'effectType', 'resType', 'res.m', 'res.std')
  # list.ds.res.m <- append(list.ds.res.m, list(ds.res.m))
  # rm(ds.res); rm(ds.res.m);
  
  #=================================
  # Select Data Merging Condition===
  #=================================
  ds.res.m <- list.ds.res.m[[analysisType.condition]]
  # if (drawWithoutRagardingRotatingVariance==1){
  #   ds.res.m <- list.ds.res.m[[4]]
  # }
  
  #=============================================================================
  # Generate Theoretical Responses & Add to Experimental Responses Data=========
  #=============================================================================
  if (analysisType.relativity==0){
    ds.res.Theoretical.m<-ds.res.m
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==-2,]$res.m <- 0
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==-1,]$res.m <- 0.5 - (0.5/2)
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==-0.5,]$res.m <- 0.5 - (0.5/3)
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==0,]$res.m <- 0.5
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==0.5,]$res.m <- 0.5 + (0.5/3)
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==1,]$res.m <- 0.5 + (0.5/2)
    ds.res.Theoretical.m[ds.res.Theoretical.m$OriRange==2,]$res.m <- 1
    ds.res.Theoretical.m$res.std <- 0
    
    # ds.res.Theoretical.m[ds.res.Theoretical.m$effectType %in% c(4,6,8,10),]$effectType<-990
    # ds.res.Theoretical.m[ds.res.Theoretical.m$effectType %in% c(5,7,9,11),]$effectType<-991
    ds.res.Theoretical.m$effectType<-99
    
    ds.res.m <- rbind(ds.res.m, ds.res.Theoretical.m)
  }
  
  # #================================================
  # # Manipulate For Graph Of Combinding All=========
  # #================================================
  # if (reOrganizeAxis %in% c(1)){
  #   if (length(intersect(c('2a', '2b', '2c', '3b', '3c'), targetData))>0){
  #     # Method 2 (2017.11.06 ~)========================================================================
  #     # # clockwise surround, horizontal, repulsion==============
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res
  #     # ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==0 & ds.res.m$effectType %in% c(4,6,8,10),]$res.m <- 
  #     # # clockwise surround, horizontal, attraction==============
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange*(-1)
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res
  #     ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==0 & ds.res.m$effectType %in% c(5,7,9,11),]$OriRange <- (-1)*ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==0 & ds.res.m$effectType %in% c(5,7,9,11),]$OriRange
  #     ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==0 & ds.res.m$effectType %in% c(5,7,9,11),]$res.m <- 1 - ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==0 & ds.res.m$effectType %in% c(5,7,9,11),]$res.m
  #     # # clockwise surround, vertical, repulsion==============
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange*(-1)
  #     # clockwise surround, vertical, attraction==============
  #     # ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res <- 2* - ds[ds$stimOrientationDifference %in% c(4,6,8,10) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res
  #     ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==1 & ds.res.m$effectType %in% c(5,7,9,11),]$res.m <- 2*ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==1 & ds.res.m$effectType %in% c(990),]$res.m - ds.res.m[ds.res.m$effectDirection %in% c(4,6,8,10) & ds.res.m$resType==1 & ds.res.m$effectType %in% c(5,7,9,11),]$res.m
  #     
  #     # 
  #     # # counterclockwise surround, horizontal, repulsion==============
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$targetGratingOrientatioinChange*(-1)
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(4,6,8,10),]$res
  #     # # counterclockwise surround, horizontal, attraction==============
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==0 & ds$effectDirection %in% c(5,7,9,11),]$res
  #     # # counterclockwise surround, vertical, repulsion==============
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(4,6,8,10),]$res
  #     # # counterclockwise surround, vertical, attraction==============
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange <- ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$targetGratingOrientatioinChange*(-1)
  #     # ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res <- 1 - ds[ds$stimOrientationDifference %in% c(5,7,9,11) & ds$resType==1 & ds$effectDirection %in% c(5,7,9,11),]$res
  #     # ds.res.m[ds.res.m$effectType %in% c(990, 991),]$effectType<-99
  #   }
  # }
  
  #=============================================
  # Select Partial Data In Whole Data===========
  #=============================================
  
  #=======================================
  # Select Effect Type of Illusion========
  #=======================================
  ds.res.m<-ds.res.m[ds.res.m$effectType %in% selectedEffectType,]
  
  
  #================================================
  # Manipulate For Graph Of Combinding All=========
  #================================================
  if (reOrganizeAxis %in% c(1)){
    # transfrom y-axis for convenience
    ds.res.m$res.m <- ds.res.m$res.m - 0.5
    ds.res.m$res.m <- ds.res.m$res.m * 2
    ds.res.m$res.std <- ds.res.m$res.std * 2
  }
  
  #=============================================
  # Absolute Y-Axis Or Relative Y-Axis==========
  #=============================================
  OriRangeVector <- c(-2, -1, -0.5, 0, 0.5, 1, 2)
  if (analysisType.relativity==1){
    for (thisOriRange in OriRangeVector){
      if (length(intersect(targetData, c('2a', '2b','2c', '3b', '3c'))) > 0){
      ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType %in% c(4,6,8,10),]$res.m <- ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType %in% c(4,6,8,10),]$res.m - ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==3,]$res.m
      ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType %in% c(5,7,9,11),]$res.m <- ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType %in% c(5,7,9,11),]$res.m - ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==3,]$res.m
      }
      if (length(intersect(targetData, c('1a', '1b'))) > 0){
        ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==1,]$res.m <- ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==1,]$res.m - ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==0,]$res.m
        ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==2,]$res.m <- ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==2,]$res.m - ds.res.m[ds.res.m$OriRange==thisOriRange & ds.res.m$effectType==0,]$res.m
      }
    }
    ds.res.m <- ds.res.m[ds.res.m$effectType!=3,]
    if (length(intersect(targetData, c('1a', '1b'))) > 0){
      ds.res.m <- ds.res.m[ds.res.m$effectType!=0,]
    }
  }  
  # effectType 3: standard (no surround)
  # effectType 4: small orientation differece between center & surround (repulsion)
  # effectType 5: big orientation differece between center & surround (attraction)
  
  #================================================
  # Manipulate For Graph Of Relativity=============
  #================================================
  if (analysisType.relativity==1){
    ds.res.m$res.m <- ds.res.m$res.m / 2
    ds.res.m$res.std <- ds.res.m$res.std / 2
  }
  
  #=======================================================================
  # Manipulate For Drawing Graph Without Regarding Roating Variance=======
  #=======================================================================
  if (drawWithoutRagardingRotatingVariance==1){
    ds.res.m <- ds.res.m[ds.res.m$sbj!=0,] # select data except total
    if (analysisType.condition==1){
      ds.res.m <- summaryBy(data = ds.res.m, res.m ~ sbj + whichEye + effectType + effectDirection + resType, FUN = c(mean, std.error))
      names(ds.res.m) <- c('sbj', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
      ds.res.m.m <- summaryBy(data = ds.res.m, res.m ~ whichEye + effectType + effectDirection + resType, FUN = c(mean, std.error))
      ds.res.m.m <- cbind(sbj=0, ds.res.m.m)
      names(ds.res.m.m) <- c('sbj', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
      ds.res.m <- rbind(ds.res.m,ds.res.m.m)
    }
    if (analysisType.condition==2){
      ds.res.m <- summaryBy(data = ds.res.m, res.m ~ sbj + effectType + resType, FUN = c(mean, std.error))
      names(ds.res.m) <- c('sbj', 'effectType', 'resType', 'res.m', 'res.std')
      ds.res.m.m <- summaryBy(data = ds.res.m, res.m ~ effectType + resType, FUN = c(mean, std.error))
      ds.res.m.m <- cbind(sbj=0, ds.res.m.m)
      names(ds.res.m.m) <- c('sbj', 'effectType', 'resType', 'res.m', 'res.std')
      ds.res.m <- rbind(ds.res.m,ds.res.m.m)
    }
    if (analysisType.condition==3){
      ds.res.m <- summaryBy(data = ds.res.m, res.m ~ sbj + effectType, FUN = c(mean, std.error))
      names(ds.res.m) <- c('sbj', 'effectType', 'res.m', 'res.std')
      ds.res.m.m <- summaryBy(data = ds.res.m, res.m ~ effectType, FUN = c(mean, std.error))
      ds.res.m.m <- cbind(sbj=0, ds.res.m.m)
      names(ds.res.m.m) <- c('sbj', 'effectType', 'res.m', 'res.std')
      ds.res.m <- rbind(ds.res.m,ds.res.m.m)
    }
    ds.res.m$res.m <- ds.res.m$res.m * 2
    ds.res.m$res.std <- ds.res.m$res.std * 2
  }
  
  # names(ds.res.m) <- c('sbj', 'OriRange', 'whichEye', 'effectType', 'effectDirection', 'resType', 'res.m', 'res.std')
  # names(ds.res.m) <- c('sbj', 'OriRange', 'effectType', 'res.m', 'res.std')
  #====================================================================
  # select data for drawing plot#======================================
  #====================================================================
  if (drawOnlyTotal==1){
    ds.res.m <- ds.res.m[ds.res.m$sbj==0,]
  }
  
  #====================================================================
  # Coding Names of Variables To More Convenient Form==================
  #====================================================================
  
  # convert scalar names to stings
  ds.res.m[ds.res.m$sbj > 9,]$sbj<-paste("참가자", ds.res.m[ds.res.m$sbj > 9,]$sbj) # I don't know exact reason, but any characterized numbers are treated as bigger than any number (even than 10000000), so you should change the number bigger than 9 to character first.
  ds.res.m[ds.res.m$sbj <= 9,]$sbj<-paste("참가자 0", ds.res.m[ds.res.m$sbj <= 9,]$sbj, sep = "") # When you change 
  ds.res.m[ds.res.m$sbj=="참가자 00",]$sbj<-"평균"
  
  if (analysisType.condition %in% c(1,2)){
    ds.res.m[ds.res.m$resType==0,]$resType<-"수평 자극에 대한 반응"
    ds.res.m[ds.res.m$resType==1,]$resType<-"수직 자극에 대한 반응"
  }
  
  # ds.res.m$OriRange<-as.character(ds.res.m$OriRange)
  if (drawWithoutRagardingRotatingVariance==0){ds.res.m$OriRange<-as.factor(ds.res.m$OriRange)}
  
  # c('Samll Orientation Difference Between Target and Surround Without Binocular Rivalry',
  #   'Big Orientation Difference Between Target and Surround Without Binocular Rivalry',
  #   'No Surround Without Binocular Rivalry')
  
  names.pool.legend <- c('작은 방위 차(15˚)',
                         '큰 방위 차(75˚)',
                         '주변 자극 없음',
                         'Small Ori Diff In B.R.',
                         'Big Ori Diff In B.R.',
                         'No Surround In B.R.',
                         'Small Ori Diff In I.O.',
                         'Big Ori Diff In I.O.',
                         'Small Ori Diff In I.O Without B.R.',
                         'Big Ori Diff In I.O. Without B.R.',
                         'Small Ori Diff In B.R Without B.R.',
                         'Big Ori Diff In B.R. Without B.R.')
  
  if (drawWithoutRagardingRotatingVariance==1){
    names.pool.legend <- c('Small(15˚)',
                           'Big(75˚)',
                           'No Surround Without B.R.',
                           'Small(15˚)',
                           'Big(75˚)',
                           'No Surround In B.R.',
                           'Small(15˚)',
                           'Big(75˚)',
                           'Small(15˚)',
                           'Big(75˚)',
                           'Small(15˚)',
                           'Big(75˚)')
  }
  
  
  index.axis.standard <- NULL
  index.axis.conditions <- NULL
  if (analysisType.relativity %in% c(0)){
    ds.res.m[ds.res.m$effectType %in% c(99),]$effectType<-"이론적(선형 증가)"
    if (length(intersect(targetData, c('2a'))) > 0){
      ds.res.m[ds.res.m$effectType==3,]$effectType<-names.pool.legend[6]
      index.axis.standard <- append(index.axis.standard, c(6))
    }
    if (length(intersect(targetData, c('1a'))) > 0){
      ds.res.m[ds.res.m$effectType==0,]$effectType<-names.pool.legend[3]
      index.axis.standard <- append(index.axis.standard, c(3))
    }
  }
  
  # make string index to character
  if (length(intersect(targetData, c('1b'))) > 0){ # if there any components in targetData are involved in c('1a', '1b')
    ds.res.m[ds.res.m$effectType==1,]$effectType<-names.pool.legend[1]
    ds.res.m[ds.res.m$effectType==2,]$effectType<-names.pool.legend[2]
    index.axis.conditions <- append(index.axis.conditions, c(1))
    index.axis.conditions <- append(index.axis.conditions, c(2))
  }
  
  if (length(intersect(targetData, c('2b'))) > 0){
    ds.res.m[ds.res.m$effectType %in% c(4),]$effectType<-names.pool.legend[4]
    ds.res.m[ds.res.m$effectType %in% c(5),]$effectType<-names.pool.legend[5]
    index.axis.conditions <- append(index.axis.conditions, c(4))
    index.axis.conditions <- append(index.axis.conditions, c(5))
  }
  if (length(intersect(targetData, c('2c'))) > 0){
    ds.res.m[ds.res.m$effectType %in% c(10),]$effectType<-names.pool.legend[11]
    ds.res.m[ds.res.m$effectType %in% c(11),]$effectType<-names.pool.legend[12]
    index.axis.conditions <- append(index.axis.conditions, c(11))
    index.axis.conditions <- append(index.axis.conditions, c(12))
  }
  if (length(intersect(targetData, c('3b'))) > 0){
    ds.res.m[ds.res.m$effectType %in% c(6),]$effectType<-names.pool.legend[7]
    ds.res.m[ds.res.m$effectType %in% c(7),]$effectType<-names.pool.legend[8]
    index.axis.conditions <- append(index.axis.conditions, c(7))
    index.axis.conditions <- append(index.axis.conditions, c(8))
  }
  if (length(intersect(targetData, c('3c'))) > 0){
    ds.res.m[ds.res.m$effectType %in% c(8),]$effectType<-names.pool.legend[9]
    ds.res.m[ds.res.m$effectType %in% c(9),]$effectType<-names.pool.legend[10]
    index.axis.conditions <- append(index.axis.conditions, c(9))
    index.axis.conditions <- append(index.axis.conditions, c(10))
  }
  
  # if (analysisType.relativity==1){
  #   # make string index to character
  #   if (length(intersect(targetData, c('1a', '1b'))) > 0){
  #     ds.res.m[ds.res.m$effectType==1,]$effectType<-names.pool.legend[2]
  #     ds.res.m[ds.res.m$effectType==2,]$effectType<-names.pool.legend[3]
  #   }
  #   if (length(intersect(targetData, c('2a', '2b', '2c', '3b', '3c'))) > 0){
  #     if (length(intersect(targetData, c('2a'))) > 0){
  #       ds.res.m[ds.res.m$effectType==3,]$effectType<-names.pool.legend[4]
  #     }
  #     if (length(intersect(targetData, c('2b'))) > 0){
  #       ds.res.m[ds.res.m$effectType %in% c(4),]$effectType<-names.pool.legend[5]
  #       ds.res.m[ds.res.m$effectType %in% c(5),]$effectType<-names.pool.legend[6]
  #     }
  #     if (length(intersect(targetData, c('2c'))) > 0){
  #       ds.res.m[ds.res.m$effectType %in% c(10),]$effectType<-names.pool.legend[11]
  #       ds.res.m[ds.res.m$effectType %in% c(11),]$effectType<-names.pool.legend[12]
  #       index.axis.conditions <- 10
  #     }
  #     if (length(intersect(targetData, c('3b'))) > 0){
  #       ds.res.m[ds.res.m$effectType %in% c(6),]$effectType<-names.pool.legend[7]
  #       ds.res.m[ds.res.m$effectType %in% c(7),]$effectType<-names.pool.legend[8]
  #     }
  #     if (length(intersect(targetData, c('3c'))) > 0){
  #       ds.res.m[ds.res.m$effectType %in% c(8),]$effectType<-names.pool.legend[9]
  #       ds.res.m[ds.res.m$effectType %in% c(9),]$effectType<-names.pool.legend[10]
  #     }
  #   }
  # }
  
  #=================================
  # Drawing Plots===================
  #=================================
  
  #============================================================
  # Restrict Rotation Variation Condition Only in Zero=========
  #============================================================
  # applying drawOnlyZeroDegree condition
  if (drawOnlyZeroDegree==1){
    ds.res.m <- ds.res.m[ds.res.m$OriRange==0,] # restrict rotation variation condtion in zero (no any rotating, just vertcal and horizontal)
    ds.res.m <- ds.res.m[ds.res.m$effectType!='Theoretical',] # (erase the theoretical condition for visibility)
  }
  
  # split data frame according to counterbalancing conditions
  if (length(intersect(c('2a', '2b', '2c', '3b', '3c'), targetData)) > 0){
    data <- list(ds.res.m[ds.res.m$whichEye==1 & ds.res.m$effectDirection %in% c(3,4,6,8,10),],
                 ds.res.m[ds.res.m$whichEye==0 & ds.res.m$effectDirection %in% c(3,4,6,8,10),],
                 ds.res.m[ds.res.m$whichEye==1 & ds.res.m$effectDirection %in% c(3,5,7,9,11),],
                 ds.res.m[ds.res.m$whichEye==0 & ds.res.m$effectDirection %in% c(3,5,7,9,11),],
                 ds.res.m)
    
    # names of each split graphs
    titles <- c("L:Vert, R:Horz & Clockwise Surround",
                "L:Horz, R:Vert & Clockwise Surround",
                "L:Vert, R:Horz & Counter-Clockwise Surround",
                "L:Horz, R:Vert & Counter-Clockwise Surround",
                "Combinding Counterbalancing Conditions")
  }
  if (length(intersect(c('1a', '1b'), targetData)) > 0){
    data <- list(ds.res.m[ds.res.m$effectDirection %in% c(0,1),], # 0: standard / 1: clockwise (60)
                 ds.res.m[ds.res.m$effectDirection %in% c(0,2),], # 0: standard / 2: counter-clockwise (120 or 240)
                 ds.res.m)
    
    # names of each split graphs
    titles <- c("Clockwise Surround",
                "Counter-Clockwise Surround",
                "Combinding Counterbalancing Conditions")
  }
  
  # determine the number of drawing graphs
  nData <- length(lengths(data))
  if (analysisType.condition %in% c(2,3,4)){
    selectedDataForGraph<-length(lengths(data))
  }
  if (analysisType.condition %in% c(1)){
    selectedDataForGraph<-c(1:(length(lengths(data))-1))
  }
  
  # names of each split graphs
  titles <- c("L:Vert, R:Horz & Clockwise Surround",
              "L:Horz, R:Vert & Clockwise Surround",
              "L:Vert, R:Horz & Counter-Clockwise Surround",
              "L:Horz, R:Vert & Counter-Clockwise Surround",
              "Combinding Counterbalancing Conditions")
  
  # determine the colors of graphs according to drawing options
  if (analysisType.relativity == 0){
    dataColors <- c("#a50f15", "#525252", "#2171b5", "#bdbdbd") # red, black, blue, gray
    if (drawOnlyStandard == 1){
      dataColors <- c("#bdbdbd", "#525252") # red, black, blue, gray
    }
  }
  if (analysisType.relativity == 1){
    dataColors <- c("#a50f15", "#2171b5")
  }
  
  if (length(intersect(targetData, c('2b', '2c', '3b','3c'))) > 1){ # when at least two of elements are including in targetData
    dataColors <- c("#a50f15", "#fcbba1", "#525252", "#9ecae1", "#2171b5", "#bdbdbd") # red, light red, black, light blue, blue, gray
    if (analysisType.relativity == 1){
      dataColors <- c("#a50f15", "#fcbba1", "#9ecae1", "#2171b5") # red, light red, black, light blue, blue, gray
    }
  }
  
  # default background color of graph
  backgroundColorForMean <- '#ffffe5'
  
  # set position of reference line in axis X & Y
  if (reOrganizeAxis %in% c(0)){
    referenceLine.y <- 0.5
  }
  if (reOrganizeAxis %in% c(1)){
    referenceLine.y <- 0
  }
  
  # set legend names
  names.legend <- c('Theoretical', names.pool.legend[index.axis.standard], names.pool.legend[index.axis.conditions])
  if (drawWithoutRagardingRotatingVariance==1){
    names.legend <- c(names.pool.legend[index.axis.standard], names.pool.legend[index.axis.conditions])
  }
  
  # set axis names
  axisNameForY <- '시계 방향으로 반응한 비율'
  axisNameForX <- '실제 중심 자극의 방위 변산'
  if (drawWithoutRagardingRotatingVariance==1){
    axisNameForY <- 'Magnitude Of The Illusion'
    axisNameForX <- 'Relative Orienation Differece Between The Target and The Surround'
  }
  
  # set size parameters in graphs
  legend.size <- 14
  stripX.size <- 13
  stripY.size <- 13
  x.text.size <- 13
  y.text.size <- 13
  x.title.size <- 15
  y.title.size <- 15
  
  # draw graphs
  graphs=list()
  library(ggplot2) # package for single plots
  library(cowplot) # package for background_grid & multiplot; function: plot_grid data[[thisGraph]]
  for (thisGraph in 1:nData){
    if (plotType==1){ # line plot
      if (analysisType.condition==1){
        graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, group = effectType, colour= effectType)) +
          geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
          geom_line(size = 1.5, aes(color=effectType)) + geom_point(size = 3, shape=21, fill = "white", aes(color=effectType)) +
          geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), width = .3, size = 0.7) +
          facet_grid(sbj ~ resType, scales="fixed") +
          panel_border() + # and a border around each panel
          xlab(axisNameForX) +
          ylab(axisNameForY) +
          ggtitle(titles[thisGraph]) +
          scale_color_manual(guide = guide_legend(title = "자극 종류"),
                             breaks = names.legend,
                             labels = names.legend,
                             values = dataColors) + # red, black, blue, gray
          geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
          geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
          theme(panel.background = element_rect(fill = "#ffffff"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(size=legend.size),
                strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                axis.text.x = element_text(size=x.text.size),
                axis.text.y = element_text(size=y.text.size),
                axis.title.x = element_text(size=x.title.size),
                axis.title.y = element_text(size=y.title.size)) +
          background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
      }
      if (analysisType.condition==2){
        graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, group = effectType, colour= effectType)) +
          geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
          geom_line(size = 1.5, aes(color=effectType)) + geom_point(size = 3, shape=21, fill = "white", aes(color=effectType)) +
          geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), width = .3, size = 0.7) +
          facet_grid(sbj ~ resType, scales="fixed") +
          panel_border() + # and a border around each panel
          xlab(axisNameForX) +
          ylab(axisNameForY) +
          ggtitle(titles[length(titles)]) +
          scale_color_manual(guide = guide_legend(title = "자극 종류"),
                             breaks = names.legend,
                             labels = names.legend,
                             values = dataColors) + # red, black, blue, gray
          geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
          geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
          theme(panel.background = element_rect(fill = "#ffffff"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(size=legend.size),
                strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                axis.text.x = element_text(size=x.text.size),
                axis.text.y = element_text(size=y.text.size),
                axis.title.x = element_text(size=x.title.size),
                axis.title.y = element_text(size=y.title.size)) +
          background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
      }
      if (analysisType.condition==3){
        graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, group = effectType, colour= effectType)) +
          geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
          geom_line(size = 1.5, aes(color=effectType)) + geom_point(size = 3, shape=21, fill = "white", aes(color=effectType)) +
          geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), width = .3, size = 0.7) +
          facet_grid(sbj ~ ., scales="fixed") +
          panel_border() + # and a border around each panel
          xlab(axisNameForX) +
          ylab(axisNameForY) +
          ggtitle(titles[length(titles)]) +
          scale_color_manual(guide = guide_legend(title = "자극 종류"),
                             breaks = names.legend,
                             labels = names.legend,
                             values = dataColors) + # red, black, blue, gray
          geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
          geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) + # When index variable for reference line is factor variable, you should indicate location of reference line as ordinal number (ex - first, second, third, ...).
          theme(panel.background = element_rect(fill = "#ffffff"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(size=legend.size),
                strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                axis.text.x = element_text(size=x.text.size),
                axis.text.y = element_text(size=y.text.size),
                axis.title.x = element_text(size=x.title.size),
                axis.title.y = element_text(size=y.title.size)) +
          background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
      }
    }
    if (plotType==2){ # bar plot
      if (drawWithoutRagardingRotatingVariance==0){ # regarding rotating variance
        if (analysisType.condition==1){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, colour= effectType, width=.75)) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(fill="white", stat="identity", position=position_dodge(), size=1.5) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(.8), width = .3, size = 0.7) +
            facet_grid(sbj ~ resType, scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            ggtitle(titles[thisGraph]) +
            scale_color_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
        }
        if (analysisType.condition==2){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, colour= effectType, width=.75)) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(fill="white", stat="identity", position=position_dodge(), size=1.5) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(.8), width = .3, size = 0.7) +
            facet_grid(sbj ~ resType, scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            ggtitle(titles[length(titles)]) +
            scale_color_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)# add thin horizontal lines, #969696
        }
        if (analysisType.condition==3){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = OriRange, y=res.m, colour= effectType, width=.75)) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(fill="white", stat="identity", position=position_dodge(), size=1.5) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(.8), width = .3, size = 0.7) +
            facet_grid(sbj ~ ., scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            ggtitle(titles[length(titles)]) +
            scale_color_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) + # When index variable for reference line is factor variable, you should indicate location of reference line as ordinal number (ex - first, second, third, ...).
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
        }
      }
      if (drawWithoutRagardingRotatingVariance==1){ # without regarding rotating variance
        if (analysisType.condition==1){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = effectType, y=res.m, colour= effectType, width=1.0)) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(colour="black", stat="identity", width = 0.6, size=1.0, position=position_dodge(width=0.1)) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(.8), width = .3, size = 0.7) +
            facet_grid(sbj ~ resType, scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            theme(legend.position='none') +
            ggtitle(titles[thisGraph]) +
            scale_fill_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
        }
        dodge <- position_dodge(width = 0.5)
        if (analysisType.condition==2){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = effectType, y=res.m, fill= effectType)) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(colour="black", stat="identity", width = 0.6, size=1.0, position=position_dodge(width=0.1)) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(), width = .3, size = 0.7) +
            facet_grid(sbj ~ resType, scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            theme(legend.position='none') +
            # scale_fill_brewer(palette='Pastel1') +
            ggtitle(titles[length(titles)]) +
            scale_fill_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) +
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)# add thin horizontal lines, #969696
        }
        if (analysisType.condition==3){
          graphs[[thisGraph]] <- ggplot(data = data[[thisGraph]], aes(x = effectType, y=res.m, colour= effectType, width=1.0)) +
            geom_rect(data = subset(ds.res.m,sbj == 'Total'),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.1, fill=backgroundColorForMean) +
            geom_bar(colour="black", stat="identity", width = 0.6, size=1.0, position=position_dodge(width=0.1)) +
            geom_errorbar(aes(ymin=res.m-res.std, ymax=res.m+res.std), position=position_dodge(.8), width = .3, size = 0.7) +
            facet_grid(sbj ~ ., scales="fixed") +
            panel_border() + # and a border around each panel
            xlab(axisNameForX) +
            ylab(axisNameForY) +
            ylim(-1,1) +
            theme(legend.position='none') +
            ggtitle(titles[length(titles)]) +
            scale_fill_manual(guide = guide_legend(title = "자극 종류"),
                               breaks = names.legend,
                               labels = names.legend,
                               values = dataColors) + # red, black, blue, gray
            geom_hline(yintercept = referenceLine.y, linetype = 'dashed', color='#969696', size = 1) +
            geom_vline(data=data.frame(xint=4, sbj=ds.res.m$sbj), aes(xintercept=xint), linetype = 'dashed', color='#969696', size = 1) + # When index variable for reference line is factor variable, you should indicate location of reference line as ordinal number (ex - first, second, third, ...).
            theme(panel.background = element_rect(fill = "#ffffff"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size=legend.size),
                  strip.text.y = element_text(size = stripY.size, colour = "black", angle = 0),
                  strip.text.x = element_text(size = stripX.size, colour = "black", angle = 0),
                  axis.text.x = element_text(size=x.text.size),
                  axis.text.y = element_text(size=y.text.size),
                  axis.title.x = element_text(size=x.title.size),
                  axis.title.y = element_text(size=y.title.size)) +
            background_grid(major = 'xy', minor = "none", colour.major = "#d9d9d9",colour.minor = "#d9d9d9", size.major = 0.2, size.minor = 0.5)
        }
      }
    }
  }
  # combine single plots to single multiplot---------------------------------------------------------------
  # method 1
  nGraphs <- length(selectedDataForGraph)
  # selectedDataForGraph <- NULL
  # for (i in 1:nGraphs){
  #   if (nrow(data[[i]]$effectType)>0){
  #     selectedDataForGraph <- append(selectedDataForGraph, i)
  #   }
  # }
  # if (length(intersect(targetData, c('2c','3c'))) > 0){
  #   selectedDataForGraph <- c(2,4)
  # }
  library(ggpubr) # package for multiplot; function: ggaarange
  if (nGraphs>1){
    multiplots <- ggarrange(plotlist = graphs[selectedDataForGraph], labels = c("A", "B", "C", "D", "E"), ncol=ceiling((length(selectedDataForGraph)-0.5)/2), nrow=round((length(selectedDataForGraph)+1)/2), common.legend = TRUE, legend="bottom")
  } else if (nGraphs==1){
    multiplots <- graphs[[selectedDataForGraph]]
  }
  multiplots