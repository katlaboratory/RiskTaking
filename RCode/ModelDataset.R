library(readr)
#library(nlme)
library(lme4)
library(stats)
library(dplyr)
library("Hmisc")
library(doParallel)
library(foreach)
library(parallel)
library(neldermead)
library(beepr)
source("FolderPath.R")

baselinePreDataX<-read_csv(paste(folderPath, "Data\\ClickByClickData.csv", sep=""))


model.data.GetModelData_0<-function(data)
{
  data_2<-filter(data)
  nrow(data_2)
  probPerIDAndCondition_2<- data_2 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE))
  head(probPerIDAndCondition_2, n=50L)
  probPerID_2<- data_2 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE))
  nrow(probPerID_2)
  head(probPerID_2, n=10L)
  
  data_2<-merge(x=data_2,
                y=probPerIDAndCondition_2, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  
  data_2<-merge(x=data_2,
                y=probPerID_2, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  
  nrow(data_2)
  head(data_2, n=50L)

   return (list(d1=data_2))
}

model.data.GetModelData_22<-function(data , boundaryTrialNumber)
{
  minTrialOrder<-min(data$trialOrder, na.rm=TRUE)
  data_2<-filter(data,  trialOrder-minTrialOrder+1>boundaryTrialNumber)
  nrow(data_2)
  probPerIDAndCondition_2<- data_2 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE),
                     avgPumps=mean(nbPumps*isCollected, na.rm=TRUE),
                     sdPumps=sd(nbPumps*isCollected, na.rm=TRUE))
  head(probPerIDAndCondition_2, n=50L)
  probPerID_2<- data_2 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE),
                     avgPumps=mean(nbPumps*isCollected, na.rm=TRUE),
                     sdPumps=sd(nbPumps*isCollected, na.rm=TRUE))
  nrow(probPerID_2)
  head(probPerID_2, n=10L)
  
  data_2<-merge(x=data_2,
                y=probPerIDAndCondition_2, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  
  data_2<-merge(x=data_2,
                y=probPerID_2, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  
  nrow(data_2)
  head(data_2, n=50L)
  
  print(paste("boundaryTrialNumber: ",boundaryTrialNumber))
  data_1<-filter(data, trialOrder-minTrialOrder+1<=boundaryTrialNumber)
  probPerIDAndCondition_1<- data_1 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE),
                     avgPumps=mean(nbPumps*isCollected, na.rm=TRUE),
                     sdPumps=sd(nbPumps*isCollected, na.rm=TRUE))
  head(probPerIDAndCondition_1, n=50L)
  probPerID_1<- data_1 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE),
                     avgLastTrialTime = mean(lastTrialTime , na.rm = TRUE),
                     sdLastTrialTime = sd(lastTrialTime , na.rm = TRUE),
                     avgPumps=mean(nbPumps*isCollected, na.rm=TRUE),
                     sdPumps=sd(nbPumps*isCollected, na.rm=TRUE))
  nrow(probPerID_1)
  head(probPerID_1, n=10L)
  
  
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerIDAndCondition_1, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerID_1, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  print(paste(nrow(data_1),nrow(data_2)))
  return (list(d1=data_1, d2=data_2))
}

model.data.GetModelData_14<-function(data , boundaryTrialNumber)
{
  minTrialOrder<-min(data$trialOrder, na.rm=TRUE)
  data_2<-filter(data,  trialOrder-minTrialOrder+1>boundaryTrialNumber)
  nrow(data_2)
  probPerIDAndCondition_2<- data_2 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)))
  head(probPerIDAndCondition_2, n=50L)
  probPerID_2<- data_2 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE))
  nrow(probPerID_2)
  head(probPerID_2, n=10L)
  
  data_2<-merge(x=data_2,
                y=probPerIDAndCondition_2, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  
  data_2<-merge(x=data_2,
                y=probPerID_2, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  
  nrow(data_2)
  head(data_2, n=50L)
  
  print(paste("boundaryTrialNumber: ",boundaryTrialNumber))
  data_1<-filter(data, trialOrder-minTrialOrder+1<=boundaryTrialNumber)
  probPerIDAndCondition_1<- data_1 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE))
  head(probPerIDAndCondition_1, n=50L)
  probPerID_1<- data_1 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)),
                     prob_expl = sum(isExploded , na.rm = TRUE)/sum(nbPumps*isExploded , na.rm = TRUE))
  nrow(probPerID_1)
  head(probPerID_1, n=10L)
  
  
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerIDAndCondition_1, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerID_1, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  print(paste(nrow(data_1),nrow(data_2)))
  return (list(d1=data_1, d2=data_2))
}

model.data.GetModelData<-function(data , boundaryTrialNumber)
{
  minTrialOrder<-min(data$trialOrder, na.rm=TRUE)
  data_2<-filter(data,  trialOrder-minTrialOrder+1>boundaryTrialNumber)
  nrow(data_2)
  probPerIDAndCondition<- data_2 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)))
  head(probPerIDAndCondition, n=50L)
  probPerID<- data_2 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)))
  nrow(probPerID)
  head(probPerID, n=10L)
  
  data_2<-merge(x=data_2,
                            y=probPerIDAndCondition, 
                            by.x=c("ID","experimentClass"), 
                            by.y=c("ID","experimentClass"), all.x = TRUE) 
  
  data_2<-merge(x=data_2,
                            y=probPerID, 
                            by.x=c("ID"), 
                            by.y=c("ID"), all.x = TRUE) 
  
  nrow(data_2)
  head(data_2, n=50L)
  print(paste("boundaryTrialNumber: ",boundaryTrialNumber))
  data_1<-filter(data, trialOrder-minTrialOrder+1<=boundaryTrialNumber)
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                            y=probPerIDAndCondition, 
                            by.x=c("ID","experimentClass"), 
                            by.y=c("ID","experimentClass"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                            y=probPerID, 
                            by.x=c("ID"), 
                            by.y=c("ID"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  print(paste(nrow(data_1),nrow(data_2)))
  return (list(d1=data_1, d2=data_2))
}

model.data.GetModelDataCondition<-function(data , boundaryTrialNumber)
{
  minTrialOrder<-min(data$trialOrder, na.rm=TRUE)
  data_2<-filter(data, trialOrder-minTrialOrder+1>boundaryTrialNumber)
  nrow(data_2)
  probPerIDAndCondition<- data_2 %>%
    dplyr::group_by(ID, experimentClass) %>%
    dplyr::summarise(nbTr = length(trialNumber), prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)))
  head(probPerIDAndCondition, n=50L)
  probPerID<- data_2 %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(prob_r = sum(nbPumps , na.rm = TRUE)/( sum(nbPumps , na.rm = TRUE) +  sum(isCollected , na.rm = TRUE)))
  nrow(probPerID)
  head(probPerID, n=10L)
  
  data_2<-merge(x=data_2,
                y=probPerIDAndCondition, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  
  data_2<-merge(x=data_2,
                y=probPerID, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  
  nrow(data_2)
  head(data_2, n=50L)
  print(paste("boundaryTrialNumber: ",boundaryTrialNumber))
  data_1<-filter(data, trialOrder-minTrialOrder+1<=boundaryTrialNumber)
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerIDAndCondition, 
                by.x=c("ID","experimentClass"), 
                by.y=c("ID","experimentClass"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  data_1<-merge(x=data_1,
                y=probPerID, 
                by.x=c("ID"), 
                by.y=c("ID"), all.x = TRUE) 
  print(paste("data_1 rows: ", nrow(data_1)))
  print(paste(nrow(data_1),nrow(data_2)))
  return (list(d1=data_1, d2=data_2))
}

