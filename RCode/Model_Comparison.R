rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("FolderPath.R")
############################# LIBRARIES AND SOURCES ###################################################
library(readr)
library(nlme)
library(lme4)
library(stats)
library(dplyr)
library("Hmisc")
library(ggplot2)
library(nortest)
library(usdm)
library(leaps)
library(car)
library(gridExtra)
library(stringr)
library(lsmeans)
library(ppcor)
library(varhandle)
library(merTools)
library(RColorBrewer)
library(ltm)
library(tibble)
library(brms)
source(paste(folderPath,"RCode\\UtilityFunctions.R",sep=""))

###### MODEL DATASET ########
model0_summary <- read_csv(paste(folderPath,"Data\\model0_Ordered.csv",sep=""))
model0_summary<-data.frame(unclass(model0_summary), check.names = FALSE, stringsAsFactors = FALSE)
model0_summary<-filter(model0_summary, mle<2000 & mle!=0 )
model0_summary$pBurst_1<-100*model0_summary$pBurst_1
model0_summary$pBurst_2<-100*model0_summary$pBurst_1

model9_summary <- read_csv(paste(folderPath,"Data\\model9_Ordered.csv",sep=""))
model9_summary<-data.frame(unclass(model9_summary), check.names = FALSE, stringsAsFactors = FALSE)

model9_summary<-filter(model9_summary, mle<2000 & mle!=0 & gammaPlus<=5 & gammaPlus>0.001 & beta<0.7)
model9_summary$pBurst_1<-100*model9_summary$pBurst_1
model9_summary$pBurst_2<-100*model9_summary$pBurst_2


model22_summary <- read_csv(paste(folderPath,"Data\\model22_Ordered.csv",sep=""))
model22_summary<-data.frame(unclass(model22_summary), check.names = FALSE, stringsAsFactors = FALSE)

model22_summary<-filter(model22_summary, mle<2000 & mle!=0 & gammaPlus<=5 & gammaPlus>0.001 & beta<0.7)
model22_summary$pBurst_1<-100*model22_summary$pBurst_1
model22_summary$pBurst_2<-100*model22_summary$pBurst_2

model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$avgLastTime_1 <- 
  model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$avgLastTime_2
model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$avgPumps_1 <- 
  model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$avgPumps_2
model22_summary[is.na(model22_summary$sdAvgLastTime_2) &!is.na(model22_summary$sdAvgLastTime_1),]$avgPumps_2 <- 
  model22_summary[is.na(model22_summary$sdAvgLastTime_2) &!is.na(model22_summary$sdAvgLastTime_1),]$avgPumps_1

model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$pBurst_1 <- 
  model22_summary[is.na(model22_summary$sdAvgLastTime_1) &!is.na(model22_summary$sdAvgLastTime_2),]$pBurst_2
model22_summary[is.na(model22_summary$sdAvgLastTime_2) &!is.na(model22_summary$sdAvgLastTime_1),]$pBurst_2 <- 
  model22_summary[is.na(model22_summary$sdAvgLastTime_2) &!is.na(model22_summary$sdAvgLastTime_1),]$pBurst_1


modelComparison<-merge(model22_summary,model9_summary, by.x=c("ID","order"), by.y=c("ID","order"), all.x=TRUE)
modelComparison<-merge(modelComparison,model0_summary, by.x=c("ID","order"), by.y=c("ID","order"), all.x=TRUE)


model0_summary_all <- read_csv(paste(folderPath,"Data\\model0_Ordered.csv",sep=""))
model9_summary_all <- read_csv(paste(folderPath,"Data\\model9_Ordered.csv",sep=""))
model22_summary_all <- read_csv(paste(folderPath,"Data\\model22_Ordered.csv",sep=""))
modelComparison_all<-merge(model22_summary_all,model9_summary_all, by.x=c("ID","order"), by.y=c("ID","order"), all.x=TRUE)
modelComparison_all<-merge(modelComparison_all,model0_summary_all, by.x=c("ID","order"), by.y=c("ID","order"), all.x=TRUE)

##### READ DATASETS FROM FILES ################
workingDataNotS4<-read_csv(paste(folderPath,"Data\\workingDataNotS4.csv",sep=""),
                         col_types = cols(GROUP = col_factor(), STUDY=col_factor())
)
workingDataNotS4<-data.frame(unclass(workingDataNotS4), check.names = FALSE, stringsAsFactors = FALSE)

workingData_Long_NOTS4<-read_csv(paste(folderPath,"Data\\workingData_Long_NOTS4.csv",sep=""),
                              col_types = cols(GROUP = col_factor(), Dass21_Depression_Cat = col_factor()
                                               , Dass21_Anxiety_Cat = col_factor(), Dass21_Stress_Cat = col_factor()
                                               , Dass21_Depression=col_double(), Dass21_Anxiety=col_double()
                                               , Dass21_Stress=col_double()
                                               , TAS20_Diagnosis = col_character(),TAS20_DifficultyDescribingFeelings=col_double()
                                               , TAS20_DifficultyIdentifyingFeeling=col_double(), TAS20_ExternallyOrientedThinking=col_double()
                                               , TAS20_TOTAL=col_double(),BIDQ_TOTAL=col_double() , BIDQ_Concerns=col_double()
                                               , BIDQ_DISTRESS=col_double(),BIDQ_AVOIDANCE=col_double(),BIDQ_PREOCCUPATIONS=col_double()
                                               ,BIDQ_FUNCT_IMPAIR=col_double()
                                               , OCIR_TOTAL=col_double(), AN_YEARS=col_double()
                                               , STUDY=col_factor())
)
workingData_Long_NOTS4<-data.frame(unclass(workingData_Long_NOTS4), check.names = FALSE, stringsAsFactors = FALSE)

workingData_trialLong_NOTS4<-read_csv(paste(folderPath,"Data\\workingData_trialLong_NOTS4.csv",sep=""),
                                   col_types = cols(GROUP = col_factor(), Dass21_Depression_Cat = col_factor()
                                                    , Dass21_Anxiety_Cat = col_factor(), Dass21_Stress_Cat = col_factor()
                                                    , Dass21_Depression=col_double(), Dass21_Anxiety=col_double()
                                                    , Dass21_Stress=col_double()
                                                    , TAS20_Diagnosis = col_character(),TAS20_DifficultyDescribingFeelings=col_double()
                                                    , TAS20_DifficultyIdentifyingFeeling=col_double(), TAS20_ExternallyOrientedThinking=col_double()
                                                    , TAS20_TOTAL=col_double(),BIDQ_TOTAL=col_double() , BIDQ_Concerns=col_double()
                                                    , BIDQ_DISTRESS=col_double(),BIDQ_AVOIDANCE=col_double(),BIDQ_PREOCCUPATIONS=col_double()
                                                    ,BIDQ_FUNCT_IMPAIR=col_double(),BIS11_Attentional_Total=col_double()
                                                    ,BIS11_MotorTotal=col_double() , BIS11_NonPlanningTotal=col_double(),BIS11_Total =col_double() 
                                                    , OCIR_TOTAL=col_double(), AN_YEARS=col_double()
                                                    , STUDY=col_factor()))

workingData_trialLong_NOTS4<-data.frame(unclass(workingData_trialLong_NOTS4), check.names = FALSE, stringsAsFactors = FALSE)

##### MODEL DATA ANALYSIS SUMMARY #####
dtLong<-dplyr::select(workingData_Long_NOTS4, SUBJ_ID, order, experimentClass, totalPumps, nbTr, isCollected, isExploded)

#modelCompByIDCondition<- merge(modelComparison,dtLong, by.x=c("ID","order"), by.y=c("SUBJ_ID","order"), all.x=TRUE)  %>%
modelCompByIDCondition<- merge(modelComparison,dtLong, by.x=c("ID","order"), by.y=c("SUBJ_ID","order"), all.y=TRUE)  %>%
    dplyr::group_by(experimentClass) %>%
  dplyr::summarise(
    mle_22=(mean(mle.x, na.rm=TRUE)/mean(totalPumps, na.rm=TRUE)),
    mle_9=(mean(mle.y, na.rm=TRUE)/mean(totalPumps, na.rm=TRUE)),
    mle_0=(mean(mle, na.rm=TRUE)/mean(totalPumps, na.rm=TRUE))
  )
modelCompByIDCondition<-data.frame(unclass(modelCompByIDCondition), check.names = FALSE, stringsAsFactors = FALSE)
dataLongX<-merge(modelComparison_all,dtLong, by.x=c("ID","order"), by.y=c("SUBJ_ID","order"), all.x=TRUE)
#dataLongX<-merge(modelComparison_all,dtLong, by.x=c("ID","order"), by.y=c("SUBJ_ID","order"), all.y=TRUE)
nrow(dataLongX)

dataLongX$mle_22_perPump<-dataLongX$mle.x/dataLongX$totalPumps
dataLongX$mle_9_perPump<-dataLongX$mle.y/dataLongX$totalPumps
dataLongX$mle_0_perPump<-dataLongX$mle/dataLongX$totalPumps

dataLongX$m0_LLpump<-log(1-dataLongX$pBurst_1/100)
dataLongX$m0_LLcollect<-log(dataLongX$pBurst_1/100)
dataLongX$m22_1_LLpump<-log(1-dataLongX$pBurst_1.x/100)
dataLongX$m22_1_LLcollect<-log(dataLongX$pBurst_1.x/100)
dataLongX$m22_2_LLpump<-log(1-dataLongX$pBurst_2.x/100)
dataLongX$m22_2_LLcollect<-log(dataLongX$pBurst_2.x/100)

dataLongX$m0_LLpump<-log(1-dataLongX$pBurst_1)
dataLongX$m0_LLcollect<-log(dataLongX$pBurst_1)
dataLongX$m22_1_LLpump<-log(1-dataLongX$pBurst_1.x)
dataLongX$m22_1_LLcollect<-log(dataLongX$pBurst_1.x)
dataLongX$m22_2_LLpump<-log(1-dataLongX$pBurst_2.x)
dataLongX$m22_2_LLcollect<-log(dataLongX$pBurst_2.x)
length(unique(dataLongX$ID))
modelComp_dataLongX<- filter(dataLongX , !is.na(experimentClass) )%>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(
    toRemove=(mean(mle, na.rm=TRUE)/mean(totalPumps, na.rm=TRUE)),
    mle_22=(mean(mle.x, na.rm=TRUE)),
    mle_9=(mean(mle.y, na.rm=TRUE)),
    mle_0=(mean(mle, na.rm=TRUE)),
    m0_LLpump=-mean(m0_LLpump, na.rm=TRUE),
    m22_1_LLpump=-mean(m22_1_LLpump, na.rm=TRUE),
    m22_2_LLpump=-mean(m22_2_LLpump, na.rm=TRUE),
    m0_LLcollect=-mean(m0_LLcollect, na.rm=TRUE),
    m22_1_LLcollect=-mean(m22_1_LLcollect, na.rm=TRUE),
    m22_2_LLcollect=-mean(m22_2_LLcollect, na.rm=TRUE)
  )
modelComp_dataLongX<-data.frame(unclass(modelComp_dataLongX), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(dplyr::select(filter(modelComp_dataLongX, !is.na(toRemove)),
                        -toRemove), paste(folderPath,"Results\\modelComp_R.csv",sep=""), row.names = FALSE)


modelComp_dataLongY<- filter(workingData_Long_NOTS4 , !is.na(experimentClass) )%>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(
    toRemove=round(mean(mle, na.rm=TRUE)/mean(totalPumps, na.rm=TRUE),3),
    mle_22=round(mean(mle, na.rm=TRUE),2),
    m22_1_LLcollect=round(-mean(log(pBurst_1/100), na.rm=TRUE),2),
    m22_2_LLcollect=round(-mean(log(pBurst_2/100), na.rm=TRUE),2),
    m22_1_LLpump=round(-mean(log(1-pBurst_1/100), na.rm=TRUE),3),
    m22_2_LLpump=round(-mean(log(1-pBurst_2/100), na.rm=TRUE),3)
  )
modelComp_dataLongY<-data.frame(unclass(modelComp_dataLongY), check.names = FALSE, stringsAsFactors = FALSE)

dataLongX$compMLE<-(-dataLongX$nbCollected_1*log(dataLongX$pBurst_1.x)-
                      dataLongX$nbCollected_2*log(dataLongX$pBurst_2.x) -log(1-dataLongX$pBurst_1.x)*(dataLongX$threshold.x-1)*dataLongX$avgPumps_1-
                      log(1-dataLongX$pBurst_2.x)*(dataLongX$nbTr+1 -dataLongX$threshold.x)*dataLongX$avgPumps_2)

