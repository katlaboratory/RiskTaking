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

source(paste(folderPath,"RCode\\UtilityFunctions.R",sep=""))

# ##### Read Datasets From Files ################
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

####### Study 0 Datasets #################
s0data<-filter(workingDataNotS4, STUDY == "S7" & 
                !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
               !is.na(avgPumps) 
)
s0data<-filter(s0data, BMI>=18.5 &BMI<=30 & avgPumps>3)

s0dataLong<-filter(workingData_Long_NOTS4, STUDY == "S7" & 
                    !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                   !is.na(avgPumps) & 
                   !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4)
)
s0dataLong<-filter(s0dataLong, SUBJ_ID %in% unique(s0data$SUBJ_ID)& avgPumps>3)

s0dataTrialLong<-filter(workingData_trialLong_NOTS4, STUDY == "S7" & 
                          !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                        !is.na(colPumps) & 
                        !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4)
)
s0dataTrialLong<-filter(s0dataTrialLong, SUBJ_ID %in% unique(s0data$SUBJ_ID) & colPumps>3)

###### Study 1 Datasets ####
s1data<-filter(workingDataNotS4, STUDY %in% c("S2") & 
                 !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                 !is.na(avgPumps)
               & !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))
s1data<-filter(s1data, BMI>=18.5 &BMI<=30 )

s1dataLong<-filter(workingData_Long_NOTS4, STUDY %in% c("S2") & 
                     !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                     !is.na(avgPumps) 
                   & !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))
s1dataLong<-filter(s1dataLong, BMI>=18.5 &BMI<=30)
s1dataTrialLong<-filter(workingData_trialLong_NOTS4, STUDY %in% c("S2") & 
                          !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                          !is.na(colPumps) #& !SUBJ_ID %in% removeSubjects
                        & !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))
s1dataTrialLong<-filter(s1dataTrialLong, BMI>=18.5 &BMI<=30 )

###### Study 0-1 Datasets ####
s0s1dataLong<-rbind(s0dataLong,s1dataLong)
s0s1dataTrialLong<-rbind(s0dataTrialLong,s1dataTrialLong)


####### Study 2 Datasets #################
s2data<-filter(workingDataNotS4, STUDY %in% c("S1","S3","S5") & 
                 !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                 !is.na(avgPumps)
               & !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))
s2dataLong<-filter(workingData_Long_NOTS4, STUDY %in% c("S1","S3","S5") & 
                     !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                     !is.na(avgPumps) 
                   & !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))
s2dataTrialLong<-filter(workingData_trialLong_NOTS4, STUDY %in% c("S1","S3","S5") & 
                          !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
                          !is.na(colPumps) & 
                         !SUBJ_ID %in% util.GetExcludedSubjectIDs_NOTS4(workingDataNotS4))

####### Study 0 & Study 1 Summary Dataset and Stats ###########
s0dataLong$nbPumps<-s0dataLong$avgPumps
s0dataLong$EDE_Q_Restraint<-s0dataLong$EDE_Q_Restrain
tmp1<-filter(s0dataLong, experimentClass=="BodyInflate" & nbPumps>3  )
tmpBOINsubj<-(unique(tmp1$SUBJ_ID))
s0_BOIN<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restrain, data=tmp1))
s0_BOIN_nb<-nrow(tmp1)
tmp1<-filter(s0dataLong, experimentClass=="BodyDeflate"& nbPumps>3   )
tmpBODEsubj<-(unique(tmp1$SUBJ_ID))
s0_BODE<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restrain, data=tmp1))
s0_BODE_nb<-nrow(tmp1)

tmp1<-filter(s0dataLong,  nbPumps>3  )
summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restrain, data=tmp1))
summaryS1<-dplyr::select(
  filter(s1dataLong, STUDY == "S2" & 
           !is.na(EDE_Q_Restrain) & EDE_Q_MissingValues == "OK"&
           !is.na(avgPumps)
         & BMI>=18.5 &BMI<=30), SUBJ_ID, AGE, BMI,EDE_Q_Total, EDE_Q_Restrain, avgPumps,avgLastTrialTime,experimentClass)


summaryS1$nbPumps<-summaryS1$avgPumps
summaryS1$EDE_Q_Restraint<-summaryS1$EDE_Q_Restrain
tmp1<-filter(summaryS1, experimentClass=="BodyInflate" & nbPumps>3)
s1_BOIN<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restraint, data=tmp1))
s1_BOIN_nb<-nrow(tmp1)
tmp1<-filter(summaryS1, experimentClass=="BodyDeflate"& nbPumps>3)
s1_BODE<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restraint, data=tmp1))
s1_BODE_nb<-nrow(tmp1)
summaryS0<-dplyr::select(s0dataLong, SUBJ_ID, AGE, BMI, EDE_Q_Total, EDE_Q_Restrain, nbPumps,avgLastTrialTime,experimentClass)
summaryS0$avgPumps<-summaryS0$nbPumps
summaryS0$EDE_Q_Restraint<-summaryS0$EDE_Q_Restrain
summaryS0S1<-rbind(summaryS0,summaryS1)

tmp1<-filter(summaryS0S1, experimentClass=="BodyInflate"  & nbPumps>3)
s0s1_BOIN<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restraint, data=tmp1))
s0s1_BOIN_nb<-nrow(tmp1)
tmp1<-filter(summaryS0S1, experimentClass=="BodyDeflate" & nbPumps>3)
s0s1_BODE<-summary(lm(nbPumps~ AGE+BMI+EDE_Q_Restraint, data=tmp1))
s0s1_BODE_nb<-nrow(tmp1)
printS0S1BOIN<-as.numeric(round(s0s1_BOIN$coefficients[4,1],2)[1])
printS0S1BODE<-as.numeric(round(s0s1_BODE$coefficients[4,1],2)[1])

#### Datasets for Compact Results #######
dataCompactSDE_trialLong<-filter(s2dataTrialLong, !is.na(AGE)& !is.na(BMI) );
dataCompactSDE_long<-filter(s2dataLong, !is.na(AGE)& !is.na(BMI) );
s0s1dataTrialLongX<-filter(s0s1dataTrialLong, experimentClass %in% c("BodyInflate", "BodyDeflate"))
s0dataTrialLongX<-filter(s0dataTrialLong, experimentClass %in% c("BodyInflate", "BodyDeflate")&nbPumps>3)
s1dataTrialLongX<-filter(s1dataTrialLong, experimentClass %in% c("BodyInflate", "BodyDeflate"))
s1dataLongX<-filter(s1dataLong, experimentClass %in% c("BodyInflate", "BodyDeflate"))
s0s1dataLongX<-filter(s0s1dataLong, experimentClass %in% c("BodyInflate", "BodyDeflate"))

s0s1s2dataTrialLong<-rbind(dataCompactSDE_trialLong,s0s1dataTrialLongX)
s0s1s2dataLong<-rbind(s2dataLong,s0s1dataLongX)
s0s1s2data<-rbind(s2data,filter(s1data, !is.na(BOIN_experimentClass) | !is.na(BODE_experimentClass)   ))
s0s1s2data<-rbind(s0s1s2data,s0data)


##### Study 0+1+2 Long#####
tmpS0S1twoS2<-filter(workingData_Long_NOTS4, !STUDY %in% c("S7") |
                       (STUDY %in% c("S7") & avgPumps>3) )
tmpS0S1twoS2<-filter(tmpS0S1twoS2, SUBJ_ID %in% unique(s0s1dataLongX$SUBJ_ID) | SUBJ_ID %in% unique(s2dataLong$SUBJ_ID))
##### Study 0+1+2 Model Short #####
sumModel<- filter(tmpS0S1twoS2)  %>%
  dplyr::group_by(SUBJ_ID) %>%
  dplyr::summarise(nbTr = sum(nbTr), 
                   pBurst_2 = mean(pBurst_2 , na.rm = TRUE),
                   pBurst_1 = mean(pBurst_1 , na.rm = TRUE),
                   sdAvgLastTime_1 = mean(sdAvgLastTime_1 , na.rm = TRUE),
                   sdAvgLastTime_2 = mean(sdAvgLastTime_2 , na.rm = TRUE),
                   avgLastTime_1 = mean(avgLastTime_1 , na.rm = TRUE),
                   avgLastTime_2 = mean(avgLastTime_2 , na.rm = TRUE),
                   avgPumps_1 = mean(avgPumps_1 , na.rm = TRUE),
                   avgPumps_2 = mean(avgPumps_2 , na.rm = TRUE),
                   sdPumps_1 = mean(sdPumps_1 , na.rm = TRUE),
                   sdPumps_2 = mean(sdPumps_2 , na.rm = TRUE),
                   z1 = mean(z1 , na.rm = TRUE),
                   z2 = mean(z2 , na.rm = TRUE),
                   threshold = mean(threshold , na.rm = TRUE),
                   EDE_Q_Restrain = first(EDE_Q_Restrain ),
                   BIDQ_TOTAL = first(BIDQ_TOTAL ),
                   BIDQ_Concerns = first(BIDQ_Concerns ),
                   BIDQ_PREOCCUPATIONS = first(BIDQ_PREOCCUPATIONS ),
                   BIDQ_AVOIDANCE = first(BIDQ_AVOIDANCE ),
                   BIDQ_FUNCT_IMPAIR = first(BIDQ_FUNCT_IMPAIR ),
                   AGE = first(AGE ),
                   BMI=first(BMI),
                   STUDY=first(STUDY)
  )
tmpM<-filter(sumModel, !STUDY %in% c( "Sx") )
tmpM$pBurst_1_L<-log(tmpM$pBurst_1)
tmpM$pBurst_2_L<-log(tmpM$pBurst_2)
tmpM$avgLastTime_L_1<-1000*log(tmpM$avgLastTime_1)
tmpM$avgLastTime_L_2<-1000*log(tmpM$avgLastTime_2)
tmpM$avgLastTime_1_L<-1000*log(tmpM$avgLastTime_1)
tmpM$avgLastTime_2_L<-1000*log(tmpM$avgLastTime_2)
tmpM$precisionPrior<-1/(tmpM$z1^2)
tmpM$precisionPosterior<-1/(tmpM$z2^2)

sum_pb1_e<-summary(lm(pBurst_1_L~AGE +BMI + EDE_Q_Restrain, data=tmpM ) )
sum_pb2_e<-summary(lm(pBurst_2_L~AGE +BMI + EDE_Q_Restrain, data=tmpM ) )
sum_pb1pb2_e<-summary(lm(pBurst_2_L-pBurst_1_L~ AGE +BMI + EDE_Q_Restrain, data=tmpM ) )
rep_pb1_e<-report.GetReportingResultLG(sum_pb1_e, "EDE_Q_Restrain")
rep_pb2_e<-report.GetReportingResultLG(sum_pb2_e, "EDE_Q_Restrain")
rep_pb1pb2_e<-report.GetReportingResultLG(sum_pb1pb2_e,  c("EDE_Q_Restrain"))
sum_threshold_e<-summary(lm(threshold~AGE +BMI + EDE_Q_Restrain, data=filter(tmpM,threshold<20)  ) )
rep_threshold_e<-report.GetReportingResultLG(sum_threshold_e,  c("EDE_Q_Restrain"))

sum_z1z2_diff_e<-summary(lm(z1-z2~1,data=filter(tmpM,z1<2&z2<2)  ))
sum_z1z2_e<-summary(lm(z1-z2~AGE +BMI + EDE_Q_Restrain,data=filter(tmpM,z1<2&z2<2)  ))
rep_z1z2_e<-report.GetReportingResultLG(sum_z1z2_e, c("EDE_Q_Restrain"))
round(sum_z1z2_diff_e$coefficients,3)
tTest_z1z2_diff_e<-t.test(filter(tmpM,z1<2&z2<2)$z1,filter(tmpM,z1<2&z2<2)$z2, paired = TRUE, alternative = "two.sided")
rep_tTest_z1z2_diff_e<-report.tTest(tTest_z1z2_diff_e)
tTest_Hesitance1Hesitance2_diff_e<-t.test(tmpM$avgLastTime_1_L,tmpM$avgLastTime_2_L, paired = TRUE, alternative = "two.sided")
rep_tTest_Hesitance1Hesitance2_diff_e<-report.tTest(tTest_Hesitance1Hesitance2_diff_e)
sum_hes1hes2_diff_e<-summary(lm(avgLastTime_1_L-avgLastTime_2_L~1, data=tmpM ))

sum_hes1hes2_e<-summary(lm(avgLastTime_1_L-avgLastTime_2_L~AGE +BMI + EDE_Q_Restrain, data=tmpM))
rep_hes1hes2_e<-report.GetReportingResultLG(sum_hes1hes2_e, c("EDE_Q_Restrain"))

#### ALL RESULTS TABLE Stimulus*Direction*EDEQ_R ####

fivList=list(c("Stimulus"),c("Direction"),c("EDE_Q_Restrain"),c("Stimulus", "Direction"),
             c("Stimulus", "EDE_Q_Restrain"),c("Direction", "EDE_Q_Restrain"),
             c("Stimulus", "Direction", "EDE_Q_Restrain"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|STUDY)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)")
)
searchStrList=list(c("StimulusBody"),c("DirectionInflate"),c("EDE_Q_Restrain"),c("StimulusBody:DirectionInflate"),
                   c("StimulusBody:EDE_Q_Restrain"),c("DirectionInflate:EDE_Q_Restrain"),
                   c("StimulusBody:DirectionInflate:EDE_Q_Restrain"))
catLevelsList=list(c(""),c(""),c(""),c(""),c(""),
                   c(""),c("")
)

ivList<-c("colPumps")
tmp<-filter(dataCompactSDE_trialLong)
dtSubset<-"S2"
dt1<-report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp, fivc=c("AGE", "BMI"))

ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, lastTrialTime<10)
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp, fivc=c("AGE", "BMI")))

ivList<-c("colPumps")
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

ivList<-c("1000*log(lastTrialTime)")

tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp, lastTrialTime<10)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

ivList<-c("log(pBurst_1)", "log(pBurst_2)", "threshold")

tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2,threshold, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2,threshold, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


ivList<-c("1000*log(avgLastTime_1)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp, log(avgLastTime_1)!=-Inf&avgLastTime_1<10)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


ivList<-c("1000*log(avgLastTime_2)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp, log(avgLastTime_2)!=-Inf&avgLastTime_2<10)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


ivList<-c("log(pBurst_2)-log(pBurst_1)", "z1-z2")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2,z1,z2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2,z1,z2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


ivList<-c("1000*log(avgLastTime_1)-1000*log(avgLastTime_2)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp, log(avgLastTime_1)!=-Inf&
              log(avgLastTime_2)!=-Inf & avgLastTime_2<10 & avgLastTime_1<10)
dtSubset<-"S0+S1+S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


fivList=list(c("Stimulus", "EDE_Q_Restrain"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
catLevelsList=list(c(""))

ivList<-c("colPumps")
tmp<-filter(dataCompactSDE_trialLong,  Direction=="Inflate")
dtSubset<-"S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

tmp<-filter(dataCompactSDE_trialLong, Direction=="Deflate")
dtSubset<-"S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("Stimulus", "EDE_Q_Restrain"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
catLevelsList=list(c(""))
ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong,  Direction=="Inflate"& lastTrialTime<10)
dtSubset<-"S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

tmp<-filter(dataCompactSDE_trialLong, Direction=="Deflate" & lastTrialTime<10)
dtSubset<-"S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("EDE_Q_Restrain"))
searchStrList=list(c("EDE_Q_Restrain"))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong,  experimentClass=="BalloonInflate")
dtSubset<-"S2 condition==BalloonInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-filter(dataCompactSDE_trialLong,  experimentClass=="BalloonDeflate")
dtSubset<-"S2 condition==BalloonDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-filter(dataCompactSDE_trialLong,  experimentClass=="BodyInflate")
dtSubset<-"S2 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-filter(dataCompactSDE_trialLong,  experimentClass=="BodyDeflate")
dtSubset<-"S2 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


ivList<-c("colPumps")
fivList=list(c("EDE_Q_Restrain"))
searchStrList=list(c("EDE_Q_Restrain"))

tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BalloonInflate")
dtSubset<-"S0+S1+S2 condition==BalloonInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BalloonDeflate")
dtSubset<-"S0+S1+S2 condition==BalloonDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BodyInflate")
dtSubset<-"S0+S1+S2 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BodyDeflate")
dtSubset<-"S0+S1+S2 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyInflate" )
dtSubset<-"S1 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyDeflate" )
dtSubset<-"S1 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
rivList=list(c("(1|SUBJ_ID)"))
tmpS0S1<-dplyr::select(s0dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyInflate" )
dtSubset<-"S0 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyDeflate" )
dtSubset<-"S0 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"))
ivList<-c("1000*log(lastTrialTime)")
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BalloonInflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 condition==BalloonInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BalloonDeflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 condition==BalloonDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BodyInflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  experimentClass=="BodyDeflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
#ivList<-c("colPumps")
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyInflate" & lastTrialTime<10)
dtSubset<-"S1 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyDeflate" & lastTrialTime<10)
dtSubset<-"S1 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
rivList=list(c("(1|SUBJ_ID)"))
tmpS0S1<-dplyr::select(s0dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyInflate" & lastTrialTime<10)
dtSubset<-"S0 condition==BodyInflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
tmp<-tmpS0S1
tmp<-filter(tmp,  experimentClass=="BodyDeflate" & lastTrialTime<10)
dtSubset<-"S0 condition==BodyDeflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))



fivList=list(c("Stimulus", "EDE_Q_Restrain"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
catLevelsList=list(c(""))
ivList<-c("colPumps")
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Inflate")
dtSubset<-"S0+S1+S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Deflate")
dtSubset<-"S0+S1+S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|STUDY)"))
fivList=list(c("Stimulus", "EDE_Q_Restrain"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
ivList<-c("1000*log(lastTrialTime)")
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Inflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, lastTrialTime, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Deflate" & lastTrialTime<10)
dtSubset<-"S0+S1+S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("Stimulus", "EDE_Q_Restrain"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
ivList<-c("log(pBurst_1)", "log(pBurst_2)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Inflate")
dtSubset<-"S0+S1+S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s0s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, pBurst_1, pBurst_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Deflate")
dtSubset<-"S0+S1+S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))


fivList=list(c("Stimulus", "EDE_Q_Restrain"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
ivList<-c("1000*log(avgLastTime_1)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Inflate"& log(avgLastTime_1)!=-Inf&avgLastTime_1<10)
dtSubset<-"S0+S1+S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Deflate"& log(avgLastTime_1)!=-Inf&avgLastTime_1<10)
dtSubset<-"S0+S1+S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("Stimulus", "EDE_Q_Restrain"))
searchStrList=list(c("StimulusBody:EDE_Q_Restrain"))
ivList<-c("1000*log(avgLastTime_2)")
tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Inflate"& log(avgLastTime_2)!=-Inf&avgLastTime_2<10)
dtSubset<-"S0+S1+S2 Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

tmpS2<-dplyr::select(dataCompactSDE_long,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmpS0S1<-dplyr::select(s1dataLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, avgLastTime_1, avgLastTime_2, STUDY, OrderF, EDE_Q_Restrain)
tmp<-rbind(tmpS2, tmpS0S1)
tmp<-filter(tmp,  Direction=="Deflate"& log(avgLastTime_2)!=-Inf&avgLastTime_2<10)
dtSubset<-"S0+S1+S2 Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("Stimulus"),c("Direction"),c("EDE_Q_Restrain"),c("Stimulus", "Direction"),
             c("Stimulus", "EDE_Q_Restrain"),c("Direction", "EDE_Q_Restrain"),
             c("Stimulus", "Direction", "EDE_Q_Restrain"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("StimulusBody"),c("DirectionInflate"),c("EDE_Q_Restrain"),c("StimulusBody:DirectionInflate"),
                   c("StimulusBody:EDE_Q_Restrain"),c("DirectionInflate:EDE_Q_Restrain"),
                   c("StimulusBody:DirectionInflate:EDE_Q_Restrain"))
catLevelsList=list(c(""),c(""),c(""),c(""),c(""),
                   c(""),c("")
)
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong)
dtSubset<-"S2 BIDQ_TOTAL"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp, fivc=c("AGE", "BMI", "BIDQ_TOTAL")))
dtSubset<-"S2 DASS Depression"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Depression")))
dtSubset<-"S2 DASS Anxiety"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Anxiety")))
dtSubset<-"S2 DASS Stress"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Stress")))
dtSubset<-"S2 DASS Stress"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Stress")))
dtSubset<-"S2 OCIR"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "OCIR_TOTAL")))
dtSubset<-"S2 BIS11_Total"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "BIS11_Total")))

ivList<-c("log(pBurst_1)", "log(pBurst_2)")
tmp<-filter(dataCompactSDE_long)
dtSubset<-"S2 BIDQ_TOTAL"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "BIDQ_TOTAL")))
dtSubset<-"S2 DASS Depression"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Depression")))
dtSubset<-"S2 DASS Anxiety"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Anxiety")))
dtSubset<-"S2 DASS Stress"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "Dass21_Stress")))
dtSubset<-"S2 OCIR"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "OCIR_TOTAL")))
dtSubset<-"S2 BIS11_Total"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI", "BIS11_Total")))

fivList=list(c("BIDQ_TOTAL"),c("Stimulus", "Direction", "BIDQ_TOTAL"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("BIDQ_TOTAL"),c("StimulusBody:DirectionInflate:BIDQ_TOTAL"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(BIDQ_TOTAL))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
fivList=list(c("Dass21_Depression"),c("Stimulus", "Direction", "Dass21_Depression"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("Dass21_Depression"),c("StimulusBody:DirectionInflate:Dass21_Depression"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(Dass21_Depression))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("Dass21_Anxiety"),c("Stimulus", "Direction", "Dass21_Anxiety"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("Dass21_Anxiety"),c("StimulusBody:DirectionInflate:Dass21_Anxiety"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(Dass21_Anxiety))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
fivList=list(c("Dass21_Stress"),c("Stimulus", "Direction", "Dass21_Stress"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("Dass21_Stress"),c("StimulusBody:DirectionInflate:Dass21_Stress"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(Dass21_Stress))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

fivList=list(c("OCIR_TOTAL"),c("Stimulus", "Direction", "OCIR_TOTAL"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("OCIR_TOTAL"),c("StimulusBody:DirectionInflate:OCIR_TOTAL"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(OCIR_TOTAL))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))
fivList=list(c("BIS11_Total"),c("Stimulus", "Direction", "BIS11_Total"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("BIS11_Total"),c("StimulusBody:DirectionInflate:BIS11_Total"))
catLevelsList=list(c(""),c(""))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(dataCompactSDE_trialLong, !is.na(BIS11_Total))
dtSubset<-"S2"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("AGE", "BMI")))

results_SDE_Table<-dt1
write_csv(dt1,paste(folderPath,"Results\\Results_nonClinical.csv",sep=""))

#### Supplementary Materials - Results Tables ###### 
tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps,lastTrialTime, STUDY, OrderF, EDE_Q_Restrain, BIDQ_TOTAL)
tmpS0S1<-dplyr::select(s0s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps,lastTrialTime, STUDY, OrderF, EDE_Q_Restrain, BIDQ_TOTAL)
tmp<-rbind(tmpS2, tmpS0S1)

lm1<-lmer(colPumps~(1|SUBJ_ID)+ (1|OrderF) + (1|STUDY), data=tmp, REML=FALSE)
lm2<-lmer(colPumps~AGE+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm3<-lmer(colPumps~AGE+BMI+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm4<-lmer(colPumps~AGE+BMI+Stimulus+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm5<-lmer(colPumps~AGE+BMI+Stimulus+Direction+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm6<-lmer(colPumps~AGE+BMI+Stimulus*Direction+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm7<-lmer(colPumps~AGE+BMI+Stimulus*Direction+EDE_Q_Restrain+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm8<-lmer(colPumps~AGE+BMI+Stimulus*Direction*EDE_Q_Restrain+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
avRes<-anova(lm1, lm2, lm3,lm4,lm5,lm6,lm7,lm8);
avRes_S0S1S2_clicks<-as.data.frame(round(avRes,3))
IVs<-as.character(c("","AGE", "AGE + BMI","AGE + BMI + Stimulus",
                    "AGE + BMI + Stimulus + Direction",
"AGE + BMI + Stimulus x Direction","AGE + BMI + Stimulus x Direction+EDE_Q_Restrain",
"AGE + BMI + Stimulus x Direction x EDE_Q_Restrain"))
avModels_S0S1S2_clicks<-data.frame(Model=rownames(avRes), IV=IVs)
sum_S0S1S2_clicks <-round(summary(lm8)$coefficients,2)
icc_S0S1S2_clicks <-report.GetICCTable(lm8)

tmpS2<-dplyr::select(dataCompactSDE_trialLong,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps,lastTrialTime, STUDY, OrderF, EDE_Q_Restrain, BIDQ_TOTAL)
tmpS0S1<-dplyr::select(s1dataTrialLongX,SUBJ_ID, AGE,BMI, experimentClass, Direction, Stimulus, colPumps,lastTrialTime, STUDY, OrderF, EDE_Q_Restrain, BIDQ_TOTAL)
tmp<-rbind(tmpS2, tmpS0S1)

tmp<-filter(tmp, lastTrialTime<10)
tmp$Hesitance<-1000*log(tmp$lastTrialTime)
lm1<-lmer(Hesitance~(1|SUBJ_ID)+ (1|OrderF) + (1|STUDY), data=tmp, REML=FALSE)
lm2<-lmer(Hesitance~AGE+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm3<-lmer(Hesitance~AGE+BMI+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm4<-lmer(Hesitance~AGE+BMI+Stimulus+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm5<-lmer(Hesitance~AGE+BMI+Stimulus+Direction+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm6<-lmer(Hesitance~AGE+BMI+Stimulus*Direction+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm7<-lmer(Hesitance~AGE+BMI+Stimulus*Direction+EDE_Q_Restrain+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
lm8<-lmer(Hesitance~AGE+BMI+Stimulus*Direction*EDE_Q_Restrain+(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=tmp, REML=FALSE)
avRes<-anova(lm1, lm2, lm3,lm4,lm5,lm6,lm7,lm8);
avRes_S0S1S2_hesitance<-as.data.frame(round(avRes,3))
IVs<-as.character(c("","AGE", "AGE + BMI","AGE + BMI + Stimulus",
                    "AGE + BMI + Stimulus + Direction",
                    "AGE + BMI + Stimulus x Direction","AGE + BMI + Stimulus x Direction+EDE_Q_Restrain",
                    "AGE + BMI + Stimulus x Direction x EDE_Q_Restrain"))
avModels_S0S1S2_hesitance<-data.frame(Model=rownames(avRes), IV=IVs)
sum_S0S1S2_hesitance <-round(summary(lm8)$coefficients,2)
icc_S0S1S2_hesitance <-report.GetICCTable(lm8)

colnames(results_SDE_Table)
covariatesTable<-filter(results_SDE_Table, DataSubset %in% c("S2 BIDQ_TOTAL",
                                                             "S2 DASS Depression",
                                                             "S2 DASS Anxiety",
                                                             "S2 DASS Stress",
                                                             "S2 OCIR",
                                                             "S2 BIS11_Total")&
                          dv %in% c("colPumps", "1000*log(lastTrialTime)")&
                          iv %in% c("Stimulus x Direction x EDE_Q_Restrain", "EDE_Q_Restrain"))
covariatesTable$XsqDf<-paste(covariatesTable$Xsqr,"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
covariatesPumping<-dplyr::select(filter(covariatesTable, dv=="colPumps") ,
                                 Covariate,  iv,N,obs,reporting_result)
covariatesHesitance<-dplyr::select(filter(covariatesTable, dv=="1000*log(lastTrialTime)") ,
                                   Covariate, iv,N,obs,reporting_result)

covariatesTable<-filter(results_SDE_Table, DataSubset %in% c("S0+S1+S2")&
                          dv %in% c("log(pBurst_1)","log(pBurst_2)", "threshold")&
                          iv %in% c("Stimulus x Direction x EDE_Q_Restrain", "EDE_Q_Restrain"))
covariatesTable$XsqDf<-paste(covariatesTable$Xsqr,"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
resultsPBurst<-dplyr::select(filter(covariatesTable) ,
                             Covariate,dv, iv,N,obs,reporting_result)


covariatesTable<-filter(results_SDE_Table, DataSubset %in% c("S0+S1+S2")&
                          dv %in% c("1000*log(avgLastTime_1)","1000*log(avgLastTime_2)")&
                          iv %in% c("Stimulus x Direction x EDE_Q_Restrain", "EDE_Q_Restrain"))
covariatesTable$XsqDf<-paste(covariatesTable$Xsqr,"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
resultsHesitanceModel<-dplyr::select(filter(covariatesTable) ,
                             Covariate,dv, iv,N,obs,reporting_result)
covariatesTable<-filter(results_SDE_Table, DataSubset %in% c("S2 BIDQ_TOTAL",
                                                             "S2 DASS Depression",
                                                             "S2 DASS Anxiety",
                                                             "S2 DASS Stress",
                                                             "S2 OCIR",
                                                             "S2 BIS11_Total")&
                          dv %in% c("log(pBurst_1)", "log(pBurst_2)")&
                          iv %in% c("Stimulus x Direction x EDE_Q_Restrain", "EDE_Q_Restrain"))
covariatesTable$XsqDf<-paste(covariatesTable$Xsqr,"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
covariatesPBurst<-dplyr::select(filter(covariatesTable) ,
                                 Covariate, dv, iv,N,obs,reporting_result)
##### Summary stats ####
sumData_S0_sumlong<- filter(summaryS0, experimentClass %in% c("BodyInflate", "BodyDeflate")& 
                                nbPumps>3  &BMI>=18.5 &BMI<=30 )%>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
  )
sumData_S0_sumlong<-data.frame(unclass(sumData_S0_sumlong), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S1_sumlong<- filter(summaryS1, experimentClass %in% c("BodyInflate", "BodyDeflate")& 
                              nbPumps>3  &BMI>=18.5 &BMI<=30 )%>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   Hesitance=paste(round(mean(1000*log(avgLastTrialTime)),2), "(",round(sd(1000*log(avgLastTrialTime)),2), ")" ,sep="")
  )
sumData_S1_sumlong<-data.frame(unclass(sumData_S1_sumlong), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S0S1_sumlong<- filter(summaryS0S1, experimentClass %in% c("BodyInflate", "BodyDeflate")& 
                            nbPumps>3  &BMI>=18.5 &BMI<=30 )%>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
  )
sumData_S0S1_sumlong<-data.frame(unclass(sumData_S0S1_sumlong), check.names = FALSE, stringsAsFactors = FALSE)
sumData_S0S1_ID<- filter(summaryS0S1, experimentClass %in% c("BodyInflate", "BodyDeflate")& 
                            nbPumps>3  &BMI>=18.5 &BMI<=30 )%>%
  dplyr::group_by(SUBJ_ID) %>%
  dplyr::summarise(N=n(), 
                   avgPumps=mean(avgPumps),
                   EDE_Q_Restrain=mean(EDE_Q_Restrain),
                   EDE_Q_Total=mean(EDE_Q_Total),
                   AGE=mean(AGE),
                   BMI=mean(BMI, na.rm=TRUE)
  )
sumData_S0S1_ID<-data.frame(unclass(sumData_S0S1_ID), check.names = FALSE, stringsAsFactors = FALSE)
sumData_S0S1_sum<- sumData_S0S1_ID%>%
  dplyr::group_by() %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   EDEQ_R=paste(round(mean(EDE_Q_Restrain),2), "(",round(sd(EDE_Q_Restrain),2), ")" ,sep=""),
                   EDEQ_T=paste(round(mean(EDE_Q_Total),2), "(",round(sd(EDE_Q_Total),2), ")" ,sep=""),
                   Age=paste(round(mean(AGE),2),"(",round(sd(AGE),2), ")" ,sep=""),
                   BMI=paste(round(mean(BMI, na.rm=TRUE),2),"(",round(sd(BMI, na.rm=TRUE),2), ")" ,sep="")
                   )
sumData_S0S1_sum<-data.frame(unclass(sumData_S0S1_sum), check.names = FALSE, stringsAsFactors = FALSE)


sumData_S2_sumlong<- s2dataLong %>%
  dplyr::group_by(experimentClass) %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   Hesitance=paste(round(mean(1000*log(avgLastTrialTime)),2), "(",round(sd(1000*log(avgLastTrialTime)),2), ")" ,sep="")
  )
sumData_S2_sumlong<-data.frame(unclass(sumData_S2_sumlong), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S2_ALL_sumlong<- s2dataLong %>%
  dplyr::group_by(STUDY, experimentClass) %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   Hesitance=paste(round(mean(avgLastTrialTime),2), "(",round(sd(avgLastTrialTime),2), ")" ,sep="")
  )%>% 
  dplyr::arrange(STUDY)
  
sumData_S2_ALL_sumlong<-data.frame(unclass(sumData_S2_ALL_sumlong), check.names = FALSE, stringsAsFactors = FALSE)


sumData_S2_sum<- s2data %>%
  dplyr::group_by() %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   EDEQ_R=paste(round(mean(EDE_Q_Restrain),2), "(",round(sd(EDE_Q_Restrain),2), ")",sep="" ),
                   EDEQ_T=paste(round(mean(EDE_Q_Total),2), "(",round(sd(EDE_Q_Total),2), ")",sep="" ),
                   CollectedPct=paste(round(mean(100*isCollectedAvg),2), "(",round(sd(100*isCollectedAvg),2), ")" ,sep=""),
                   ExplodedPct=paste(round(mean(100*isExplodedAvg),2), "(",round(sd(100*isExplodedAvg),2), ")" ,sep=""),
                   Age=paste(round(mean(AGE),2),"(",round(sd(AGE),2), ")" ,sep=""),
                   BMI=paste(round(mean(BMI, na.rm=TRUE),2),"(",round(sd(BMI, na.rm=TRUE),2), ")" ,sep="")
  )
sumData_S2_sum<-data.frame(unclass(sumData_S2_sum), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S2_sum<- s2data %>%
  dplyr::group_by() %>%
  dplyr::summarise(N=n(), 
                   AvgPumps=paste(round(mean(avgPumps),2), "(",round(sd(avgPumps),2), ")" ,sep=""),
                   EDEQ_R=paste(round(mean(EDE_Q_Restrain),2), "(",round(sd(EDE_Q_Restrain),2), ")",sep="" ),
                   EDEQ_T=paste(round(mean(EDE_Q_Total),2), "(",round(sd(EDE_Q_Total),2), ")",sep="" ),
                   Age=paste(round(mean(AGE),2),"(",round(sd(AGE),2), ")" ,sep=""),
                   BMI=paste(round(mean(BMI, na.rm=TRUE),2),"(",round(sd(BMI, na.rm=TRUE),2), ")" ,sep="")
  )
sumData_S2_sum<-data.frame(unclass(sumData_S2_sum), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S2_sum2<- s2data %>%
  dplyr::group_by() %>%
  dplyr::summarise(
                   Hesitance=paste(round(mean(1000*log(avgLastTrialTime),na.rm=TRUE),2), "(",round(sd(1000*log(avgLastTrialTime),na.rm=TRUE),2), ")",sep="" ),
                   EDEQ_R=paste(round(mean(EDE_Q_Restrain,na.rm=TRUE),2), "(",round(sd(EDE_Q_Restrain,na.rm=TRUE),2), ")",sep="" ),
                   OCIR_TOTAL=paste(round(mean(OCIR_TOTAL,na.rm=TRUE),2), "(",round(sd(OCIR_TOTAL,na.rm=TRUE),2), ")",sep="" ),
                   Dass21_Anxiety=paste(round(mean(Dass21_Anxiety,na.rm=TRUE),2), "(",round(sd(Dass21_Anxiety,na.rm=TRUE),2), ")",sep="" ),
                   Dass21_Depression=paste(round(mean(Dass21_Depression,na.rm=TRUE),2), "(",round(sd(Dass21_Depression,na.rm=TRUE),2), ")",sep="" ),
                   Dass21_Stress=paste(round(mean(Dass21_Stress,na.rm=TRUE),2), "(",round(sd(Dass21_Stress,na.rm=TRUE),2), ")",sep="" ),
                   BIS11_Total=paste(round(mean(BIS11_Total,na.rm=TRUE),2), "(",round(sd(BIS11_Total,na.rm=TRUE),2), ")",sep="" )
  )
sumData_S2_sum2<-data.frame(unclass(sumData_S2_sum2), check.names = FALSE, stringsAsFactors = FALSE)

sumData_S2_sum3<- filter(s2data, avgLastTrialTime<10) %>%
  dplyr::group_by() %>%
  dplyr::summarise(
    Hesitance=paste(round(mean(1000*log(avgLastTrialTime),na.rm=TRUE),2), "(",round(sd(1000*log(avgLastTrialTime),na.rm=TRUE),2), ")",sep="" )  )
sumData_S2_sum3<-data.frame(unclass(sumData_S2_sum3), check.names = FALSE, stringsAsFactors = FALSE)


#### Correlations #####
s2BigData<-dplyr::select(
  s0s1s2data
  , avgPumps, avgLastTrialTime,EDE_Q_Restrain,EDE_Q_Total, BIDQ_TOTAL, Dass21_Depression, Dass21_Anxiety,
  Dass21_Stress, BIS11_Total, OCIR_TOTAL)

corMatrix<-rcorr(as.matrix(s2BigData) , type = c("pearson","spearman"))
corMatrix_r<-round(corMatrix$r,2)

s2BigData<-dplyr::select(
  s0s1s2data
  , EDE_Q_Restrain,EDE_Q_Total, EDE_Q_Eating, EDE_Q_Weight, EDE_Q_Shape, BIDQ_TOTAL)

corMatrix<-rcorr(as.matrix(s2BigData) , type = c("pearson","spearman"))
corMatrix_r2<-round(corMatrix$r,2)

#### Cronbach's Alpha ####

edeqDt2<-dplyr::select(filter(s2data, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) ),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
ca_edeq<-cronbach.alpha(edeqDt2)
cron_alpha_edeq_s2<-round(ca_edeq$alpha,2);

edeqDt1<-dplyr::select(filter(s1data, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) ),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
ca_edeq<-cronbach.alpha(edeqDt1)
cron_alpha_edeq_s1<-round(ca_edeq$alpha,2);

edeqDt0<-dplyr::select(filter(s0data, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) ),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
ca_edeq<-cronbach.alpha(edeqDt0)
cron_alpha_edeq_s0<-round(ca_edeq$alpha,2);

edeqDt<-rbind(edeqDt2,edeqDt1)
edeqDt<-rbind(edeqDt,edeqDt0)
ca_edeq<-cronbach.alpha(edeqDt)
cron_alpha_edeq_all<-round(ca_edeq$alpha,2);

##### To Print S0 and S1 ####

printS0BOIN_Int<-paste(round(s0_BOIN$coefficients[1,1],2),report.GetStars(s0_BOIN$coefficients[1,4]), sep="")
printS0BOIN_AGE<-paste(round(s0_BOIN$coefficients[2,1],2),report.GetStars(s0_BOIN$coefficients[2,4]), sep="")
printS0BOIN_BMI<-paste(round(s0_BOIN$coefficients[3,1],2),report.GetStars(s0_BOIN$coefficients[3,4]), sep="")
printS0BOIN_EDEQR<-paste("**",round(s0_BOIN$coefficients[4,1],2),"** ",report.GetStars(s0_BOIN$coefficients[4,4]), sep="")
printS1BOIN_Int<-paste(round(s1_BOIN$coefficients[1,1],2),report.GetStars(s1_BOIN$coefficients[1,4]), sep="")
printS1BOIN_AGE<-paste(round(s1_BOIN$coefficients[2,1],2),report.GetStars(s1_BOIN$coefficients[2,4]), sep="")
printS1BOIN_BMI<-paste(round(s1_BOIN$coefficients[3,1],2),report.GetStars(s1_BOIN$coefficients[3,4]), sep="")
printS1BOIN_EDEQR<-paste("**",round(s1_BOIN$coefficients[4,1],2),"** ",report.GetStars(s1_BOIN$coefficients[4,4]), sep="")
printS0S1BOIN_Int<-paste(round(s0s1_BOIN$coefficients[1,1],2),report.GetStars(s0s1_BOIN$coefficients[1,4]), sep="")
printS0S1BOIN_AGE<-paste(round(s0s1_BOIN$coefficients[2,1],2),report.GetStars(s0s1_BOIN$coefficients[2,4]), sep="")
printS0S1BOIN_BMI<-paste(round(s0s1_BOIN$coefficients[3,1],2),report.GetStars(s0s1_BOIN$coefficients[3,4]), sep="")
printS0S1BOIN_EDEQR<-paste("**",round(s0s1_BOIN$coefficients[4,1],2),"** ",report.GetStars(s0s1_BOIN$coefficients[4,4]), sep="")
printS0BODE_Int<-paste(round(s0_BODE$coefficients[1,1],2),report.GetStars(s0_BODE$coefficients[1,4]), sep="")
printS0BODE_AGE<-paste(round(s0_BODE$coefficients[2,1],2),report.GetStars(s0_BODE$coefficients[2,4]), sep="")
printS0BODE_BMI<-paste(round(s0_BODE$coefficients[3,1],2),report.GetStars(s0_BODE$coefficients[3,4]), sep="")
printS0BODE_EDEQR<-paste("**",round(s0_BODE$coefficients[4,1],2),"** ",report.GetStars(s0_BODE$coefficients[4,4]), sep="")
printS1BODE_Int<-paste(round(s1_BODE$coefficients[1,1],2),report.GetStars(s1_BODE$coefficients[1,4]), sep="")
printS1BODE_AGE<-paste(round(s1_BODE$coefficients[2,1],2),report.GetStars(s1_BODE$coefficients[2,4]), sep="")
printS1BODE_BMI<-paste(round(s1_BODE$coefficients[3,1],2),report.GetStars(s1_BODE$coefficients[3,4]), sep="")
printS1BODE_EDEQR<-paste("**",round(s1_BODE$coefficients[4,1],2),"** ",report.GetStars(s1_BODE$coefficients[4,4]), sep="")
printS0S1BODE_Int<-paste(round(s0s1_BODE$coefficients[1,1],2),report.GetStars(s0s1_BODE$coefficients[1,4]), sep="")
printS0S1BODE_AGE<-paste(round(s0s1_BODE$coefficients[2,1],2),report.GetStars(s0s1_BODE$coefficients[2,4]), sep="")
printS0S1BODE_BMI<-paste(round(s0s1_BODE$coefficients[3,1],2),report.GetStars(s0s1_BODE$coefficients[3,4]), sep="")
printS0S1BODE_EDEQR<-paste("**",round(s0s1_BODE$coefficients[4,1],2),"** ",report.GetStars(s0s1_BODE$coefficients[4,4]), sep="")

multiLevelTable_S0<- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("Variable", "S0_BOIN", "S0_BODE", "S1_BOIN", "S1_BODE","S0_S1_BOIN","S0_S1_BODE" )
colnames(multiLevelTable_S0) <- x
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="Stimulus", S0_BOIN="Body", S0_BODE="Body" , S1_BOIN="Body" , S1_BODE="Body", S0_S1_BOIN="Body", S0_S1_BODE="Body"))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
              data.frame(Variable="Direction", S0_BOIN="Inflate" , S1_BOIN="Inflate", S0_S1_BOIN="Inflate", S0_BODE="Deflate" , S1_BODE="Deflate", S0_S1_BODE="Deflate"))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="N", S0_BOIN=as.character(s0_BOIN_nb), S1_BOIN=as.character(s1_BOIN_nb), S0_S1_BOIN=as.character(s0s1_BOIN_nb), S0_BODE=as.character(s0_BODE_nb) , S1_BODE=as.character(s1_BODE_nb), S0_S1_BODE=as.character(s0s1_BODE_nb)))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="Intercept", S0_BOIN=printS0BOIN_Int , S1_BOIN=printS1BOIN_Int, S0_S1_BOIN=printS0S1BOIN_Int, S0_BODE=printS0BODE_Int , S1_BODE=printS1BODE_Int, S0_S1_BODE=printS0S1BODE_Int))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="AGE Slope", S0_BOIN=printS0BOIN_AGE , S1_BOIN=printS1BOIN_AGE, S0_S1_BOIN=printS0S1BOIN_AGE, S0_BODE=printS0BODE_AGE , S1_BODE=printS1BODE_AGE, S0_S1_BODE=printS0S1BODE_AGE))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="BMI Slope", S0_BOIN=printS0BOIN_BMI , S1_BOIN=printS1BOIN_BMI, S0_S1_BOIN=printS0S1BOIN_BMI, S0_BODE=printS0BODE_BMI , S1_BODE=printS1BODE_BMI, S0_S1_BODE=printS0S1BODE_BMI))
multiLevelTable_S0<-rbind(multiLevelTable_S0,
               data.frame(Variable="EDEQ Restraint Slope", S0_BOIN=printS0BOIN_EDEQR , S1_BOIN=printS1BOIN_EDEQR, S0_S1_BOIN=printS0S1BOIN_EDEQR, S0_BODE=printS0BODE_EDEQR , S1_BODE=printS1BODE_EDEQR, S0_S1_BODE=printS0S1BODE_EDEQR))
x <- c("Variable", "S0 BOIN", "S0 BODE", "S1 BOIN", "S1 BODE","S0&S1 BOIN","S0&S1 BODE" )
colnames(multiLevelTable_S0) <- x


##### Print Summary Stats ####

summaryTable2<- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("RowName","S0S1", "S2")
colnames(summaryTable2)<-x
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="",
                                S0S1="Mean(sd)",
                                S2="Mean(sd)"
                     ))
summaryTable2<-rbind(summaryTable2,
                    data.frame(RowName="N",
                      S0S1=paste(sumData_S0S1_sum$N[1]),
                               S2=paste(sumData_S2_sum$N[1])
                               ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="Age(sd)",
                                S0S1=paste(sumData_S0S1_sum$Age[1]),
                                S2=paste(sumData_S2_sum$Age[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="BMI(sd)",
                                S0S1=paste(sumData_S0S1_sum$BMI[1]),
                                S2=paste(sumData_S2_sum$BMI[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="Avg Pumps(sd)",
                                S0S1=paste(sumData_S0S1_sum$AvgPumps[1]),
                                S2=paste(sumData_S2_sum$AvgPumps[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="Hesitance(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$Hesitance[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="EDE-Q Total(sd)",
                                S0S1=paste(sumData_S0S1_sum$EDEQ_T[1]),
                                S2=paste(sumData_S2_sum$EDEQ_T[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="EDEQ Restraint(sd)",
                                S0S1=paste(sumData_S0S1_sum$EDEQ_R[1]),
                                S2=paste(sumData_S2_sum$EDEQ_R[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="DASS-21 Anxiety(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$Dass21_Anxiety[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="DASS-21 Depression(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$Dass21_Depression[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="DASS-21 Stress(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$Dass21_Stress[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="OCD-10 Total(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$OCIR_TOTAL[1])
                     ))
summaryTable2<-rbind(summaryTable2,
                     data.frame(RowName="BIS-11 Total(sd)",
                                S0S1="",
                                S2=paste(sumData_S2_sum2$BIS11_Total[1])
                     ))
rownames(summaryTable2)<-summaryTable2$RowName;summaryTable2$RowName<-NULL
x <- c("S0+S1", "S2")
colnames(summaryTable2)<-x
summaryTable2

summaryTable<- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("N", "AvgPumps", "EDEQ_R", "Age", "BMI" )
summaryTable<-rbind(summaryTable,
             data.frame(N="**S0+S1**",AvgPumps="",
                        EDEQ_R="", Age="",BMI="" ))
summaryTable<-rbind(summaryTable,
                              data.frame(N=paste(sumData_S0S1_sum$N[1]),
                                         AvgPumps=sumData_S0S1_sum$AvgPumps[1],
                                         EDEQ_R=sumData_S0S1_sum$EDEQ_R[1], 
                                         Age=sumData_S0S1_sum$Age[1],
                                         BMI=sumData_S0S1_sum$BMI[1]))

summaryTable<-rbind(summaryTable,
                              data.frame(N="**S2**",AvgPumps="",
                                         EDEQ_R="", Age="",BMI=""))
summaryTable<-rbind(summaryTable,
                              data.frame(N=paste(sumData_S2_sum$N[1]),
                                         AvgPumps=sumData_S2_sum$AvgPumps[1],
                                         EDEQ_R=sumData_S2_sum$EDEQ_R[1], 
                                         Age=sumData_S2_sum$Age[1],
                                         BMI=sumData_S2_sum$BMI[1]))
summaryTable

summaryTable_Condition<- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Condition", "N", "AvgPumps", "Hesitance" )
summaryTable_Condition<-rbind(summaryTable_Condition,
                              data.frame(Condition="**S0+S1**",
                                         N="",AvgPumps="",
                                         Hesitance="" ))
for(ii in 1:nrow(sumData_S0S1_sumlong))
{
  summaryTable_Condition<-rbind(summaryTable_Condition,
                              data.frame(Condition=sumData_S0S1_sumlong$experimentClass[ii],
                                         N=paste(sumData_S0S1_sumlong$N[ii]),AvgPumps=sumData_S0S1_sumlong$AvgPumps[ii],
                                         Hesitance=""  ))
}
summaryTable_Condition<-rbind(summaryTable_Condition,
                              data.frame(Condition="**S2**",
                                         N="",AvgPumps="",
                                         Hesitance=""  ))
for(ii in 1:nrow(sumData_S2_sumlong))
{
  summaryTable_Condition<-rbind(summaryTable_Condition,
                                data.frame(Condition=sumData_S2_sumlong$experimentClass[ii],
                                           N=paste(sumData_S2_sumlong$N[ii]),AvgPumps=sumData_S2_sumlong$AvgPumps[ii],
                                           Hesitance=sumData_S2_sumlong$Hesitance[ii]  ))
}


summaryTable_Condition_ALL<- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("Study", "Condition", "N", "AvgPumps", "Hesitance" )
summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                  data.frame(Study="**S0**"  , Condition="",
                                             N="",AvgPumps="",
                                             Hesitance="" ))
for(ii in 1:nrow(sumData_S0_sumlong))
{
  summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                    data.frame(Study="", Condition=sumData_S0_sumlong$experimentClass[ii],
                                               N=paste(sumData_S0_sumlong$N[ii]),AvgPumps=sumData_S0_sumlong$AvgPumps[ii],
                                               Hesitance=""  ))
}
summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                  data.frame(Study="**S1**"  , Condition="",
                                             N="",AvgPumps="",
                                             Hesitance="" ))
for(ii in 1:nrow(sumData_S1_sumlong))
{
  summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                    data.frame(Study="", Condition=sumData_S1_sumlong$experimentClass[ii],
                                               N=paste(sumData_S1_sumlong$N[ii]),AvgPumps=sumData_S1_sumlong$AvgPumps[ii],
                                               Hesitance=sumData_S1_sumlong$Hesitance[ii]  ))
}
summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                              data.frame(Study="**S0+S1**"  , Condition="",
                                         N="",AvgPumps="",
                                         Hesitance="" ))
for(ii in 1:nrow(sumData_S0S1_sumlong))
{
  summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                data.frame(Study="", Condition=sumData_S0S1_sumlong$experimentClass[ii],
                                           N=paste(sumData_S0S1_sumlong$N[ii]),AvgPumps=sumData_S0S1_sumlong$AvgPumps[ii],
                                           Hesitance=""  ))
}
summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                              data.frame(Study="**S2**"  , Condition="",
                                         N="",AvgPumps="",
                                         Hesitance=""  ))
for(ii in 1:nrow(sumData_S2_sumlong))
{
  summaryTable_Condition_ALL<-rbind(summaryTable_Condition_ALL,
                                data.frame(Study="", Condition=sumData_S2_sumlong$experimentClass[ii],
                                           N=paste(sumData_S2_sumlong$N[ii]),AvgPumps=sumData_S2_sumlong$AvgPumps[ii],
                                           Hesitance=sumData_S2_sumlong$Hesitance[ii]  ))
}
colnames(summaryTable_Condition_ALL)
summaryTable_Condition_ALL2<-dplyr::select(summaryTable_Condition_ALL, Study, Condition, N, AvgClicks=AvgPumps)



