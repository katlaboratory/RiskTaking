rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("FolderPath.R")
############################# Libraries and Sources ###################################################
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
#library(export)
library(varhandle)
library(nnet)
library(fpc)
library(combinat)
library(cluster)
library(factoextra)
library(fpc)
library(NbClust)
library(RColorBrewer)
library(ltm)
source(paste(folderPath,"RCode\\UtilityFunctions.R",sep=""))

##### Read Datasets From Files ################
workingData_S4<-read_csv(paste(folderPath,"Data\\workingData_S4.csv",sep=""),
               col_types = cols(GROUP = col_factor(), STUDY=col_factor())
                         )
workingData_S4<-data.frame(unclass(workingData_S4), check.names = FALSE, stringsAsFactors = FALSE)

workingData_Long_S4<-read_csv(paste(folderPath,"Data\\workingData_Long_S4.csv",sep=""),
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
workingData_Long_S4<-data.frame(unclass(workingData_Long_S4), check.names = FALSE, stringsAsFactors = FALSE)
summary(workingData_Long_S4)
workingData_trialLong_S4<-read_csv(paste(folderPath,"Data\\workingData_trialLong_S4.csv",sep=""),
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

workingData_trialLong_S4<-data.frame(unclass(workingData_trialLong_S4), check.names = FALSE, stringsAsFactors = FALSE)

###### Model Dataset SHORT: sumModel ######
sumModel<- filter(workingData_Long_S4,!is.na(avgPumps)) %>%
  dplyr::group_by(GROUP, SUBJ_ID) %>%
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
                   AGE = first(AGE ),
                   BMI = first(BMI ),
                   EDE_Q_Restrain = first(EDE_Q_Restrain ),
                   EDE_Q_Total = first(EDE_Q_Total ),
                   nbTr = mean(nbTr,na.rm = TRUE ),
                   STUDY=first(STUDY),
                   Dass21_Depr = first( Dass21_Depression),
                   Dass21_Anx = first(Dass21_Anxiety ),
                   Dass21_Str = first( Dass21_Stress)
  )


#### Descriptives ####
s4Descriptives_group<- filter(workingData_S4 , !is.na(GROUP) & !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE") %>%
  dplyr::group_by(GROUP) %>%
  dplyr::summarise(meanAge=round(mean(AGE, na.rm=TRUE),2), 
                   sdAge=round(sd(AGE, na.rm=TRUE),2),
                   meanBMI = round(mean(BMI, na.rm=TRUE),2),
                   sdBMI = round(sd(BMI, na.rm=TRUE),2),
                   meanPumps=round(mean(avgPumps, na.rm=TRUE),2),
                   sdPumps=round(sd(avgPumps, na.rm=TRUE),2),
                   meanEDEQR=round(mean(EDE_Q_Restrain, na.rm=TRUE),2),
                   sdEDEQR=round(sd(EDE_Q_Restrain, na.rm=TRUE),2),
                   N=n())
s4Descriptives_group<-data.frame(unclass(s4Descriptives_group), check.names = FALSE, stringsAsFactors = FALSE)
x <- c("Group", "Age", "(sd)" , "BMI" , "(sd)", "Avg Pumps", "(sd)" , "EDEQ Restraint" , "(sd)" , "N")
colnames(s4Descriptives_group) <- x
Map(
  function(x,y){
    s4Descriptives_group$Group<<-gsub(x,y,s4Descriptives_group$Group )
  }, 
  c("RE","HC_L","HC_H"), 
  c("AN-WR", "HC-L","HC-H")
)
s4Descriptives_group2<- filter(workingData_S4 , !is.na(GROUP) & !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE") %>%
  dplyr::group_by(GROUP) %>%
  dplyr::summarise(
    label="Mean(sd)",
    N=n(),
    meanAge=paste(round(mean(AGE, na.rm=TRUE),2), "(", 
                   round(sd(AGE, na.rm=TRUE),2),")",sep=""),
    meanBMI = paste(round(mean(BMI, na.rm=TRUE),2), "(", 
                   round(sd(BMI, na.rm=TRUE),2),")",sep=""),
    meanPumps=paste(round(mean(avgPumps, na.rm=TRUE),2),"(", 
                   round(sd(avgPumps, na.rm=TRUE),2),")",sep=""),
    meanHesitance=paste(round(mean(1000*log(avgLastTrialTime), na.rm=TRUE),2),"(", 
                    round(sd(1000*log(avgLastTrialTime), na.rm=TRUE),2),")",sep=""),
    meanEDEQT=paste(round(mean(EDE_Q_Total, na.rm=TRUE),2),"(", 
                    round(sd(EDE_Q_Total, na.rm=TRUE),2),")",sep=""),
    meanEDEQR=paste(round(mean(EDE_Q_Restrain, na.rm=TRUE),2),"(", 
                   round(sd(EDE_Q_Restrain, na.rm=TRUE),2),")",sep=""),
    meanDass21_Anxiety=paste(round(mean(Dass21_Anxiety, na.rm=TRUE),2),"(", 
                          round(sd(Dass21_Anxiety, na.rm=TRUE),2),")",sep=""),
    meanDass21_Depression=paste(round(mean(Dass21_Depression, na.rm=TRUE),2),"(", 
                          round(sd(Dass21_Depression, na.rm=TRUE),2),")",sep=""),
    meanDass21_Stress=paste(round(mean(Dass21_Stress, na.rm=TRUE),2),"(", 
                          round(sd(Dass21_Stress, na.rm=TRUE),2),")",sep=""),
    meanOCIR_TOTAL=paste(round(mean(OCIR_TOTAL, na.rm=TRUE),2),"(", 
                            round(sd(OCIR_TOTAL, na.rm=TRUE),2),")",sep=""),
    meanBIS11_Total=paste(round(mean(BIS11_Total, na.rm=TRUE),2),"(", 
                            round(sd(BIS11_Total, na.rm=TRUE),2),")",sep="")
    )
s4Descriptives_group2<-data.frame(unclass(s4Descriptives_group2), check.names = FALSE, stringsAsFactors = FALSE)
x <- c("Group","", "N","Age(sd)" , "BMI(sd)", "Avg Pumps(sd)", "Hesitance(sd)", "EDE-Q Total(sd)" , "EDE-Q Restraint(sd)",
     "DASS-21 Anxiety(sd)", "DASS-21 Depression(sd)", "DASS-21 Stress(sd)", "OCD-10 Total(sd)","BIS-11 Total(sd)")
colnames(s4Descriptives_group2) <- x
Map(
  function(x,y){
    s4Descriptives_group2$Group<<-gsub(x,y,s4Descriptives_group2$Group )
  }, 
  c("RE","HC_L","HC_H"), 
  c("AN-WR", "HC-L","HC-H")
)
x<-s4Descriptives_group2[["Group"]]
s4Descriptives_group2[["Group"]]<-NULL
s4Descriptives_group2<-t(s4Descriptives_group2)
colnames(s4Descriptives_group2) <- as.character(x)


s4Descriptives_group3<- filter(workingData_S4 , !is.na(GROUP) 
                               & !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE"
                               & avgLastTrialTime <10) %>%
  dplyr::group_by(GROUP) %>%
  dplyr::summarise(
    label="Mean(sd)",
    N=n(),
    meanHesitance=paste(round(mean(1000*log(avgLastTrialTime), na.rm=TRUE),2),"(", 
                        round(sd(1000*log(avgLastTrialTime), na.rm=TRUE),2),")",sep="")
  )
s4Descriptives_group3<-data.frame(unclass(s4Descriptives_group3), check.names = FALSE, stringsAsFactors = FALSE)
x <- c("Group","", "N", "Hesitance(sd)")
colnames(s4Descriptives_group3) <- x
Map(
  function(x,y){
    s4Descriptives_group3$Group<<-gsub(x,y,s4Descriptives_group3$Group )
  }, 
  c("RE","HC_L","HC_H"), 
  c("AN-WR", "HC-L","HC-H")
)
x<-s4Descriptives_group3[["Group"]]
s4Descriptives_group3[["Group"]]<-NULL
s4Descriptives_group3<-t(s4Descriptives_group3)
colnames(s4Descriptives_group3) <- as.character(x)



s4Descriptives_groupCondition<- filter(workingData_Long_S4 ,!is.na(GROUP) &  !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE") %>%
  dplyr::group_by(GROUP, experimentClass) %>%
  dplyr::summarise(meanPumps=round(mean(avgPumps, na.rm=TRUE),2),
                   sdPumps=round(sd(avgPumps, na.rm=TRUE),2),
                   N=n())
s4Descriptives_groupCondition<-data.frame(unclass(s4Descriptives_groupCondition), check.names = FALSE, stringsAsFactors = FALSE)
x <- c("Group", "Condition", "Avg Pumps", "(sd)"  , "N")
colnames(s4Descriptives_groupCondition) <- x
s4Descriptives_groupCondition$Group<-as.character(s4Descriptives_groupCondition$Group)

orderedGroups<-c("HC_L","AN","RE","HC_H")
dtDescr<-filter(sumModel,!GROUP%in% c("HC_EXCLUDE", "ANRE_EXCLUDE"))
dtDescr<-within(dtDescr,GROUP <- relevel(GROUP, ref = "HC_L"));
descSumAGE<-summary(lm(AGE~GROUP,data=dtDescr))
descSumBMI<-summary(lm(BMI~GROUP,data=dtDescr))
descSumEDEQR<-summary(lm(EDE_Q_Restrain~GROUP,data=dtDescr))
descSumEDEQT<-summary(lm(EDE_Q_Total~GROUP,data=dtDescr))
descSumDassA<-summary(lm(Dass21_Anx~GROUP,data=dtDescr))
descSumDassD<-summary(lm(Dass21_Depr~GROUP,data=dtDescr))
descSumDassS<-summary(lm(Dass21_Str~GROUP,data=dtDescr))
dtTemp<-data.frame(round(descSumAGE$coefficients,2))

rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("AGE","","","")
dtDescrComp<-dtTemp
dtTemp<-data.frame(round(descSumBMI$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("BMI","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtTemp<-data.frame(round(descSumEDEQR$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("EDE_Q Restraint","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtTemp<-data.frame(round(descSumEDEQT$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("EDE_Q Total","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtTemp<-data.frame(round(descSumDassA$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("Dass Anxiety","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtTemp<-data.frame(round(descSumDassD$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("Dass Depression","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtTemp<-data.frame(round(descSumDassS$coefficients,2))
rownames(dtTemp)<-NULL;dtTemp$Group<-orderedGroups;dtTemp$Var<-c("Dass Stress","","","")
dtDescrComp<-rbind(dtDescrComp,dtTemp)
dtDescrComp<-data.frame(Var=dtDescrComp$Var,Group=dtDescrComp$Group,MeanAndDiff=dtDescrComp$Estimate,
                        SD=dtDescrComp$Std..Error,t=dtDescrComp$t.value,p=dtDescrComp$Pr...t..)


### tmpXXXL Long Definition ##################
tmpXXXL<-filter(workingData_Long_S4 , !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE")
tmpXXXL<-within(tmpXXXL,GROUP <- relevel(GROUP, ref = "HC_L"));summary(tmpXXXL$GROUP)
tmpXXXL$pBurst_1_L<-log(tmpXXXL$pBurst_1)
tmpXXXL$pBurst_2_L<-log(tmpXXXL$pBurst_2)

### tmpXXX TrialLong Definition ##################
tmpXXX<-filter(workingData_trialLong_S4 , !GROUP=="HC_EXCLUDE" & !GROUP=="ANRE_EXCLUDE")
tmpXXX<-within(tmpXXX,GROUP <- relevel(GROUP, ref = "HC_L"));summary(tmpXXX$GROUP)
dtTemp<-filter(workingData_S4 , !is.na(EDE_Q_Restrain) & GROUP=="AN")
corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-data.frame(group="AN", left="pumps", right="edeqr", condition="",cor=corResult$estimate,pValue=corResult$p.value)
dtTemp<-filter(tmpXXXL, GROUP=="AN" & experimentClass=="BodyInflate", !is.na(EDE_Q_Restrain))

corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="AN", left="pumps", right="edeqr", condition="BodyInflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="AN" & experimentClass=="BodyDeflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="AN", left="pumps", right="edeqr", condition="BodyDeflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(workingData_S4 , !is.na(EDE_Q_Restrain) & GROUP=="RE")
corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="pumps", right="edeqr", condition="",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="RE" & experimentClass=="BodyInflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="pumps", right="edeqr", condition="BodyInflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="RE" & experimentClass=="BodyDeflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgPumps, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="pumps", right="edeqr", condition="BodyDeflate",cor=corResult$estimate,pValue=corResult$p.value))

#hesitance
dtTemp<-filter(workingData_S4 , !is.na(EDE_Q_Restrain) & GROUP=="AN")
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="AN", left="hesitance", right="edeqr", condition="",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="AN" & experimentClass=="BodyInflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="AN", left="hesitance", right="edeqr", condition="BodyInflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="AN" & experimentClass=="BodyDeflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="AN", left="hesitance", right="edeqr", condition="BodyDeflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(workingData_S4 , !is.na(EDE_Q_Restrain) & GROUP=="RE")
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="hesitance", right="edeqr", condition="",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="RE" & experimentClass=="BodyInflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="hesitance", right="edeqr", condition="BodyInflate",cor=corResult$estimate,pValue=corResult$p.value))
dtTemp<-filter(tmpXXXL, GROUP=="RE" & experimentClass=="BodyDeflate", !is.na(EDE_Q_Restrain))
corResult<-cor.test(dtTemp$avgLastTrialTime_L, dtTemp$EDE_Q_Restrain)
correlationsWithinGroupMainEffects<-rbind(correlationsWithinGroupMainEffects,
                                          data.frame(group="RE", left="hesitance", right="edeqr", condition="BodyDeflate",cor=corResult$estimate,pValue=corResult$p.value))

correlationsWithinGroupMainEffects$cor<-round(correlationsWithinGroupMainEffects$cor,2)
correlationsWithinGroupMainEffects$pValue<-round(correlationsWithinGroupMainEffects$pValue,2)
rownames(correlationsWithinGroupMainEffects)<-NULL



######  Hesitance~ Stimulus*Direction*GROUP For Tables #####
xtlmm1<-lmer(1000*log(lastTrialTime)~(1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm0<-lmer(1000*log(lastTrialTime)~AGE + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm1<-lmer(1000*log(lastTrialTime)~AGE + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm2<-lmer(1000*log(lastTrialTime)~AGE + Stimulus + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm2b<-lmer(1000*log(lastTrialTime)~AGE + Direction + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm3<-lmer(1000*log(lastTrialTime)~AGE + Direction + Stimulus + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm4<-lmer(1000*log(lastTrialTime)~AGE + Direction*Stimulus + (1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm5<-lmer(1000*log(lastTrialTime)~AGE + Direction*Stimulus + GROUP +(1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtlm6<-lmer(1000*log(lastTrialTime)~AGE + Direction*Stimulus*GROUP +(1|SUBJ_ID)+ (1|OrderF), data=tmpXXX, REML=FALSE)
xtimeaov<-anova(xtlmm1,xtlm0, xtlm1, xtlm2, xtlm3, xtlm4, xtlm5, xtlm6);xtimeaov;

xtimelm6<-summary(xtlm6);
xtimelm2<-summary(xtlm2);
xtimelm2b<-summary(xtlm2b);





##### ALL RESULTS TABLE Direction*Stimulus*GROUP ######

     fivList=list(c("Stimulus"),c("Direction"),c("GROUP"),c("Stimulus", "Direction"),
                  c("Stimulus", "GROUP"),c("Direction", "GROUP"),
                  c("Stimulus", "Direction", "GROUP"))
     rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)", "(1|GROUP)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|GROUP)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)", "(1|GROUP)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)"),
                  c("(1|SUBJ_ID)", "(1|OrderF)")
                  )
     searchStrList=list(c("StimulusBody"),c("DirectionInflate"),c("GROUP"),c("StimulusBody:DirectionInflate"),
                        c("StimulusBody:GROUP"),c("DirectionInflate:GROUP"),
                        c("StimulusBody:DirectionInflate:GROUP"))
     catLevelsList=list(c(""),c(""),c("AN","RE","HC_H"),c(""),c("AN","RE","HC_H"),
                        c("AN","RE","HC_H"),c("AN","RE","HC_H")
                        )

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) )
dtSubset<-"ALL"
dt1<-report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp)
ivList<-c( "1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & lastTrialTime<20)
dtSubset<-"ALL <20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp))

ivList<-c("log(pBurst_1)", "log(pBurst_2)","log(pBurst_1)-log(pBurst_2)", "threshold")
tmp<-filter(tmpXXXL, !is.na(GROUP) )
dtSubset<-"ALL"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp))

ivList<-c("1000*log(avgLastTime_1)")
tmp<-filter(tmpXXXL, !is.na(GROUP) & log(avgLastTime_1)!=-Inf& avgLastTime_1<20)
dtSubset<-"ALL<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
ivList<-c("1000*log(avgLastTime_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP) & log(avgLastTime_2)!=-Inf& avgLastTime_2<20)
dtSubset<-"ALL<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
ivList<-c("1000*log(avgLastTime_1)-1000*log(avgLastTime_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP) & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf&
              avgLastTime_1<20&avgLastTime_2<20)
dtSubset<-"ALL<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))


ivList<-c("colPumps")
catLevelsList=list(c(""),c(""),c("AN","RE"),c(""),c("AN","RE"),
                   c("AN","RE"),c("AN","RE")
)
tmp<-filter(tmpXXX, !is.na(GROUP)  & GROUP %in% c("AN", "HC_L", "RE"))
dtSubset<-"ANvsHC_LvsRE"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("AN"),c(""),c("AN"),
                   c("AN"),c("AN")
)
tmp<-filter(tmpXXX, !is.na(GROUP)  & GROUP %in% c("AN", "HC_L"))
dtSubset<-"ANvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("RE"),c(""),c("RE"),
                   c("RE"),c("RE")
)
tmp<-filter(tmpXXX, !is.na(GROUP)  & GROUP %in% c("RE", "HC_L"))
dtSubset<-"REvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("HC_H"),c(""),c("HC_H"),
                   c("HC_H"),c("HC_H")
)
catLevelsList=list(c(""),c(""),c("AN"),c(""),c("AN"),
                   c("AN"),c("AN")
)


ivList<-c("1000*log(lastTrialTime)")
catLevelsList=list(c(""),c(""),c("AN","RE"),c(""),c("AN","RE"),
                   c("AN","RE"),c("AN","RE")
)
tmp<-filter(tmpXXX, !is.na(GROUP)  & GROUP %in% c("AN", "HC_L", "RE") &lastTrialTime<20)
dtSubset<-"ANvsHC_LvsRE<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("AN"),c(""),c("AN"),
                   c("AN"),c("AN")
)
tmp<-filter(tmpXXX, !is.na(GROUP)  & GROUP %in% c("AN", "HC_L")&lastTrialTime<20)
dtSubset<-"ANvsHC_L<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

ivList<-c("log(pBurst_1)", "log(pBurst_2)","log(pBurst_1)-log(pBurst_2)")
catLevelsList=list(c(""),c(""),c("AN"),c(""),c("AN"),
                   c("AN"),c("AN")
)
tmp<-filter(tmpXXXL, !is.na(GROUP) & GROUP %in% c("AN", "HC_L"))
dtSubset<-"ANvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("RE"),c(""),c("RE"),
                   c("RE"),c("RE")
)
tmp<-filter(tmpXXXL, !is.na(GROUP) & GROUP %in% c("RE", "HC_L"))
dtSubset<-"REvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

ivList<-c("1000*log(avgLastTime_1)", "1000*log(avgLastTime_2)","1000*log(avgLastTime_1)-1000*log(avgLastTime_2)")
catLevelsList=list(c(""),c(""),c("AN"),c(""),c("AN"),
                   c("AN"),c("AN")
)
tmp<-filter(tmpXXXL, !is.na(GROUP) & GROUP %in% c("AN", "HC_L") & log(avgLastTime_1)!=-Inf&
              log(avgLastTime_2)!=-Inf)
dtSubset<-"ANvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("RE"),c(""),c("RE"),
                   c("RE"),c("RE")
)
tmp<-filter(tmpXXXL, !is.na(GROUP) & GROUP %in% c("RE", "HC_L") & log(avgLastTime_1)!=-Inf&
              log(avgLastTime_2)!=-Inf)
dtSubset<-"REvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
catLevelsList=list(c(""),c(""),c("HC_H"),c(""),c("HC_H"),
                   c("HC_H"),c("HC_H")
)

fivList=list(c("GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"))
searchStrList=list(c("(Intercept)"))
catLevelsList=list(c("AN"))
ivList<-c("log(pBurst_2)-log(pBurst_1)")
catLevelsList=list(c(""))
tmp<-filter(tmpXXXL, !is.na(GROUP) & GROUP %in% c("AN", "HC_L"))
dtSubset<-"ANvsHC_L"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))



# Post hocs all groups
fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("AN","RE","HC_H"))

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) & Direction=="Inflate")
dtSubset<-"ALL - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

tmp<-filter(tmpXXX, !is.na(GROUP) & Direction=="Deflate")
dtSubset<-"ALL - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & Stimulus=="Body")
dtSubset<-"ALL - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("AN","RE","HC_H"))
ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & Direction=="Inflate" & lastTrialTime<20)
dtSubset<-"ALL - Direction==Inflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

tmp<-filter(tmpXXX, !is.na(GROUP) & Direction=="Deflate"& lastTrialTime<20)
dtSubset<-"ALL - Direction==Deflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & Stimulus=="Body"& lastTrialTime<20)
dtSubset<-"ALL - Stimulus==Body<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))


ivList<-c("log(pBurst_1)", "log(pBurst_2)")
fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP) & Direction=="Inflate")
dtSubset<-"ALL - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP) & Direction=="Deflate")
dtSubset<-"ALL - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP) & Stimulus=="Body")
dtSubset<-"ALL - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

ivList<-c("1000*log(avgLastTime_1)", "1000*log(avgLastTime_2)")
fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))

tmp<-filter(tmpXXXL, !is.na(GROUP) & Direction=="Inflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ALL - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP) & Direction=="Deflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ALL - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP) & Stimulus=="Body" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ALL - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
# Post-hocs 2 groups
fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("AN"))

ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L") & Direction=="Inflate"
            & lastTrialTime<20)
dtSubset<-"ANvsHC_L - Direction==Inflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L") & Direction=="Deflate"
            & lastTrialTime<20)
dtSubset<-"ANvsHC_L - Direction==Deflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L")& Stimulus=="Body"
            & lastTrialTime<20)
dtSubset<-"ANvsHC_L - Stimulus==Body<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("AN"))

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L") & Direction=="Inflate")
dtSubset<-"ANvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
ivList<-c("colPumps", "1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L") & Direction=="Deflate")
dtSubset<-"ANvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("AN", "HC_L")& Stimulus=="Body")
dtSubset<-"ANvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))



fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))
ivList<-c("log(pBurst_1)", "log(pBurst_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Direction=="Inflate")
dtSubset<-"ANvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Direction=="Deflate")
dtSubset<-"ANvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Stimulus=="Body")
dtSubset<-"ANvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))

ivList<-c("1000*log(avgLastTime_1)", "1000*log(avgLastTime_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Direction=="Inflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ANvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Direction=="Deflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ANvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("AN", "HC_L") & Stimulus=="Body" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"ANvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
#### RE vs HC_L posthocs
fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("RE"))

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L") & Direction=="Inflate")
dtSubset<-"REvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L") & Direction=="Deflate")
dtSubset<-"REvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L")& Stimulus=="Body")
dtSubset<-"REvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Stimulus", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)"))
searchStrList=list(c("StimulusBody:GROUP"))
catLevelsList=list(c("RE"))

ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L") & Direction=="Inflate"
            & lastTrialTime<20)
dtSubset<-"REvsHC_L - Direction==Inflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L") & Direction=="Deflate"
            & lastTrialTime<20)
dtSubset<-"REvsHC_L - Direction==Deflate<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXX, !is.na(GROUP) & GROUP %in% c("RE", "HC_L")& Stimulus=="Body"
            & lastTrialTime<20)
dtSubset<-"REvsHC_L - Stimulus==Body<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))


fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))
ivList<-c("log(pBurst_1)", "log(pBurst_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Direction=="Inflate")
dtSubset<-"REvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Direction=="Deflate")
dtSubset<-"REvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Stimulus=="Body")
dtSubset<-"REvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))

fivList=list(c("Stimulus", "GROUP"))
searchStrList=list(c("StimulusBody:GROUP"))

ivList<-c("1000*log(avgLastTime_1)", "1000*log(avgLastTime_2)")
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Direction=="Inflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"REvsHC_L - Direction==Inflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Direction=="Deflate" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"REvsHC_L - Direction==Deflate"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))
fivList=list(c("Direction", "GROUP"))
searchStrList=list(c("DirectionInflate:GROUP"))
tmp<-filter(tmpXXXL, !is.na(GROUP)& GROUP %in% c("RE", "HC_L") & Stimulus=="Body" & log(avgLastTime_1)!=-Inf&log(avgLastTime_2)!=-Inf)
dtSubset<-"REvsHC_L - Stimulus==Body"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp))


#### COVARIATES
fivList=list(c("Stimulus"),c("Direction"),c("GROUP"),c("Stimulus", "Direction"),
             c("Stimulus", "GROUP"),c("Direction", "GROUP"),
             c("Stimulus", "Direction", "GROUP"))
rivList=list(c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)", "(1|GROUP)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|GROUP)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|GROUP)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Direction)"),
             c("(1|SUBJ_ID)", "(1|OrderF)", "(1|Stimulus)"),
             c("(1|SUBJ_ID)", "(1|OrderF)")
)
searchStrList=list(c("StimulusBody"),c("DirectionInflate"),c("GROUP"),c("StimulusBody:DirectionInflate"),
                   c("StimulusBody:GROUP"),c("DirectionInflate:GROUP"),
                   c("StimulusBody:DirectionInflate:GROUP"))
catLevelsList=list(c(""),c(""),c("AN","RE","HC_H"),c(""),c("AN","RE","HC_H"),
                   c("AN","RE","HC_H"),c("AN","RE","HC_H")
)

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) )
dtSubset<-"ALL-DassDepression"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                 dtSubset, tmp, fivc=c("Dass21_Depression", "AGE")))
ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP)&  lastTrialTime<20)
dtSubset<-"ALL-DassDepression<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("Dass21_Depression", "AGE")))

ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) )
dtSubset<-"ALL-DassStress"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("Dass21_Stress", "AGE")))

ivList<-c( "1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP) & lastTrialTime<20)
dtSubset<-"ALL-DassStress<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("Dass21_Stress", "AGE")))


ivList<-c("colPumps")
tmp<-filter(tmpXXX, !is.na(GROUP) )
dtSubset<-"ALL-DassAnxiety"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("Dass21_Anxiety", "AGE")))

ivList<-c("1000*log(lastTrialTime)")
tmp<-filter(tmpXXX, !is.na(GROUP)&  lastTrialTime<20)
dtSubset<-"ALL-DassAnxiety<20"
dt1<-rbind(dt1,report.GenerateResultsTable(fivList, rivList, searchStrList, catLevelsList, ivList,
                                           dtSubset, tmp, fivc=c("Dass21_Anxiety", "AGE")))


catLevelsList=list(c(""),c(""),c("RE"),c(""),c("RE"),
                   c("RE"),c("RE"))

results_SDG_Table<-dt1
results_SDG_Table<-dplyr::rename(results_SDG_Table, "$X^2$"=Xsqr)
results_SDG_Table<-dplyr::rename(results_SDG_Table, "$f^2$"=Fsqr)
write_csv(dt1,paste(folderPath,"Results\\Results_Clinical.csv",sep=""))




#### Supplementary Materials Results     ######
lm1<-lmer(colPumps~(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm2<-lmer(colPumps~AGE+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm3<-lmer(colPumps~AGE+Stimulus+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm4<-lmer(colPumps~AGE+Stimulus+Direction+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm5<-lmer(colPumps~AGE+Stimulus*Direction+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm6<-lmer(colPumps~AGE+Stimulus*Direction+GROUP+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
lm7<-lmer(colPumps~AGE+Stimulus*Direction*GROUP+(1|SUBJ_ID)+(1|OrderF),data=tmpXXX)
avRes<-anova(lm1,lm2,lm3,lm4,lm5,lm6,lm7)
avRes_SDG_clicks<-as.data.frame(round(avRes,3))
IVs<-as.character(c("","AGE","AGE + Stimulus",
                    "AGE + Stimulus + Direction",
                    "AGE + Stimulus x Direction","AGE + Stimulus x Direction + GROUP",
                    "AGE + Stimulus x Direction x GROUP"))
avModels_SDG_clicks<-data.frame(Model=rownames(avRes), IV=IVs)
sum_SDG_clicks <-round(summary(lm7)$coefficients,2)
icc_SDG_clicks <-report.GetICCTable(lm7)

tmp<-filter(tmpXXX, lastTrialTime<20)
tmp$hesitance<-1000*log(tmp$lastTrialTime)
lm1<-lmer(hesitance~(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm2<-lmer(hesitance~AGE+(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm3<-lmer(hesitance~AGE+Stimulus+(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm4<-lmer(hesitance~AGE+Stimulus+Direction+(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm5<-lmer(hesitance~AGE+Stimulus*Direction+(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm6<-lmer(hesitance~AGE+Stimulus*Direction+GROUP+(1|SUBJ_ID)+(1|OrderF),data=tmp)
lm7<-lmer(hesitance~AGE+Stimulus*Direction*GROUP+(1|SUBJ_ID)+(1|OrderF),data=tmp)
avRes<-anova(lm1,lm2,lm3,lm4,lm5,lm6,lm7)
avRes_SDG_hesitance<-as.data.frame(round(avRes,3))
IVs<-as.character(c("","AGE","AGE + Stimulus",
                    "AGE + Stimulus + Direction",
                    "AGE + Stimulus x Direction","AGE + Stimulus x Direction + GROUP",
                    "AGE + Stimulus x Direction x GROUP"))
avModels_SDG_hesitance<-data.frame(Model=rownames(avRes), IV=IVs)
sum_SDG_hesitance <-round(summary(lm7)$coefficients,2)
icc_SDG_hesitance <-report.GetICCTable(lm7)
colnames(results_SDG_Table)
covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ALL-DassDepression",
                                                             "ALL-DassStress",
                                                             "ALL-DassAnxiety"
                                                             )&
                          dv %in% c("colPumps")&
                          iv %in% c("Stimulus x Direction x GROUP", "GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
covariatesPumping<-dplyr::select(filter(covariatesTable, dv=="colPumps") ,
                                 Covariate,iv, N,obs,reporting_result)
covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ALL-DassDepression<20",
                                                             "ALL-DassStress<20",
                                                             "ALL-DassAnxiety<20"
                                                             )&
                          dv %in% c("1000*log(lastTrialTime)")&
                          iv %in% c("Stimulus x Direction x GROUP", "GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset

covariatesHesitance<-dplyr::select(filter(covariatesTable, dv=="1000*log(lastTrialTime)") ,
                                   Covariate,iv, N,obs,reporting_result)

covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ANvsHC_L - Direction==Inflate",
                                                             "ANvsHC_L - Direction==Deflate",
                                                             "REvsHC_L - Direction==Inflate",
                                                             "REvsHC_L - Direction==Deflate")&
                          dv %in% c("colPumps")&
                          iv %in% c("Stimulus x GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Direction<-covariatesTable$DataSubset
covariatesPumpingPostHocs<-dplyr::select(filter(covariatesTable, dv=="colPumps") ,
                                         Direction, iv,N,obs,reporting_result)

covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ANvsHC_L - Direction==Inflate<20",
                                                             "ANvsHC_L - Direction==Deflate<20",
                                                             "REvsHC_L - Direction==Inflate<20",
                                                             "REvsHC_L - Direction==Deflate<20")&
                          dv %in% c("1000*log(lastTrialTime)")&
                          iv %in% c("Stimulus x GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Direction<-covariatesTable$DataSubset

covariatesHesitancePostHoc<-dplyr::select(filter(covariatesTable, dv=="1000*log(lastTrialTime)") ,
                                          Direction, iv,N,obs,reporting_result)

covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ALL")&
                          dv %in% c("log(pBurst_1)","log(pBurst_2)", "threshold")&
                          iv %in% c("Stimulus x Direction x GROUP", "GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
resultsPBurst<-dplyr::select(filter(covariatesTable) ,
                                Covariate,dv, iv,N,obs,reporting_result)


covariatesTable<-filter(results_SDG_Table, DataSubset %in% c("ALL<20")&
                          dv %in% c("1000*log(avgLastTime_1)","1000*log(avgLastTime_2)")&
                          iv %in% c("Stimulus x Direction x GROUP", "GROUP"))
covariatesTable$XsqDf<-paste(covariatesTable[["$X^2$"]],"(",covariatesTable$Xdf,")",sep="")
covariatesTable$Covariate<-covariatesTable$DataSubset
resultsHesitanceModel<-dplyr::select(filter(covariatesTable) ,
                                        Covariate,dv,iv, N,obs,reporting_result)

##### colPumps ~ Stimulus*Direction*GROUP For Tables ######
tmp<-filter(tmpXXX, !is.na(GROUP))
lm_base_SDG<-lmer(colPumps~ (1|SUBJ_ID) + (1|OrderF) , data = tmp, REML=FALSE)
lm0_SDG<-lmer(colPumps~ AGE +(1|SUBJ_ID) + (1|OrderF) , data = tmp, REML=FALSE)
lm1_SDG<-lmer(colPumps~ AGE + Stimulus + (1|SUBJ_ID) + (1|OrderF), data = tmp, REML=FALSE)
lm2_SDG<-lmer(colPumps~ AGE + Stimulus + Direction + (1|SUBJ_ID) + (1|OrderF), data = tmp, REML=FALSE)
lm3_SDG<-lmer(colPumps~ AGE + Stimulus*Direction + (1|SUBJ_ID) + (1|OrderF), data = tmp, REML=FALSE)
lm4_SDG<-lmer(colPumps~ AGE + Stimulus*Direction + GROUP + (1|SUBJ_ID) + (1|OrderF), data = tmp, REML=FALSE)
lm5_SDG<-lmer(colPumps~ AGE + Stimulus*Direction*GROUP + (1|SUBJ_ID) + (1|OrderF), data = tmp, REML=FALSE)
av_SDG<-anova(lm_base_SDG,lm0_SDG, lm1_SDG,lm2_SDG,lm3_SDG,lm4_SDG,lm5_SDG)
sm_SDG<-summary(lm5_SDG)

###### ICC Tables #####

multiLevelTable_S2_a2_ICC<- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("RandomEffect", "ICC" )
colnames(multiLevelTable_S2_a2_ICC) <- x
multiLevelTable_S2_a2_ICC<-rbind(multiLevelTable_S2_a2_ICC,
                                 data.frame(RandomEffect="SUBJ_ID",ICC=round(100*report.GetICC(lm5_SDG,"SUBJ_ID"),2) ))
multiLevelTable_S2_a2_ICC<-rbind(multiLevelTable_S2_a2_ICC,
                                 data.frame(RandomEffect="OrderF",ICC=round(100*report.GetICC(lm5_SDG,"OrderF"),2) ))
multiLevelTable_S2_a2_ICC<-rbind(multiLevelTable_S2_a2_ICC,
                                 data.frame(RandomEffect="Residual",ICC=round(100*report.GetICC(lm5_SDG,"Residual"),2) ))
x <- c("Random Effect (Level)", "ICC %" )
colnames(multiLevelTable_S2_a2_ICC) <- x

multiLevelTable_S2_a2_ICC_h<- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("RandomEffect", "ICC" )
colnames(multiLevelTable_S2_a2_ICC_h) <- x
multiLevelTable_S2_a2_ICC_h<-rbind(multiLevelTable_S2_a2_ICC_h,
                                 data.frame(RandomEffect="SUBJ_ID",ICC=round(100*report.GetICC(xtlm6,"SUBJ_ID"),2) ))
multiLevelTable_S2_a2_ICC_h<-rbind(multiLevelTable_S2_a2_ICC_h,
                                 data.frame(RandomEffect="OrderF",ICC=round(100*report.GetICC(xtlm6,"OrderF"),2) ))
multiLevelTable_S2_a2_ICC_h<-rbind(multiLevelTable_S2_a2_ICC_h,
                                 data.frame(RandomEffect="Residual",ICC=round(100*report.GetICC(xtlm6,"Residual"),2) ))
x <- c("Random Effect (Level)", "ICC %" )
colnames(multiLevelTable_S2_a2_ICC_h) <- x



##### Correlations #################
dataForCorr<-dplyr::select(
  filter(workingData_S4, STUDY %in% c("S4","S8") & 
          !is.na(avgPumps)
          )
  , avgPumps, avgLastTrialTime,AGE, BMI, EDE_Q_Restrain,EDE_Q_Total, 
  Dass21_Depression, Dass21_Anxiety,
  Dass21_Stress)

corMatrix<-rcorr(as.matrix(dataForCorr) , type = c("pearson","spearman"))
corMatrix_r<-round(corMatrix$r,2)

dtxxxx<-filter(workingData_S4, STUDY %in% c("S4","S8") & 
                 !is.na(avgPumps)
)



dtCorrIn<-filter(workingData_S4, STUDY %in% c("S4","S8") & 
                   !is.na(avgPumps) #& !SUBJ_ID %in% c("S4_8", "S4_10")
                 )




dtCorrIn<-filter(workingData_S4, STUDY %in% c("S4","S8") & 
                   !is.na(avgPumps) & !SUBJ_ID %in% c("S4_8", "S4_10"))
dtCorrIn$hesitance<- 1000*log(dtCorrIn$avgLastTrialTime)
dtCorrIn$avgClicks<-dtCorrIn$avgPumps
dtCorrIn$pbLoss_1_L<-(log(dtCorrIn$BOIN_pBurst_1)+log(dtCorrIn$BODE_pBurst_1)+log(dtCorrIn$BAIN_pBurst_1)+log(dtCorrIn$BADE_pBurst_1))/4
dtCorrIn$pbLoss_2_L<-(log(dtCorrIn$BOIN_pBurst_2)+log(dtCorrIn$BODE_pBurst_2)+log(dtCorrIn$BAIN_pBurst_2)+log(dtCorrIn$BADE_pBurst_2))/4

dtCorr_AN<-filter(dtCorrIn, GROUP=="AN")
dataForCorr<-dplyr::select(dtCorr_AN
                           , avgClicks, hesitance,pbLoss_1_L,pbLoss_2_L,EDE_Q_Total
                           
                           )

corMatrix<-rcorr(as.matrix(dataForCorr) , type = c("pearson","spearman"))
corMatrix_r4_an<-round(corMatrix$r,2)
corMatrix_rp4_an<-round(corMatrix$P,2)

corMatrix_r4_an<-corMatrix_r4_an[5:nrow(corMatrix_r4_an),1:4]
corMatrix_rp4_an<-corMatrix_rp4_an[5:nrow(corMatrix_rp4_an),1:4]

dtCorr_RE<-filter(dtCorrIn, GROUP=="RE")
dataForCorr<-dplyr::select(dtCorr_RE
                           , avgClicks, hesitance,pbLoss_1_L,pbLoss_2_L,EDE_Q_Total
                           
                           )

corMatrix<-rcorr(as.matrix(dataForCorr) , type = c("pearson","spearman"))
corMatrix_r4_re<-round(corMatrix$r,2)
corMatrix_rp4_re<-round(corMatrix$P,2)

corMatrix_r4_re<-corMatrix_r4_re[5:nrow(corMatrix_r4_re),1:4]
corMatrix_rp4_re<-corMatrix_rp4_re[5:nrow(corMatrix_rp4_re),1:4]

#### Cronbach A ####
edeqDt<-dplyr::select(filter(workingData_S4, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) & GROUP %in% c("AN","RE")),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
unique(workingData_S4$EDE_Q_MissingValues)
#edeqDt[]<-lapply(edeqDt,as.numeric)
ca_anre<-cronbach.alpha(edeqDt)
cron_alpha_edeq_anre<-round(ca_anre$alpha,2);
edeqDt<-dplyr::select(filter(workingData_S4, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) & GROUP %in% c("HC_L","HC_H")),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
unique(workingData_S4$EDE_Q_MissingValues)
#edeqDt[]<-lapply(edeqDt,as.numeric)
ca_hclh<-cronbach.alpha(edeqDt)
cron_alpha_edeq_hclh<-round(ca_hclh$alpha,2);

edeqDt<-dplyr::select(filter(workingData_S4, EDE_Q_MissingValues=="OK" & EDE_Q_Restrain!="#DIV/0!"
                             & !is.na(EDE_Q_Restrain)& !is.na(EDE_Q_Eating)& 
                               !is.na(EDE_Q_Shape)& !is.na(EDE_Q_Weight) ),
                      (EDE_Q_Restrain),(EDE_Q_Eating),(EDE_Q_Shape),(EDE_Q_Weight))
unique(workingData_S4$EDE_Q_MissingValues)
#edeqDt[]<-lapply(edeqDt,as.numeric)
ca_all<-cronbach.alpha(edeqDt)
cron_alpha_edeq_all<-round(ca_all$alpha,2);

##### Supervised Learning: Predicting The Group #######
workingData_S4$BOIN_avgLastTime_1_L<-log(workingData_S4$BOIN_avgLastTime_1)
workingData_S4$BODE_avgLastTime_1_L<-log(workingData_S4$BODE_avgLastTime_1)
workingData_S4$BODE_pBurst_1_L<-log(workingData_S4$BODE_pBurst_1)
workingData_S4$BOIN_pBurst_1_L<-log(workingData_S4$BOIN_pBurst_1)
workingData_S4$hesitanceBODEminBOIN <-workingData_S4$BODE_pBurst_1_L-workingData_S4$BOIN_pBurst_1_L
workingData_S4$pBurstBODEminBOIN<-workingData_S4$BODE_avgLastTime_1_L-workingData_S4$BOIN_avgLastTime_1_L
dtAN<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("AN"))
dtHCL<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("HC_L"))

dtHCH<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("HC_H"))
dtHCEXCLUDE<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("HC_EXCLUDE"))
dtANEXCLUDE<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("ANRE_EXCLUDE"))
dtRE<-filter(workingData_S4, !is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) &!is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("RE"))
dtPrediction<-filter(workingData_S4,!is.na(BODE_pBurst_1_L) &!is.na(BOIN_pBurst_1_L) & !is.na(BODEminBOIN) &!is.na(BMI) & GROUP %in% c("AN", "HC_L"))
dtPrediction$BOIN_avgLastTime_1_L
dtPrediction$G<-NA
dtPrediction$G<-as.factor(dtPrediction$G)
levels(dtPrediction$G)<-c("A","H")
dtPrediction[dtPrediction$GROUP=="HC_L",]$G<-"H"
dtPrediction[dtPrediction$GROUP=="AN",]$G<-"A"
ratio<-length(dtPrediction[dtPrediction$GROUP=="HC_EXCLUDE",]$G)/length(dtPrediction[dtPrediction$GROUP %in% c("AN","HC_L"),]$G)
ratio<-0.5
dtPrediction<-within(dtPrediction, G <- relevel(G, ref = "H"))
summary(dtPrediction$BODE_pBurst_1_L)
dtPrediction$BODEminBOINpb1<-dtPrediction$BODE_pBurst_1_L-dtPrediction$BOIN_pBurst_1_L
glm1<-glm(G ~ BMI+ BOIN_pBurst_1_L+EDE_Q_Restrain + BODEminBOIN, data = filter(dtPrediction, G %in% c("H","A")), family = "binomial")
glm1<-glm(G ~ BODE_pBurst_1_L+BOIN_pBurst_1_L+ BODEminBOIN+EDE_Q_Restrain, data = filter(dtPrediction, G %in% c("H","A")), family = "binomial")
glm1<-glm(G ~ AGE+BOIN_pBurst_1_L + BOIN_avgLastTrialTime_L, data = filter(dtPrediction, G %in% c("H","A")), family = "binomial")

sumglm1<-summary(glm1)

dtFrTruth<-data.frame(ratioX=double(), ANpredictedANX=integer(), ANpredictedHCX=integer(), 
                      HCpredictedANX=integer() , HCpredictedHCX=integer(),
                      HCHpredictedANX=integer() , HCHpredictedHCX=integer(),
                      SuccessPctX=integer(),ANSuccessPctX=integer(),HCSuccessPctX=integer(),
                      HCHpredicted_success=integer(),HCEXpredicted_success=integer(),
                      REpredicted_success=integer()
                      )
glmForm<-"G ~ AGE+ BOIN_pBurst_1_L + BOIN_avgLastTrialTime_L"
for(i in (4:16))
{
  ratioLoop<-(i/20)
  truthTable<-report.GetTruthTable(glmForm, dtPrediction , ratioLoop)
  newdata1<-dplyr::select(dtHCH,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
  reportHCH<-report.GetCategories(glm1, newdata1 , ratioLoop) 
  reportHCH_success<-round(100*reportHCH$H/(reportHCH$H+reportHCH$A))
  newdata1<-dplyr::select(dtHCEXCLUDE,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
  reportHCEX<-report.GetCategories(glm1, newdata1 , ratioLoop) 
  reportHCEX_success<-round(100*reportHCEX$H/(reportHCEX$H+reportHCEX$A))
  newdata1<-dplyr::select(dtRE,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
  reportRE<-report.GetCategories(glm1, newdata1 , ratioLoop) 
  reportRE_success<-round(100*reportRE$H/(reportRE$H+reportRE$A))
  
  SuccessPct<-round(100*(truthTable$ANpredictedAN$A + truthTable$HCpredictedHC$H)/
                      (truthTable$ANpredictedAN$A + truthTable$HCpredictedHC$H + truthTable$ANpredictedHC$H + truthTable$HCpredictedAN$A ))
  ANSuccessPct<-round(100*(truthTable$ANpredictedAN$A)/
                                  (truthTable$ANpredictedAN$A + truthTable$ANpredictedHC$H  ))
  HCSuccessPct<-round(100*(truthTable$HCpredictedHC$H)/
                                  (truthTable$HCpredictedHC$H + truthTable$HCpredictedAN$A  ))
  dtFrTruth<-rbind(dtFrTruth, data.frame(ratioX=ratioLoop, 
                                         ANpredictedANX=truthTable$ANpredictedAN$A, 
                                         ANpredictedHCX=truthTable$ANpredictedHC$H, 
                                         HCpredictedANX=truthTable$HCpredictedAN$A , 
                                         HCpredictedHCX=truthTable$HCpredictedHC$H,
                                         HCHpredictedANX=reportHCH$A,
                                         HCHpredictedHCX=reportHCH$H,
                                         SuccessPctX=SuccessPct,
                                         ANSuccessPctX=ANSuccessPct,
                                         HCSuccessPctX=HCSuccessPct,
                                         HCHpredicted_success=reportHCH_success,
                                         HCEXpredicted_success=reportHCEX_success,
                                         REpredicted_success=reportRE_success) 
                   )
}

newdata1<-dplyr::select(dtHCH,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
report<-report.GetCategories(glm1, newdata1 , ratio); print(paste("A:",report$A, ", H:", report$H, sep=""))
newdata1<-dplyr::select(dtHCEXCLUDE,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
report<-report.GetCategories(glm1, newdata1 , ratio); print(paste("A:",report$A, ", H:", report$H, sep=""))
newdata1<-dplyr::select(dtRE,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
report<-report.GetCategories(glm1, newdata1 , ratio); print(paste("A:",report$A, ", H:", report$H, sep=""))
newdata1<-dplyr::select(dtAN,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
report<-report.GetCategories(glm1, newdata1 , ratio); print(paste("A:",report$A, ", H:", report$H, sep=""))
newdata1<-dplyr::select(dtHCL,AGE,BMI,EDE_Q_Restrain,BODEminBOIN,BOIN_pBurst_1_L,BODE_pBurst_1_L,avgPumps,BODE_avgPumps,avgLastTrialTime_L,BOIN_avgLastTrialTime_L)
report<-report.GetCategories(glm1, newdata1 , ratio); print(paste("A:",report$A, ", H:", report$H, sep=""))


dtFrTruth
ggplot(data=dtFrTruth , aes(x=ratioX))+
  geom_line(aes(y=SuccessPctX, color="TotalSuccess%"))+
  geom_point(aes(y=SuccessPctX, color="TotalSuccess%"))+
  geom_line(aes(y=ANSuccessPctX, color="ANSuccess%"))+
  geom_point(aes(y=ANSuccessPctX, color="ANSuccess%"))+
  geom_line(aes(y=HCSuccessPctX, color="HCSuccess%"))+
  geom_point(aes(y=HCSuccessPctX, color="HCSuccess%"))+
  geom_line(aes(y=HCHpredicted_success, color="HCH_predSuccess%"))+
  geom_point(aes(y=HCHpredicted_success, color="HCH_predSuccess%"))+
  geom_line(aes(y=HCEXpredicted_success, color="HCEX_predSuccess%"))+
  geom_point(aes(y=HCEXpredicted_success, color="HCEX_predSuccess%"))+
  geom_line(aes(y=REpredicted_success, color="RE_predSuccess%"))+
  geom_point(aes(y=REpredicted_success, color="RE_predSuccess%"))+
  scale_color_manual(name="Line Type", 
                     breaks = c("TotalSuccess%","ANSuccess%","HCSuccess%","HCH_predSuccess%","HCEX_predSuccess%","RE_predSuccess%" ),
                     values = c("TotalSuccess%"="blue","ANSuccess%"="red","HCSuccess%"="darkgreen","HCH_predSuccess%"="lightgreen","HCEX_predSuccess%"="green","RE_predSuccess%"="pink" ) )+
  ggtitle(paste("Success % per cutoff point ratioX \nin grouping using the predictions of:\n",glmForm,sep=""))+
  xlab("Clustering Decision Threshold / Ratio")+
  ylab("Clustering Success %")+
  ggsave(paste(folderPath,"Results\\PredictingGroup.png",sep=""), width = 8, height = 6)

write_csv(dtFrTruth,paste(folderPath,"Results\\PredictingGroups.csv",sep=""))
