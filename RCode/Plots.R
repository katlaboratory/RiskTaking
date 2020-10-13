rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(lme4)
library(merTools)
library(ggplot2)
source("FolderPath.R")
source(paste(folderPath,"RCode\\summarySE.R",sep=""))
source(paste(folderPath,"RCode\\R_rainclouds.R",sep=""))


ncData<-read_csv(paste(folderPath,"Data\\workingData_trialLong_NOTS4.csv",sep=""))
ncData[ncData$experimentClass=="BodyInflate" ,]$experimentClass<-"BodyIncrease"
ncData[ncData$experimentClass=="BodyDeflate" ,]$experimentClass<-"BodyDecrease"
ncData[ncData$experimentClass=="BalloonInflate" ,]$experimentClass<-"BalloonIncrease"
ncData[ncData$experimentClass=="BalloonDeflate" ,]$experimentClass<-"BalloonDecrease"
ncData$experimentClass<-as.factor(ncData$experimentClass)
ncData$STUDY<-as.factor(ncData$STUDY)
ncData$OrderF<-as.factor(ncData$OrderF)
ncData<-data.frame(unclass(ncData), check.names = FALSE, stringsAsFactors = FALSE)

cData<-read_csv(paste(folderPath,"Data\\workingData_Long_S4.csv",sep=""))
cData[cData$experimentClass=="BodyInflate" ,]$experimentClass<-"BodyIncrease"
cData[cData$experimentClass=="BodyDeflate" ,]$experimentClass<-"BodyDecrease"
cData[cData$experimentClass=="BalloonInflate" ,]$experimentClass<-"BalloonIncrease"
cData[cData$experimentClass=="BalloonDeflate" ,]$experimentClass<-"BalloonDecrease"
cData$experimentClass<-as.factor(cData$experimentClass)
cData$Hesitance<-1000*log(cData$avgLastTrialTime)
cData<-data.frame(unclass(cData), check.names = FALSE, stringsAsFactors = FALSE)

RegressionComponentsDescriptionList<-c("Fixed Effects", "Random Effects", "Full Model")
RegressionComponentsLevelList<-c(0.9999,0.95,0.95)
RegressionComponentsList<-c("fixed", "random", "full")
RegressionComponentsList<-c("fixed")
for(cnt in 1:length(RegressionComponentsList))
  #for(cnt in 1:1)
{
  dtALL<-filter(ncData, !is.na(AGE)& !is.na(BMI)& lastTrialTime<20)
  lmerAll<-lmer(1000*log(lastTrialTime)~AGE + BMI + Direction*Stimulus*EDE_Q_Restrain +(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=dtALL, REML=FALSE)
  dtALL$predlmer<-predict(lmerAll, newdata = dtALL, re.form = NA)
  dtALL_PI <- predictInterval(merMod = lmerAll, newdata = dtALL,
                              level = RegressionComponentsLevelList[cnt], n.sims = 1000,
                              stat = "mean", type="linear.prediction",
                              which=c(RegressionComponentsList[cnt]),
                              include.resid.var=T)
  dtALL<-cbind(dtALL,dtALL_PI)
  dtALL$Condition<-as.factor(dtALL$experimentClass)
  
  plot_ncHES<-ggplot(dtALL, aes(x = EDE_Q_Restrain, y = fit, color = Condition) ) +
    geom_smooth(method=lm, level=RegressionComponentsLevelList[cnt], aes(fill=Condition))+
    ylab('Hesitance')+
    xlab('EDE-Q Restraint' )+
    scale_colour_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+
    #ggtitle(paste("Hesitance vs EDE_Q Restraint\nAll Conditions ",RegressionComponentsDescriptionList[cnt] ,sep=""))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggsave(paste(folderPath,"Results\\hesitanceVsEDEQR_allConditions_X",RegressionComponentsList[cnt],".png",sep=""),height=3, width=10 , dpi = 1000)
  
  
  dtALL<-filter(ncData, !is.na(AGE)& !is.na(BMI))
  lmerAll<-lmer(colPumps~AGE + BMI + Direction*Stimulus*EDE_Q_Restrain +(1|SUBJ_ID)+ (1|OrderF)+ (1|STUDY), data=dtALL, REML=FALSE)
  dtALL$predlmer<-predict(lmerAll, newdata = dtALL, re.form = NA)
  dtALL_PI <- predictInterval(merMod = lmerAll, newdata = dtALL,
                              level = RegressionComponentsLevelList[cnt], n.sims = 1000,
                              stat = "mean", type="linear.prediction",
                              which=c(RegressionComponentsList[cnt]),
                              include.resid.var=T)
  dtALL<-cbind(dtALL,dtALL_PI)
  dtALL$Condition<-as.factor(dtALL$experimentClass)
  
  plot_ncClicks<-ggplot(dtALL, aes(x = EDE_Q_Restrain, y = fit, color = Condition) ) +
    geom_smooth(method=lm, level=RegressionComponentsLevelList[cnt], aes(fill=Condition))+
    ylab('Clicks')+
    xlab('EDE-Q Restraint' )+
    scale_colour_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+
    #ylim(0,40)+
    #ggtitle(paste("Collected Pumps vs EDE_Q Restraint\nAll Conditions ",RegressionComponentsDescriptionList[cnt],sep=""))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    ggsave(paste(folderPath,"Results\\pumpsVsEDEQR_allConditions_X",RegressionComponentsList[cnt],".png",sep=""),height=3, width=10 ,dpi = 1000)
  
}
sC_summaryStats <- summarySE(cData, measurevar = "avgPumps",na.rm = TRUE,
                             groupvars=c("GROUP", "experimentClass"))
sC_summaryStats$GROUP <-  as.factor(sC_summaryStats$GROUP)
sC_summaryStats$experimentClass <-  as.factor(sC_summaryStats$experimentClass)
plot_cClicks <- ggplot(cData,aes(x=experimentClass, y=avgPumps, fill = GROUP, colour = GROUP))+
  geom_flat_violin(aes(fill = GROUP),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(position = position_jitter(width = .1, height = 0.05), size = 1, shape = 18)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, aes(x =  as.numeric(experimentClass)-.1,
                                                                             y = avgPumps_mean,
                                                                             group = GROUP, colour = GROUP), 
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, aes(x =  as.numeric(experimentClass)-.1, y = avgPumps_mean, 
                                                                                group = GROUP,   ymin = avgPumps_mean-ci, 
                                                                                ymax = avgPumps_mean+ci, colour =GROUP), width = 0.2, size = 0.8)+
  ylab('Clicks')+
  xlab('' )+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  #ggtitle("Clinical Study: Clicks per Group and Condition")+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylim(0,75) +
  #theme(axis.title.x = element_text(hjust=0))+
  theme(axis.title.x = element_text(hjust=0),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggsave(paste(folderPath,"Results\\PumpPerCondAndGroup_X.png",sep=""), width = 10, height = 6, dpi=1000)
plot_cClicks


hesDt<-filter(cData,avgLastTrialTime<10)
sCH_summaryStats <- summarySE(filter(hesDt), measurevar = "Hesitance",na.rm = TRUE,
                              groupvars=c("GROUP", "experimentClass"))
sCH_summaryStats$GROUP <-  as.factor(sCH_summaryStats$GROUP)
sCH_summaryStats$experimentClass <-  as.factor(sCH_summaryStats$experimentClass)

plot_ncHES <- ggplot(hesDt,aes(x=experimentClass, y=Hesitance, fill = GROUP, colour = GROUP))+
  geom_flat_violin(aes(fill = GROUP),position = position_nudge(x = .1, y = 0),
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(position = position_jitter(width = .1, height = 0.05), size = 1, shape = 18)+
  geom_point(position=position_dodge(width=0.12),data = sCH_summaryStats, aes(x =  as.numeric(experimentClass)-.1,
                                                                              y = Hesitance_mean,
                                                                              group = GROUP, colour = GROUP), 
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sCH_summaryStats, aes(x =  as.numeric(experimentClass)-.1, y = Hesitance_mean, 
                                                                                 group = GROUP,   ymin = Hesitance_mean-ci, 
                                                                                 ymax = Hesitance_mean+ci, colour =GROUP), width = 0.2, size = 0.8)+
  ylab('Hesitance')+
  xlab('' )+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  #ggtitle("Clinical Study: Hesitance in Exploration per Group and Condition")+
  geom_hline(yintercept=0, linetype = "dashed")+
  #ylim(13,16) +
  theme(axis.title.x = element_text(hjust=0),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.title.x = element_text(hjust=0))+
  ggsave(paste(folderPath,"Results\\HesitancePerCondAndGroup_X.png",sep=""), width = 10, height = 6, dpi=1000)
plot_ncHES
