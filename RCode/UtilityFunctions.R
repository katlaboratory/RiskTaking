
util.flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    col = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
    #p = pmat[ut]
  )
}

util.GetFullDataSetFromIdenticalFiles<-function(flist,fpath)
{
  cnt<-FALSE
  for (file in flist)
  {
    if(grepl("csv", file)==FALSE)
    {next}
    # if the merged dataset doesn't exist, create it
    if (cnt==FALSE){
      tdataset <- read.csv(paste(fpath,file,sep=""), header=TRUE)
      cnt<-TRUE
      next
    }

    if (exists("tdataset")){
      temp_dataset <-read.csv(paste(fpath,file,sep=""), header=TRUE)
      tdataset<-rbind(tdataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  res<-NA
  if(cnt==TRUE)
  {res<-tdataset}
  return(res)
}


util.GetPValueMLM<-function(data , predictorName, dependentVariableName, useStudyLevel=TRUE)
{
  tmpDt<-data[!is.na(data[[predictorName]]), ]
  
  if (nrow(tmpDt) == 0)
  {return(1)}

  multiLevelString<-"(1|SUBJ_ID) + (1|OrderF)"
  if(useStudyLevel==TRUE & length(unique(tmpDt$STUDY))>1 )
  {multiLevelString<-paste(multiLevelString,"+ (1|STUDY)", sep="")}
  strFormula_1<-paste(dependentVariableName, "~ AGE + BMI + Direction*Stimulus + ", multiLevelString, sep="")
  strFormula_2<-paste(dependentVariableName, "~ AGE + BMI + Direction*Stimulus + ",   predictorName, "+"  , multiLevelString, sep="")
  
  lm_0<-lmer(as.formula(strFormula_1), data = tmpDt, REML = FALSE)
  lm_1<-lmer(as.formula(strFormula_2), data = tmpDt, REML = FALSE)
  vano<-anova(lm_0, lm_1)
  res<-c(vano[2,]$`Pr(>Chisq)` , summary(lm_1)$coefficients[2,][1])
  return(res)
}

util.GetPValueMLMInteractions<-function(data , predictorName, dependentVariableName, useStudyLevel=TRUE)
{
  tmpDt<-data[!is.na(data[[predictorName]]), ]
  
  if (nrow(tmpDt) == 0)
  {return(1)}
  #print(nrow(tmpDt))
  # print(dependentVariableName)
  #print(predictorName)
  multiLevelString<-"(1|SUBJ_ID) + (1|OrderF)"
  if(useStudyLevel==TRUE & length(unique(tmpDt$STUDY))>1 )
  {multiLevelString<-paste(multiLevelString,"+ (1|STUDY)", sep="")}
  strFormula_1<-paste(dependentVariableName, "~ AGE + BMI + Direction*Stimulus + ", multiLevelString, sep="")
  strFormula_2<-paste(dependentVariableName, "~ AGE + BMI + Direction*Stimulus + ",   predictorName, "+"  , multiLevelString, sep="")
  strFormula_3<-paste(dependentVariableName, "~ AGE + BMI + Direction*Stimulus*",   predictorName, "+"  , multiLevelString, sep="")
  
  lm_0<-lmer(as.formula(strFormula_2), data = tmpDt, REML = FALSE)
  lm_1<-lmer(as.formula(strFormula_3), data = tmpDt, REML = FALSE)
  
  vano<-anova(lm_0, lm_1)
  res<-c(vano[2,]$`Pr(>Chisq)` , summary(lm_1)$coefficients[2,][1])
  return(res)
}

util.GetAllSignificantPredictorsForOneDPMLM<-function(data , dependentVariableName , useStudyLevel=TRUE, getInteractions =FALSE)
{
  predVars2<-alldata.independents
  for(cnt in 1:length(alldata.independents))
  {
    #print(alldata.independents[cnt])
    if(min(data[[alldata.independents[cnt]]], na.rm=TRUE) == Inf & max(data[[alldata.independents[cnt]]], na.rm=TRUE) == -Inf)
    {
      predVars2<-predVars2[!predVars2 %in% c(alldata.independents[cnt])]
    }
  }
  predVars2<-predVars2[!predVars2 %in% c("AGE", "BMI")]
  res <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(res)<-c("Predictor", "P", "Slope")
  rCnt<-1
  tmpDt<-data[!(data[[dependentVariableName]] %in% 
                  unique(outliers.GetOulierValues(data, c(dependentVariableName))$OUTLIER_VALUE)), ]
  #[!(data$SUBJ_ID %in% 
              #    unique(outliers.GetOulierSubjects(data, c(dependentVariableName))$SUBJ_ID)), ]
  for (cnt in 1:length(predVars2))
  {
    print(predVars2[cnt])
    oneCalc<-NA
    if(getInteractions == FALSE)
    {
      oneCalc<-util.GetPValueMLM(tmpDt, predVars2[cnt], dependentVariableName, useStudyLevel)
    }
    else
    {
      oneCalc<-util.GetPValueMLMInteractions(tmpDt, predVars2[cnt], dependentVariableName, useStudyLevel)
    }
    pv<-oneCalc[1]
    
    if(pv<0.05)
    {
      prdList1<-list(Predictor = predVars2[cnt], P= pv, Slope= oneCalc[2])
      
      res<-rbind(res, prdList1, stringsAsFactors=FALSE)
      rCnt<- rCnt +1
    }
  }
  
  return(res)
}




util.GetExcludedSubjectIDs_NOTS4<-function(data)
{
  resultDf<-data#[!data$STUDY %in% c("S4"), ]
  resultDf<-resultDf[resultDf$AGE < 18| resultDf$BMI <= 16.5, ]
  return(unique(resultDf$SUBJ_ID))
}

util.GetExcludedSubjectIDs_S4<-function(data)
{
  resultDf<-data  
  resultDf<-resultDf[resultDf$AGE < 18 | resultDf$SUBJ_ID %in% c("S4_22", "S4_37" , "S4_71", "S8_20" ,  "S8_47", "S8_48"), ]
  # S4_22 (AN) has psychosis, S4_37 (RE) has bulimia, 
  # S4_71 (HC) has anxiety disorder
  # S8_47 RE already participated as AN previously
  # S8_48 MALE
  # S8_20 same person as S4_12
  return(unique(resultDf[!is.na(resultDf$SUBJ_ID),]$SUBJ_ID))
}


util.GetComparisonResults<-function(data , dependentVariable, predictor, predictorMissing=NA, stim=NA, direct=NA, useBMI=TRUE, useAGE=TRUE, useStudyAsLevel=TRUE , ageBiggerThan=0, othePredictors=NA, useOutliers = TRUE)
{
  nbFormulas <-1
  formula0<-""
  formula1<-""
  formula2<-""
  formula3<-""
  addIdAndOrder<-" + (1|SUBJ_ID) + (1|OrderF)"
  addStudy<-" + (1|STUDY)"
  tmp<-data[!is.na(data[[predictor]]),]
  if(useOutliers == TRUE)
  {
    tmp<-tmp[  !(tmp$SUBJ_ID %in% unique(outliers.GetOulierSubjects(tmp,c(dependentVariable))$SUBJ_ID)), ]
  }
  if(!is.na(predictorMissing))
  {
    tmp<-tmp[tmp[[predictorMissing]] == "OK", ]
  }
  if(!is.na(stim))
  {
    tmp<-tmp[tmp$Stimulus == stim,]
  }
  if(!is.na(direct))
  {
    tmp<-tmp[tmp$Direction == direct,]
  }
  if(!is.na(direct) & !is.na(stim))
  {
    tmp<-tmp[tmp$Direction == direct & tmp$Stimulus == stim,]
  }          
  if(ageBiggerThan!=0)
  {
    tmp<-tmp[tmp$AGE>ageBiggerThan, ]
  }
  print(paste("rows: ",nrow(tmp)))
  formula0<-paste(dependentVariable, "~", sep="")
  if(useAGE == TRUE & useBMI == FALSE)
  {
    formula0<-paste(formula0, "AGE", sep="")
  }
  else if(useBMI == TRUE & useAGE == FALSE)
  {
    formula0<-paste(formula0, "BMI", sep="")
  }
  else if(useBMI == TRUE & useAGE == TRUE)
  {
    formula0<-paste(formula0, "AGE + BMI", sep="")
  }
  if(!is.na(othePredictors))
  {
    formula0<-paste(formula0, "+", othePredictors, sep="")
  }
  
  
  
  addDirAndOrStim<-""
  if(!is.na(stim) & !is.na(direct))
  {
    nbFormulas<-2
    addDirAndOrStim<-""   
  }
  else if(!is.na(stim))
  {
    nbFormulas<-4
    addDirAndOrStim<-"+ Direction"
  }
  else if(!is.na(direct))
  {
    nbFormulas<-4
    addDirAndOrStim<-"+ Stimulus"
  }
  else
  {
    nbFormulas<-4
    addDirAndOrStim<-"+ Direction*Stimulus"
  }
  
  if(nbFormulas == 2)
  {
    formula1<-paste(formula, predictor , addIdAndOrder, sep="")
  }
  if( nbFormulas == 4)
  {
    formula1<-paste(formula0, addDirAndOrStim , addIdAndOrder, sep="")
    formula2<-paste(formula0, addDirAndOrStim," + ", predictor , addIdAndOrder, sep="")
    formula3<-paste(formula0, addDirAndOrStim,"*", predictor , addIdAndOrder, sep="")
  }
  
  formula0<-paste(formula0, addIdAndOrder, sep="")
  
  if(useStudyAsLevel==TRUE)
  { formula0<-paste(formula0, addStudy, sep="")
  formula1<-paste(formula1, addStudy, sep="")
  formula2<-paste(formula2, addStudy, sep="")
  formula3<-paste(formula3, addStudy, sep="")
  }
  
  print(formula0)
  print(formula1)
  print(formula2)
  print(formula3)
  
  anv<-NA
  
  LModel1<-lmer(as.formula(formula0), data = tmp)
  LModel2<-NA
  LModel3<-NA
  LModel4<-NA
  if(nbFormulas == 2)
  {
    LModel2<-lmer(as.formula(formula1), data = tmp)
    anv<-anova(LModel1, LModel2)
    lmForPairs<-LModel2
  }
  if(nbFormulas == 4)
  {
    LModel2<-lmer(as.formula(formula1), data = tmp)
    LModel3<-lmer(as.formula(formula2), data = tmp)
    LModel4<-lmer(as.formula(formula3), data = tmp)
    anv<-  anova(LModel1, LModel2, LModel3, LModel4)
    
  }
  prAnova<-print(anv)
  
  pairsResults1<-NA
  pairsResults2<-NA
  spVec<-NA
  if(nbFormulas == 4)
  {
    if(is.na(stim) & is.na(direct))
    { 
      spVec <- c("Direction","Stimulus")
    }
    else if(is.na(stim))
    {
      spVec <- c("Stimulus")
    }
    else if(is.na(direct))
    { 
      spVec <- c("Direction")
    }
  }
  return(list(resAv=anv , resLm4 = LModel4 , specsVector = spVec))
  
  if(nbFormulas == 4)
  {
    if(is.na(stim) & is.na(direct))
    { 
      fiber.emt1 <- emtrends(object =LModel4, specs = c("Direction"), var = predictor, pbkrtest.limit = 50000)
      pairsResults1<-pairs(fiber.emt1)
      fiber.emt2 <- emtrends(object =LModel4, specs =c("Stimulus"), var = predictor, pbkrtest.limit = 50000)
      pairsResults2<-pairs(fiber.emt2)
    }
    else if(is.na(stim))
    {
      fiber.emt3 <- emtrends(object =LModel4, specs =c("Stimulus"), var = predictor, pbkrtest.limit = 50000)
      pairsResults2<-pairs(fiber.emt3)
    }
    else if(is.na(direct))
    { 
      inter<-quote(LModel4)
      fiber.emt4 <- emtrends(object = inter, specs = c("Direction"), var = predictor, pbkrtest.limit = 50000)
      pairsResults1<-pairs(fiber.emt4)
    }
  }
  
  
  return(list(resAv=anv , resDirection = pairsResults1, resStimulus = pairsResults2, resLm4 = LModel4))
}

report.GetBayesianEquation<-function(summaryLG, evidenceIV, priorBeliefIV)
{
  biasBU<- round(summaryLG$coefficients['(Intercept)','Estimate'],2)
  lambdaBU<- -round(summaryLG$coefficients[priorBeliefIV,'Estimate'],2)
  evidenceBUfactor<-round(summaryLG$coefficients[evidenceIV,'Estimate']/lambdaBU,2)
  evidenceBUfixed<-round(biasBU/lambdaBU,2)
  res<-paste("BeliefUpdate=",lambdaBU,"x( (" ,evidenceBUfixed, "+",evidenceBUfactor, "xEvidence) - PriorBelief)",sep="")
  return (res)
}
report.GetReportingResultLG_2vars<-function(summaryLG, strIV1, strIV2, strShortIV1, strShortIV2)
{
  return (paste(
    "a=", round(summaryLG$coefficients["(Intercept)","Estimate"],2),"(",
    round(summaryLG$coefficients["(Intercept)","Std. Error"],2), "), t","a" ,"=",
    round(summaryLG$coefficients["(Intercept)","t value"],2), ", ", 
    report.GetPStr(summaryLG$coefficients["(Intercept)","Pr(>|t|)"]), 
    ", b",strShortIV1,"=" , round(summaryLG$coefficients[strIV1,"Estimate"],2),"(",
    round(summaryLG$coefficients[strIV1,"Std. Error"],2), "), t",strShortIV1 ,"=",
    round(summaryLG$coefficients[strIV1,"t value"],2), ", ", 
    report.GetPStr(summaryLG$coefficients[strIV1,"Pr(>|t|)"]), 
    ", b",strShortIV2,"=" , round(summaryLG$coefficients[strIV2,"Estimate"],2),"(",
    round(summaryLG$coefficients[strIV2,"Std. Error"],2), "), t",strShortIV2 ,"=",
    round(summaryLG$coefficients[strIV2,"t value"],2), ", ", 
    report.GetPStr(summaryLG$coefficients[strIV2,"Pr(>|t|)"]), 
    ", F(",
    round(summaryLG$fstatistic["numdf"]),",",round(summaryLG$fstatistic["dendf"]),")=",
    round(summaryLG$fstatistic["value"],2),", Adj $R^2$=",
    round(summaryLG$adj.r.squared,3),
    sep=""))
}
report.GetReportingResultLG<-function(summaryLG, indiVariableStr)
{
  return (paste(
    "b=" , round(summaryLG$coefficients[indiVariableStr,"Estimate"],3),"(",
    round(summaryLG$coefficients[indiVariableStr,"Std. Error"],3), "), t=",
    round(summaryLG$coefficients[indiVariableStr,"t value"],2), ", ", 
    report.GetPStr(summaryLG$coefficients[indiVariableStr,"Pr(>|t|)"]), ", F(",
    round(summaryLG$fstatistic["numdf"]),",",round(summaryLG$fstatistic["dendf"]),")=",
    round(summaryLG$fstatistic["value"],2),", Adj $R^2$=",
    round(summaryLG$adj.r.squared,3),
    sep=""))
}
report.GetReportingResultLG_Group1<-function(summaryLG, indiVariableStr , strCategLevel)
{
  #report.GetReportingResultLG<-function(summaryLG, indiVariableStr)
  IV<-paste(indiVariableStr,strCategLevel, sep="")
  return (paste(
    "$b_{",strCategLevel,"}$=" , round(summaryLG$coefficients[IV,"Estimate"],3),"(",
    round(summaryLG$coefficients[IV,"Std. Error"],3), 
    "), $t_{",strCategLevel,"}$=",
    round(summaryLG$coefficients[IV,"t value"],2),", ",
    report.GetPStr(summaryLG$coefficients[IV,"Pr(>|t|)"]), 
    sep=""))
}
report.GetReportingResultLG_Groups<-function(summaryLG, indiVariableStr, vecCategoryLevels)
{
  #report.GetReportingResultLG<-function(summaryLG, indiVariableStr)
  IVresults<-""
  firstComma<-""
  for(lev in vecCategoryLevels)
  {
    IVresults<-paste(IVresults,  firstComma,
                     report.GetReportingResultLG_Group1(summaryLG, indiVariableStr , lev),
                     sep="")
    firstComma<-", "
  }
  return (paste(
    IVresults,", ",
    "F(",
    round(summaryLG$fstatistic["numdf"]),",",round(summaryLG$fstatistic["dendf"]),")=",
    round(summaryLG$fstatistic["value"],2),", Adj $R^2$=",
    round(summaryLG$adj.r.squared,3),
    sep=""))
}
report.GetReportingResultLG_Groups<-function(summaryLG, vecIndiVariables, vecCategoryLevels)
{
  #report.GetReportingResultLG<-function(summaryLG, indiVariableStr)
  IVresults<-""
  firstComma<-""
  for(indiVariableStr in vecIndiVariables)
  {
    for(lev in vecCategoryLevels)
    {
      IVresults<-paste(IVresults,  firstComma,
                       report.GetReportingResultLG_Group1(summaryLG, indiVariableStr , lev),
                       sep="")
      firstComma<-", "
    }
  }
  return (paste(
    IVresults,", ",
    "F(",
    round(summaryLG$fstatistic["numdf"]),",",round(summaryLG$fstatistic["dendf"]),")=",
    round(summaryLG$fstatistic["value"],2),", Adj $R^2$=",
    round(summaryLG$adj.r.squared,3),
    sep=""))
}
report.GetShortReportingResultLG<-function(summaryLG, indiVariableStr)
{
  return (paste(
    "b=" , round(summaryLG$coefficients[indiVariableStr,"Estimate"],3),"(",
    round(summaryLG$coefficients[indiVariableStr,"Std. Error"],3), "), ", 
    report.GetPStr(summaryLG$coefficients[indiVariableStr,"Pr(>|t|)"]),
    sep=""))
}
report.GetShortReportingResultMLM<-function(mlm0,mlmN, aovResult, strIV , strMLM)
{
  sumMLM<-summary(mlmN)$coefficients
  reporting_result<-paste(
    "b=", round(sumMLM[strIV,"Estimate"],2), 
    "(", round(sumMLM[strIV,"Std. Error"],2), "), ",
    "t=",round(sumMLM[strIV,"t value"],2) , ", ",
    "$X^2$(",aovResult[strMLM,"Df"] ,")=", round(aovResult[strMLM,"Chisq"],2),", ",
    report.GetPStr(aovResult[strMLM,"Pr(>Chisq)"]),", ",
    "$R^2$=", round(report.GetRsq(mlm0,mlmN),3),", ",
    "$f^2$=", round(report.Getfsq(report.GetRsq(mlm0,mlmN)),3),
    sep=""
  )
  return(reporting_result)
}
report.GetShortReportingResultMLM_Group1<-function(mlm0,mlmN, aovResult, indiVariableStr, strCategoryLevel , strMLM)
{
  #report.GetShortReportingResultMLM(lm0_SDG,lm5_SDG, av_SDG, 
  #                   "StimulusBody:DirectionInflate:GROUPAN" , "lm5_SDG")
  IV<-paste(indiVariableStr, strCategoryLevel, sep="")
  sumMLM<-summary(mlmN)$coefficients
  reporting_result<-paste(
    "$b_{",strCategoryLevel ,"}$=", round(sumMLM[IV,"Estimate"],2), 
    "(", round(sumMLM[IV,"Std. Error"],2), "), ",
    "$t_{",strCategoryLevel,"}$=",round(sumMLM[IV,"t value"],2) , 
    sep=""
  )
  return(reporting_result)
}
report.GetShortReportingResultMLM_Groups<-function(mlm0,mlmN, aovResult, indiVariableStr, vecCategoryLevels , strMLM)
{
  IVresults<-""
  firstComma<-""
  for(lev in vecCategoryLevels)
  {
    IVresults<-paste(IVresults,  firstComma,
                     report.GetShortReportingResultMLM_Group1(mlm0,mlmN, aovResult, indiVariableStr, lev , strMLM),
                     sep="")
    firstComma<-", "
  }
  reporting_result<-paste(
    IVresults, ", ",
    "$X^2$(",aovResult[strMLM,"Df"] ,")=", round(aovResult[strMLM,"Chisq"],2),", ",
    report.GetPStr(aovResult[strMLM,"Pr(>Chisq)"]),", ",
    "$R^2$=", round(report.GetRsq(mlm0,mlmN),3),", ",
    "$f^2$=", round(report.Getfsq(report.GetRsq(mlm0,mlmN)),3),
    sep=""
  )
  return(reporting_result)
}

report.GetShortReportingResultMLM_Group1<-function(mlm0,mlmN, aovResult, indiVariableStr, strCategoryLevel , strMLM)
{
  #report.GetShortReportingResultMLM(lm0_SDG,lm5_SDG, av_SDG, 
  #                   "StimulusBody:DirectionInflate:GROUPAN" , "lm5_SDG")
  IV<-paste(indiVariableStr, strCategoryLevel, sep="")
  sumMLM<-summary(mlmN)$coefficients
  reporting_result<-paste(
    "$b_{",strCategoryLevel ,"}$=", round(sumMLM[IV,"Estimate"],2), 
    "(", round(sumMLM[IV,"Std. Error"],2), "), ",
    "$t_{",strCategoryLevel,"}$=",round(sumMLM[IV,"t value"],2) , 
    sep=""
  )
  return(list(reporting_result=reporting_result, catLevel = strCategoryLevel, b=round(sumMLM[IV,"Estimate"],2),
              b_sd=round(sumMLM[IV,"Std. Error"],2), t=round(sumMLM[IV,"t value"],2) ) )
}
report.GetShortReportingResultMLM_Groups<-function(mlm0,mlmN, aovResult, indiVariableStr, vecCategoryLevels , strMLM)
{
  IVresults<-""
  firstComma<-""
  for(lev in vecCategoryLevels)
  {
    IVresults<-paste(IVresults,  firstComma,
                     report.GetShortReportingResultMLM_Group1(mlm0,mlmN, aovResult, indiVariableStr, lev , strMLM)$reporting_result,
                     sep="")
    firstComma<-", "
  }
  reporting_result<-paste(
    IVresults, ", ",
    "$X^2$(",aovResult[strMLM,"Df"] ,")=", round(aovResult[strMLM,"Chisq"],2),", ",
    report.GetPStr(aovResult[strMLM,"Pr(>Chisq)"]),", ",
    "$R^2$=", round(report.GetRsq(mlm0,mlmN),3),", ",
    "$f^2$=", round(report.Getfsq(report.GetRsq(mlm0,mlmN)),3),
    sep=""
  )
  return(list(reporting_result=reporting_result, pv=report.GetPStr(aovResult[strMLM,"Pr(>Chisq)"]),
              ChiSqr=round(aovResult[strMLM,"Chisq"],2),
              ChiSqrDf=aovResult[strMLM,"Df"], Rsqr=round(report.GetRsq(mlm0,mlmN),3),
              fsqr=round(report.Getfsq(report.GetRsq(mlm0,mlmN)),3)))
}

report.tTest<-function(tTest)
{
  return (paste("t= ", round(tTest$statistic,2), ", df= ", tTest$parameter,
                ", " ,report.GetPStr(tTest$p.value), sep=""  ))
}

report.GetStars<-function(x)
{
  res<-""
  if(x>0.05)
  {res<-""}
  else if(x>0.01)
  {res<-"*"}
  else if(x>0.001)
  {res<-"**"}
  else if(x<=0.001)
  {res<-"***"}
  return(res)
}
report.GetPStr_old<-function(x)
{
  res<-""
  if(x>0.05)
  {res<-paste("p=", round(x,2) ) }
  else if(x>0.01)
  {res<-"**p<0.05**"}
  else if(x>0.001)
  {res<-"**p<0.01**"}
  else if(x<=0.001)
  {res<-"**p<0.001**"}
  return(res)
}
report.GetPStr<-function(x)
{
  res<-""
  if(x>0.05)
  {res<-paste("p=", round(x,3) ) }
  else if(x>0.01)
  {res<-paste("p=", round(x,3) )}
  else if(x>0.001)
  {res<-paste("p=", round(x,3) )}
  else if(x<=0.001)
  {res<-"**p<0.001**"}
  return(res)
}
report.GetPStrVector<-function(vecx)
{
  resVec<-NA
  cnt<-0
  for(x in vecx)
  {
    res<-NA
    if(is.na(x))
    {res<-NA}
    else if(x>0.05)
    {res<-paste("p=", round(x,2) ) }
    else if(x>0.01)
    {res<-"**p<0.05**"}
    else if(x>0.001)
    {res<-"**p<0.01**"}
    else if(x<=0.001)
    {res<-"**p<0.001**"}
    if(cnt==0)
    {resVec<-c(res)}
    else
    {
      resVec<-rbind(resVec, res)
    }
    cnt<-cnt+1
  }
  return(resVec)
}
report.GetRsq<-function(lm_base, lm_current)
{
  xvarLM1<-as.data.frame(VarCorr(lm_base))
  xtotalVarLM1<-sum(xvarLM1$vcov)
  xresidLM1<-xvarLM1[xvarLM1$grp=='Residual','vcov']
  xvarLM6<-as.data.frame(VarCorr(lm_current))
  xtotalVarLM6<-sum(xvarLM6$vcov)
  xresidLM6<-xvarLM6[xvarLM6$grp=='Residual','vcov']
  xr2lm6<-1-(xtotalVarLM6/xtotalVarLM1); 
  return(xr2lm6)
}
report.Getfsq<-function(Rsq)
{
  return(Rsq/(1-Rsq))
}
report.GetRsqVector<-function(lmVector)
{
  resultVector<-NA
  lm_base<-lmVector[1]
  cnt<-1
  for(x_lm in lmVector)
  {
    if(cnt==1)
    {cnt<-cnt+1;next}
    resultVector<- rbind(resultVector,report.GetRsq(unlist(lm_base), unlist(x_lm)) )
  }
  return(resultVector)
}
report.GetfsqVector<-function(lmVector)
{
  resultVector<-NA
  lm_base<-lmVector[1]
  cnt<-1
  for(x_lm in lmVector)
  {
    if(cnt==1)
    {cnt<-cnt+1;next}
    resultVector<- rbind(resultVector,report.Getfsq(report.GetRsq(lm_base, x_lm)) )
    
    cnt<-cnt+1
  }
  return(resultVector)
}
report.GetICCTable<-function(lmFinal)
{
  sICC<-summary(lmFinal)
  vecOfRandomVariables<-c(names(sICC$varcor), "Residual")
  mlm_ICC<- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("RandomEffect", "ICC" )
  colnames(mlm_ICC) <- x
  for(var in vecOfRandomVariables)
  {
    mlm_ICC<-rbind(mlm_ICC,data.frame(RandomEffect=var,ICC=round(100*report.GetICC(lmFinal,var),2) ))
  }
  x <- c("Random Effect (Level)", "ICC %" )
  colnames(mlm_ICC) <- x
  return(mlm_ICC)
}

report.GetICC<-function(lm_current, levelName)
{
  xvarDF<-as.data.frame(VarCorr(lm_current))
  xtotalVar<-sum(xvarDF$vcov)
  print(xtotalVar)
  xlevelVar<-xvarDF[xvarDF$grp==levelName,'vcov']
  print(xlevelVar)
  res<- (xlevelVar/xtotalVar); 
  return(res)
}
report.GetPChiStr<-function(x)
{
  res<-""
  if(x>0.05)
  {res<-paste("Pr(>Chisq)=", round(x,2) ) }
  else if(x>0.01)
  {res<-"Pr(>Chisq)<0.05*"}
  else if(x>0.001)
  {res<-"Pr(>Chisq)<0.01**"}
  else if(x<=0.001)
  {res<-"Pr(>Chisq)<0.001***"}
  return(res)
}

report.GetMLMResults<-function(dv, fivc, fiv , riv, dt)
{
  lmerMinusOne<-NA
  lmerFinal<-NA
  avResult<-NA
  strRandomEffects<-paste(riv, collapse="+")
  lmerBase<-lmer(as.formula(paste(dv, "~",strRandomEffects, sep="")) , data=dt, REML=FALSE)
  strFixedCovariates<-paste(fivc, collapse="+")
  strFixedIV<-""
  fixIV<-fiv
  if(length(fixIV)>1)
  {
    strFixedIVm1<-paste(paste(fixIV[1:(length(fixIV)-1)],collapse="*"),"+",fixIV[length(fixIV)],sep="")
    strFixedIVfin<-paste(fixIV,collapse="*")
    lmerMinusOne<-lmer(as.formula(paste(dv, "~",strFixedCovariates,"+",strFixedIVm1,"+",strRandomEffects, sep="")) , data=dt, REML=FALSE)
    lmerFinal<-lmer(as.formula(paste(dv, "~",strFixedCovariates,"+",strFixedIVfin,"+",strRandomEffects, sep="")) , data=dt, REML=FALSE)
    avResult<-anova(lmerMinusOne,lmerFinal)
  }
  else if(length(fixIV)==1)
  {
    strFixedIVfin<-fixIV[1]
    lmerMinusOne<-lmer(as.formula(paste(dv, "~",strFixedCovariates,"+",strRandomEffects, sep="")) , data=dt, REML=FALSE)
    lmerFinal<-lmer(as.formula(paste(dv, "~",strFixedCovariates,"+",strFixedIVfin,"+",strRandomEffects, sep="")) , data=dt, REML=FALSE)
    avResult<-anova(lmerMinusOne,lmerFinal)
  }
  
  return(list(lmerBase = lmerBase, lmerFinal=lmerFinal, avResult=avResult,
              iv=paste(c(unlist(fiv)), collapse=" x ")))
}

report.GetReportingResults<-function(resMLM,
                                     stringOfFinalResult,groupsList,
                                     stringLmerFinal)
{
  lmerBase<-resMLM$lmerBase
  lmerFinal<-resMLM$lmerFinal 
  avResults<-resMLM$avResult
  reporting_nbObs<-paste( "nbObservations=" ,summary(lmerFinal)$devcomp$dims["N"] , ", nbParticipants=" ,summary(lmerFinal)$ngrps["SUBJ_ID"], sep="")
  reportingResult<-report.GetShortReportingResultMLM_Groups(lmerBase,lmerFinal, avResults,
                                                            stringOfFinalResult,groupsList,
                                                            stringLmerFinal) 
  reporting_icc<-report.GetICCTable(lmerFinal)
  rep_N<-ifelse("SUBJ_ID" %in% names(summary(lmerFinal)$ngrps), summary(lmerFinal)$ngrps["SUBJ_ID"],NA)
  return (list(reportingResult=reportingResult , reporting_nbObs=reporting_nbObs,
               reporting_icc=reporting_icc, reporting_N=rep_N,reporting_Nobs=summary(lmerFinal)$devcomp$dims["N"] ))
}


report.GetCategories<-function(glm1, newdataIN , ratio)
{
  newdataIN$prGroup<-predict(glm1, newdata = newdataIN, type = "response")
  newdataIN$G<-NA
  newdataIN[!is.na(newdataIN$prGroup) & newdataIN$prGroup>=ratio,]$G<-"A" 
  newdataIN[!is.na(newdataIN$prGroup) & newdataIN$prGroup<ratio,]$G<-"H" 
  newdataIN$G<-as.factor(newdataIN$G)
  return(list(A=summary(newdataIN$G)["A"] , H=summary(newdataIN$G)["H"]))
}

report.GetTruthTable<-function(glmFormula, dtPrediction , ratio)
{
  glm1<-glm(as.formula(glmFormula), data = filter(dtPrediction, G %in% c("H","A")), family = "binomial")
  
  newdataAN<-filter(dtPrediction, G %in% c("A"))
  newdataHC<-filter(dtPrediction, G %in% c("H"))
  anCat<-report.GetCategories(glm1, newdataAN , ratio)
  hcCat<-report.GetCategories(glm1, newdataHC , ratio)
  return(list(
    ANpredictedAN= anCat["A"],
    ANpredictedHC= anCat["H"],
    HCpredictedAN= hcCat["A"],
    HCpredictedHC= hcCat["H"]
  ))
}
predict.GetGroupSuccessPct_1combo_apply<-function(clusterList)
{
  print(groupListXXX);print(clusterList)
  for(i in  1:length(clusterList))
  {
    if(length(dataXXX[dataXXX$clustering==clusterList[i],]$clustering)>0)
    {
      dataXXX[dataXXX$clustering==clusterList[i],]$clustering<-groupListXXX[i]
    }
  }
  
  sumClustering<-summary(as.factor(paste(dataXXX$GROUP, dataXXX$clustering,sep="_")))
  #print(sumClustering)
  AN_ClusterSuccess<-100*(sumClustering['AN_ANcluster']/(sumClustering['AN_ANcluster']+sumClustering['AN_HCcluster']+sumClustering['AN_REcluster']))
  HC_ClusterSuccess<-100*(sumClustering['HC_L_HCcluster']/(sumClustering['HC_L_ANcluster']+sumClustering['HC_L_HCcluster']+sumClustering['HC_L_REcluster']))
  RE_ClusterSuccess<-100*(sumClustering['RE_REcluster']/(sumClustering['RE_ANcluster']+sumClustering['RE_HCcluster']++sumClustering['RE_REcluster']))
  
  return (c(AN_ClusterSuccess, HC_ClusterSuccess, RE_ClusterSuccess))
  
}
predict.GetGroupSuccessPct_1combo<-function(data,groupList, clusterList)
{
  #print(groupList);print(clusterList)
  for(i in  1:length(clusterList))
  {
    if(length(data[data$clustering==clusterList[i],]$clustering)>0)
    {
      data[data$clustering==clusterList[i],]$clustering<-groupList[i]
    }
  }
  
  sumClustering<-summary(as.factor(paste(data$GROUP, data$clustering,sep="_")))
  #print(sumClustering)
  AN_ClusterSuccess<-100*(sumClustering['AN_ANcluster']/(sumClustering['AN_ANcluster']+sumClustering['AN_HCcluster']+sumClustering['AN_REcluster']))
  HC_ClusterSuccess<-100*(sumClustering['HC_L_HCcluster']/(sumClustering['HC_L_ANcluster']+sumClustering['HC_L_HCcluster']+sumClustering['HC_L_REcluster']))
  RE_ClusterSuccess<-100*(sumClustering['RE_REcluster']/(sumClustering['RE_ANcluster']+sumClustering['RE_HCcluster']++sumClustering['RE_REcluster']))
  
  return (c(AN_ClusterSuccess, HC_ClusterSuccess, RE_ClusterSuccess))
  
}
predict.GetGroupSuccessPct_1combo_2G<-function(data,groupList, clusterList)
{
  #print(groupList);print(clusterList)
  for(i in  1:length(clusterList))
  {
    if(length(data[data$clustering==clusterList[i],]$clustering)>0)
    {
      data[data$clustering==clusterList[i],]$clustering<-groupList[i]
    }
  }
  
  sumClustering<-summary(as.factor(paste(data$GROUP, data$clustering,sep="_")))
  #print(sumClustering)
  AN_ClusterSuccess<-100*(sumClustering['AN_ANcluster']/(sumClustering['AN_ANcluster']+sumClustering['AN_HCcluster']))
  HC_ClusterSuccess<-100*(sumClustering['HC_L_HCcluster']/(sumClustering['HC_L_ANcluster']+sumClustering['HC_L_HCcluster']))

  return (c(AN_ClusterSuccess, HC_ClusterSuccess))
  
}
predict.GetGroupSuccessPct_1combo_2GH<-function(data,groupList, clusterList)
{
  #print(groupList);print(clusterList)
  for(i in  1:length(clusterList))
  {
    if(length(data[data$clustering==clusterList[i],]$clustering)>0)
    {
      data[data$clustering==clusterList[i],]$clustering<-groupList[i]
    }
  }
  
  sumClustering<-summary(as.factor(paste(data$GROUP, data$clustering,sep="_")))
  #print(sumClustering)
  HCH_ClusterSuccess<-100*(sumClustering['HC_H_HCHcluster']/(sumClustering['HC_H_HCHcluster']+sumClustering['HC_L_HCcluster']))
  HC_ClusterSuccess<-100*(sumClustering['HC_L_HCcluster']/(sumClustering['HC_H_HCHcluster']+sumClustering['HC_L_HCcluster']))
  
  return (c(HCH_ClusterSuccess, HC_ClusterSuccess))
  
}
predict.GetGroupSuccessPct_1combo_4G<-function(data,groupList, clusterList)
{
  #print(groupList);print(clusterList)
  for(i in  1:length(clusterList))
  {
    if(length(data[data$clustering==clusterList[i],]$clustering)>0)
    {
      data[data$clustering==clusterList[i],]$clustering<-groupList[i]
    }
  }
  
  sumClustering<-summary(as.factor(paste(data$GROUP, data$clustering,sep="_")))
  #print(sumClustering)
  AN_ClusterSuccess<-100*(sumClustering['AN_ANcluster']/(sumClustering['AN_ANcluster']+sumClustering['AN_HCcluster']+sumClustering['AN_REcluster']+sumClustering['AN_HCHcluster']))
  HC_ClusterSuccess<-100*(sumClustering['HC_L_HCcluster']/(sumClustering['HC_L_ANcluster']+sumClustering['HC_L_HCcluster']+sumClustering['HC_L_REcluster']+sumClustering['HC_L_HCHcluster']))
  RE_ClusterSuccess<-100*(sumClustering['RE_REcluster']/(sumClustering['RE_ANcluster']+sumClustering['RE_HCcluster']+sumClustering['RE_REcluster']+sumClustering['RE_HCHcluster']))
  HCH_ClusterSuccess<-100*(sumClustering['HC_H_HCHcluster']/(sumClustering['HC_H_ANcluster']+sumClustering['HC_H_HCcluster']+sumClustering['HC_H_REcluster']+sumClustering['HC_H_HCHcluster']))
  
  return (c(AN_ClusterSuccess, HC_ClusterSuccess, HCH_ClusterSuccess, RE_ClusterSuccess))
  
}
predict.GetGroupSuccessPct<-function(data, groupList, clusterList)
{
  permutationsGroups<-permn(clusterList)
  resultList<-c(0,0,0)
  resPerm<-NA; maxAn<-0;maxHc<-0;maxRe<-0
  maxAnPerm<-NA;maxHcPerm<-NA;
  for(permutation in permutationsGroups)
  {
    ls<-predict.GetGroupSuccessPct_1combo(data,groupList,permutation)
    if(!is.na(mean(ls, na.rm=TRUE))& mean(ls, na.rm=TRUE) >mean(resultList, na.rm=TRUE) )
    { print(ls);print(permutation)
      resultList<-ls;resPerm<-permutation}
    if(!is.na(ls[1]) & ls[1]>maxAn){maxAn<-ls[1];maxAnPerm<-permutation;maxAnSuccess<-ls}
    if(!is.na(ls[2]) & ls[2]>maxHc){maxHc<-ls[2];maxHcPerm<-permutation;maxHcSuccess<-ls}
    if(!is.na(ls[3]) & ls[3]>maxRe){maxRe<-ls[3]}
  }
  return (list(successPct=resultList, perm=resPerm, maxAn=maxAn, maxHc=maxHc, maxRe=maxRe,
               maxAnPerm=maxAnPerm, maxHcPerm=maxHcPerm,maxAnSuccess=maxAnSuccess, maxHcSuccess=maxHcSuccess))
}
predict.GetGroupSuccessPct_2G<-function(data, groupList, clusterList)
{
  permutationsGroups<-permn(clusterList)
  resultList<-c(0,0,0)
  resPerm<-NA; maxAn<-0;maxHc<-0;
  maxAnPerm<-NA;maxHcPerm<-NA;
  for(permutation in permutationsGroups)
  {
    ls<-predict.GetGroupSuccessPct_1combo_2G(data,groupList,permutation)
    if(!is.na(mean(ls, na.rm=TRUE))& mean(ls, na.rm=TRUE) >mean(resultList, na.rm=TRUE) )
    { print(ls);print(permutation)
      resultList<-ls;resPerm<-permutation}
    if(!is.na(ls[1]) & ls[1]>maxAn){maxAn<-ls[1];maxAnPerm<-permutation;maxAnSuccess<-ls}
    if(!is.na(ls[2]) & ls[2]>maxHc){maxHc<-ls[2];maxHcPerm<-permutation;maxHcSuccess<-ls}
  }
  return (list(successPct=resultList, perm=resPerm, maxAn=maxAn, maxHc=maxHc,
               maxAnPerm=maxAnPerm, maxHcPerm=maxHcPerm,maxAnSuccess=maxAnSuccess, maxHcSuccess=maxHcSuccess))
}
predict.GetGroupSuccessPct_2GH<-function(data, groupList, clusterList)
{
  permutationsGroups<-permn(clusterList)
  resultList<-c(0,0,0)
  resPerm<-NA; maxHch<-0;maxHc<-0;
  maxHchPerm<-NA;maxHcPerm<-NA;
  for(permutation in permutationsGroups)
  {
    ls<-predict.GetGroupSuccessPct_1combo_2GH(data,groupList,permutation)
    if(!is.na(mean(ls, na.rm=TRUE))& mean(ls, na.rm=TRUE) >mean(resultList, na.rm=TRUE) )
    { print(ls);print(permutation)
      resultList<-ls;resPerm<-permutation}
    if(!is.na(ls[1]) & ls[1]>maxHch){maxHch<-ls[1];maxHchPerm<-permutation;maxHchSuccess<-ls}
    if(!is.na(ls[2]) & ls[2]>maxHc){maxHc<-ls[2];maxHcPerm<-permutation;maxHcSuccess<-ls}
  }
  return (list(successPct=resultList, perm=resPerm, maxHch=maxHch, maxHc=maxHc,
               maxHchPerm=maxHchPerm, maxHcPerm=maxHcPerm,maxHchSuccess=maxHchSuccess, maxHcSuccess=maxHcSuccess))
}
predict.GetGroupSuccessPct_4G<-function(data, groupList, clusterList)
{
  permutationsGroups<-permn(clusterList)
  resultList<-c(0,0,0,0)
  resPerm<-NA; maxAn<-0;maxHc<-0;
  maxAnPerm<-NA;maxHcPerm<-NA;
  for(permutation in permutationsGroups)
  {
    ls<-predict.GetGroupSuccessPct_1combo_4G(data,groupList,permutation)
    if(!is.na(mean(ls, na.rm=TRUE))& mean(ls, na.rm=TRUE) >mean(resultList, na.rm=TRUE) )
    { print(ls);print(permutation)
      resultList<-ls;resPerm<-permutation}
    if(!is.na(ls[1]) & ls[1]>maxAn){maxAn<-ls[1];maxAnPerm<-permutation;maxAnSuccess<-ls}
    if(!is.na(ls[2]) & ls[2]>maxHc){maxHc<-ls[2];maxHcPerm<-permutation;maxHcSuccess<-ls}
  }
  return (list(successPct=resultList, perm=resPerm, maxAn=maxAn, maxHc=maxHc,
               maxAnPerm=maxAnPerm, maxHcPerm=maxHcPerm,maxAnSuccess=maxAnSuccess, maxHcSuccess=maxHcSuccess))
}
predict.GetClusteringInternalValidationIndex<-function(distMatrix, clustering, rounding=2, clName="")
{
  clStats<-cluster.stats(distMatrix, clustering, G2=TRUE, G3=TRUE) 
  return (list(name=clName, dunn=round(clStats$dunn,rounding), g2=round(clStats$g2,rounding), 
               g3=round(clStats$g3,rounding), silhouette=round(clStats$avg.silwidth,rounding)))
}

predict.GetValidationKMeans<-function(dataPredX, dataDt , distanceD, clustersX, 
                                      groupList, clustersList, clName)
{
  dtPred<-dataPredX
  kmx<-kmeans(dataDt, centers=clustersX, nstart = 25)
  dtPred$clustering<-kmx$cluster
  rrr<-predict.GetGroupSuccessPct(dtPred, groupList, clustersList)
  km<-predict.GetGroupSuccessPct_1combo(dtPred, groupList,  rrr$perm)
  mean(km)
  kmResults<-predict.GetClusteringInternalValidationIndex(distanceD, 
                                                          dtPred$clustering, 2,clName)
  return(list(name=kmResults$name, dunn=kmResults$dunn, g2=kmResults$dunn, 
              g3=kmResults$g3, silhouette=kmResults$silhouette,
              anSuccess=round(km["AN_ANcluster"],1),hcSuccess=round(km["HC_L_HCcluster"],1),
              reSuccess=round(km["RE_REcluster"],1),
              meanSuccess=round(mean(km, na.rm=TRUE),1)
  ))
}
predict.GetValidationKMeans_2G<-function(dataPredX, dataDt , distanceD, clustersX, 
                                      groupList, clustersList, clName)
{
  dtPred<-dataPredX
  kmx<-kmeans(dataDt, centers=clustersX, nstart = 25)
  dtPred$clustering<-kmx$cluster
  rrr<-predict.GetGroupSuccessPct_2G(dtPred, groupList, clustersList)
  km<-predict.GetGroupSuccessPct_1combo_2G(dtPred, groupList,  rrr$perm)
  mean(km)
  kmResults<-predict.GetClusteringInternalValidationIndex(distanceD, 
                                                          dtPred$clustering, 2,clName)
  return(list(name=kmResults$name, dunn=kmResults$dunn, g2=kmResults$dunn, 
              g3=kmResults$g3, silhouette=kmResults$silhouette,
              anSuccess=round(km["AN_ANcluster"],1),hcSuccess=round(km["HC_L_HCcluster"],1),
              meanSuccess=round(mean(km, na.rm=TRUE),1)
  ))
}

predict.GetValidationHClust<-function(dataPredX, dataDt , distanceD, clustersX, 
                                      groupList, clustersList, clName)
{
  dtPred<-dataPredX
  fit <- hclust(distanceD, method="ward.D2")
  groups <- cutree(fit, k=clustersX) 
  dtPred$clustering<-groups
  rrr<-predict.GetGroupSuccessPct(dtPred, groupList, clustersList)
  km<-predict.GetGroupSuccessPct_1combo(dtPred, groupList,  rrr$perm)
  mean(km)
  kmResults<-predict.GetClusteringInternalValidationIndex(distanceD, 
                                                          dtPred$clustering, 2,clName)
  return(list(name=kmResults$name, dunn=kmResults$dunn, g2=kmResults$dunn, 
              g3=kmResults$g3, silhouette=kmResults$silhouette,
              anSuccess=round(km["AN_ANcluster"],1),hcSuccess=round(km["HC_L_HCcluster"],1),
              reSuccess=round(km["RE_REcluster"],1),
              meanSuccess=round(mean(km, na.rm=TRUE),1)
  ))
}
predict.GetValidationHClust_2G<-function(dataPredX, dataDt , distanceD, clustersX, 
                                      groupList, clustersList, clName)
{
  dtPred<-dataPredX
  fit <- hclust(distanceD, method="ward.D2")
  groups <- cutree(fit, k=clustersX) 
  dtPred$clustering<-groups
  rrr<-predict.GetGroupSuccessPct_2G(dtPred, groupList, clustersList)
  km<-predict.GetGroupSuccessPct_1combo_2G(dtPred, groupList,  rrr$perm)
  mean(km)
  kmResults<-predict.GetClusteringInternalValidationIndex(distanceD, 
                                                          dtPred$clustering, 2,clName)
  return(list(name=kmResults$name, dunn=kmResults$dunn, g2=kmResults$dunn, 
              g3=kmResults$g3, silhouette=kmResults$silhouette,
              anSuccess=round(km["AN_ANcluster"],1),hcSuccess=round(km["HC_L_HCcluster"],1),
              meanSuccess=round(mean(km, na.rm=TRUE),1)
  ))
}


report.GenerateResultsTable<-function(fivList, rivList, searchStrList, catLevelsList, ivList,
                                      dtSubset, dataSet, fivc=c("AGE"))
{
  dfResultsColPumpsGroup<-data.frame(  DataSubset=character(),dv=character(),
                                       iv=character(),N=numeric(), Nobs=numeric(),p=character(), Xsqr=numeric(),Xdf=numeric(),Fsqr=numeric(),
                                       reporting_result=character(),
                                       stringsAsFactors = FALSE)
  
  rowArgumentsDF <- data.frame(fivList=matrix(fivList, nrow=length(fivList), byrow=T),
                               rivList=matrix(rivList, nrow=length(rivList), byrow=T),
                               searchStrList=matrix(searchStrList, nrow=length(searchStrList), byrow=T),
                               catLevelsList=matrix(catLevelsList, nrow=length(catLevelsList), byrow=T))
  for(ivNb in 1:length(ivList))                                   
  {
    for(rowNb in 1:nrow(rowArgumentsDF))
    {
      res<-report.GetMLMResults(dv=ivList[ivNb], fivc=fivc, 
                                fiv=c(unlist(rowArgumentsDF$fivList[rowNb])) , riv=c(unlist(rowArgumentsDF$rivList[rowNb])),
                                dt=tmp)
      reporting<-report.GetReportingResults(res,unlist(rowArgumentsDF$searchStrList[rowNb]),
                                            c(unlist(rowArgumentsDF$catLevelsList[rowNb])),"lmerFinal" )
      #dfResultsColPumpsGroup$reporting_result<-as.character(dfResultsColPumpsGroup$reporting_result)
      #dfResultsColPumpsGroup$dv<-as.character(dfResultsColPumpsGroup$dv)
      #dfResultsColPumpsGroup$iv<-as.character(dfResultsColPumpsGroup$iv)
      #dfResultsColPumpsGroup$p<-as.character(dfResultsColPumpsGroup$p)
      #dfResultsColPumpsGroup$DataSubset<-as.character(dfResultsColPumpsGroup$DataSubset)
      #report.GetShortReportingResultMLM_Groups(lm_base_SDG,lm5_SDG, av_SDG, 
      #                                         "StimulusBody:DirectionInflate:GROUP",c("AN","RE","HC_H"),
      #                                         "lm5_SDG")
      print(reporting$reportingResult$reporting_result)
      thisResult<-list(
        DataSubset=dtSubset,
        dv=ivList[ivNb],
        iv=res$iv, 
        N=reporting$reporting_N,
        obs=reporting$reporting_Nobs,
        p=reporting$reportingResult$pv,
        Xsqr=reporting$reportingResult$ChiSqr,
        Xdf=reporting$reportingResult$ChiSqrDf,
        Fsqr=reporting$reportingResult$fsqr,
        reporting_result=reporting$reportingResult$reporting_result)

     # if(nrow(dfResultsColPumpsGroup)>0)
    #  {print("existing rows")
    #    dfResultsColPumpsGroup<-rbind(dfResultsColPumpsGroup, thisResult)}
    #  else{print("new row")
       # print(as.data.frame(as.matrix(thisResult)))
        dfResultsColPumpsGroup<-rbind(dfResultsColPumpsGroup,as.data.frame((thisResult)))
        #}
    }
  }
  rownames(dfResultsColPumpsGroup)<-NULL
  return(dfResultsColPumpsGroup)
}


util.t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  invisible(t.col)
  return(t.col)
}