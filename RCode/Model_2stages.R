setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("FolderPath.R")
source(paste(folderPath,"RCode\\ModelDataset.R",sep=""))

##### PARAMETERS RANGE ##################

pBurst_min<-0.001
pBurst_max<-0.7
pBurst_step<-0.001
thresh_min<-1
thresh_max<-20
thresh_step<-1


###### -----------------R x 2 model --------------###########################

model220.calculate_loglikelihood_AllBalloons<-function(pBurst, nbPumps, isCollected)
{
  if(pBurst<pBurst_min | pBurst>=pBurst_max)
  {
    loglikelihood_h <- -2000
    return(loglikelihood_h)
  }
  loglikelihood_h<-  0
  for(j in 1:length(nbPumps))
  {
    loglikelihood_h<- loglikelihood_h + model220.calculate_loglikelihood_h( pBurst, nbPumps[j], isCollected[j])
  }
  return(loglikelihood_h)
}

model220.calculate_loglikelihood_h<-function(pBelief_h, nbPumps, isCollected)
{
  if(nbPumps==0)
  {return(0)}
  loglikelihood_h = 0
  r_h<- 1-pBelief_h
  for(j in 1:nbPumps-1)
  {
    if(loglikelihood_h <= -2000)
    {
      loglikelihood_h = -2000
    }
    else
    { 
      if(log(r_h)< -2000)
      {loglikelihood_h= -2000}
      else
      {
        loglikelihood_h<- loglikelihood_h+log(r_h)
      }
    }
  }
  if(isCollected==1)
  {
    if (loglikelihood_h<= -2000 | log(1-r_h)<= -2000) 
    {
      loglikelihood_h = -2000
    }
    else
    { 
      loglikelihood_h<- loglikelihood_h+log(1-r_h)
    }
  }
  return(loglikelihood_h)
}

# EXAMPLE - TESTS
model220.calculate_loglikelihood_h(0.02, 45, 1)
model220.calculate_loglikelihood_AllBalloons( pBurst=0.1,  nbPumps= c(45, 60, 55), isCollected= c(1, 0, 1))


model220.calculate_loglikelihood_ID_X0<-function(data, subID,threshold)
{
  tmp<-filter(data, ID==subID)
  dtTmp<-model.data.GetModelData_22(tmp, threshold)
  ndt2<-nrow(dtTmp$d2)
  ndt1<-nrow(dtTmp$d1)
  res1 <-0
  res2 <-0
  pExpl_1<-0
  pExpl_2<-0
  sdAvgLastTime_1<-0
  sdAvgLastTime_2<-0
  avgLastTime_1<-0
  avgLastTime_2<-0
  avgPumps_1<-0
  sdPumps_1<-0
  avgPumps_2<-0
  sdPumps_2<-0
  isC_1<-0
  isC_2<-0
  if( ndt1 != 0 )  
  {
    dtOne<-dtTmp$d1
    nbP_1<-dtOne$nbPumps
    isC_1<-dtOne$isCollected
    pBurst_1<- 1-dtOne$prob_r.y[1]
    pExpl_1<-dtOne$prob_expl.y[1]
    sdAvgLastTime_1<-dtOne$sdLastTrialTime.y[1]
    avgLastTime_1<-dtOne$avgLastTrialTime.y[1]
    avgPumps_1<-dtOne$avgPumps.y[1]
    sdPumps_1<-dtOne$sdPumps.y[1]
    print(avgLastTime_1)
    pExpl_1<-(sum((1-isC_1), na.rm=TRUE)/ (sum((1-isC_1)*nbP_1, na.rm=TRUE)) )
    #pExpl_1<-(sum((1-isC_1), na.rm=TRUE)/ (sum(nbP_1, na.rm=TRUE)) )
    if((sum((1-isC_1)*nbP_1, na.rm=TRUE))==0)
    {pExpl_1<-0}
    if(pBurst_1==0)
    {res1<- -2000}
    else
    {
      res1 <- model220.calculate_loglikelihood_AllBalloons(pBurst_1, nbP_1, isC_1)[1]
      print(res1)
    }
  }
  if( ndt2 != 0)  
  {
    dtTwo<-dtTmp$d2
    nbP_2<-dtTwo$nbPumps
    isC_2<-dtTwo$isCollected
    pBurst_2<- 1-dtTwo$prob_r.y[1]
    pExpl_2<-dtTwo$prob_expl.y[1]
    sdAvgLastTime_2<-dtTwo$sdLastTrialTime.y[1]
    avgLastTime_2<-dtTwo$avgLastTrialTime.y[1]
    avgPumps_2<-dtTwo$avgPumps.y[1]
    sdPumps_2<-dtTwo$sdPumps.y[1]
    print(avgLastTime_2)
    pExpl_2<-(sum((1-isC_2), na.rm=TRUE)/ (sum((1-isC_2)*nbP_2, na.rm=TRUE)) )
    #pExpl_2<-(sum((1-isC_2), na.rm=TRUE)/ (sum(nbP_2, na.rm=TRUE)) )
    if((sum((1-isC_2)*nbP_2, na.rm=TRUE))==0)
    {pExpl_2<-0}
    if( pBurst_2==0)  
    {res2<- -2000}
    else
    { 
      res2 <- model220.calculate_loglikelihood_AllBalloons(pBurst_2, nbP_2, isC_2)[1]
      print(res2)
    }
  }
  if(ndt1==0 & ndt2==0)
  {res1<- -2000;res2<- -2000;pBurst_1<-0;pBurst_2<-0;pExpl_1<-0;pExpl_2<-0}
  
  if(ndt1==0 & ndt2!=0)
  {pBurst_1<-pBurst_2;pExpl_1<-pExpl_2}
  else if(ndt1!=0 & ndt2==0)
  {pBurst_2<-pBurst_1;pExpl_2<-pExpl_1}
  print(paste( "pExpl_1" ,pExpl_1, "pExpl_2",pExpl_2, "sdAvgLastTime_1", sdAvgLastTime_1 ,"sdAvgLastTime_2", sdAvgLastTime_2, "avgLastTime_1", avgLastTime_1 ,"avgLastTime_2", avgLastTime_2, 
               "avgPumps_1",avgPumps_1,"avgPumps_2", avgPumps_2,"sdPumps_1", sdPumps_1,"sdPumps_2", sdPumps_2,"isC_1", sum(isC_1,na.rm=T),"isC_2",sum(isC_2, na.rm=T)   ))
  ress <- data.frame(matrix(ncol = 15, nrow = 0))
  x <- c("MaxLikelihood", "pBurst_1", "pBurst_2", "pExpl_1" , "pExpl_2", "sdAvgLastTime_1", "sdAvgLastTime_2", "avgLastTime_1","avgLastTime_2", "avgPumps_1", "avgPumps_2", "sdPumps_1", "sdPumps_2","isCollected_1","isCollected_2")
  colnames(ress) <- x
  ress<-rbind(ress, data.frame(MaxLikelihood= round(-res1-res2,4), 
                               pBurst_1 = round(pBurst_1,6), 
                               pBurst_2 = round(pBurst_2,6),
                               pExpl_1 = round(pExpl_1,6), 
                               pExpl_2 = round(pExpl_2,6),
                               sdAvgLastTime_1 = round(sdAvgLastTime_1,6),
                               sdAvgLastTime_2 = round(sdAvgLastTime_2,6),
                               avgLastTime_1 = round(avgLastTime_1,6),
                               avgLastTime_2 = round(avgLastTime_2,6),
                               avgPumps_1 = round(avgPumps_1,6),
                               avgPumps_2 = round(avgPumps_2,6),
                               sdPumps_1 = round(sdPumps_1,6),
                               sdPumps_2 = round(sdPumps_2,6),
                               isCollected_1 =  sum(isC_1,na.rm=T),
                               isCollected_2 =  sum(isC_2,na.rm=T)
                               ) )

  return(ress)
}

model220.calculate_loglikelihood_ID_1<-function(data, subID, threshold)
{
  
  resultList<-list(ID =subID,  mle=2000, pBurst_1=0, pBurst_2=0, pExpl_1=0 , pExpl_2=0, sdAvgLastTime_1 =0, sdAvgLastTime_2 =0, avgLastTime_1 =0, avgLastTime_2 =0,
                   avgPumps_1=0, avgPumps_2=0, sdPumps_1=0, sdPumps_2=0, isCollected_1 = 0, isCollected_2 = 0)

    res1<-model220.calculate_loglikelihood_ID_X0(data, subID, threshold)
    if(is.na(res1))
    {return (NA)}
    
    
      resultList$mle[1]<- res1$MaxLikelihood[1]
      resultList$pBurst_1[1]<-res1$pBurst_1[1]
      resultList$pBurst_2[1]<-res1$pBurst_2[1]
      resultList$pExpl_1[1]<-res1$pExpl_1[1]
      resultList$pExpl_2[1]<-res1$pExpl_2[1]
      resultList$sdAvgLastTime_1[1]<-res1$sdAvgLastTime_1[1]
      resultList$sdAvgLastTime_2[1]<-res1$sdAvgLastTime_2[1]
      resultList$avgLastTime_1[1]<-res1$avgLastTime_1[1]
      resultList$avgLastTime_2[1]<-res1$avgLastTime_2[1]
      resultList$avgPumps_1[1]<-res1$avgPumps_1[1]
      resultList$avgPumps_2[1]<-res1$avgPumps_2[1]
      resultList$sdPumps_1[1]<-res1$sdPumps_1[1]
      resultList$sdPumps_2[1]<-res1$sdPumps_2[1]
      resultList$isCollected_1[1]<-res1$isCollected_1[1]
      resultList$isCollected_2[1]<-res1$isCollected_2[1]
  return(resultList)
}


#### ---- JOIN MODELS ------############
model222.calculate_loglikelihood_ID_X0<-function(data, subID, threshold)
{
  model220.nodeResult<- model220.calculate_loglikelihood_ID_1(data = data, subID = subID, threshold=threshold)
  if(is.na(model220.nodeResult))
  {return(NA)}
  return (data.frame(ID = model220.nodeResult$ID,
                     mle = model220.nodeResult$mle,
                     pBurst_1 = model220.nodeResult$pBurst_1,
                     pBurst_2 = model220.nodeResult$pBurst_2,
                     pExpl_1 = model220.nodeResult$pExpl_1,
                     pExpl_2 = model220.nodeResult$pExpl_2,
                     sdAvgLastTime_1 = model220.nodeResult$sdAvgLastTime_1,
                     sdAvgLastTime_2 = model220.nodeResult$sdAvgLastTime_2,
                     avgLastTime_1 = model220.nodeResult$avgLastTime_1,
                     avgLastTime_2 = model220.nodeResult$avgLastTime_2,
                     avgPumps_1 =model220.nodeResult$avgPumps_1,
                     avgPumps_2 =model220.nodeResult$avgPumps_2,
                     sdPumps_1 =model220.nodeResult$sdPumps_1,
                     sdPumps_2 =model220.nodeResult$sdPumps_2,
                     isCollected_1=model220.nodeResult$isCollected_1,
                     isCollected_2=model220.nodeResult$isCollected_2
  )[1,])
}

model222.calculate_loglikelihood_ID<-function(data, subID)
{
  maxOrder<-max(filter(data, ID==subID)$order, na.rm=TRUE)
  print(maxOrder)
  totalResult<-data.frame(ID=NA, order=NA, totalMLE=NA, mle=NA,  pBurst_1=NA,pBurst_2=NA,  pExpl_1=NA,pExpl_2=NA  ,threshold=NA, sdAvgLastTime_1=NA, sdAvgLastTime_2=NA, avgLastTime_1=NA, avgLastTime_2=NA,
                          avgPumps_1=NA,  avgPumps_2=NA, sdPumps_1=NA, sdPumps_2=NA, isCollected_1=NA, isCollected_2=NA)
  for(j in 1:maxOrder)
  { 
    orderDt<-filter(data, order==j)
    res<-NA
    
    for(i in seq(thresh_min,thresh_max, thresh_step))
    {
      oneCalcResults<-NA
      print(paste("order", j , "thresh", i))
      oneCalcResults<-model222.calculate_loglikelihood_ID_X0(data=orderDt, subID=subID, threshold=i)
      
      if(is.na(oneCalcResults))
      {return(totalResult)}
      
      oneCalcResults$threshold<-i
      
      print(oneCalcResults)
      
      if(is.na(res))
      {
        res<-oneCalcResults
      }
      else
      {
        if(res$mle>oneCalcResults$mle)
        {res<-oneCalcResults}
      }
    }
    print("completed 1 order")
    if(is.na(totalResult))
    {
      print(maxOrder)
      print("adding 1st order result")
      res$order<-j
      totalResult<-res
      print("adding 1st order result")
    }
    else
    {
      print(maxOrder)
      print("adding j order result")
      res$order<-j
      totalResult<-rbind(totalResult, res)
      print("adding j order result")
    }
  }
  print("finished Orders")
  totalResult$totalMLE<-sum(totalResult$mle)
  return(totalResult)
}
  

##### Simulation Data #############
model22.GetExplosionTrial<-function()
{
  nbMax<-116
  res<-nbMax
  for(i in 1:nbMax)
  {
    expl<-sample(i:nbMax, 1, replace=T)#random(1, i:nbMax)
    
    if(expl[1]==nbMax)
    {
      res<-i
      break;
    }
  }
  return (res)
}

model22.GetExplosionTrials<-function(nbTrials)
{
  res<-rep(NA,nbTrials)
  for(i in 1:nbTrials)
  {
    res[i]<-model22.GetExplosionTrial()
  }
  return (res)
}

model22.pumpOrcollect<-function(pBust)
{
  pbSample=sample(1:100000 , 1)
  res<-0
  #print(paste(round(100000*(1-pBust)) , pbSample))
  if (round(100000*(1-pBust))<=pbSample)
  {res<- 1}
  return (res)
}

model22.getOneTrialSims<-function(pb, nbSims)
{
  trialPumps<-NA 
  trialColOrExpl<-NA
  explosions=model22.GetExplosionTrials(nbSims)  
  
  for (expl in explosions)
  {
    for (k in 1:expl)
    {
      #print(paste(expl , k))
      pOrc=model22.pumpOrcollect(pb)
      if (pOrc==1)
      {
        if(is.na(trialPumps))
        {
          trialPumps<-c(k) 
          trialColOrExpl<-c(1)
        }
        else
        {
          trialPumps<-c(trialPumps, k)
          trialColOrExpl<-c(trialColOrExpl, 1)
        }
        break
      }
#      if(k==expl)
#      {
#        if(is.na(trialPumps))
#        {
#          trialPumps<-c(k)
#          trialColOrExpl<-c(0)
#        }
#        else
#        {
#          trialPumps<-c(trialPumps, k)
#          trialColOrExpl<-c(trialColOrExpl, 0)
#        }
#      }
    }
  }
  meanTrials<-mean(trialPumps, na.rm=TRUE)
  return(meanTrials)
}

model22.getOneTrial<-function(pb)
{
  ash1<-1
  for(i in 1:116)
  {
    ash1=ash1*(1-pb)
    if(ash1<0.5)
    {
      randChoice=sample(0:1 , 1)
      return (i-randChoice)
    }
  }
}

model22.getAvgPumps2<-function(pb1, pb2, threshold, nbTrials)
{
  resPumps<-NA
  
  m1num<-(threshold-1)*model22.getOneTrial(pb1)
  m2num<-(nbTrials-threshold+1)*model22.getOneTrial(pb2)
  return((m1num+m2num)/nbTrials )
}

models.GetHistogramCollectedPct<-function(nbSims,beta, gammaPlus, pbustr1 , nbRuns)
{
  sims100<-c(NA,nbSims)
  for(i in 1:nbSims)
  { 
    nbTrials<-nbRuns
    sims100[i]<-sum(model222.SimulateRuns_Exploration(beta, gammaPlus, pbustr1,nbTrials)$isCollected)/nbTrials*100
  }
  hist(sims100, breaks = 20)
  return (mean(sims100))
}

models.ReadAllModelData<-function(pathLastPart)
{
  model16_summary <- read_csv(paste(folderPath,pathLastPart,sep=""))
  model16_summary<-data.frame(unclass(model16_summary), check.names = FALSE, stringsAsFactors = FALSE)
  
  model16_summary<-filter(model16_summary, mle<2000 & mle!=0 )
  model16_summary$pBurst_1<-model16_summary$pBurst_1
  model16_summary$pBurst_2<-model16_summary$pBurst_2
  model16_summary$pExpl_1<-model16_summary$pExpl_1
  model16_summary$pExpl_2<-model16_summary$pExpl_2

  model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$pBurst_1 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$pBurst_2
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$pBurst_2 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$pBurst_1
  model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$pExpl_1 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$pExpl_2
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$pExpl_2 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$pExpl_1
  model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$sdAvgLastTime_1 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$sdAvgLastTime_2
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$sdAvgLastTime_2 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$sdAvgLastTime_1
  model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$avgLastTime_1 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_1) &!is.na(model16_summary$sdAvgLastTime_2),]$avgLastTime_2
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$avgLastTime_2 <- 
    model16_summary[is.na(model16_summary$sdAvgLastTime_2) &!is.na(model16_summary$sdAvgLastTime_1),]$avgLastTime_1
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) | is.na(model16_summary$sdAvgLastTime_1),]$threshold <- 20
  model16_summary$LearningBool <- 1
  model16_summary[is.na(model16_summary$sdAvgLastTime_2) | is.na(model16_summary$sdAvgLastTime_1),]$LearningBool <- 0
  return(model16_summary)
}
################# RUNS #################


cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S1"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle,pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2,threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S1.csv", sep=""), row.names = FALSE)
stopCluster(cl)

cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S2"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle,
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2,
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S2.csv", sep=""), row.names = FALSE)
stopCluster(cl)


cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S3"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle, 
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2,
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S3.csv", sep=""), row.names = FALSE)
stopCluster(cl)

cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S4"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle, 
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2,
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S4.csv", sep=""), row.names = FALSE)
stopCluster(cl)

cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S5"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle, 
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2,
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S5.csv", sep=""), row.names = FALSE)
stopCluster(cl)

cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S7"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle, 
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2, 
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S7.csv", sep=""), row.names = FALSE)
stopCluster(cl)

cl<-makeCluster(1)
registerDoParallel(cl)
t3<-Sys.time()
tmp<-filter(baselinePreDataX, substr(ID,1,2) %in% c("S8"))
subjectList<-unique(tmp$ID)
model222.results <- foreach(subjectID =subjectList , .combine = rbind, 
                            .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                            .inorder = FALSE) %dopar% 
  {nodeResult<- model222.calculate_loglikelihood_ID(data = tmp, subID = subjectID)
  if(!is.na(nodeResult))
  {data.frame(ID = nodeResult$ID, Order =nodeResult$order, TotalMLE=nodeResult$totalMLE ,mle = nodeResult$mle, 
              pBurst_1=nodeResult$pBurst_1,
              pBurst_2=nodeResult$pBurst_2,  pExpl_1=nodeResult$pExpl_1,  pExpl_2=nodeResult$pExpl_2, 
              threshold=nodeResult$threshold,
              sdAvgLastTime_1=nodeResult$sdAvgLastTime_1, sdAvgLastTime_2=nodeResult$sdAvgLastTime_2,
              avgLastTime_1=nodeResult$avgLastTime_1, avgLastTime_2=nodeResult$avgLastTime_2,
              avgPumps_1=nodeResult$avgPumps_1,avgPumps_2=nodeResult$avgPumps_2,sdPumps_1=nodeResult$sdPumps_1,sdPumps_2=nodeResult$sdPumps_2,
              nbCollected_1=nodeResult$isCollected_1,nbCollected_2=nodeResult$isCollected_2)
  }
  }
Sys.time()-t3
summary(model222.results$mle)
sum(model222.results$mle)
write.csv(model222.results, paste(folderPath, "Results\\modelResults_S8.csv", sep=""), row.names = FALSE)
stopCluster(cl)


###### SIMULATIONS ###################
longDataset_nc<-read.csv(paste(folderPath,"Data\\workingData_Long_NOTS4.csv",sep=""))
longDataset_nc$pBurst_1<-longDataset_nc$pBurst_1/100
longDataset_nc$pBurst_2<-longDataset_nc$pBurst_2/100
fullModelsS2<-dplyr::select(filter(longDataset_nc, !is.na(nbTr) & nbTr>0 & STUDY %in% c("S1","S3","S5")),
                            SUBJ_ID,pBurst_1,pBurst_2,threshold,nbTr, avgPumps)
longDataset_c<-read.csv(paste(folderPath,"Data\\workingData_Long_S4.csv",sep=""))
longDataset_c$pBurst_1<-longDataset_c$pBurst_1/100
longDataset_c$pBurst_2<-longDataset_c$pBurst_2/100
fullModelsS4<-dplyr::select(filter(longDataset_c, !is.na(pBurst_1) &!is.na(nbTr) & nbTr>0 & STUDY %in% c("S4","S8")),
                            SUBJ_ID,pBurst_1,pBurst_2,threshold,nbTr, avgPumps)

allSubjsPumps<-NA
cl<-makeCluster(1)
registerDoParallel(cl)
rowsRange<-c(1:nrow(fullModelsS2))
stage.results <- foreach(rowX =rowsRange , .combine = rbind, 
                         .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                         .inorder = FALSE) %do% 
  {
    dt<-fullModelsS2[rowX,]
    print(rowX)
    nodeResult<- mean(model22.getAvgPumps2(dt$pBurst_1, dt$pBurst_2, dt$threshold, dt$nbTr), na.rm=TRUE)
    if(!is.na(nodeResult))
    {data.frame(trialPumps=nodeResult, ID=dt$SUBJ_ID)}
  }
stopCluster(cl)
allSubjsPumps<-stage.results$trialPumps

jpeg(paste(folderPath,"Results\\S2SubjsPumps.jpg",sep=""), width = 1100, height = 450)
par(mfrow=c(1,2))
hist(allSubjsPumps,breaks = 100, 
     main = "Non-Clinical Lab Study Average Pumps \n(simulation)",
     xlab="Average Pumps Per Participant and Condition",
     ylim=c(0,50),xlim=c(0,120))
hist(fullModelsS2$avgPumps,breaks = 100, 
     main = "Non-Clinical  Lab Study Average Pumps \n(actual)",
     xlab="Average Pumps Per Participant and Condition",
     ylim=c(0,50),xlim=c(0,120))
dev.off()

allSubjsPumps<-NA
cl<-makeCluster(1)
registerDoParallel(cl)
rowsRange<-c(1:nrow(fullModelsS4))
stage.results <- foreach(rowX =rowsRange , .combine = rbind, 
                         .packages=c("doParallel","foreach", "parallel" , "neldermead", "readr" , "lme4", "stats" , "dplyr" , "Hmisc"),
                         .inorder = FALSE) %do% 
  {
    dt<-fullModelsS4[rowX,]
    print(rowX)
    nodeResult<- mean(model22.getAvgPumps2(dt$pBurst_1, dt$pBurst_2, dt$threshold, dt$nbTr), na.rm=TRUE)
    if(!is.na(nodeResult))
    {data.frame(trialPumps=nodeResult, ID=dt$SUBJ_ID)}
  }
stopCluster(cl)
allSubjsPumps<-stage.results$trialPumps
mean(allSubjsPumps, na.rm=TRUE);summary(fullModelsS4$avgPumps)
jpeg(paste(folderPath,"Results\\S4SubjsPumps.jpg",sep=""), width = 1100, height = 450)
par(mfrow=c(1,2))
hist(allSubjsPumps,breaks = 100, 
     main = "Clinical Average Pumps (simulation)",
     xlab="Average Pumps Per Participant and Condition",
     ylim=c(0,20),xlim=c(0,120))
hist(fullModelsS4$avgPumps,breaks = 100, 
     main = "Clinical Average Pumps (actual)",
     xlab="Average Pumps Per Participant and Condition",
     ylim=c(0,20),xlim=c(0,120))
dev.off()


