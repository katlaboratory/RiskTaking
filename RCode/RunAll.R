rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("FolderPath.R")
source(paste(folderPath,"RCode\\Analysis_Clinical.R",sep=""))
rmarkdown::render(paste(folderPath,'RCode\\SM_C.rmd',sep=""), output_format = 'word_document')

source(paste(folderPath,"RCode\\Analysis_nonClinical.R",sep=""))
rmarkdown::render(paste(folderPath,'RCode\\SM_NC.rmd',sep=""), output_format = 'word_document')

source(paste(folderPath,"RCode\\Model_Comparison.R",sep=""))

source(paste(folderPath,"RCode\\Plots.R",sep=""))
