#This file will make a table of all the info we have on Day 1 and turn it into a DO probability to pass on to another
#program that will take the MSE level data (or end of semester level data).

# let us first boost the admissions table with genter and HS provenance.

<<<<<<< HEAD
=======
#Meeting plan for Sameer and I Thursday:
#Understand git branches (checkout, merge, add, commit)

>>>>>>> findingDO
# ---- load data
rm(list=ls())
path.to.data.directory<-"C:/Work/JAC/PAREA/CEGEP_Data_mining_for_student_success/DawsonCollege/ScienceProgram/2016/FindingDO/"
load(paste0(path.to.data.directory,'student_success.RData'))

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)
library(caret)
library(pROC)

source('DOfinder.R')
DO<-DOfinder(20103,20083)

#----boost admission data
e<-etudiant[,.(student_number,sexe)]

setkey(admission,student_number)
setkey(e,student_number)

better.adm <- merge(admission,e)

#Now you need to get the etudiant_cours_secondaire and get their HS to see if that could be a predictor.
#Note that you will likely be increasing the number of predictors from a few to a shit ton.

