
# This file will use the list of DO found in the DOfinder files and use it to figure things out about them.


#Can you finish cegep without graduating: is there any state in which this is possible? Just took 1 class to get into
#some program at Uni? 
#Let's make a DT with all the info about the DOs
#Get the population indicator from admission
#Past schooling from etudes_precedentes
#Gender from etudiant, langue maternelle, postal code?
#Note, COteR from inscription table

#Clean up this file and have DOfinder be DO-functions remove all the inputs and libraries to it so that every file
#that I create afterwards will be using from this functions.

#Create a RProj file so that all files will be put in the same project.

## ---- load-data ----
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

#Start fresh by looking at the SO status for many of the DOs. The hunch is that MANY people DO without ever registering
#for a single class meaning that they would show up in admission, but not in inscription. 
#this may be as large as HALF the DOs ever admitted by the college.
DOS<-etudiant_session$student_number %in% DO$student_number
DOG<-etudiant_session$student_number %in% student_certification$student_number

etudiant_session[,DO:=ifelse(DOS,1,ifelse(DOG,0,0))]

DOfinder <- glm(DO ~ program + SPE + TypeFrequentation,etudiant_session,family="binomial")
summary(DOfinder)

ES <- etudiant_session
ES$Pred <- predict(DOfinder,ES,type="response")

#confusionMatrix(data2$Pred,data2$WITHDRAWAL)
rocCur<-roc(etudiant_session$DO,ES$Pred)
auc(rocCur)
#Area under the curve: 0.84
ci.auc(rocCur)
#0.838-0.841

## ---- IDESDO ---- 
IDESDO<-etudiant_session[student_number %in% DO$student_number][,.(student_number,IDEtudiantSession,SPE,
                                                                   TypeFrequentation)]
IDESDO[,DO:=ifelse(etudiant_session$student_number == DO[student_number],1,ifelse(etudiant_session$student_number
                                                    == student_certification$student_number,0,0))]
IDESDO[,.N,by=COUNT]

## ----DO stats ----
DOstat<- etudiant_session[student_number %in% DO$student_number][,.(student_number,TypeFrequentation,ansession,IDEtudiantSession)]
DOstat[,.N, by=TypeFrequentation]
DOstat[, ':=' (COUNT = .N), by=student_number]
DOstatSO<-DOstat[TypeFrequentation=="SO"]


## ---- Test ----

#Has anyone with SO as frequentation ever graduated?
gradcheck<- etudiant_session[student_number %in% student_certification$student_number][,.(student_number,TypeFrequentation)]
gradcheck[, ':=' (COUNT = .N), by=student_number]
#Yes, many people have SOs. When do they get put in there? 
length(unique(gradcheck$student_number))
#28660
length(unique(gradcheck$student_number[gradcheck$TypeFrequentation=="SO"]))
#17927
#About 63% of graduates have an SO status in their profile in contrast to 13468/14038 (96% of DOs)
#It seems that there is little difference between the presence of SO status for people who DO or grad

## ---- Ins ----
InIDESDO <- inscription[IDEtudiantSession %in% DOstatSO$IDEtudiantSession]
InIDESDO[,.N,by=Note]

## ---- Test ----
MSEDO<-eval_etudiant[student_number %in% DO$student_number]
MSEgrad <- eval_etudiant[student_number %in% gradcheck$student_number]
MSEDO[,.N,by=MSE]
MSEgrad[,.N,by=MSE]
#Are the ratios of passing, at risk and failing the same for DOs and grads?
#Passing DO: 267541/327152 = 81.8
#Passing grad: 477441/543562 = 87.7

#At-risk DO: 30299/327152 = 9.3
#At-risk grad: 38205/543562 = 7.0

#Failing DO: 24472/327152 = 7.5
#Failing grad: 22691/543562 = 4.2

#By definition, this means that there are plenty of students who get a failing MSE, but pass. 

#Let's try focusing on the semester when the DO happens.

#I want to grab the last lines where a student number appears. This is hard... 

#Let's try looking at the average performance of DO vs grads. 
temp1<-MSEDO[student_number %in% etudiant_session$student_number]
temp2<-etudiant_session[student_number %in% MSEDO$student_number]

setkey(temp1,temp2,student_number)

## ---- Predicting DOs ----
AdDO<-admission[student_number %in% DO$student_number]
Adgrad<-admission[student_number %in% student_certification$student_number]

admission[,DO:=ifelse(admission$student_number %in% DO$student_number,1,ifelse(admission$student_number %in% 
                                                          student_certification$student_number,0,0))]

DOfind <- glm(DO ~ program + reforme + population + IndicateurReadmission, admission,family="binomial")
summary(DOfind)

adm2 <- admission
adm2$Pred <- predict(DOfind,adm2,type="response")

confusionMatrix(round(adm2$Pred),admission$DO)
rocCur<-roc(admission$DO,adm2$Pred)
auc(rocCur)
#Area under the curve: 0.859
ci.auc(rocCur)
#0.856-0.862

## ---- Test----


## ---- MSE ----
#Let's build a prediction model based on the MSE evals only. 

DOS1<-eval_etudiant$student_number %in% DO$student_number
DOG1<-eval_etudiant$student_number %in% student_certification$student_number
eval_etudiant[,DO:=ifelse(DOS1,1,ifelse(DOG1,0,0))]

#I want to replace the fucking IDgroupe with the fucking class number... can't we take parameters form
#two tables when merging things. I know this will get solved in 5 minutes.

names(eval_etudiant)[3]<-paste("IDGroupe")
names(cours)[1]<-paste("IDGroupe")
setkey(cours,IDGroupe)
setkey(eval_etudiant,IDGroupe)
eval_etudiant<-merge(eval_etudiant,cours,by="IDGroupe")

eval_etudiant[,cours:=]





## ---- Test 


#There are 10k people who were re-admitted! (readmission ==1)

#Need to figure out if for these students during their path they switched from full time to part time to drop.
#Make Sam's states variable again, but with a semester-by-semester path.
