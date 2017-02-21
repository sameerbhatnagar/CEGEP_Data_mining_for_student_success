# This file will use the list of DO found in the DOfinder files and use it to figure things out about them.


#Can you finish cegep without graduating: is there any state in which this is possible? Just took 1 class to get into
#some program at Uni? 
#Let's make a DT with all the info about the DOs
#Get the population indicator from admission
#Past schooling from etudes_precedentes
#Gender from etudiant, langue maternelle, postal code?
#Note, COteR from inscription table

## ---- load-data ----
rm(list=ls())
path.to.data.directory<-"C:/Work/JAC/PAREA/CEGEP_Data_mining_for_student_success/DawsonCollege/ScienceProgram/2016/FindingDO/"
load(paste0(path.to.data.directory,'student_success.RData'))

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)

source('DOfinder.R')
DO<-DOfinder(20103)

#Start fresh by looking at the SO status for many of the DOs. The hunch is that MANY people DO without ever registering
#for a single class meaning that they would show up in admission, but not in inscription. 
#this may be as large as HALF the DOs ever admitted by the college. 

## ---- DO-Admin ----
AdCounted<-DO[student_number %in% admission$student_number]
length(unique(AdCounted$student_number))

AdLeftout<- DO[student_number %NI% admission$student_number]
length(unique(AdLeftout$student_number))

## ---- IDESDO ----
IDESDO<-etudiant_session[student_number %in% DO$student_number][,.(student_number,IDEtudiantSession,SPE,
                                                                   TypeFrequentation)]
IDESDO[,':=' (COUNT=.N),by=student_number]
IDESDO[,.N,by=COUNT]
IDESDO1<-IDESDO[IDESDO$COUNT==1]

## ---- Test ----
InCounted<-IDESDO[IDEtudiantSession %in% inscription$IDEtudiantSession]
length(unique(InCounted$student_number))
#There are 20.6k here.

InLeftout<- IDESDO[IDEtudiantSession %NI% inscription$IDEtudiantSession]
length(unique(InLeftout$student_number))
#36k. This means that at one time or another ALL DOs had a IDES that wasn't in inscription. I think this means 
#that they were registered, but didn't take any classes. This may be the case for every single student...checking:
t<-etudiant_session[IDEtudiantSession %NI% inscription$IDEtudiantSession]
length(unique(etudiant_session$student_number))
length(unique(t$student_number))
#So it isn't the case, but it is for almost every single student but 17k. Some people have TPL,TPL,TPL,TPL and then out
# the tricky part happens when the SO gets put in there...

## ----DO stats ----
DOstat<- etudiant_session[student_number %in% AdLeftout$student_number][,.(student_number,TypeFrequentation,ansession,IDEtudiantSession)]
DOstat[,.N, by=TypeFrequentation]
DOstat[, ':=' (COUNT = .N), by=student_number]
DOstat1<-etudiant_session[student_number %in% IDESDO1$student_number][,.(student_number,TypeFrequentation,ansession,IDEtudiantSession)]
DOstat1SO<-DOstat1[TypeFrequentation=="SO"]

## ---- Test ----

#Has anyone with SO as frequentation ever graduated?
gradcheck<- etudiant_session[student_number %in% student_certification$student_number][,.(student_number,TypeFrequentation)]
#Yes, many people have SOs. When do they get put in there?
length(unique(gradcheck$student_number[which(gradcheck$TypeFrequentation=="SO")]))
#17817
length(unique(gradcheck$student_number))
#28349

#Let's look at the inscription table to see if there is anything different there.
## ---- Ins ----
InIDESDO <- inscription[IDEtudiantSession %in% DOstat1SO$IDEtudiantSession]
InIDESDO[,.N,by=Note]

## ---- Test ----

#There are 128k unique IDES that corresponds to ALL DOs. 
#When switching to inscription, it drops to 83k as seen by the length InCounted.


#Find the number of DO who have only 1 semester in the college with SO as their status. 
#There are 167k lines to grad check (because they all show up a fuck ton of times (many classes) with 28k unique stus
#There are 15k lines to DOstat and 15k uniques because they all only took 1 class (if that...)

#Leftout<- DO[student_number %NI% DO1$student_number]
#This gives 15k students who are in the DOfinder list, but NOT the admission's table.  
#Correct<-DO[student_number %NI% Leftout$student_number]
#This is the unique number of students who are in the DOlis and in the admissions table. 

doublets<-DO1[,.SD,by=student_number][duplicated(DO1,by="student_number")]

#There are 10k people who were re-admitted! (readmission ==1)

#Need to figure out if for these students during their path they switched from full time to part time to drop.
#Make Sam's states variable again, but with a semester-by-semester path.
