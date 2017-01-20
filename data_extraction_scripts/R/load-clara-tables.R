rm(list=ls())
library(data.table)
library(magrittr)

path.to.data.directory <- '~/Documents/science_program/onGoingEval/CLARA_database_mirror/DawsonCollege/'

## ---- etudiant ----
clara.table<-'Etudiant'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

etudiant <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                 col.names = fmt$V7)

## ---- admission ----
clara.table<-'Admission'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

admission <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                 col.names = fmt$V7)

## ---- etudiant-session ----
clara.table<-'EtudiantSession'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

etudiant_session <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                              col.names = fmt$V7,drop=nrow(fmt))

## ---- cours ----
clara.table<-'Cours'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

cours <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                         col.names = fmt$V7)

## ---- inscription ----
clara.table<-'Inscription'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

inscription <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
              col.names = fmt$V7,drop = nrow(fmt))

## ---- student-certification ----
clara.table<-'StudentCertification'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

student_certification <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                    col.names = fmt$V7)

## ---- certification-type ----
clara.table<-'CertificationType'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

certification_type <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                              col.names = fmt$V7)

rm(clara.table,fmt)

save.image(file = paste0(path.to.data.directory,'student_success.RData'))

