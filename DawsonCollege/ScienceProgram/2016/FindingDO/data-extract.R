rm(list=ls())
library(data.table)
library(magrittr)
library(stringr)

# path.to.data.directory <- '~/Documents/science_program/onGoingEval/CLARA_database_mirror/DawsonCollege/'
path.to.data.directory <- 'Student Success/Dawson/'

## ---- etudiant ----
clara.table<-'Etudiant'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
bad.rows <- which(stringr::str_count(lines,',') == 8)
for (i in 1:length(bad.rows)){
  chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][5]
  halfstr<-substr(lines[bad.rows[i]],0,chartorem-1)
  halfstr2<-substr(lines[bad.rows[i]],chartorem+1,stringr::str_length(lines[bad.rows[i]]))
  lines[bad.rows[i]]<-stringr::str_c(halfstr,halfstr2,sep="")
}
li<-paste(lines,collapse='\n')
write(li,"Student Success/Dawson/EtudiantClean.txt",ncolumns=8)

etudiant <-fread(paste0(path.to.data.directory,'EtudiantClean','.txt'),sep = ',',col.names = fmt$V7)

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
setnames(student_certification,'AnSession','ansession')

## ---- certification-type ----
clara.table<-'CertificationType'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

certification_type <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                           col.names = fmt$V7)

## ---- evaluation-etudiant ----
clara.table<-'EvaluationEtudiant'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

evaluation_etudiant <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                            col.names = fmt$V7)
setnames(evaluation_etudiant,'AnSession','ansession')
setnames(evaluation_etudiant,'IdGroupe','IDGroupe')

## ---- Cours Secondaire ----
clara.table<-'CoursSecondaire'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
bad.rows <- which(stringr::str_count(lines,',') >= 10)
for (i in 1:length(bad.rows)){
  count<-stringr::str_count(lines[bad.rows[i]],',')
  if(count==10){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][4]
    halfstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    halfstr2<-substr(lines[bad.rows[i]],chartorem+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(halfstr,halfstr2,sep="")
  }
  else if(count==11){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][4]
    chartorem2<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][5]
    thirdstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    thirdstr2<-substr(lines[bad.rows[i]],chartorem+1,chartorem2-2)
    thirdstr3<-substr(lines[bad.rows[i]],chartorem2+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(thirdstr,thirdstr2,thirdstr3,sep="")
  }
  else if(count ==12){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][4]
    chartorem2<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][5]
    chartorem3<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][6]
    quartstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    quartstr2<-substr(lines[bad.rows[i]],chartorem+1,chartorem2-2)
    quartstr3<-substr(lines[bad.rows[i]],chartorem2+1,chartorem3-2)
    quartstr4<-substr(lines[bad.rows[i]],chartorem3+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(quartstr,quartstr2,quartstr3,quartstr4,sep="")
  }
}
li<-paste(lines,collapse='\n')
write(li,"Student Success/Dawson/CoursSecondaireClean2.txt",ncolumns=10)

cours_secondaire <-fread(paste0(path.to.data.directory,'CoursSecondaireClean2','.txt'),sep = ',',col.names = fmt$V7)

## ---- etudiant-cours-secondaire ----
clara.table<-'EtudiantCoursSecondaire'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
bad.rows <- which(stringr::str_count(lines,',') >= 18)
for (i in 1:length(bad.rows)){
  count<-stringr::str_count(lines[bad.rows[i]],',')
  if(count==18){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][17]
    halfstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    halfstr2<-substr(lines[bad.rows[i]],chartorem+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(halfstr,halfstr2,sep="")
  }
  else if(count==19){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][17]
    chartorem2<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][18]
    thirdstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    thirdstr2<-substr(lines[bad.rows[i]],chartorem+1,chartorem2-2)
    thirdstr3<-substr(lines[bad.rows[i]],chartorem2+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(thirdstr,thirdstr2,thirdstr3,sep="")
  }
}
li<-paste(lines,collapse='\n')

write(li,paste0(path.to.data.directory,'EtudiantCoursSecondaireClean1','.txt'),ncolumns=18)
etudiant_cours_secondaire <-fread(paste0(path.to.data.directory,'EtudiantCoursSecondaireClean1','.txt'),sep = ',',
                                  col.names = fmt$V7)

rm(clara.table,fmt,thirdstr,thirdstr2,thirdstr3,halfstr,halfstr2,quartstr,quartstr2,quartstr3,quartstr4,chartorem,
   chartorem2,chartorem3,count,bad.rows,li,lines,i)

save.image(file = paste0(path.to.data.directory,'student_success.RData'))