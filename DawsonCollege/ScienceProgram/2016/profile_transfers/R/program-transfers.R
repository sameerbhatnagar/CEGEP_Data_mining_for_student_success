## ---- load-data ----
rm(list=ls())
load('student_success.RData')
library(data.table)
library(magrittr)
library(reshape2)

## ---- science-admits ----
##### Looking at program transfers through etudiant_session table
science.prog.codes <- c('200P1','200P2','200H1','200H2','200E2')
a<-admission
setkey(a,student_number)
a<-a[order(ansessionDebut)]
admission.unique<-a[!duplicated(a$student_number)]
science.admits<-admission.unique[population=='A' & program %in% science.prog.codes,student_number]

## ---- profile-transfers ----
etudiant_session.science <- etudiant_session[student_number %in% science.admits]
etudiant_session.science <- etudiant_session.science[IDEtudiantSession %in% inscription$IDEtudiantSession]

etudiant_session.science <- etudiant_session.science[,.(student_number,program,ansession)]
setkey(etudiant_session.science,student_number,ansession)
etudiant_session.science[,semester:=seq(.N),by=student_number]

'%NI%' <- function(x,y)!('%in%'(x,y))
wierd<-etudiant_session.science[semester==1][program %NI% science.prog.codes,student_number]
etudiant_session.science<-etudiant_session.science[student_number %NI% wierd]
setkey(etudiant_session.science,student_number,ansession)
etudiant_session.science[,semester:=seq(.N),by=student_number]

setkey(etudiant_session.science,student_number,program)
science.profile.transfers<-unique(etudiant_session.science)


setkey(science.profile.transfers,student_number, ansession)
# science.profile.transfers[,.N,by=student_number]$N %>% table()
science.profile.transfers[,profile.attempt:=seq(.N),by=student_number]


## ---- program-transfers ----
##### try collapsing profiles into sectors/programs
science.profile.transfers$program.c <- factor(science.profile.transfers$program)
levels(science.profile.transfers$program.c) <- 
  list(ESP =c('08162','09162'),DSP=c('08164','09164'),
       Enviro='200E2',
       Health=c('200H1','200H2'),
       Pure=c('200P1','200P2'),
       EngTech=c('213GS','221B0','241A0','24311','241A3','243B0','243B3'),
       Social=c('300AA','300AB','300AE','300AH','300AK','300AM','300AP',
                '300EA','300','300EB','300EH','300EG','300EK','300EM','300EP','300ET',
                '300FA','300FB','300FE','300FH','300FK','300FP','313FA'),
       SocialCareer=c('410A0','410B0','410D0','420A0'),
       SMSECareer=c('140B0','140B1','142C0','144A0','180A0','210A0','210AB'),
       ArtsCareer=c('500D3','500G1','500G1','500F3','500F6','500G6',
                    '510A0','574B0','57082','700B1','700B2'),
       other=c('08003','08072','08103','08106','08125','08165','01965'))
setkey(science.profile.transfers,student_number,ansession)
science.program.transfers <- science.profile.transfers[,.(student_number,ansession,program.c)]

setkey(science.program.transfers,student_number,program.c)
science.program.transfers<-unique(science.program.transfers)

setkey(science.program.transfers,student_number, ansession)
# science.program.transfers[,.N,by=student_number]$N %>% table()

science.program.transfers[,prog.attempt:=seq(.N),by=student_number]

## ---- program-transfer-confusion-matrices ----
program.transfer <- 
  dcast(science.program.transfers[,.(student_number,prog.attempt,program.c)],
        student_number ~ prog.attempt, value.var = 'program.c')

table(program.transfer$`1`,program.transfer$`2`,useNA = 'always',deparse.level = 2)[c(2,3,1),
                                                                                    c(10,4,6,3,8,7,2,1,5)] %>% 
  xtable::xtable(caption = 'Program Transfers over the last 10 years: the rows represent the program of admission, and the columns represent the program/sector to which students transferred. The first column represents the students who never transferred (and stayed in the program to which they were initialy admitted') %>% 
  xtable::print.xtable(floating.environment = 'figure',table.placement = 'H')
table(program.transfer$`1`,program.transfer$`2`,useNA = 'always',deparse.level = 2)[c(2,3,1),
                                                                                    c(10,4,6,3,8,7,2,1,5)] %>% 
  prop.table(margin=1) %>% round(2) %>% 
  xtable::xtable(caption = 'Program Transfers over the last 10 years: the rows represent the program of admission, and the columns represent the program/sector to which students transferred. The first column represents the students who never transferred (and stayed in the program to which they were initialy admitted. Same as table above, but normalized to the total number of students initially admitted to each of the profiles (sum of each row =1)') %>% 
  xtable::print.xtable(floating.environment = 'figure',table.placement = 'H')




##### Looking at program transfers through admission table
# 
# # how many programs do students go through?
# admission[,.N,by=(student_number)]$N %>% table()
# 
# # program transfers (population B) into regular P&A Fall 2013
# admission[program=='200P2' & ansessionDebut==20133,.N,by=population]
# 
# a<-admission
# a[,num_programs:=.N,by=student_number]
# setkey(a,ansessionDebut)
# 
# 
# s<-20133
# science <- a[grep('200',a$program)][ansessionDebut==s | ansessionDebut==(s+8)
#                                                     ][num_programs<3
#                                                       ][,.(student_number,ansessionDebut,
#                                                          program)]
# 
# profile <- dcast(science,student_number ~ ansessionDebut, value.var = 'program')
# term1<-quote(colnames(profile)[2])
# term2 <-quote(colnames(profile)[3])
# profile[is.na(profile$eval(term2)),]
# 
# table(profile$`20133`,profile$`20141`,useNA = 'always',deparse.level = 2)
# table(profile$`20133`,profile$`20141`,useNA = 'always',deparse.level = 2) %>% 
#   prop.table(margin=1) %>% round(2)

