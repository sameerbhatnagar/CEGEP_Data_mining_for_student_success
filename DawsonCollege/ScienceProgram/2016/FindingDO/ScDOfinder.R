#This file will find the people who are most likely to be Drop-Outs. NOT TRANSFERS!
#The goal is to eventually compare this with a list of students who were intervened on to see if it worked.

#THe scope will be only science to somplify the code for the first time around.

load('student_success.RData')
earliest.ansession<-20103

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)

source('predict-science-enrollment-functions.R')
'%NI%' <- function(x,y)!('%in%'(x,y))
science.prog.codes <- c('200P1','200P2','200H1','200H2','200E2','08162','08164','09162','09164')

## ---- count-students-per-semester ----
etudiant_session.science<-etudiant_session[program %in% science.prog.codes][ansession>=earliest.ansession]

inscription.science<-inscription[IDEtudiantSession %in% etudiant_session.science$IDEtudiantSession]

student_term_program<-etudiant_session.science[IDEtudiantSession %in% inscription.science$IDEtudiantSession,
                                               .(student_number,ansession,program)][ansession %% 10 !=2]


# About 16 students are registered for the same course in the same semester; we remove the duplicate records
setkey(student_term_program,student_number,ansession)
student_term_program<-unique(student_term_program)
# collapse First Choice and Regular
student_term_program$program <- factor(student_term_program$program)
levels(student_term_program$program) <- 
  list(ESP =c('08162','09162'),DSP=c('08164','09164'),
       Enviro='200E2',
       Health=c('200H1','200H2'),
       Pure=c('200P1','200P2'))


science_enrollment_by_semester <- student_term_program[,.N,by=ansession][order(ansession)]
science_enrollment_by_semester_program <- student_term_program[,.N,by=.(ansession,program)][order(ansession,program)]
science_enrollment_by_semester_program.cast<-dcast.data.table(science_enrollment_by_semester_program,program ~ ansession, value.var = 'N')
#write.csv(science_enrollment_by_semester,'predict-science-enrollement-tables/science_enrollment_by_semester.csv', row.names = F)
#write.csv(science_enrollment_by_semester_program,'predict-science-enrollement-tables/science_enrollment_by_semester_program.csv',row.names = F)
#write.csv(science_enrollment_by_semester_program.cast,'predict-science-enrollement-tables/science_enrollment_by_semester_program_matrix.csv',row.names = F)
science_enrollment_by_semester[order(ansession)] %>% knitr::kable(row.names = F,
                                                                  caption = 'Total Science Enrollment per semester')

## ---- graduation-dates ----
student_certification_all<-
  student_certification[student_number %in% student_term_program$student_number
                        ][,.(student_number,ansession,program)]
setnames(student_certification_all,'ansession','ansession')
student_certification_all[ansession %% 10 ==2, ansession:=ansession-1L]
science_graduation_by_semester<-student_certification_all[,.N,by=ansession][order(ansession)]
#write.csv(science_graduation_by_semester,'predict-science-enrollement-tables/science_graduation_by_semester.csv', row.names = F)
science_graduation_by_semester %>% knitr::kable(row.names = F)


## ---- states ----
setkey(student_term_program,student_number,ansession)
student_term_program_graduated<-rbind(student_term_program,student_certification_all)

setkey(student_term_program_graduated,student_number,ansession)
student_term_program_graduated[,term:=seq(.N),by=student_number]

# parse out if ansession is fall or winter semester
student_term_program_graduated[,semester:=ifelse(ansession %% 10 ==3,'Fall',ifelse(ansession %% 10 == 1,'Winter','Summer'))]
setkey(student_term_program_graduated,student_number,term)

# create state variable for each student in their program, in the given term and semester

science_progs<-c('Health','Pure','ESP','DSP','Enviro')

student_term_program_graduated[program %in% science_progs,state:=paste0(semester,'-',program,'-',term)]
#the above line will look at the program in the variable and if it isn't 200b0, then it will create the string
#IF it is 200B0, then NA gets put in the col. That is why the following line checks for NAs instead of prog=200b0.
student_term_program_graduated[program %NI% science_progs,state:='Graduated']

lost_students_last_state<-student_term_program_graduated[,.SD[.N],by=student_number][state!='Graduated'][ansession!=20163]
lost_students_last_state[,state:='Out']

#The list of lost_students has now removed any student who has not transferred.
#The last step to catch people who are not DO, would be to check certification tables of other colleges.

ScDORate<-lost_students_last_state[,.N]/(lost_students_last_state[,.N] + nrow(student_term_program_graduated[by=student_number][state=='Graduated']))
ScDORate
#.196

