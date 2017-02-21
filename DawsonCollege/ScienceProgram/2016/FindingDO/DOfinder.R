#This file will find the people who are most likely to be Drop-Outs. NOT TRANSFERS!
#The goal is to eventually compare this with a list of students who were intervened on to see if it worked.

<<<<<<< HEAD
#A drop out is defined as someone who did not graduate. We check the certification table to check that. 
=======
#THe scope will be only science to somplify the code for the first time around.
>>>>>>> upstream/master

load('student_success.RData')

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)

source('predict-science-enrollment-functions.R')
'%NI%' <- function(x,y)!('%in%'(x,y))


DOfinder<-function(a,b){

earliest.ansession<-a
earliest.admission<-b
  
## ---- count-students-per-semester ----
admission.all<-admission[cohorte >= earliest.admission]

etudiant_session.all<-etudiant_session[ansession>=earliest.ansession & student_number %in% admission.all$student_number]

inscription.all<-inscription[IDEtudiantSession %in% etudiant_session.all$IDEtudiantSession]

student_term_program<-etudiant_session.all[,.(student_number,ansession,program)][ansession %% 10 !=2]


setkey(student_term_program,student_number,ansession)
student_term_program<-unique(student_term_program)
# collapse First Choice and Regular
student_term_program$program <- factor(student_term_program$program)

all_enrollment_by_semester <- student_term_program[,.N,by=ansession][order(ansession)]
all_enrollment_by_semester_program <- student_term_program[,.N,by=.(ansession,program)][order(ansession,program)]
all_enrollment_by_semester_program.cast<-dcast.data.table(all_enrollment_by_semester_program,program ~ ansession, value.var = 'N')
#write.csv(all_enrollment_by_semester,'predict-all-enrollement-tables/all_enrollment_by_semester.csv', row.names = F)
#write.csv(all_enrollment_by_semester_program,'predict-all-enrollement-tables/all_enrollment_by_semester_program.csv',row.names = F)
#write.csv(all_enrollment_by_semester_program.cast,'predict-all-enrollement-tables/all_enrollment_by_semester_program_matrix.csv',row.names = F)
<<<<<<< HEAD
all_enrollment_by_semester[order(ansession)] %>% knitr::kable(row.names = F,                                                                  caption = 'Total all Enrollment per semester')
=======
all_enrollment_by_semester[order(ansession)] %>% knitr::kable(row.names = F,
                                                                  caption = 'Total all Enrollment per semester')
>>>>>>> upstream/master

## ---- graduation-dates ----
student_certification_all<-
  student_certification[student_number %in% student_term_program$student_number
                        ][,.(student_number,ansession,program)]
setnames(student_certification_all,'ansession','ansession')
student_certification_all[ansession %% 10 ==2, ansession:=ansession-1L]
all_graduation_by_semester<-student_certification_all[,.N,by=ansession][order(ansession)]
#write.csv(all_graduation_by_semester,'predict-all-enrollement-tables/all_graduation_by_semester.csv', row.names = F)
all_graduation_by_semester %>% knitr::kable(row.names = F)


## ---- states ----
setkey(student_term_program,student_number,ansession)
student_term_program_graduated<-rbind(student_term_program,student_certification_all)

setkey(student_term_program_graduated,student_number,ansession)
student_term_program_graduated[,term:=seq(.N),by=student_number]

# parse out if ansession is fall or winter semester
student_term_program_graduated[,semester:=ifelse(ansession %% 10 ==3,'Fall',ifelse(ansession %% 10 == 1,'Winter','Summer'))]
setkey(student_term_program_graduated,student_number,term)

# create state variable for each student in their program, in the given term and semester

grad_codes<-unique(student_certification_all$program)

student_term_program_graduated[program %NI% grad_codes,state:=paste0(semester,'-',program,'-',term)]
#the above line will look at the program in the variable and if it isn't 200b0, then it will create the string
#IF it is 200B0, then NA gets put in the col. That is why the following line checks for NAs instead of prog=200b0.
student_term_program_graduated[program %in% grad_codes,state:='Graduated']

<<<<<<< HEAD
lost_students_last_state<-student_term_program_graduated[,.SD[.N],by=student_number][state!='Graduated'][ansession!=20163]
<<<<<<< HEAD
#Make a separate data table to each student number. 
lost_students_last_state[,state:='Out']

#CHECK THE PEOPLE WHO HAVE A HIATUS

=======
lost_students_last_state[,state:='Out']

>>>>>>> upstream/master
=======
lost_students_last_state<-student_term_program_graduated[,.SD[.N],by=student_number][state!='Graduated'][ansession!=20171][ansession!=20163]
#Make a separate data table to each student number. 
lost_students_last_state[,state:='Out']

>>>>>>> findingDO
#The list of lost_students has now removed any student who has not transferred.
#The last step to catch people who are not DO, would be to check certification tables of other colleges.

DORate<-lost_students_last_state[,.N]/(lost_students_last_state[,.N] + nrow(student_term_program_graduated[by=student_number][state=='Graduated']))
DORate
#0.322
#Changed nothing.
return(lost_students_last_state)
}
