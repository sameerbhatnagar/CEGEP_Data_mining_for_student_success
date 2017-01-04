## ---- load-data ----
rm(list=ls())

path.to.data.directory<-"~/Documents/science_program/onGoingEval/CLARA_database_mirror/DawsonCollege/"

load(paste0(path.to.data.directory,'student_success.RData'))

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)
source('predict-science-enrollment-functions.R')

science.prog.codes <- c('200P1','200P2','200H1','200H2','200E2','08162','08164','09162','09164')


## ---- count-students-per-semester ----
etudiant_session.science<-etudiant_session[program %in% science.prog.codes][ansession>=20103]

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


science_enrollment_by_semester <- student_term_program[,.N,by=ansession]
science_enrollment_by_semester_program <- student_term_program[,.N,by=.(ansession,program)][order(ansession,program)]
science_enrollment_by_semester_program.cast<-dcast.data.table(science_enrollment_by_semester_program,program ~ ansession, value.var = 'N')
write.csv(science_enrollment_by_semester,'predict-science-enrollement-tables/science_enrollment_by_semester.csv', row.names = F)
write.csv(science_enrollment_by_semester_program,'predict-science-enrollement-tables/science_enrollment_by_semester_program.csv',row.names = F)
write.csv(science_enrollment_by_semester_program.cast,'predict-science-enrollement-tables/science_enrollment_by_semester_program_matrix.csv',row.names = F)
science_enrollment_by_semester[order(ansession)] %>% knitr::kable(row.names = F,
                                                caption = 'Total Science Enrollment per semester')

## ---- graduation-dates ----
student_certification_science<-
  student_certification[student_number %in% student_term_program$student_number
                        ][program %in% '200B0'][,.(student_number,AnSession,program)]
setnames(student_certification_science,'AnSession','ansession')
student_certification_science[ansession %% 10 ==2, ansession:=ansession-1L]
science_graduation_by_semester<-student_certification_science[,.N,by=ansession][order(ansession)]
write.csv(science_graduation_by_semester,'predict-science-enrollement-tables/science_graduation_by_semester.csv', row.names = F)
science_graduation_by_semester %>% knitr::kable(row.names = F)

## ---- states ----
setkey(student_term_program,student_number,ansession)
student_term_program_graduated<-rbind(student_term_program,student_certification_science)

setkey(student_term_program_graduated,student_number,ansession)
student_term_program_graduated[,term:=seq(.N),by=student_number]

# parse out if ansession is fall or winter semester
student_term_program_graduated[,semester:=ifelse(ansession %% 10 ==3,'Fall',ifelse(ansession %% 10 == 1,'Winter','Summer'))]
setkey(student_term_program_graduated,student_number,term)

# create state variable for each student in their program, in the given term and semester
student_term_program_graduated[program!='200B0',state:=paste0(semester,'-',program,'-',term)]
student_term_program_graduated[is.na(state),state:='Graduated']

lost_students_last_state<-student_term_program_graduated[,.SD[.N],by=student_number][state!='Graduated'][ansession!=20163]
lost_students_last_state[,state:='Out']

student_states<-rbind(student_term_program_graduated,lost_students_last_state)
setkey(student_states,student_number,term)
student_states[,count:=seq(.N),by=student_number]
setkey(student_states,student_number,count)

write.csv(student_states,'predict-science-enrollement-tables/student_states.csv',quote = F,row.names = F)


## ---- demo-student-states ----
# typical student states leading to graduation
s1<-student_states[state=='Graduated',student_number] %>% sample(1)
student_states[student_number==s1] %>% knitr::kable()

# examples of students' states who eventually transfer out of science
s2<-student_states[state=='Out',student_number] %>% sample(2)
student_states[student_number == s2[1]] %>% knitr::kable()
student_states[student_number == s2[2]] %>% knitr::kable()

# Good example of student starting in ESP
student_states[student_number=='838D1FEDF96289E7659A95322BBCCC30532FBEB062B358D0CAAABDFE7DF798E4'] %>% knitr::kable()

## ---- state-transitions ----
state_list<-student_states[,state] %>% unique()

probs<- sapply(state_list, state_transition_probabilities) %>% unlist() %>% data.frame()
probs$transition<-row.names(probs)
probs<-probs %>% data.table()
setnames(probs,'.','transition.probability')
probs[,c('state1','state2'):=tstrsplit(transition,'.',fixed=T)]
probs[,transition:=NULL]
probs %>% head(10) %>% knitr::kable()
probs.matrix<-dcast.data.table(probs,state1 ~ state2, value.var = 'transition.probability',fill = 0)
write.csv(probs,file = 'predict-science-enrollement-tables/state_transitions_probabilities.txt',row.names=F,quote = F)
write.csv(probs.matrix,file = 'predict-science-enrollement-tables/state_transitions_probabilities_matrix.txt',row.names=F,quote = F)

## ---- demo-probs-grad-out ----
# which states are most likely to lead to transferring out?
probs[state2=='Out'][order(state1)] %>% knitr::kable()
# which states are most likely to lead to graduation?
probs[state2=='Graduated'][order(state1)] %>% knitr::kable()

## ---- predict-enrollments-w20141-w2015 ----
# initialize counts for winter0
NOW <- 20163 
start.ansession<-20141
start.semester <- ifelse(start.ansession %% 10==1,'Winter','Fall')
next.semester<-ifelse(start.semester=='Fall','Winter','Fall')

s1<-student_states[ansession==start.ansession][grep(start.semester,state),.N,by=state][order(state)]

# project to one semester forward
state.projections<-projections(s1,probs,start.ansession)

# keep track of who leaves the science program, and how
exits<-state.projections$state.transition.projection[state2=='Graduated' | state2=='Out',.(state1,state2,state2.population.projection)]
exits[,ansession:=start.ansession]
exits.history<-exits

# append new admits of new semester
current.ansession<-start.ansession+2
s2.new <- update_new_admits(state.projections$s2,student_states,start.ansession+2)

# append fall1 to enrollment history (actual and projected)
enrollment.history<-enrollment_update(current.ansession,s2.new,science_enrollment_by_semester_program)

# project one MORE semester forward
state.projections <- projections(s2.new,probs,current.ansession) 

# keep track of who leaves the science program, and how
exits<-state.projections$state.transition.projection[state2=='Graduated' | state2=='Out',.(state1,state2,state2.population.projection)]
exits[,ansession:=current.ansession]
exits.history <- rbind(exits.history,exits)
  
# append newest admits of new semester
current.ansession <- current.ansession+8
s3.new<-update_new_admits(state.projections$s2,student_states,current.ansession)

# append winter1 to enrollment history (actual and projected)
enrollment.history<-enrollment_update(current.ansession,s3.new,enrollment.history)

## ---- predict-enrollments-w2015-w2021 ----
# repeat above, moving projections forward one year
w15 = list(s3.new=s3.new,enrollment.history=enrollment.history,exits.history=exits.history)
w16<-project_enrollments_one_year_forward(w15,current.ansession)

# repeat above, moving projections forward one year
current.ansession<-current.ansession+10
w17<-project_enrollments_one_year_forward(w16,current.ansession)

# repeat above, moving projections forward one year
# assuming all profiles admit the same number as students as they have in the past, but 
# "Pure" admits 300 students in the fall
current.ansession<-current.ansession+10
w18<-project_enrollments_one_year_forward(w17,current.ansession)

# repeat above, moving projections forward one year
current.ansession<-current.ansession+10
w19<-project_enrollments_one_year_forward(w18,current.ansession)

# repeat above, moving projections forward one year
current.ansession<-current.ansession+10
w20<-project_enrollments_one_year_forward(w19,current.ansession)

# repeat above, moving projections forward one year
current.ansession<-current.ansession+10
w21<-project_enrollments_one_year_forward(w20,current.ansession)

## ---- build-tables-for-plotting-enrollment-history ----
enrollment.history<-w21$enrollment.history
total.projected<-enrollment.history[grep('projected',program),sum(N), by=ansession]
setnames(total.projected,'V1','N')
total.projected[,program:='Total.projected']
enrollment.history<-rbind(enrollment.history,total.projected)

## ---- build-tables-for-plotting-exit-history ----
#projected exits
exits.history.projected<-w21$exits.history
exits.history.projected[,c('semester','program','term'):=tstrsplit(state1,'-')]
exits.history.projected <- exits.history.projected[,sum(state2.population.projection),by=.(ansession,program,state2)]

exits.history.projected <- exits.history.projected[program !='ESP'][program!='DSP']
exits.history.projected[,line.type:='projected']
setnames(exits.history.projected,'V1','N')

# actual exits
grads <- student_certification_science$student_number %>% unique()
grad.profiles<-student_states[student_number %in% grads][,.SD[.N-1L],by=student_number][,.(student_number,ansession,program)]
graduation.history<-grad.profiles[,.N,by=.(ansession,program)]
graduation.history[,line.type:='actual']
graduation.history[,state2:='Graduated']

'%NI%' <- function(x,y)!('%in%'(x,y))
outs.profiles<-student_states[student_number %NI% grads][,.SD[.N],by=student_number]
outs.history<-outs.profiles[ansession<NOW][,.N,by=.(ansession,program)]
outs.history[,line.type:='actual']
outs.history[,state2:='Out']
setkey(outs.history, ansession,program)
exits.history<-rbind(graduation.history,outs.history,exits.history.projected)



## ---- plot-enrollment-history ----
science_enrollment_by_semester[,program:='Total']
enrollment.history<-rbind(science_enrollment_by_semester,enrollment.history)
enrollment.history[,c('color','line.type'):=tstrsplit(program,'.',fixed=T)]
enrollment.history[is.na(line.type),line.type:='actual']
enrollment.history<-enrollment.history[ansession>=20141]
enrollment.history$ansession <- factor(enrollment.history$ansession)

p <- ggplot(enrollment.history, aes(x=ansession, y=N, group=program))
p <- p + geom_line(aes(linetype=line.type,color=color))
p + scale_y_continuous(breaks = seq(0,1500,100)) + 
  scale_color_manual(values = c("Health"='red',"Pure"='blue',"Total"='purple',
                                "ESP"='yellow',"DSP"='orange',"Enviro"='green')) +
  ggtitle('Science Program Enrollment Projections') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ---- print-enrollment-numbers ----
enrollment.history[order(ansession,program),.(ansession,program,round(N))] %>% knitr::kable()

## ---- plot-exits-history ----
exits.history[,program:=paste0(program,'.',state2)]
exits.history<-exits.history[grep('[Enviro|Health|Pure]',program)]
exits.history<-exits.history[ansession>=20141]
exits.history$ansession <- factor(exits.history$ansession)

p <- ggplot(exits.history, aes(x=ansession, y=N, group=program))
p <- p + geom_line(aes(linetype=line.type))
p
p + scale_y_continuous(breaks = seq(0,1500,100)) 
+ 
  scale_color_manual(values = c("Health"='red',"Pure"='blue',"Enviro"='green')) +
  ggtitle('Science Program Graduation and Transfer out Projections')
