###### how do students in ESP do in NYB?

## ---- setup ----
library(data.table)
library(ggplot2)
library(magrittr)
library(xtable)
library(cowplot)
rm(list=ls())
load("student_success.RData")
'%NI%' <- function(x,y)!('%in%'(x,y))

setkey(admission,program)
esp <- admission["08162",student_number]
science <- admission[grep("200",admission$program),student_number]

etudiants.session.esp <- etudiant_session[student_number %in% esp,IDEtudiantSession]
inscription.esp <- inscription[IDEtudiantSession %in% etudiants.session.esp,]

etudiants.session.science <- etudiant_session[student_number %in% science,IDEtudiantSession]
inscription.science <- inscription[IDEtudiantSession %in% etudiants.session.science,]

## ---- nyb ----
x <- "NYB"
nyb <- cours[grep(x,cours$course, fixed = T),IDGroupe]

inscription.esp.nyb <- inscription.esp[IDGroupe %in% nyb]
inscription.science.nyb <- inscription[IDGroupe %in% nyb]

inscription.science.nyb[,admit_program:='regular']
inscription.esp.nyb[,admit_program:='esp']

inscription.nyb <- rbind(inscription.science.nyb,inscription.esp.nyb)
inscription.nyb[,admit_program:=as.factor(admit_program)]
table(inscription.nyb$admit_program) %>% data.table() %>% 
  setnames(c("V1","N"),c("Prog","Seats")) %>% xtable() %>% print(include.rownames=F)

## ---- nyb-plot ----
setkey(inscription.nyb,admit_program)
ggplot(inscription.nyb, aes(admit_program,Note)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$Note, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$Note, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = 60) +
  ggtitle(paste("Distribution of grades in for all",x,"courses")) +
  ylab(paste(x,"Grade"))

ggplot(inscription.nyb[CoteR<50,], aes(admit_program,CoteR)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$CoteR, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), linetype="dashed" ) + 
  ggtitle(paste("Distribution of R-Scores in for all",x,"courses")) + ylab(paste(x,"CoteR"))


## ---- math-nyb ----
x <- "201NYB"
nyb <- cours[grep("201NYB",cours$course),IDGroupe]

inscription.esp.nyb <- inscription.esp[IDGroupe %in% nyb]
inscription.science.nyb <- inscription[IDGroupe %in% nyb]

inscription.science.nyb[,admit_program:='regular']
inscription.esp.nyb[,admit_program:='esp']

inscription.nyb <- rbind(inscription.science.nyb,inscription.esp.nyb)
inscription.nyb[,admit_program:=as.factor(admit_program)]
table(inscription.nyb$admit_program) %>% data.table() %>% 
  setnames(c("V1","N"),c("Prog","Seats")) %>%  
  xtable() %>% print(include.rownames=F)

## ---- math-nyb-plot ----
setkey(inscription.nyb,admit_program)
ggplot(inscription.nyb, aes(admit_program,Note)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$Note, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$Note, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = 60) +
  ggtitle(paste("Distribution of grades in for all",x,"courses")) +
  ylab(paste(x,"Grade"))

ggplot(inscription.nyb[CoteR<50,], aes(admit_program,CoteR)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$CoteR, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), linetype="dashed" ) + 
  ggtitle(paste("Distribution of R-Scores in for all",x,"courses")) + ylab(paste(x,"CoteR"))

## ---- chem-nyb ----
x <- "202NYB"
nyb <- cours[grep(x,cours$course),IDGroupe]

inscription.esp.nyb <- inscription.esp[IDGroupe %in% nyb]
inscription.science.nyb <- inscription[IDGroupe %in% nyb]

inscription.science.nyb[,admit_program:='regular']
inscription.esp.nyb[,admit_program:='esp']

inscription.nyb <- rbind(inscription.science.nyb,inscription.esp.nyb)
inscription.nyb[,admit_program:=as.factor(admit_program)]
table(inscription.nyb$admit_program) %>% data.table() %>% 
  setnames(c("V1","N"),c("Prog","Seats")) %>% xtable() %>% print(include.rownames=F)

## ---- chem-nyb-plot ----
setkey(inscription.nyb,admit_program)
ggplot(inscription.nyb, aes(admit_program,Note)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$Note, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$Note, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = 60) +
  ggtitle(paste("Distribution of grades in for all",x,"courses")) +
  ylab(paste(x,"Grade"))

ggplot(inscription.nyb[CoteR<50,], aes(admit_program,CoteR)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$CoteR, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), linetype="dashed" ) + 
  ggtitle(paste("Distribution of R-Scores in for all",x,"courses")) + ylab(paste(x,"CoteR"))

## ---- phys-nyb ----
x <- "203NYB"
nyb <- cours[grep(x,cours$course),IDGroupe]

inscription.esp.nyb <- inscription.esp[IDGroupe %in% nyb]
inscription.science.nyb <- inscription[IDGroupe %in% nyb]

inscription.science.nyb[,admit_program:='regular']
inscription.esp.nyb[,admit_program:='esp']

inscription.nyb <- rbind(inscription.science.nyb,inscription.esp.nyb)
inscription.nyb[,admit_program:=as.factor(admit_program)]
table(inscription.nyb$admit_program) %>% data.table() %>% 
  setnames(c("V1","N"),c("Prog","Seats")) %>% 
  xtable() %>% print(include.rownames = F)

## ---- phys-nyb-plot ----
setkey(inscription.nyb,admit_program)
ggplot(inscription.nyb, aes(admit_program,Note)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$Note, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$Note, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = 60) +
  ggtitle(paste("Distribution of grades in for all",x,"courses")) +
  ylab(paste(x,"Grade"))

ggplot(inscription.nyb[CoteR<50,], aes(admit_program,CoteR)) + 
  geom_boxplot(notch = T) +
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), 
             linetype="dotted", colour = 'red' ) +
  geom_hline(yintercept = median(inscription.nyb["regular",]$CoteR, na.rm = T), 
             linetype="dashed", colour = 'blue' ) + 
  geom_hline(yintercept = median(inscription.nyb["esp",]$CoteR, na.rm = T), linetype="dashed" ) + 
  ggtitle(paste("Distribution of R-Scores in for all",x,"courses")) + ylab(paste(x,"CoteR"))

## ---- graduation ----

#  Find all currently active students
current.students <- etudiant_session[ansession==20163]
current.registered.students <- 
  current.students[IDEtudiantSession %in% inscription$IDEtudiantSession]

# students admitted into ESP, and have graduated from ANY program
setkey(admission,program)
esp <- admission["08162",.(student_number,ansessionDebut)]
setkey(esp,student_number)
esp.grads <- student_certification[student_number %in% esp$student_number,]
setkey(esp.grads,student_number)
esp.grads<- esp.grads[esp,nomatch=0]

# calculate graduation time in years
esp.grads[,grad.time:=round(ansession/10)-round(ansessionDebut/10)]
esp.grads[,n:=.N,by=list(ansession,program)]

# remove current esp students to calculate "lost" students over the years
current.registered.students.esp <- current.registered.students[student_number %in% esp$student_number]
dropped.esp = nrow(esp) - nrow(esp.grads) - nrow(current.registered.students.esp)
dropped.esp.prop = (100*dropped.esp/nrow(esp)) %>% round()

# repeat for science: students admitted into science, but graduate from ANY program
science <- admission[grep("200",admission$program),.(student_number,ansessionDebut)]

# remove admission records for all program student transferred into after getting into science
setkey(science,student_number,ansessionDebut)
science<- unique(science,by="student_number")
setkey(science,student_number)

science.grads <- student_certification[student_number %in% science$student_number]
# remove certification records for second time some students graduated
setkey(science.grads,student_number,ansession)
science.grads<- unique(science.grads,by="student_number")

setkey(science.grads,student_number)
science.grads<- science.grads[science,nomatch=0]
science.grads[,grad.time:=round(ansession/10)-round(ansessionDebut/10)]

current.registered.students.science <- 
  current.registered.students[student_number %in% science$student_number]
dropped.science = nrow(science) - nrow(science.grads) - nrow(current.registered.students.science)
dropped.science.prop = (100*dropped.science/nrow(science)) %>% round()

data.table(Program=c("Students Initially Admitted","Students Graduated from Any program",
             "Current Active Students","Students who drop","Fraction of Students who drop (%)"),
           ESP=c(nrow(esp),nrow(esp.grads),nrow(current.registered.students.esp),
                 dropped.esp,dropped.esp.prop),
           Science=c(nrow(science),nrow(science.grads),nrow(current.registered.students.science),
                     dropped.science,dropped.science.prop)) %>% 
  xtable(auto=T) %>% print(include.rownames=F)



## ----graduation-plot ----
e<- esp.grads[n>=2][ansession>=20101]
e %>% ggplot(aes(program)) +
  geom_bar() + facet_wrap(~ansession) + theme(axis.text.x = element_text(angle=90))

# esp.prog.codes <- c("08162","09162")
# esp.after <- admission[student_number %in% esp,][program %NI% esp.prog.codes,]
# table(esp.after$program) %>% sum()


## ---- NOT-RUN ----

# esp.id.etud.sess <- etudiant_session$IDEtudiantSession[which(esp$student_number %in% etudiant_session$student_number)]
# 
# 
# ## test for one student
# s <- esp[1]
# which(s$student_number==etudiant_session$student_number)
# etudiant_session$program[which(s$student_number==etudiant_session$student_number)]
# 
# 
# # hist(nyb$CoteR)
# 
# # what is the OVERALL average for the different NYB's?
# nyb[,mean(Note,na.rm = T), by=course]
# nyb[,mean(CoteR,na.rm = T), by=course]
# nyb[,sd(Note,na.rm = T), by=course]
# nyb[,sd(CoteR,na.rm = T), by=course]
# 
# 
# # only 32 times in the last 10 years has an esp student taken NYB???
# esp$IDEtudiantSession %in% nyb$IDEtudiantSession %>% 
#   which() %>% 
#   length()



