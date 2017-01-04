
### How do students doe in NYA after a remedial course, seperated by ESP/DSP?


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
dsp<- admission["08164",student_number]

etudiants.session.esp <- etudiant_session[student_number %in% esp,IDEtudiantSession]
inscription.esp <- inscription[IDEtudiantSession %in% etudiants.session.esp,]

etudiants.session.dsp <- etudiant_session[student_number %in% dsp,IDEtudiantSession]
inscription.dsp <- inscription[IDEtudiantSession %in% etudiants.session.dsp,]

## ---- nya ----
x <- "203NYA"
nya <- cours[grep(x,cours$course, fixed = T),IDGroupe]

inscription.esp.nya <- inscription.esp[IDGroupe %in% nya]
inscription.dsp.nya <- inscription[IDGroupe %in% nya]

inscription.dsp.nya[,admit_program:='dsp']
inscription.esp.nya[,admit_program:='esp']

inscription.nya <- rbind(inscription.dsp.nya,inscription.esp.nya)
inscription.nya[,admit_program:=as.factor(admit_program)]
table(inscription.nya$admit_program) %>% data.table() %>% 
  setnames(c("V1","N"),c("Prog","Seats")) %>% xtable() %>% print(include.rownames=F)
