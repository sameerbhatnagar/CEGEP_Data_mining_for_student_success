## ---- load-data ----
rm(list = ls())
library(magrittr)
library(data.table)
load('student_success.RData')
source('science-pipeline-functions.R')

## ---- term1 ----
s <- 20133

## ---- term2 ----
s <- 20141

## ---- term3 ----
s <- 20143

## ---- term4 ----
s <- 20151

## ---- term5 ----
s <- 20153

## ---- term6 ----
s <- 20161

## ---- math ----
c <- '201NYA05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '201NYB05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '201NYC05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '201BZS05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '201BZF05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()


## ---- chem ----
c <- '202NYA05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '202NYB05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '202BZF05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '202BZG05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

## ---- physics ----
c <- '203NYA05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '203NYB05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '203NYC05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '203BZA05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '203BZE05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

# c <- '360420DW'
# dt <- seats(s,c) 
# xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

## ---- bio ----
c <- '101NYA05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '101BZE05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '101BZH05'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

c <- '101BZPDW'
dt <- seats(s,c) 
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

## ---- geology ----
c <- '205BZG05'
dt <- seats(s,c)
xt(dt,s,c)
# table(dt$program.c,dt$ansessionDebut)
# table(dt$program.c,dt$ansessionDebut) %>% sum()
# table(dt$program,dt$ansessionDebut) %>% sum()

## ---- NOT-RUN ----
g<-cours[course=='203NYA05',IDGroupe]
s<-etudiant_session[ansession==20141,IDEtudiantSession]
s203NYA <- inscription[IDEtudiantSession %in% s & IDGroupe %in% g,IDEtudiantSession]

## ---- find-off-grid-students-for-each-profile ----
# define off-grid students as those who have taken a college level science course 
# before cohort starts

s <- 20133
p <- c('200P1','200P2')
pure.admits <- admission[program %in% p & ansessionDebut==s,student_number]
sessions <- etudiant_session[student_number %in% pure.admits,
                             .(IDEtudiantSession,student_number,ansession,program)]
sessions.earlier <- sessions[ansession<s,]
setkey(sessions.earlier,student_number)
sessions.earlier %>% duplicated() %>% which() %>% length()

c <- c('201NYA05','202NYA05','203NYA05')
cours.g <- cours[course %in% c,.(IDGroupe,course)]


# c <- c('101NYA05','101BZE05')
etudiant_session.s.p <- etudiant_session[ & program %in% p,
                                             .(IDEtudiantSession,ansession,program,student_number)]
setkey(etudiant_session.s.p,IDEtudiantSession)

inscription.g<- inscription[IDGroupe %in% cours.g$IDGroupe & 
                              IDEtudiantSession %in% etudiant_session.s.p$IDEtudiantSession,
                            .(IDEtudiantSession,IDGroupe,Note)]
setkey(inscription.g,IDEtudiantSession)
inscription.g <- inscription.g[etudiant_session.s.p,nomatch=0]
setkey(cours.g,IDGroupe)
setkey(inscription.g,IDGroupe)
inscription.g <- inscription.g[cours.g,nomatch=0]

# etudiant.session.cohort<- etudiant_session[ansession==s & program %in% p,]
# etudiant.session.cohort<-etudiant.session.cohort[IDEtudiantSession %in%
#                                                    inscription$IDEtudiantSession,]
# 
# etudiant.session.cohort <- etudiant.session.cohort[student_number %in% on.grid.year1,]

