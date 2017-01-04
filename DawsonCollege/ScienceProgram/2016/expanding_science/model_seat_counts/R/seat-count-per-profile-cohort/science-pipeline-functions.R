## ---- function-seats-per-course ----
seats <- function(semester, course.name){
  group.ids <- cours[course==course.name,IDGroupe]
  session.ids <- etudiant_session[ansession==semester,IDEtudiantSession]
  
  s <- inscription[IDEtudiantSession %in% session.ids & IDGroupe %in% group.ids]
  setkey(s,IDEtudiantSession)
  session.ids <- etudiant_session[IDEtudiantSession %in% s$IDEtudiantSession & ansession==semester,
                                  .(IDEtudiantSession,student_number,SPE,ansession,program)]
  setkey(session.ids,IDEtudiantSession)
  s <- s[session.ids,nomatch=0]
  setkey(s,IDEtudiantSession)
  session.ids <- etudiant_session[IDEtudiantSession %in% s$IDEtudiantSession & ansession==semester,
                                  .(IDEtudiantSession,student_number,SPE,ansession,program)]
  setkey(session.ids,IDEtudiantSession)
  s<- s[session.ids,nomatch=0]
  admission.cols<-admission[ansessionDebut<=semester,.(ansessionDebut,student_number,program)]
  setkey(admission.cols,student_number,program)
  setkey(s,student_number,program)
  s<-s[admission.cols,nomatch=0][ansessionDebut<=semester]
  s$program.c <- factor(s$program)
  levels(s$program.c) <- list(ESP =c('08162','09162'),DSP=c('08164','09164'),
                              Enviro='200E2',
                              Health=c('200H1','200H2'),
                              Pure=c('200P1','200P2'),
                              EngTech=c('213GS','221B0','241A0','241A3','243B0','243B3'),
                              Social=c('300EA','300EB','300EH','300EG','300EK','300EM','300EP',
                                       '300FA','300FB','300FE','300FH','300FK','300FP','313FA'),
                              SocialCareer=c('410A0','410B0','410D0','420A0'),
                              SMSECareer=c('140B1','142C0','144A0','180A0','210AB'),
                              ArtsCareer=c('500D3','500G1','500G1','500F3','500F6','500G6',
                                           '510A0','574B0','57082','700B1','700B2'),
                              SpringBoard='08106')
  s
}

## ---- function-print-table ----
xt <- function(dt,s,c){
  t <- table(dt$program.c,dt$ansessionDebut)
  seats <- sum(t)
  t %>% 
    xtable::xtable(caption = paste('Distribution of',seats,'Seats in',c,s,'by profile and year of admission')) %>% 
    xtable::print.xtable(floating.environment = 'figure',table.placement = 'H')
}