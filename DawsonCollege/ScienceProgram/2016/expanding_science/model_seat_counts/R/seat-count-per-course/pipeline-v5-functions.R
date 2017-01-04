## ---- student-lists ----
# given semester,profile codes, and program codes, get list of students admitted to those profiles,
# keeping only those for whom it is the first time they have been admitted to the program
student_list <- function(semester,profile.codes){
  semester.six.back <- semester-30
  science.prog.codes <- c('200P1','200P2','200H1','200H2','200E2','08162','08164','09162','09164')
  
  
  ## ---- students ----
  students.all <- admission[ansessionDebut==semester][program %in% profile.codes][,student_number] %>% unique()
  students.old <- admission[student_number %in% students.all][program %in% science.prog.codes][ansessionDebut<semester & ansessionDebut > semester.six.back,student_number] %>% unique()
  setdiff(students.all,students.old)
  
}


## ---- seat-count-function ----
# given a start semester, and a set of profile codes, find number of seats occupied by students
# admitted to the profiles in all science courses over the six semesters following the 
# start semester
seat_count<-function(semester,profile){
  students.new <- student_list(semester,profile)
  
  ## ---- courses ----
  science.prog.courses <- c('201NYA05','201NYB05','201NYC05','201BZS05','201BZF05',
                            '202NYA05','202NYB05','202BZF05','202BZG05',
                            '203NYA05','203NYC05','203NYB05','203BZE05','203BZA05',
                            '101NYA05','101BZE05','101BZH05',
                            '360420DW')
  semester.six.back <- semester-30
  semester.six.forward <- semester+28

  
  ## ---- seats ----
  sessions <- etudiant_session[student_number %in% students.new][ansession>=semester & ansession<=semester.six.forward,IDEtudiantSession]
  groups <- cours[course %in% science.prog.courses, IDGroupe]
  seats <- inscription[IDEtudiantSession %in% sessions & IDGroupe %in% groups][,.(IDEtudiantSession,IDGroupe,Note)]
  
  # merge tables to add student numbers and course names to seats table
  setkey(seats,IDGroupe)
  cours.course.names <- cours[,.(IDGroupe,course)]
  setkey(cours.course.names,IDGroupe)
  seats<-seats[cours.course.names,nomatch=0]
  
  setkey(seats,IDEtudiantSession)
  etudiant_session.student.numbers<-etudiant_session[,.(IDEtudiantSession,ansession,student_number)]
  setkey(etudiant_session.student.numbers,IDEtudiantSession)
  seats<-seats[etudiant_session.student.numbers,nomatch=0]
  
  ## ---- seats-results ----
  seats[,.N,by=list(ansession,course)]
}


## ---- seat-count-function-all-profiles ----
# repeat seat_count for multiple profiles
seat_count_all_profiles <- function(semester){
  
  profile <- c('200P1','200P2')
  pure <- seat_count(semester,profile)
  pure[,profile:='Pure']
  
  profile <- c('200H1','200H2')
  health <- seat_count(semester,profile)
  health[,profile:='Health']
  
  profile <- c('200E2')
  env <- seat_count(semester,profile)
  env[,profile:='Enviro']
  
  profile <- c('08162','08164')
  espdsp <- seat_count(semester,profile)
  espdsp[,profile:='ESPDSP']
  
  rbind(health,pure,env,espdsp)
}  

## ---- seat_count_all_profiles_fall_and_winter_admits ----
# given a start year,
# repeat seat counts for all profiles, for both fall and winter admissions in a given year,
# and return combined table
seat_count_all_profiles_fall_and_winter_admits <- function(year){
  
  semester.fall <- 10*year + 3
  semester.winter <- semester.fall + 8
  
  seats.science.fall <- seat_count_all_profiles(semester.fall)
  seats.science.fall[,Population:='A']
  
  seats.science.winter <- seat_count_all_profiles(semester.winter)
  seats.science.winter[,Population:='B']
  
  seats.science <- rbind(seats.science.fall,seats.science.winter)

  # extract fall and winter terms as seperate tables
  seats.science[,term:=ifelse(ansession %% 10 ==3,'Fall',ifelse(ansession %% 10 == 1,'Winter','Summer'))]
  seats.science<-seats.science[!(term =='Summer'),]
  seats.science[,year:=round(ansession/10,0)]
  seats.science[,ansession:=NULL]
  
  # seats.science[,sum(.SD$N),by=list(term,year,course)]
  
  seat.count.profile.distribution <- dcast.data.table(
    seats.science,year + term + course ~ profile,value.var = 'N',fun.aggregate = sum) 
  
  seat.count.profile.distribution[,Total:=sum(Health,Pure,Enviro,ESPDSP, na.rm = T),
                                  by=list(year,term,course)]
  return(seat.count.profile.distribution)
  }

## ---- collapse-all-years ----
# given seat counts for each course given over six semesters, collapse into 'representative year'
# which simulates effect of admission numbers, failure rates, program transfer rates,
# all kept at same levels for three years
collapse_all_years <- function(dt)
{
  ## ---- seat-counts-all-years ----
  seats.science.all.at.once<-dt[,sum(Total),by=list(term,course)]
  setnames(seats.science.all.at.once,'V1','current.seats')
  seats.science.all.at.once
}

## ---- project-required-seats ----
project_seats_required <- function(year,semester,additional.admits,profile.name,profile.codes)
{
  students.new <- student_list(semester,profile.codes)
  
  current <- length(students.new)
  new <- current + additional.admits
  r <- new/current

  dt <- seat_count_all_profiles_fall_and_winter_admits(year)
  dt[,paste0(profile.name,'.projected'):=round(r*get(profile.name),1)]
  
  cols.projected<-setdiff(colnames(dt),c(profile.name,'year','Total','course','term'))
  cols.actual<- setdiff(colnames(dt),c(paste0(profile.name,'.projected'),'year','Total','course','term'))
  dt.projected<-dt[,lapply(.SD,sum),by=.(term,course),.SDcols=cols.projected]
  dt.actual<-dt[,lapply(.SD,sum),by=.(term,course),.SDcols=cols.actual][,.(course,term,get(profile.name))]
  setnames(dt.actual,'V3',profile.name)
  
  setkey(dt.projected,term,course)
  setkey(dt.actual,term,course)
  dt<-dt.actual[dt.projected]
  
  dt[,Total:=Reduce('+',.SD),.SDcols=cols.actual]
  dt[,Total.projected:=Reduce('+',.SD),.SDcols=cols.projected]
  return(dt)
}