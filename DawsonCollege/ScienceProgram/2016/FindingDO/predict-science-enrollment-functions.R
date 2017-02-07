## ---- calculate-state-transition-probabilities ----
state_transition_probabilities <- function(state1){
  s1<-student_states[state==state1,student_number]
  t <- substr(state1,nchar(state1),nchar(state1)) %>% as.integer()
  student_states[student_number %in% s1][term==t+1,state] %>% table() %>% prop.table() %>% round(2)
}


## ---- project-state1-state2 ----
projections <- function(s1,probs,current.ansession)
{
  current.semester <- ifelse(current.ansession %% 10==1,'Winter','Fall')

  probs.state1<-probs[grep(current.semester,state1),.(transition.probability,state1,state2)]
  
  setkey(s1,state)
  setkey(probs.state1,state1)
  projection<-probs.state1[s1]
  projection[,state2.population.projection:=transition.probability*N]
  
  s2 <- projection[,sum(state2.population.projection),by=state2]
  setnames(s2,'V1','N')
  
  state.projections<-list(s2=s2,state.transition.projection=projection)
  
  return(state.projections)
}

## ---- append-new-admits-to-state2 ----
update_new_admits <- function(s2,student_states,current.ansession)
{
  setnames(s2,'state2','state')
  current.semester <- ifelse(current.ansession %% 10==1,'Winter','Fall')
  
  # use actual enrollement data to append number of new admits into each profile
  if (current.ansession<=NOW){
    new.admits<-student_states[ansession==current.ansession][term==1][grep(current.semester,state),.N,by=state]
    state1<-rbind(s2,new.admits)
    write.csv(state1,
              file = paste0(
                'predict-science-enrollement-tables/population-distributions-by-state/',current.ansession,'.csv'),
              row.names = F)
    return(state1)  
  }
  # if projecting forward, use average number of students admitted to each profile in all past
  # instances of this semester (different for winter or fall)
  else{
    new.states <- c(paste0(current.semester,'-ESP-1'),paste0(current.semester,'-DSP-1'),
                    paste0(current.semester,'-Pure-1'),paste0(current.semester,'-Health-1'),
                    paste0(current.semester,'-Enviro-1'))
    mean.new.admits<-student_states[state %in% new.states,.N,by=.(state,ansession)][,mean(N),by=state][]
    setnames(mean.new.admits,'V1','N')
    setkey(mean.new.admits,state)
    new.admits<-data.table(state=new.states)
    setkey(new.admits,state)
    new.admits<-new.admits[mean.new.admits]
    if (current.semester=='Fall'){
      new.admits[state=='Fall-Pure-1',N:=300]
    }
    state1<-rbind(s2,new.admits)
    write.csv(state1,
              file = paste0(
                'predict-science-enrollement-tables/population-distributions-by-state/',current.ansession,'.csv'),
              row.names = F)
        return(state1)
  }
}

## ---- update-enrollment-history ----
enrollment_update <- function(current.ansession,s2.new,enrollment.history)
{
  current.semester <- ifelse(current.ansession %% 10==1,'Winter','Fall')
  s2.new[,c('semester','program','term'):=tstrsplit(state,'-')]
  
  enrollments<-s2.new[semester==current.semester,sum(N),by=program]
  setnames(enrollments,'V1','N')
  enrollments[,ansession:=current.ansession]
  enrollments[,program:=paste0(program,'.projected')]
  enrollment.history.update<-rbind(enrollment.history,enrollments)
  return(enrollment.history.update)
}

## ----project-enrollements-one-year-forward ----
project_enrollments_one_year_forward <- function(data,current.ansession){

s3.new<-data$s3.new
enrollment.history<-data$enrollment.history
exits.history <- data$exits.history
  
# project to fall2
s1 <- s3.new
state.projections<-projections(s1,probs,current.ansession)

# keep track of who leaves the science program, and how
exits<-state.projections$state.transition.projection[state2=='Graduated' | state2=='Out',.(state1,state2,state2.population.projection)]
exits[,ansession:=current.ansession]
exits.history<-rbind(exits.history,exits)

# append new admits fall2
current.ansession<-current.ansession+2
s2.new <- update_new_admits(state.projections$s2,student_states,current.ansession)

# append fall1 to enrollment history (actual and projected)
enrollment.history<-enrollment_update(current.ansession,s2.new,enrollment.history)

# project to winter2
state.projections <- projections(s2.new,probs,current.ansession) 

# keep track of who leaves the science program, and how
exits<-state.projections$state.transition.projection[state2=='Graduated' | state2=='Out',.(state1,state2,state2.population.projection)]
exits[,ansession:=current.ansession]
exits.history <- rbind(exits.history,exits)

# append new admits winter2
current.ansession <- current.ansession+8
s3.new<-update_new_admits(state.projections$s2,student_states,current.ansession)

# append winter1 to enrollment history (actual and projected)
enrollment.history<-enrollment_update(current.ansession,s3.new,enrollment.history)
year.projections <- list(s3.new=s3.new,enrollment.history=enrollment.history,exits.history=exits.history)
return(year.projections)
}