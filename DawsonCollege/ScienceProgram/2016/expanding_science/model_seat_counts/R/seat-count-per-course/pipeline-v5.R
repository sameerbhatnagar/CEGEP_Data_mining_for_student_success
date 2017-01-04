## ---- load-data ----
rm(list=ls())
load('student_success.RData')
library(data.table)
library(magrittr)

source('pipeline-v5-functions.R')
science.prog.codes <- c('200P1','200P2','200H1','200H2','200E2','08162','08164','09162','09164')

## ---- student-counts ----
student_list(20133,c('200P1','200P2')) %>% length()
student_list(20133,c('200H1','200H2')) %>% length()
student_list(20133,c('200E2')) %>% length()
student_list(20141,c('200P1','200P2')) %>% length()
student_list(20141,c('200H1','200H2')) %>% length()

## ---- student-counts-ESPDSP ----
student_list(20133,c('08162','08164')) %>% length()
student_list(20133,c('08162','08164','09162','09164')) %>% length()

## ---- seat-counts ----
seat_count_all_profiles_fall_and_winter_admits(2013) %>% knitr::kable()

## ---- collapse-all-years ----
seat_count_all_profiles_fall_and_winter_admits(2013) %>% collapse_all_years() %>% knitr::kable()

## ---- projections-new-admits ----
additional.admits <- 300 - student_list(20133,c('200P1','200P2')) %>% length()
project_seats_required(2013,20133,additional.admits,'Pure',c('200P1','200P2')) %>% knitr::kable()

## ----projections-all-years ----
bottleneck.courses <- c('101NYA05','101BZE05','202NYA05','202NYB05','202BZF05','202BZG05')


year <- 2010
semester <- 20103
additional.admits <- 300 - student_list(semester,c('200P1','200P2')) %>% length()
project_seats_required(year,semester,additional.admits,'Pure',c('200P1','200P2'))[course %in% bottleneck.courses
                                                                               ] %>% knitr::kable()

year <- 2011
semester <- 20113
additional.admits <- 300 - student_list(semester,c('200P1','200P2')) %>% length()
project_seats_required(year,semester,additional.admits,'Pure',c('200P1','200P2'))[course %in% bottleneck.courses
                                                                                  ] %>% knitr::kable()

year <- 2012
semester <- 20123
additional.admits <- 300 - student_list(semester,c('200P1','200P2')) %>% length()
project_seats_required(year,semester,additional.admits,'Pure',c('200P1','200P2'))[course %in% bottleneck.courses
                                                                                  ] %>% knitr::kable()

## ----sanity-check ----
additional.admits <- 100
project_seats_required(2013,20133,additional.admits,'Health',c('200H1','200H2'))[course %in% bottleneck.courses
                                                                                 ] %>% knitr::kable()



