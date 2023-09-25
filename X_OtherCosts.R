####################################################
# Project:  OPTED WP5 - Translation
# Task:     Quickly check which countries to 
#           translate next (along costs)
#
# Author:   @ChRauh (20.09.2023)
####################################################

library(tidyverse)

# Dropbox path
dp <-"C:/Users/rauh/Dropbox/OPTED datasets"


# Hungary - speeches
speeches <- read_rds(file = paste0(dp, "/Hungary/Corpus_speeches_hungary.rds"))
speeches$characters <- nchar(speeches$text)
sum(is.na(speeches$characters)) # 2 - what is going on here ?
characters <- sum(speeches$characters, na.rm = T)
dollars <- (characters/1000000)*20 # 20 Dollars per 1 mio characters
euros <- dollars * 0.93 # As of September 20 2023
euros # 19931 € - Uff!

min(speeches$date) # 1994-06-28
max(speeches$date) # 2022-03-10


# CZ - speeches
speeches <- read_rds(file = paste0(dp, "/Czech Republic/Corpus_Speeches_CZ.RDS"))
speeches$characters <- nchar(speeches$text)
sum(is.na(speeches$characters)) # 0
characters <- sum(speeches$characters, na.rm = T)
dollars <- (characters/1000000)*20 # 20 Dollars per 1 mio characters
euros <- dollars * 0.93 # As of September 20 2023
euros # 6420 - consistent with prior estimates

min(speeches$date) # 1993-01-01
max(speeches$date) # 2017-10-16


# Croatia - speeches
speeches <- read_rds(file = paste0(dp, "/Croatia/Corpus_speeches_croatia.RDS"))
speeches$characters <- nchar(speeches$text)
sum(is.na(speeches$characters)) # 0
characters <- sum(speeches$characters, na.rm = T)
dollars <- (characters/1000000)*20 # 20 Dollars per 1 mio characters
euros <- dollars * 0.93 # As of September 20 2023
euros # 8474 - consistent with prior estimates

min(speeches$date) # 2003-12-22
max(speeches$date) # 2020-05-15


