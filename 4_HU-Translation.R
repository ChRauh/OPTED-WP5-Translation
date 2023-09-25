####################################################
# Project:  OPTED WP5 - Translation
# Task:     Translate HU speeches
#
# Author:   @ChRauh (25.09.2023)
####################################################


# Packages & auth #####
library(tidyverse)
library(progress)

library(googleLanguageR)
gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key to OPTED money ...


# Dropbox path
dp <-"C:/Users/rauh/Dropbox/OPTED datasets"


# Hungary - speech corpus ####
# PLS version dated from August 24 2023

speeches <- read_rds(file = paste0(dp, "/Hungary/Corpus_speeches_hungary.rds"))
speeches$tr_id <- 1:nrow(speeches) # To have at leat one unuique id



# Cost estimation ####

speeches$characters <- nchar(speeches$text)
sum(is.na(speeches$characters)) # 2 - what is going on here ?
characters <- sum(speeches$characters, na.rm = T)
dollars <- (characters/1000000)*20 # 20 Dollars per 1 mio characters
euros <- dollars * 0.94 # As of September 25 2023
euros # 20145.31 €

rm(characters, dollars, euros)


# Some checks ###

test <- speeches %>% filter(is.na(characters)) # Two empty chair speeches with missing link? - catch that before translating
test <- speeches %>% filter(characters < 10)  # 0
test <- speeches %>% filter(characters < 50)  # 6844 - looks legit - ELNÖK - note, however, that spe.aker names are in speech text ...
rm(test)
gc()


# Translation ####

# ATTENTION WHEN REPEATING THE LOOP
speeches$lang_Google <- NA
speeches$translatedText <- NA


# # Sample for testing
# speeches <- speeches %>%
#   sample_n(size = 10)


# API translation speech-by-speech (if applicable)

pb <- progress_bar$new(total = nrow(speeches)) # Progress
for(i in 1:nrow(speeches)) {
  
  # Check if character in sentences
  if (is.na(speeches$text[i])) {next}  
  
  # Send current speech to Google API and collect response
  tr <- gl_translate(
    speeches$text[i],
    target = "en",
    format = "text",
    source = "", # automatic language detection
    model = "nmt")
  
  # Store API response in sentence data
  speeches$translatedText[i] <- tr$translatedText[1]
  speeches$lang_Google[i] <- tr$detectedSourceLanguage[1]
  
  # Update progress
  pb$tick()
  
}

gc()



# Export ####
write_rds(speeches, "./data/HU_speeches_PLS-Translated.rds")









