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
dp <-"C:/Users/rauh/Dropbox/OPTED datasets" # WZB


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
test <- speeches %>% filter(characters < 50)  # 6844 - looks legit - ELNÖK - note, however, that speaker names are in speech text ...
rm(test)
gc()


# Translation ####

# ATTENTION WHEN REPEATING THE LOOP
speeches$lang_Google <- NA
speeches$translatedText <- NA


# # Sample for testing
# speeches <- speeches %>%
#   sample_n(size = 10)

# Re-load partially translated corpus
speeches <- read_rds("./data/HU_speeches_PLS-Translated_backup.rds")


# API translation speech-by-speech (if applicable)

pb <- progress_bar$new(total = nrow(speeches)) # Progress
for(i in 1:nrow(speeches)) {
  
  # Check if character in sentences
  if (is.na(speeches$text[i])) {next}  
  
  # Check if already translated (in case of abortion in between)
  if(!is.na(speeches$translatedText[i]))  {next} 
  
  # Exclude a couple of extraordinary speeches that duplicated several times (affects 370 obs in total, see below)
  if(speeches$speech_id[i] %in% c("20182022_102_49", "20182022_176_35", "20182022_192_33", "20182022_50_2")) {next}
  
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

sum(is.na(speeches$translatedText)) # 37272 after first run

gc()


# Export ####
write_rds(speeches, "./data/HU_speeches_PLS-Translated.rds")




# # Inspect ####
# # after first run broke at 450608
# 
# sum(is.na(speeches$translatedText)) # 37272
# test <- speeches %>% filter(is.na(translatedText))
# 
# # Two extraordinary long speeches (that's why it broke, payload limit) that are duplicated multiple times
# # speech_id: 20182022_50_2 - this is
# # speech_id: 20182022_50_4
# 
# # Looks like the same speech text, but meta data vary (a.o. agenda)
# 
# # Check duplicates (only in no-translated text)
# duplicates <- test %>% 
#   group_by(speech_id) %>% 
#   summarise(count = n()) %>% 
#   arrange(desc(count))
# 
# # For fucks sake ... what is going on here???
# 
# # Inspect text lengts
# hist(test$characters)
# hist(speeches$characters)
# 
# test2 <- speeches %>% filter(characters > 60000) # 397
# test2 <- speeches %>% filter(characters > 60000 & is.na(translatedText)) # 370
# length(unique(test2$speech_id)) # 4
# table(test2$speech_id) # 20182022_102_49 20182022_176_35 20182022_192_33   20182022_50_2
# table(test2$characters) # Only 4 different sizes - text duplicates!
# 
# test3 <- speeches %>% filter(speech_id %in% unique(test2$speech_id)) # Cross check
# nrow(test2) == nrow(test3) # TRUE - I exclude those speeches from translation (370 cases)
# 
# rm(test, test2, test3, duplicates, pb, tr, i)
# gc()

# One might want to check the speeches with 17113 characters as well ...
# 25978 also extremely often duplicated!
# 31185
# 27296
# 48524






