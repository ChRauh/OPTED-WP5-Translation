####################################################
# Project:  OPTED WP5 - Translation
# Task:     Translate EP speeches on sentence  level 
#           with the Google Cloud Translation API
#
# Author:   @ChRauh (20.09.2023)
####################################################



# Packages #####

library(tidyverse)
library(progress)

library(googleLanguageR)
gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key to OPTED money ...



# EP speeches ####
# Sentence tokenized and with language detection etc.
sent <- read_rds("./data/EP_speeches_PLS-SentenceTokenizedForTranslation.rds")


# Translation ####

# To keep track of potential errors and minimize the amount of data/costs to be burned at once
# I decided to proceed iteratively, calling the APi only when a sentence is to be translated (non-English, more than one 'letter', see earlier script)
# and only when the translated column is not populated yet (this way the loop can be run recursively, but not the column initialization!)

# ATTENTION WHEN REPEATING THE LOOP
sent$lang_Google <- NA
sent$translatedText <- NA


# # Sample for testing
# sent <- sent %>%
#   sample_n(size = 100)


# API translation sentence by sentence (if applicable)

pb <- progress_bar$new(total = nrow(sent)) # Progress
for(i in 1:nrow(sent)) {
  
  # Test if current row/sentence needs to be translated
  if (!sent$toBeTranslated[i]) {next}
  if (!is.na(sent$translatedText[i])) {next}  
  
  # Send current sentence to Google API and collect response
  tr <- gl_translate(
    sent$sentence[i],
    target = "en",
    format = "text",
    source = "", # automatic language detection
    model = "nmt")

  # Store API response in sentence data
  sent$translatedText[i] <- tr$translatedText[1]
  sent$lang_Google[i] <- tr$detectedSourceLanguage[1]
  
  # Update progress
  pb$tick()

}

gc()


# Export sentence-level results ####
write_rds(sent, "./data/EP_speeches_PLS-Sentences_Translated.rds")




# Aggregate to speech level####

# Store speech ids in which translation occurred 
tr_speeches <- sent %>% 
  filter(toBeTranslated) %>% # Only speeches which at least one translated sentence
  select(id) %>% 
  unique() %>% 
  pull() # Vector of respective speech ids


# Aggregate sentences to speech text
speeches <- sent %>% 
  mutate(text = ifelse(is.na(translatedText), # If there is no translation
                       sentence,              # Keep the raw text, else ...
                       translatedText)) %>%   # Keep the translation
  group_by(id) %>%                            # Speech level grouping
  summarise(translatedText = paste(text, collapse = " ")) # Sentences separate by exactly one white-space

  
# Mark speeches in which translations occurred  
# Along the ids identified above
speeches <- speeches %>% 
  mutate(translationInSpeech = ifelse(id %in% tr_speeches, T, F))
sum(speeches$translationInSpeech)



# Export speech level results ####
# after merging them with the original meta data 

# Original speech level data
org_speeches <- read_rds("./data/EP_speeches_PLS-original.rds") 

# Join translations and original data
speeches <- speeches %>% 
  left_join(org_speeches, by = "id")

# # of obs of original data?
nrow(speeches) == 509649

# Write this to disk
write_rds(speeches, "./data/EP_speeches_PLS-Translated.rds")





