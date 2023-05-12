library(tidyverse)

# General info ####

# The easiest way to speak to Google's cloud translation API from r is ggoleLanguageR: # https://cran.r-project.org/web/packages/googleLanguageR/index.html

# Before this can be done, the Google service must be set up (at: https://console.cloud.google.com/) along the following steps

# Log-In with valid Google account, create billing setup (credit card or paypal)
# Create project for th translation task in this account, activate the Google Translate API for this project
# Create a service account for that project, grant/limit access for this account to the Google Translate API
# Generate OAuth key for this service account in .json format and store it (privately !!!) in you local translation project
# Call the package in your local project and authenticate yourself: 

library(googleLanguageR) 
gl_auth("./private/translationtest-384920-e072ae55fbf8.json")



# Pricing ####

# https://cloud.google.com/translate/pricing?hl=en
# Conditions may change ...

# Billing is done monthly and works by number of characters submitted to the API

# First 500k characters per month are free
# Everything between 500k and 1 billion characters costs *20$ per 1 million characters*
# Above 1 billion characters discount prices can be negotiated

# Note: Google says that the language of the submitted text is autodetected free of charge!
# "Note that if you don't specify a source language for the translate method, Cloud Translation detects the source language for you. 
# You are only charged for the text that you provided; there's no additional charge for the detection in addition to the translation. 
# For example, if you submit 1,000 characters for translation without specifying the source language, you are only charged for the 1,000 characters."

# -> very useful for WP5 and the EP translation in particular, but we should maybe test that

# Cost control
# is a tricky issue - Google doesn't allow to easily set a financial limit after which the API stops working
# Thus one really need to take care in how much data is sent to the API and when
# But two things help:
# 1. Set monthly budget notifications - define limit per month, auto-send notifications by mail when 30%, 50%, or 80% are reaching
# https://cloud.google.com/billing/docs/how-to/budgets?hl=en
# 2. Set api limits/quotas - quite thorny, haven't fully understood this and does not work in financial units ...
# https://cloud.google.com/apis/docs/capping-api-usage?hl=en



# Questions ####

# How quickly does billing occur? If not immediate, proceed with utmost caution!
# To check my test project: https://console.cloud.google.com/billing/01A249-CB11C7-6A7EEF/budgets?project=translationtest-384920

# Translation model: base or nmt? 
# nmt is the neural network one, use this!

# If we want to avoid language detection, can we vectorize the source language specficiation?
# And what language labels does the API use? Looks like ISO2 but should be verified
# see https://cloud.google.com/translate/docs/languages?hl=en, its actually ISO-639-Code

# Translate chair speeches?

# Sentence level translation?
# Advisable for EP speeches in which language mixes occur, see below
# Challenege, because on the the one hand we don't want to spent money to 'translate' sentences already in english
# but local language detection is also error prone on the other
# My current thinking: do local language detection by at least three different algorithms and send everything to Google 
# on which they do not agree on English ...


# First small sandbox test ####

german_test <- c("Das ist mein deutscher Text, der hoffentlich sehr gut übersetzt wird!",
                 "Die Mehrwertssteuer in Deutschlang ist definitiv zu hoch und sollte sich an realistischeren juristischen Grundsätzen orientieren.",
                 "Wir alles wissen: Die EU ist ein bürpkratisches Monstrum das dunklen Mächsten dient!") # Note: The typo is corrected by google
              
nchar(german_test)
sum(nchar(german_test)) # 282

# german_result <-
#   gl_translate(
#   german_test,
#   target = "en",
#   format = "text",
#   source = "de",
#   model = "nmt")

german_result$translatedText # Quite impressive


# 69 characters first    
# 282 characters translated  # Booking apparently takes some time (daily??? - this makes it risky)  # 



# Bigger test that goes beyond the 500k charcters limit ####

# # Load EP corpus
# # Before final translation check back with Jan and Lukas for final version !!!!
# 
# ep <- read_rds("C:/Users/rauh/Dropbox/OPTED datasets/EP/Corpus_speeches_EP.RDS")
# 
# 
# # Character count and crude language detection
# library(cld2)
# 
# ep <- ep %>% 
#   mutate(characters = nchar(text),
#          language = detect_language(text,plain_text = TRUE)) # Takes a bit
# 
# table(ep$language)
# summary(ep$characters) # Average is 1893 characters, i.e. we need at least 264 speeches to break the free character limit of the API
# 
# # Sample of non-english speeches
# 
# speechsample <- ep %>% 
#   filter(language != "en") %>% 
#   sample_n(350)
# 
# sum(speechsample$characters) # 515569 in my first attempt, save this for reference
# 
# write_rds(speechsample, "./testdata/ep-non-english-350.rds")

# # Reload
# speechsample <- read_rds("./testdata/ep-non-english-350.rds")
# 
# # Translate with language detection
# 
# start <- Sys.time()
# ep_test <-
#   gl_translate(
#     speechsample$text,
#     target = "en",
#     format = "text",
#     source = "", # Should result in automatic language detection
#     model = "nmt")
# duration <- Sys.time() - start
# duration # 1.2 mins for 350 speeches 
# 
# 
# # Add to original data and export
# speechsample$gtranslation <- ep_test$translatedText
# speechsample$gdetection <- ep_test$detectedSourceLanguage
# 
# write_rds(speechsample, "./testdata/ep-non-english-350-Translated.rds")

# 10 mins after running this, no costs on my account (yet)
# Maybe it kicks in after the 1 million characters are reached? Maybe daily billing ... let's wait and see


# Sample inspection ####

speechsample <- read_rds("./testdata/ep-non-english-350-Translated.rds")


# Random translation inspections
i <- sample(1:nrow(speechsample), 1)
i
speechsample$gdetection[i]
speechsample$text[i]
speechsample$gtranslation[i]
# Impressive!


# Language detection inconsistencies

sum(speechsample$language != speechsample$gdetection) # 14
sum(speechsample$language != speechsample$gdetection)/nrow(speechsample) # 4%

langdiff <- speechsample %>% # Full examples
  filter(language != gdetection)

# qualitative inspection suggests that the local cld2 if more frequently wrong than Google - as expected, and good for us
# Language switches occur within EP speeches, and Google seems to detect and to translate the majority part !
# Maybe a sentence tokenizationcould improve this -- better or more consistent translation would result, but more careful data managment needed, long run time, but should be cost-equaivalent

langdiff %>% # re-occurring language pairs?
  select(c(language, gdetection)) %>% 
  group_by(language, gdetection) %>% 
  summarise(count = n())

# hr (croatian) and bs (bosnian) occur in 4 out of 14 times (once with Serbian in addition)
# Unsurprising as these languages are very close - and in the examples here a reasonable English text persists in all of them

langdiff$tr_language <- detect_language(langdiff$gtranslation, plain_text = TRUE) # CLD estimates for the Google-translated text
table(langdiff$tr_language)

# Half og thos result in english text (according to the error prone cld)
# The other half is -- qualitatively assessed -- mixed language material
# Strong arguments for sentence tokenization before auto-translation



# Sentence tokenization experiment ####

# Reload speech sample
speechsample <- read_rds("./testdata/ep-non-english-350.rds")

# Tokenize
# Quanteda or tidytext offer alternative approaches, cf.: https://stackoverflow.com/questions/47211643/tokenizing-sentences-with-unnest-tokens-ignoring-abbreviations
library(corpus)
sentences <- text_split(speechsample$text,
                        units = "sentences") %>% 
  as.data.frame() %>% 
  mutate(text = as.character(text)) # Seems to return a list element

# Guess language with cld
sentences$language <- detect_language(sentences$text, plain_text = TRUE)
# Looks very consistent
# But also shows that our pre-selection left English texts in there

# - better shoot up the whole corpus???
# Or sentence-tokenize it first and then select on this basis??? XXX


# Translate sentences via google API - COST!

sum(nchar(sentences$text)) #515569 (~ 10$)

start <- Sys.time()
ep_sentence_test <-
  gl_translate(
    sentences$text,
    target = "en",
    format = "text",
    source = "", # Should result in automatic language detection
    model = "nmt")
duration <- Sys.time() - start
duration # ~ 8.2 mins for 3444 sentences (factor 8)

# One request status error in there - "Request Status Code:  400"
sum(is.na(ep_sentence_test$translatedText)) # 0, good! Bad request seem to get repeated ...

# Combine with raw data and export
sentences$gdetection <- ep_sentence_test$detectedSourceLanguage
sentences$gtranslation <- ep_sentence_test$translatedText
write_rds(sentences, "./testdata/ep-non-english-350-Translated_sentenceLevel.rds")

# Quick look at language inconsistencies
sum(sentences$gdetection != sentences$language, na.rm = T) # 126
sum(sentences$gdetection != sentences$language, na.rm = T)/nrow(sentences) # 3.6%
# qualitatively the yugoslawian languages again and quite a few missing clds - so this should be significantly better!








