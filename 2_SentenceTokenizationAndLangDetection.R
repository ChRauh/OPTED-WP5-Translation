####################################################
# Project:  OPTED WP5 - Translation
# Task:     Tokenize EP speeches into sentences and
#           detect languages
#
# Author:   @ChRauh (20.09.2023)
####################################################


# Packages #####

library(tidyverse)
library(tidytext)
library(spacyr)
spacy_initialize()
library(progress)
library(patchwork)
library(googleLanguageR)

# Language detection packages
library(fastText)
file_ftz = system.file("language_identification/lid.176.ftz", package = "fastText") # The small pre-trained model
library(cld2)
library(cld3)


# EP speech corpus ####
# Version with unique ID
speeches <- read_rds("./data/EP_speeches_PLS-original.rds") %>% 
  select(c(id, text))
gc()



# # Compare tokenizers ####
# # tidytext vs  spacyr 
# 
# comp <- data.frame()
# 
# pb <- progress_bar$new(total = 100)
# for (i in 1:100) {
#   
#   # Sample for testing
#   sample <- speeches %>%
#     sample_n(100, replace = F)
#   
#   # Tokenize with tidytext
#   start <- Sys.time()
#   sentences <- sample %>%
#     group_by(id) %>%
#     unnest_tokens(output = sentence,
#                   input = text,
#                   token = "sentences",
#                   to_lower = F) %>%
#     ungroup()
#   duration1 <- Sys.time() - start
# 
#   
#   # Tokenize with spacyr
#   start <- Sys.time()
#   sample$doc_id <- paste0("text", 1:nrow(sample)) # The way in qhich spacr assigns doc_ids
#   sentences2 <- spacy_tokenize(sample$text,
#                                what = "sentence",
#                                output = "data.frame") %>% 
#     left_join(sample %>% select(doc_id, id), by = "doc_id") %>% 
#     select(-doc_id) %>% 
#     rename(sentence = token) %>% 
#     select(names(sentences)) # same column order as above
#   duration2 <- Sys.time() - start
#   
#   # Store parameters
#   current <- data.frame(tt.duration = duration1,
#                         sp.duration = duration2,
#                         tt.length = mean(nchar(sentences$sentence)),
#                         sp.length = mean(nchar(sentences2$sentence)),
#                         tt.sentences = nrow(sentences),
#                         sp.sentences = nrow(sentences2))
#   
#   # Append to target
#   comp <- rbind(comp, current)
#   
#   # Update progress
#   pb$tick()
# 
# }
# 
# 
# # Plot differences
# pl.sentences <-
#   ggplot(comp, aes(x = tt.sentences, y= sp.sentences))+
#   geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red")+
#   geom_point(alpha = .7)+
#   labs(title = "Number of \'sentences\' retrieved",
#        x = "tidytext",
#        y= "spacyR")+
#   theme_bw()
# 
# pl.length <-
#   ggplot(comp, aes(x = tt.length, y= sp.length))+
#   geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red")+
#   geom_point(alpha = .7)+
#   labs(title = "Average number of characters in \'sentences\'",
#        x = "tidytext",
#        y= "spacyR")+
#   theme_bw()
# 
# pl.duration <-
#   ggplot(comp, aes(x = as.numeric(tt.duration, units = "secs"), y= as.numeric(sp.duration, units = "secs")))+
#   geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red")+
#   geom_point(alpha = .7)+
#   labs(title = "Time the tokenizer required (secs)",
#        x = "tidytext",
#        y= "spacyR")+
#   theme_bw()
# 
# 
# pl.comp <- 
#   pl.duration / (pl.sentences + pl.length) +
#   plot_annotation(title = "Comparing tokenizers",
#                   subtitle = "Based on 100 random samples of 100 (multilingual) EP speeches, 
#                   tidytext is much faster than spacyR, and returns fewer \'sentences\' that contain more characters.\n",
#                   theme = theme(plot.title = element_text(hjust = 0.5, size = 16),
#                                 plot.subtitle = element_text(hjust = 0.5, size = 12)))
# 
# ggsave("./plots/tokenizerComp.png", pl.comp, height = 14, width = 22, units = "cm")
# 
# rm(list=setdiff(ls(), "speeches"))
# gc()




# # Sentence tokenization - full corpus ####
# 
# # Tokenize with tidytext
# start <- Sys.time()
# sentences <- speeches %>%
#     group_by(id) %>%
#     unnest_tokens(output = sentence,
#                   input = text,
#                   token = "sentences",
#                   to_lower = F) %>%
#     ungroup()
# duration <- Sys.time() - start
# duration # 52 secs on Dell!?
# 
# # Export
# write_rds(sentences, "./data/EP_speeches_PLS-SentenceTokenized.rds")

sent <- read_rds("./data/EP_speeches_PLS-SentenceTokenized.rds")


# Inspect ####

# Some sentence parameters
sent$wordcount <- str_count(sent$sentence, "\\w+")
sent$chars <- nchar(sent$sentence)
sent$letters <- str_count(sent$sentence, "[A-Za-z]")

spe.sent <- sent %>% 
  group_by(id) %>% 
  summarise(sentences = n())

# Wordcount inspection
summary(sent$wordcount)
test <- sent %>% filter(wordcount > 32) # 3rd Quartile - lots of normal stuff
test <- sent %>% filter(wordcount > 100) # Much looks correct, lots on non_anglish on first impression  - sometimes agenda items with doc titles and sponsor etc show up here
test <- sent %>% filter(wordcount > 1000) # 12, all agenda items (probably not spoken words but no language errors)
test <- sent %>% filter(wordcount < 14) # 1st Quartile - normal short sentences
test <- sent %>% filter(wordcount < 7) # 230k still - reasonable stuff, lots of bracketed interventions
test <- sent %>% filter(wordcount == 0) # 616 - Only puntuation and formating markers like ⁂ - probably need to be excluded from  language detection and translation

# Character count
summary(sent$chars)
test <- sent %>% filter(chars < 3) # 2073 - ordered or decimal numbers (like "1."), abbreviations / initials (like "M."), and individual punctuation characters
# Her slight issues may occur when aggregating this pack to the speech level, e.g. where the tokenizer has split a decimal number - maybe manually correct after aggregation?

# Letter count
summary(sent$letters)
test <- sent %>% filter(letters <= 3) # 7850 - lots of greek and cyrillic (well...) - these should not be excluded from language detection and translation!

# str_count("Κυρία Πρόεδρε, κάθε χώρα της Ευρωπαϊκής Ένωσης", "\\w") # counts this as letters
sent$letters <- str_count(sent$sentence, "\\w") - str_count(sent$sentence, "[0-9]") - str_count(sent$sentence, fixed("_")) # All letters (from whatever alphabet)

summary(sent$letters)
test <- sent %>% filter(letters <= 3) # 5241 - lots of valid examples ("How?")
test <- sent %>% filter(letters <= 2) # 3949 - still lots of valid examples ("No!")  - but also lots of digits and abbreviations as seen above
test <- sent %>% filter(letters == 0) # All stuff that does not / cannot be translated
test <- sent %>% filter(letters == 1) # 458 - these are the initials - seems to be the sweet spot - try detecting languages ananything with letters >1
test <- sent %>% filter(letters < 2) # 3066 - looks about right - all-non translatable stuff 

# Sentences per speech
hist(spe.sent$sentences)
test <- sent %>% filter(id %in% spe.sent$id[spe.sent$sentences > 50]) # Looks reasonable - 198 is a good example for a multilingual speech
test <- sent %>% filter(id %in% spe.sent$id[spe.sent$sentences > 250]) # Looks reasonable

# 198- first couple of sentences contain four different languages - test example!
# 227370 - Good example for wrongly split sentence - "Es entsteht zurzeit innerhalb der Grenzen der Europäischen Union ein 29.", "Staat.".

# Except for decimal/ordered numbers/dates and abbreviations splitting looks quite good



# Language detection ####
# Circling around potential errors along the letter limit developed above

# fastText
sent$lang_ft <- NA
sent$lang_ft[sent$letters > 1] <- fastText::language_identification(input_obj = sent$sentence[sent$letters > 1],
                                                                     pre_trained_language_model_path = file_ftz,
                                                                     k = 1,
                                                                     th = 0.0,
                                                                     threads = 1,
                                                                     verbose = T)[[1]] # This one takes a bit ...
# Compact language detector 2
sent$lang_cld2 <-NA
sent$lang_cld2[sent$letters > 1] <- cld2::detect_language(sent$sentence[sent$letters > 1])

# Compact language detector 3
sent$lang_cld3 <-NA
sent$lang_cld3[sent$letters > 1] <- cld3::detect_language(sent$sentence[sent$letters > 1])


# How often do they agree?
sent$lang_agree <- (sent$lang_cld2 == sent$lang_cld3 & sent$lang_ft == sent$lang_cld2 &  sent$lang_ft == sent$lang_cld3)
sent$lang_agree <- ifelse(is.na(sent$lang_ft) & is.na(sent$lang_cld2) & is.na(sent$lang_cld3),
                          T,
                          sent$lang_agree)
sent$lang_agree[is.na(sent$lang_agree)] <- F
sum(sent$lang_agree)/nrow(sent) # ~96%, nice

# Where do they disagree?
test <- sent %>% filter(!lang_agree)
# All cases in which either of the clds fails to provide an answer or is markedly off 
# fastText iis probably not always right, but clearly the best 

# -> Translate everything that is not "en" according to fastText and that has more than 1 letter

# Other cross-checks
test <- sent %>% filter(is.na(lang_ft)) # Good!
test <- sent %>% filter(lang_ft != "en" & letters > 1) # Stuff to be translated - Looks good!
test <- sent %>% filter(!(lang_ft != "en" & letters > 1)) # Stuff that doesn't need to be translated - Looks good, except a few short senetnces wrognly calssified as english ("Pozdrawiam.", "Fru talman!", "Formand!")
test <- sent %>% filter(!(lang_ft != "en" & letters > 1) & !lang_agree) # closer look on potentially false negatives - 63843 -- all very short, the overarching majority true English 



# Mark translation need ####
# Based on the above insights

sent$toBeTranslated <- (sent$lang_ft != "en" & sent$letters > 1)
sum(sent$toBeTranslated) # 1423824   



# Cost estimation ####

# Number of characters to be sent to the API
characters <- sum(nchar(sent$sentence[sent$toBeTranslated]))
  
# In Dollars - 20$ for 1 Mio characters
dollars <- (characters/1000000)*20

# In Euro - today: 0.94€ = 1$
euros <- dollars*0.94
round(euros, 2) # 4238.62€


# Export sentence level data for translation ####

# Unique sentence id!
sent$sentence_id <- 1:nrow(sent)

# export
write_rds(sent, "./data/EP_speeches_PLS-SentenceTokenizedForTranslation.rds")

# Language detection results on speech level
#lang.speeches <- read_rds("./data/EP-LangDetection_SpeechLevel.rds")



# Construct translation example ####


# First few sentences of speech 198 
# Explicit example of multi-lingualism
m.sent <- read_rds("./data/EP_speeches_PLS-SentenceTokenizedForTranslation.rds") %>% 
  filter(id == 198) %>% 
  slice_head(n = 22)

m.text <- paste(m.sent$sentence, collapse = " ")
m.text


# Translation - speech level

library(googleLanguageR)
gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key for OPTED money ...

m.text_transl <-
  gl_translate(
    m.text,
    target = "en",
    format = "text",
    source = "", # automatic language detection
    model = "nmt")


# Translation - sentence level

m.sent$translatedText <- NA
m.sent$lang_Google <- NA

# for(i in 1:nrow(m.sent)) {
#   
#   tr <- gl_translate(
#     m.sent$sentence[i],
#     target = "en",
#     format = "text",
#     source = "", # automatic language detection
#     model = "nmt")
#   
#   m.sent$translatedText[i] <- tr$translatedText[1]
#   m.sent$lang_Google[i] <- tr$detectedSourceLanguage[1]
#   
# }

m.text_transl_sent <- paste(m.sent$translatedText, collapse = " ")

# Export results
example <- data.frame(raw_text = m.text,
                      speechLevelTranslation = m.text_transl$translatedText[1],
                      sentenceLevelTranslation = m.text_transl_sent)
write_rds(example, "./testdata/MultiLingualTranslationExample.rds")

# Have a look
library(kableExtra)
example %>% kbl()

# Get info on this speech

speech.info <- read_rds("./data/EP_speeches_PLS-original.rds") %>% 
  filter(id == 198) %>% 
  select(date, speaker, agenda)
  
speech.info

# Date: 2019-04-17
# Speaker: Krišjānis Kariņš
# Agenda: Debate with the Prime Minister of the Republic of Latvia, Krišjānis Kariņš, on the Future of Europe (debate)



