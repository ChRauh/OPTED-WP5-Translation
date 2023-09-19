####################################################
# Project:  OPTED WP5 - Translation
# Task:     Tokenize EP speeches into sentences and
#           detect languages
#
# Author:   @ChRauh (18.09.2023)
####################################################


# Packages #####

library(tidyverse)
library(tidytext)
library(spacyr)
spacy_initialize()
library(progress)
library(patchwork)

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




# Sentence tokenization - full corpus ####

# Tokenize with tidytext
start <- Sys.time()
sentences <- speeches %>%
    group_by(id) %>%
    unnest_tokens(output = sentence,
                  input = text,
                  token = "sentences",
                  to_lower = F) %>%
    ungroup()
duration <- Sys.time() - start
duration # 52 secs on Dell!?

# Export
write_rds(sentences, "./data/EP_speeches_PLS-SentenceTokenized.rds")



# Language detection ####
# Circling around errors



mutate(lang_ft = fastText::language_identification(input_obj = sentence,
                                                   pre_trained_language_model_path = file_ftz,
                                                   k = 1,
                                                   th = 0.0,
                                                   threads = 1,
                                                   verbose = F)[[1]],
       lang_cld2 = cld2::detect_language(sentence),
       lang_cld3 = cld3::detect_language(sentence)) %>%


# Tokenization errors
# German dates



# Language detection on speech level
#lang.speeches <- read_rds("./data/EP-LangDetection_SpeechLevel.rds")


