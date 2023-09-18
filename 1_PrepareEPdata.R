####################################################
# Project:  OPTED WP5 - Translation
# Task:     Inspect languages in EP corpus
#           to choose best way of auto-translation
#
# Author:   @ChRauh (18.09.2023)
####################################################



# Packages #####

library(tidyverse)
library(tidytext)

# Language detection packages
library(fastText)
library(cld2)
library(cld3)
library(textcat)
library(franc)


# EP speeches data ####

# # Version 15.09.2023 - ParlLawSpeech Dropbox
# # Lukas Hetzer and Jan Schwalbach have given green light to use this version
# speeches <- read_rds("C:/Users/rauh/Dropbox/OPTED datasets/EP/Corpus_speeches_EP.RDS")
# 
# # Create unique row ID
# speeches$id <- 1:nrow(speeches)
# 
# # Store the data locally to have one fixed version
# write_rds(speeches, "./data/EP_speeches_PLS-original.rds", compress =  "gz")

# Reload original speeche collection with unique ids
speeches <- read_rds("./data/EP_speeches_PLS-original.rds")



# # Language detection proof-of-concept exercise ####
# 
# test <- data.frame(text = c("This is an English text. Not much else is to say about this.",
#                             "Das ist ein deutscher Text. Mehr gibt es dazu nicht zu sagen.",
#                             "Il s'agit d'un texte français. Il n'y a plus rien à dire à ce sujet.",
#                             "Ez magyar szöveg. Erről nincs több mondanivaló.",
#                             "Jedná se o český text. Víc k tomu není co říct.",
#                             "Este é un texto galego. Non hai máis que dicir sobre iso.",
#                             "Este es un texto en español. No hay nada más que decir al respecto.",
#                             "Testun Cymraeg yw hwn. Nid oes dim mwy i'w ddweud am hynny.",
#                             "Aquest és un text en català. No hi ha res més a dir sobre això.",
#                             "Ovo je bosanski tekst. Nema više šta da se kaže o tome.",
#                             "Ovo je hrvatski tekst. O tome se nema više što reći.",
#                             "This is a mixed text. Manches darin ist in deutscher Sprache. Otras frases están nuevamente escritas en español. Et le français est également utilisé.",
#                             "Dieser Text enthält unterschiedliche Sprachen, wie ihr seht. Például magyarul mond valamit a beszélő. És hirtelen németül angolul: Now you are surprised!"),
#                    ecpected = c("english",
#                                 "german",
#                                 "french",
#                                 "hungarian",
#                                 "czech",
#                                 "galician",
#                                 "spanish",
#                                 "welsh",
#                                 "catalan",
#                                 "bosnian",
#                                 "croatian",
#                                 "mixed",
#                                 "mixed"))
# 
# # Textcat (n-gram based)
# test$lang_textcat <- textcat(test$text)
# 
# # Google compact language detection
# # In prior projects cld2 proofed more accurate for short texts ...
# test$lang_cld2 <- cld2::detect_language(test$text)
# test$lang_cld3 <- cld3::detect_language(test$text)
# 
# 
# # Franc (not vectorised)
# test$lang_franc <- NA 
# # test$prob_franc <- NA 
# for (i in 1:nrow(test)){
#   test$lang_franc[i] <- franc(test$text[i])
#   # test$prob_franc[i] <- franc_all(test$text[i])[1,2]
# }
# test$lang_franc[test$lang_franc == "sco"] <- NA # undefined language code
# 
# 
# # fastText
# # Using the smaller pre-trained data set: https://fasttext.cc/docs/en/language-identification.html
# file_ftz = system.file("language_identification/lid.176.ftz", package = "fastText")
# 
# # Extract most likely language
# # Firs element returns iso-codes, second elemnt would return probabilities
# test$lang_fastText <- fastText::language_identification(input_obj = test$text,
#                                                         pre_trained_language_model_path = file_ftz,
#                                                         k = 1,
#                                                         th = 0.0,
#                                                         threads = 1,
#                                                         verbose = TRUE)[[1]]
# 
# test$prob_fastText <- fastText::language_identification(input_obj = test$text,
#                                                         pre_trained_language_model_path = file_ftz,
#                                                         k = 1,
#                                                         th = 0.0,
#                                                         threads = 1,
#                                                         verbose = TRUE)[[2]]
# 
# 
# # Run test translation and language detection with google api here!
# 
# 
# # Auto-translation with google
# library(googleLanguageR) 
# gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key for OPTED money ...
# 
# nchar(paste(test$text, collapse = " ")) # 951
# 
# # test_translation <-
# #   gl_translate(
# #     test$text,
# #     target = "en",
# #     format = "text",
# #     source = "", # automatic language detection
# #     model = "nmt")
# 
# 
# translation <- test_translation %>% 
#   rename(lang_GoogleAPI = detectedSourceLanguage) %>% 
#   select(lang_GoogleAPI, translatedText)
# 
# 
# test <-  cbind(test, translation)
# 
# write_rds(test, "./testdata/ProofOfConcept.rds")

test <- read_rds("./testdata/ProofOfConcept.rds")




# Language detection in the EP - speech level ####

# start <- Sys.time()
# speeches$lang_cld2 <- cld2::detect_language(speeches$text)
# speeches$lang_cld3 <- cld3::detect_language(speeches$text)
# speeches$lang_fastText <- fastText::language_identification(input_obj = speeches$text,
#                                                         pre_trained_language_model_path = file_ftz,
#                                                         k = 1,
#                                                         th = 0.0,
#                                                         threads = 1,
#                                                         verbose = TRUE)[[1]]
# speeches$prob_fastText <- fastText::language_identification(input_obj = speeches$text,
#                                                         pre_trained_language_model_path = file_ftz,
#                                                         k = 1,
#                                                         th = 0.0,
#                                                         threads = 1,
#                                                         verbose = TRUE)[[2]]
# end <- Sys.time()
# end-start # 5 mins on Dell only
# 
# 
# # Export lang detection results
# write_rds(speeches %>% select(c(id, lang_cld2, lang_cld3, lang_fastText, prob_fastText)), 
#                               "./data/EP-LangDetection_SpeechLevel.rds")


# Re-load for analytic purposes (note that lots of columns have been dropped above)
speeches <- read_rds("./data/EP-LangDetection_SpeechLevel.rds")




# Assess agreement of language detection tools ####
speeches$lang_agreement <- (speeches$lang_cld2 == speeches$lang_cld3 & speeches$lang_fastText == speeches$lang_cld2 &  speeches$lang_fastText == speeches$lang_cld3)
speeches$lang_agreement[is.na(speeches$lang_agreement)] <- F
sum(is.na(speeches$lang_agreement))

print(paste0("Share of speeches on which the three detection tools agree: ", round((sum(speeches$lang_agreement)/nrow(speeches))*100, 2), "%"))
print(paste0("Number of speeches on which they disagree: ", sum(!speeches$lang_agreement)))




# Plot detected language distributions ####

# what did they find?
unique(speeches$lang_fastText)
unique(speeches$lang_cld2)
unique(speeches$lang_cld3)

# Counts by detection method
languages <- speeches %>% select(starts_with("lang")) %>% 
  select(-lang_agreement) %>% 
  pivot_longer(cols = 1:3) %>% 
  rename(detector = name,
         language = value) %>% 
  mutate(detector = str_remove(detector, "lang_")) %>% 
  mutate(language = str_remove(language, "-Latn")) %>% 
  group_by(detector, language) %>% 
  summarise(count = n())

# Get the names right (as far as it can get)
isocodes <- ISOcodes::ISO_639_2 %>% 
  select(Alpha_2, Name) %>% 
  rename(language = Alpha_2,
         langname = Name) %>% 
  filter(!is.na(language))

languages <- languages %>% 
  left_join(isocodes, by = c("language"))

languages$langname[is.na(languages$langname)] <- languages$language[is.na(languages$langname)]

# Order by average prevalence  
lang.order <- languages %>% 
  group_by(langname) %>% 
  summarise(mean = mean(count)) %>% 
  arrange(desc(mean))
languages$langname2 <- factor(languages$langname, levels = lang.order$langname)


# Overall prevalence
lang.order %>% 
  mutate(share = mean/sum(lang.order$mean)) %>% 
  ggplot(aes(x=share, y= fct_rev(factor(langname, levels = lang.order$langname))))+
  geom_col(width = .7)+
  scale_x_continuous(expand = expansion(mult = c(0, .1)), labels = scales::label_percent())+
  labs(x = "Share of overall speeches in corpus",
       y = "",
       title = "Estimated language prevalence in ParlLawSpeech EP speeches corpus",
       subtitle = "Speech-level language detection, averaged across the three detection methods")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold"))

# Prevalence by detector type
ggplot(languages, aes(x=log(count), y = fct_rev(langname2), color = detector, fill = detector))+
  geom_col(position = position_dodge2(preserve = "single", width = .7))+
  scale_x_continuous(expand = expansion(mult = c(0, .1)))+
  labs(fill = "Language detection method: ",
       color ="Language detection method: ",
       x = "EP speech count\n(logged for visual purposes)",
       y= "",
       title = "Language detection in the ParlLawSpeech EP speeches corpus",
       subtitle = "Detection on the level of the full speech text")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold"))


# Clearly English speeches ####

# All three detection methods should agree
speeches$clearly_english <- (speeches$lang_cld2 == "en" & speeches$lang_cld3 == "en" & speeches$lang_fastText == "en")
speeches$clearly_english[is.na(speeches$clearly_english)] <- F # Happens when one of the detectors has returned an NA, must thus be FALSE
sum(speeches$clearly_english)

# Add full speech intro (again, dropped above )
df <- read_rds("./data/EP_speeches_PLS-original.rds") %>% 
  select(c(id, date ,text))
speeches <- speeches %>% 
  left_join(df, by = "id")
rm(df)
gc()

# Share of clearly english speeches by month
en.share <- speeches %>% 
  mutate(month = as.character(date) %>% str_remove("-[0-9]{1,2}$")) %>% 
  group_by(month) %>% 
  summarise(en.count = sum(clearly_english),
            count = n())  %>% 
  ungroup() %>% 
  mutate(en.share = en.count/count)

# Plotting parameters
breaks <- en.share %>% 
  select(month) %>% 
  filter(str_detect(month, "-01$")) %>% 
  pull(month)
labels <- str_remove_all(breaks, "-01")
  
# Plot
ggplot(en.share, aes(x = month, y = en.share, group = 1))+
  geom_line(color = "#001489", size = 1.2)+
  scale_y_continuous(labels = scales::label_percent())+
  scale_x_discrete(breaks = breaks, labels = labels)+
  labs(title = "Share of English speeches in raw PLS EP speeches corpus",
       subtitle = "cld2, cld3, and fastText detection on speech-level agree on \'en\'",
       y = "Share of \'clearly English\' speeches\n",
       x = "month")+
  theme_bw()+
  theme(axis.text = element_text(color = "black"))




# Certainty of the fastText detection #####
# Assumption: the more multilingualism in a speech, the less secure the algorithm (on speech level)

speeches$clearly_english2 <- ifelse(speeches$clearly_english, "English detected by all three algorithms", "At least one algorithm detected non-English language")


ggplot(speeches, aes(x=prob_fastText, ..scaled..))+
  # geom_histogram(width = .7, bins = 240, color = NA, fill = "blue")+
  geom_density(color = NA, fill = "blue")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,.1))+
  facet_wrap(.~clearly_english2, nrow = 2)+
  labs(y = "Densityn",
       x = "\nClassification probability\nfor most-likely language of speech",
       title = "Classification certainty of the fastText language detection in EP speech corpus")+
  theme_bw()

round(sum(speeches$prob_fastText>=.9)/nrow(speeches), 2)*100



# # Language detection on the sentence level ####
# # Random samples of 50.000 speeches (~10%)
# # One for agreement on 'en' one without such agreement
# 
# # Speech detection runs for ~ 1h
# 
# file_ftz = system.file("language_identification/lid.176.ftz", package = "fastText")
# 
# sample.en <- speeches %>% 
#   filter(clearly_english) %>% 
#   sample_n(50000, replace = F) %>% 
#   group_by(id) %>% 
#   unnest_tokens(output = sentence,
#                 input = text,
#                 token = "sentences",
#                 to_lower = F) %>% 
#   filter(str_detect(sentence, "[a-z]")) %>% # Only sentence with at least one lowercase letter
#   mutate(lang = fastText::language_identification(input_obj = sentence,
#                                                   pre_trained_language_model_path = file_ftz,
#                                                   k = 1,
#                                                   th = 0.0,
#                                                   threads = 1,
#                                                   verbose = F)[[1]]) %>% 
#   group_by(id) %>% # speech level
#   summarise(detected.languages = length(unique(lang))) %>% 
#   mutate(speech.en = "Speech level detection: English")
# 
# 
# sample.fo <- speeches %>% 
#   filter(!clearly_english) %>% 
#   sample_n(50000, replace = F) %>% 
#   group_by(id) %>% 
#   unnest_tokens(output = sentence,
#                 input = text,
#                 token = "sentences",
#                 to_lower = F) %>% 
#   filter(str_detect(sentence, "[a-z]")) %>% # Only sentence with at least one lowercase letter
#   mutate(lang = fastText::language_identification(input_obj = sentence,
#                                                   pre_trained_language_model_path = file_ftz,
#                                                   k = 1,
#                                                   th = 0.0,
#                                                   threads = 1,
#                                                   verbose = F)[[1]]) %>% 
#   group_by(id) %>% # speech level
#   summarise(detected.languages = length(unique(lang))) %>% 
#   mutate(speech.en = "Speech level detection: Non-English")
# 
# sample <- rbind(sample.en, sample.fo)
# 
# write_rds(sample, "./data/LanguageCountSample.rds")

sample <- read_rds("./data/LanguageCountSample.rds")

ggplot(sample, aes(x= detected.languages))+
  geom_histogram(width = .7, bins = max(sample$detected.languages), color = NA, fill = "blue")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(1,max(sample$detected.languages), 1))+
  facet_wrap(.~speech.en, nrow = 2)+
  labs(y = "Frequency (number of speeches)\n",
       x = "\nNumber of different languages detected in speech",
       title = "Multilingualism in the EP speech corpus",
       subtitle = paste0("Random sample of ", nrow(sample), " speeches, fastText language detection on sentence level"))+
  theme_bw()


ggplot(sample, aes(x = detected.languages, y = speech.en))+
  geom_violin(fill = "blue", color = NA)+
  labs(y = "Frequency (number of speeches)\n",
       x = "\nNumber of different languages detected in speech",
       title = "Multilingualism in the EP speech corpus",
       subtitle = paste0("Random sample of ", nrow(sample), " speeches, fastText language detection on sentence level"))+
  theme_bw()



round(sum(sample$detected.languages>1)/nrow(sample),2)*100

round(sum(sample$detected.languages[sample$speech.en == "Speech level detection: English"]>1)/nrow(sample[sample$speech.en == "Speech level detection: English", ]),2)*100

max(sample$detected.languages)
max(sample$detected.languages[sample$speech.en == "Speech level detection: English"])



# SOTEU extracts


soteu <- "This was wrong, not just for the climate, but also for our public finances, and our independence. And we are still paying for this today.
Only a few visionaries understood that the real problem was fossil fuels themselves, not just their price. Among them were our Danish friends.
When the oil crisis hit, Denmark started to invest heavily into harnessing the power of the wind. 
They laid the foundations for its global leadership in the sector and created tens of thousands of new jobs. 
This is the way to go! Not just a quick fix, but a change of paradigm, a leap into the future. 
STAYING THE COURSE AND PREPARING FOR THE FUTURE Mesdames et Messieurs les Députés, La bonne nouvelle est que cette transformation nécessaire a commencé.
Elle a lieu en mer du Nord et en mer Baltique, où nos États membres ont massivement investi dans l'éolien en mer. 
Elle a lieu en Sicile, où la plus grande usine solaire d'Europe produira bientôt la toute dernière génération de panneaux solaires. 
Et elle a lieu dans le nord de l'Allemagne, où les trains régionaux roulent désormais à l'hydrogène vert. L'hydrogène peut changer la donne pour l'Europe.
L'Union Européenne achètera dix avions amphibies légers et trois hélicoptères supplémentaires pour compléter notre flotte.
Voilà la solidarité européenne en action Honourable Members, The last years have shown how much Europe can achieve when it is united. 
After an unprecedented pandemic, our economic output overtook pre-crisis levels in record time. 
We went from having no vaccine to securing over 4 billion doses for Europeans and for the world.
And in record time, we came up with SURE – so that people could stay in their jobs even if their companies had run out of work. 
We were in the deepest recession since World War 2. We achieved the fastest recovery since the post-war boom.
And we will revise the Late Payment Directive – because it is simply not fair that 1 in 4 bankruptcies are due to invoices not being paid on time.
For millions of family businesses, this will be a lifeline in troubled waters. 
Der Mangel an Personal ist eine weitere Herausforderung für Europas Unternehmen. Die Zahl der Arbeitslosen ist so niedrig wie nie zuvor. 
Das ist gut! Aber gleichzeitig liegt die Zahl der offenen Stellen auf Rekordniveau. Ob Lastwagenfahrer, Kellnern oder Flughafenpersonal. 
Ob auch Krankenpfleger, Ingenieurinnen oder IT-Technikerinnen. Von Ungelernt bis Universitätsabschluß, Europa braucht sie alle! 
Wir müssen daher viel stärker in die Aus- und Weiterbildung investieren. 
Mit gleichgesinnten Partnern können wir auch außerhalb unserer Grenzen Arbeitsstandards und Umweltstandards sichern. 
Wir müssen vor allem unsere Beziehungen zu diesen Partnern und zu wichtigen Wachstumsregionen erneuern.
Ich werde daher die Abkommen mit Chile, Mexiko und Neuseeland zur Ratifizierung vorlegen. 
Und wir treiben die Verhandlungen mit bedeutenden Partnern wie Australien und Indien voran. 
But securing supplies is only a first step. The processing of these metals is just as critical. 
Today, China controls the global processing industry. Almost 90 % of rare earths and 60 % of lithium are processed in China. 
We will identify strategic projects all along the supply chain, from extraction to refining, from processing to recycling. 
And we will build up strategic reserves where supply is at risk."


# # Auto-translation with google
# library(googleLanguageR)
# gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key for OPTED money ...
# 
# nchar(soteu) # 3356

# soteu_translation <-
#   gl_translate(
#     soteu,
#     target = "en",
#     format = "text",
#     model = "nmt")

# write_rds(soteu_translation, "./testdata/SOTEU_translation.rds")



# # SOTEU extracts - sentence level
# 
# 
# soteu <- "This was wrong, not just for the climate, but also for our public finances, and our independence. And we are still paying for this today.
# Only a few visionaries understood that the real problem was fossil fuels themselves, not just their price. Among them were our Danish friends.
# When the oil crisis hit, Denmark started to invest heavily into harnessing the power of the wind. 
# They laid the foundations for its global leadership in the sector and created tens of thousands of new jobs. 
# This is the way to go! Not just a quick fix, but a change of paradigm, a leap into the future. 
# STAYING THE COURSE AND PREPARING FOR THE FUTURE Mesdames et Messieurs les Députés, La bonne nouvelle est que cette transformation nécessaire a commencé.
# Elle a lieu en mer du Nord et en mer Baltique, où nos États membres ont massivement investi dans l'éolien en mer. 
# Elle a lieu en Sicile, où la plus grande usine solaire d'Europe produira bientôt la toute dernière génération de panneaux solaires. 
# Et elle a lieu dans le nord de l'Allemagne, où les trains régionaux roulent désormais à l'hydrogène vert. L'hydrogène peut changer la donne pour l'Europe.
# L'Union Européenne achètera dix avions amphibies légers et trois hélicoptères supplémentaires pour compléter notre flotte.
# Voilà la solidarité européenne en action Honourable Members, The last years have shown how much Europe can achieve when it is united. 
# After an unprecedented pandemic, our economic output overtook pre-crisis levels in record time. 
# We went from having no vaccine to securing over 4 billion doses for Europeans and for the world.
# And in record time, we came up with SURE – so that people could stay in their jobs even if their companies had run out of work. 
# We were in the deepest recession since World War 2. We achieved the fastest recovery since the post-war boom.
# And we will revise the Late Payment Directive – because it is simply not fair that 1 in 4 bankruptcies are due to invoices not being paid on time.
# For millions of family businesses, this will be a lifeline in troubled waters. 
# Der Mangel an Personal ist eine weitere Herausforderung für Europas Unternehmen. Die Zahl der Arbeitslosen ist so niedrig wie nie zuvor. 
# Das ist gut! Aber gleichzeitig liegt die Zahl der offenen Stellen auf Rekordniveau. Ob Lastwagenfahrer, Kellnern oder Flughafenpersonal. 
# Ob auch Krankenpfleger, Ingenieurinnen oder IT-Technikerinnen. Von Ungelernt bis Universitätsabschluß, Europa braucht sie alle! 
# Wir müssen daher viel stärker in die Aus- und Weiterbildung investieren. 
# Mit gleichgesinnten Partnern können wir auch außerhalb unserer Grenzen Arbeitsstandards und Umweltstandards sichern. 
# Wir müssen vor allem unsere Beziehungen zu diesen Partnern und zu wichtigen Wachstumsregionen erneuern.
# Ich werde daher die Abkommen mit Chile, Mexiko und Neuseeland zur Ratifizierung vorlegen. 
# Und wir treiben die Verhandlungen mit bedeutenden Partnern wie Australien und Indien voran. 
# But securing supplies is only a first step. The processing of these metals is just as critical. 
# Today, China controls the global processing industry. Almost 90 % of rare earths and 60 % of lithium are processed in China. 
# We will identify strategic projects all along the supply chain, from extraction to refining, from processing to recycling. 
# And we will build up strategic reserves where supply is at risk."
# 
# 
# soteu_sentences <- soteu %>% 
#   as.data.frame() %>% 
#   rename(text = 1) %>% 
#   mutate(id = 1) %>% 
#   group_by(id) %>%
#   unnest_tokens(output = sentence,
#                 input = text,
#                 token = "sentences",
#                 to_lower = F) %>%
#   mutate(lang = fastText::language_identification(input_obj = sentence,
#                                                   pre_trained_language_model_path = file_ftz,
#                                                   k = 1,
#                                                   th = 0.0,
#                                                   threads = 1,
#                                                   verbose = F)[[1]])
# 
# # Auto-translation with google
# library(googleLanguageR)
# gl_auth("./private/opted-wp5-translation-4abef9cc0428.json") # Key for OPTED money ...
# 
# soteu_translation <-
#   gl_translate(
#     soteu_sentences$sentence,
#     target = "en",
#     format = "text",
#     model = "nmt")
# 
# soteu_translation$detectedLanguageFastText <- soteu_sentences$lang
# 
# write_rds(soteu_translation, "./testdata/SOTEU_translation_sentenceLevel.rds")


