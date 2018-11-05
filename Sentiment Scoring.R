#                                                                               
#                          2018 EOC Script                                      
#                                                                               
# Purpose: Create sentiment scores and term frequency for 2018                     
#          EOC survey responses                                                 
# Created by: Geoff Koch                                                        
# Created on: 4/16/2018                                                         
# Validated by:                                                    
# Validated on:                                                      
#################################################################################

source("INTERNALPATH/Functions.R")
note_that("begin_SCRIPT")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,magrittr,tidyr,tidytext,tm,readr,stringr,RODBC,topicmodels,ggplot2,qdap,dbplyr,data.table,sentimentr)

note_that("begin_loadingData")
eocData <- queryDB("INTERNALPATH/EOCandNoEOCData.sql",
                   odbc = "APUS_DW", cache = "eocData") %>%
  rename(CourseNumber = LatestCrseNbr) %>%
  mutate(sesnstdt = lubridate::as_date(sesnstdt))

OER.data <- fread("INTERNALPATH/OERCourseList.csv")
OER.data <- OER.data %>%
  mutate(`OER Implementation Date` = if_else(`OER Implementation Date` == "pre-2017","12/31/2016",`OER Implementation Date`)) %>%
  mutate(`OER Implementation Date` = lubridate::parse_date_time(orders = "mdy",`OER Implementation Date`)) %>%
  rename(CourseNumber = `Course Number`,
         OERDate = `OER Implementation Date`) %>%
  mutate(CourseNumber = trimws(CourseNumber))

eocData <- eocData %>%
  left_join(OER.data, by = "CourseNumber") %>%
  mutate(isOER = if_else(sesnstdt > OERDate ,1,0)) %>%
  mutate(isOER = if_else(is.na(isOER),0,isOER))


eoc.comments <- eocData %>%
  filter(hasCompletedEOC == 1) %>%
  filter(!is.na(HowDidExceedExpectations) | !is.na(HowDidExpectationsDiffer) | !is.na(CommentsOnCourse) | !is.na(AdditionalCommentsaboutCourse)) %>%
  mutate(AdditionalComments = paste0(AdditionalCommentsaboutCourse," ",CommentsOnCourse)) %>%
  select(regid,HowDidExceedExpectations,HowDidExpectationsDiffer,AdditionalComments) %>%
  mutate(HowDidExceedExpectations = as.character(HowDidExceedExpectations),
         HowDidExpectationsDiffer = as.character(HowDidExpectationsDiffer)) %>%
  mutate(HowDidExceedExpectations = str_replace(HowDidExceedExpectations,"Leave blank if N/A",""), #replacing Leave blank if N/A with blanks
         HowDidExpectationsDiffer = str_replace(HowDidExpectationsDiffer,"Leave blank if N/A",""),
         AdditionalComments = str_replace(AdditionalComments,"Leave blank if N/A Leave blank if N/A",""),
         AdditionalComments = str_replace(AdditionalComments,"Leave blank if N/A",""))

eoc.library <- eocData %>%
  filter(hasCompletedEOC == 1 & `LikeToProvideLibraryFeedback?` == 1) %>%
  mutate(AdditionalLibraryComments = as.character(AdditionalLibraryComments)) %>%
  mutate(AdditionalLibraryComments = str_replace(AdditionalLibraryComments,"Leave blank if N/A",""))

note_that("end_loadingData")

note_that("begin_SentimentProcessing")
sentiment.eoc <- rbind(sentiment_by(eoc.comments$AdditionalComments, by =eoc.comments$regid) %>%
                      filter(word_count > 0) %>%
                      mutate(Question = "AdditionalComments"),
                    sentiment_by(eoc.comments$HowDidExceedExpectations, by =eoc.comments$regid) %>%
                      filter(word_count > 0) %>%
                      mutate(Question = "HowDidExceedExpectations"),
                    sentiment_by(eoc.comments$HowDidExpectationsDiffer, by =eoc.comments$regid) %>%
                      filter(word_count > 0) %>%
                      mutate(Question = "HowDidExpectationsDiffer"))

sentiment.library <- sentiment_by(eoc.library$AdditionalLibraryComments, by =eoc.library$regid) %>%
                      filter(word_count > 0)

note_that("end_SentimentProcessing")
  
note_that("begin_Tokenizing")

eoc.tokenized <- rbind(tokenize(eoc.comments,"AdditionalComments") %>% select(regid,word,Question,word_stem),
                       tokenize(eoc.comments,"HowDidExpectationsDiffer") %>% select(regid,word,Question,word_stem),
                       tokenize(eoc.comments,"HowDidExceedExpectations") %>% select(regid,word,Question,word_stem))

eoc.librarytoken <- tokenize(eoc.library,"AdditionalLibraryComments") %>% select(regid,word,Question,word_stem)

#get associated sentiment for each word used in previous sentiment scoring
eoc.tokenized <- left_join(eoc.tokenized,lexicon::hash_sentiment_jockers_rinker %>% rename(word = x, sentiment.score = y), by ="word")
eoc.librarytoken <- left_join(eoc.librarytoken,lexicon::hash_sentiment_jockers_rinker %>% rename(word = x, sentiment.score = y), by ="word")

#getting emotions for words - NOT USING
#eoc.tokenized <- left_join(eoc.tokenized, lexicon::nrc_emotions %>% rename(word = term), by = "word")
#eoc.librarytoken <- left_join(eoc.librarytoken, lexicon::nrc_emotions %>% rename(word = term), by = "word")

note_that("end_Tokenizing")

note_that("begin_Writing")
eocData.final <- left_join(eocData, eoc.tokenized, by ="regid")
eocData.final <- left_join(eocData.final,sentiment.eoc, by = c("regid","Question"))

eoc.libraryfinal <- left_join(eoc.library, eoc.librarytoken, by ="regid")
eoc.libraryfinal <- left_join(eoc.libraryfinal, sentiment.library, by = c("regid")) %>%
  select(regid,starts_with("Library"),AdditionalLibraryComments,`DidYouUseLibrary?`,24:58)

fwrite(eocData.final, "INTERNALPATH/EOCSurvey2018.csv" )
fwrite(eoc.libraryfinal, "INTERNALPATH/EOCLibrarySurvey2018.csv")
note_that("end_Writing")

note_that("end_SCRIPT")




