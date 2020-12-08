#Query a database via sql script

queryDB <- function(sql, path = TRUE, odbc = "DEFAULT"){
  if (!require("pacman")){install.packages("pacman");p_load(RODBC)}
  sql <- sql(if(!path) {sql} else {readChar(sql, file.size(sql))})
  connection <- odbcConnect(odbc)
  data <- sqlQuery(connection, sql, stringsAsFactors = TRUE)
  close(connection)
  return(data)
}

#tokenize a dataframe

tokenize <- function(df, colname, sentiment.type = "afinn", sentimentremoved = "stopwords.sentimentremoved"){
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,magrittr,tidytext,SnowballC,stringr)
  if(exists(sentimentremoved)) 
    sentimentremoved
  else stopwords.sentimentremoved<- anti_join(stop_words,get_sentiments(sentiment.type)) #give me all stop words that are not related to sentiment
  df <- df %>%
    unnest_tokens_("word",colname) %>%
    mutate(Question = colname) %>%
    anti_join(stopwords.sentimentremoved, by = "word") %>% #give me all words that are not stop words
    filter(!str_detect(word,"[0-9]")) %>%
    filter(!str_detect(word,"[[:punct:]]")) %>%
    mutate(word_stem = wordStem(word))
}


# Help inform and track the script progress for the user
note_that <- function(note, message = TRUE, log = TRUE, refresh.log = FALSE) {
  lapply(note, FUN = function(x) {
    stamp <- Sys.time()
    item <- if(log) {
      if(exists("noteLog") & !refresh.log) {
        rbind(noteLog$log, data.frame(message = x, timestamp = stamp, duration = difftime( stamp,max(noteLog$log$timestamp),)))}
      else {data.frame(message = x, timestamp = stamp, duration = difftime(stamp, stamp))}}
    if(message) message(sprintf("%s - %s", x, stamp))
    noteLog <<- if(log) {list(log = item)}
  })
  noteLog$totalDuration <<- if(log) {difftime(max(noteLog$log$timestamp), min(noteLog$log$timestamp))}
}


# Polarity function for a data frame

eocPolarity <- function(data, field, qWeight, amplify, id ="RECORDID"){ #change question weight if question is inherently positive/negative
  setwd(getwd())
  polReader <- readTabular(mapping=list(content = field, id = id))
  polCorpus <- VCorpus(DataframeSource(data), readerControl = list(reader=polReader))
  polCorpus <- tm_map(polCorpus,removePunctuation)
  polCorpus <- tm_map(polCorpus, stripWhitespace)
  df <- as.data.frame(polCorpus)
  polKey <- read.csv("...//Projects//Surveys//End of Course Survey 2016//Data//Raw Data//PolarityKey.csv")
  polData <- qdap::polarity(df$text, grouping.var = data$RegistrationID,
                            polarity.frame = polKey, constrain = TRUE,
                            negators = qdapDictionaries::negation.words,
                            amplifiers = qdapDictionaries::amplification.words,
                            deamplifiers = qdapDictionaries::deamplification.words,
                            question.weight = qWeight, amplifier.weight = amplify, n.before = 1, 
                            n.after = 1, rm.incomplete = FALSE, digits = 2) #default is two digits and 1 word before & after
  dfPol <- data.frame(polData)
  exportPol <- data.frame(lapply(dfPol, as.character), stringsAsFactors=FALSE) #coerce data frame to exportable format
  exportPol
}

# Remove HTML formatting from Forum Posts in Sakai

removeHTML.forums <- function(df,numcolumn){
  df[[numcolumn]] <- gsub("</p>", "\n",df[[numcolumn]])
  df[[numcolumn]] <- gsub("<[^>]+>", "",df[[numcolumn]])
  df[[numcolumn]] <- gsub("&ldquo;", "'",df[[numcolumn]])
  df[[numcolumn]] <- gsub("&rdquo;", "'", df[[numcolumn]]) #right quote
  df[[numcolumn]] <- gsub("&hellip;", "...", df[[numcolumn]]) #elipses
  df[[numcolumn]] <- gsub("&#39;", "'", df[[numcolumn]]) #apostrophe 
  df[[numcolumn]] <- gsub("&reg;", "'", df[[numcolumn]]) #registered trademark
  df[[numcolumn]] <- gsub("&amp;", "&", df[[numcolumn]]) #ampersand
  df[[numcolumn]] <- gsub("&rsquo;", "'", df[[numcolumn]]) #apostrophe
  df[[numcolumn]] <- gsub("&quot;", "'", df[[numcolumn]]) #quotes
  df[[numcolumn]] <- gsub("&nbsp;", " ", df[[numcolumn]]) #nonbreak space
  df[[numcolumn]] <- gsub("\r\n", "", df[[numcolumn]])
  df[[numcolumn]] <- gsub("\t", "", df[[numcolumn]])
  df[[numcolumn]] <- gsub("&lt;", "<", df[[numcolumn]]) #Less Than
  df[[numcolumn]] <- gsub("&gt;", ">", df[[numcolumn]]) #Greater Than
  df[[numcolumn]] <- gsub("(&e)(acute)", "e", df[[numcolumn]])#accent mark
  df[[numcolumn]] <- gsub("<o:p&gt;</o:p&gt;", "", df[[numcolumn]])#Office Namespace
  df[[numcolumn]] <- gsub("(Last Edited By )(.*)(\\d+:\\d+ PM|\\d+:\\d+ AM)", " ", df[[numcolumn]],perl = T) }


# to query access files
queryAccess <- function(file, type, datasource,newfield) {
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,RODBC)
  channel <- odbcConnectAccess2007(file)
  if(tolower(type) == "table") {
    data <- sqlFetch(channel,datasource)
  }
  else {
    data <-  sqlQuery(channel,datasource)
  }
  close(channel)
  if(is.na(newfield)) {return(data)}
  else {data <- data %>%
    mutate(customfield = newfield)}
  return(data)}

# flatten the returned NSC detail file

simpleNSC.return <- function(path){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,readr,tidyr,lubridate) 
  
  df <- read.csv(path)
  df$Enrollment.Begin <- as.Date(parse_date_time(x = df$Enrollment.Begin, 
                                                 orders = "ymd",
                                                 locale = "eng"))
  df$Enrollment.End <- as.Date(parse_date_time(x = df$Enrollment.End, 
                                               orders = "ymd",
                                               locale = "eng"))
  df$Graduation.Date <- as.Date(parse_date_time(x = df$Graduation.Date, 
                                                orders = "ymd",
                                                locale = "eng"))
  df.flat <- select(df,6,9,10,12,14,15,18) %>%
    drop_na() %>% 
    group_by(Requester.Return.Field,College.Code.Branch,College.Name,X2.year...4.year,Enrollment.Major.1) %>%
    summarise(FirstEnrollment = min(Enrollment.Begin),
              LastEnrollment = max(Enrollment.End)) 
  df.grad <- df %>%
    select(6,9,22,23:25) %>%
    filter(Graduated. == 'Y')
  df.final <- left_join(df.flat,df.grad,by = c("Requester.Return.Field","College.Code.Branch"))
  return(df.final)
}

# A clustering function that utilizes gower distancing (mixed variable types) and PAM clustering (mixed vars)
pamgow <- function(df,IDvarcol, numclust = NA){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(dplyr,magrittr,tidyr,readr,stringr,RODBC,ggplot2,data.table,broom,dummies,cluster,Rtsne)
  
  gower_dist <- daisy(df[,-1],
                      metric = "gower")
  gower_mat <- as.matrix(gower_dist)
  
  # Calculate silhouette width for many k using PAM
  sil_width <- c(NA)
  for(i in 2:10){
    
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i)
    
    sil_width[i] <- pam_fit$silinfo$avg.width
    
  }
  
  # Plot sihouette width (higher is better)
  
  plot(1:10, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:10, sil_width)
  
  # x <- readline("Based on the silhouette width, how many clusters should we compute? ")
  # x <- as.numeric(x)

  
  #describe the clusters
  pam_fit <- pam(gower_dist, diss = TRUE, k = numclust)
  
  pam_results <- df[,-1] %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  #visualize the clusters
  
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  tsne_data <-cbind(tsne_data,df[1])
  
  
  col <- colnames(df[1])
  
  df.cluster <- inner_join(df,tsne_data, by =col)
  
  return(df.cluster)
  }

#Cohen's D function from the effsize package has to have vectors passed. Since we use dataframes with tidyverse, here is a good function
do.cohen.d <- function(df1,df2,column1,column2) {
  if (!require("pacman")) install.packages("pacman")
  p_load(effsize,purrr)
  mydata1 <- as_vector(df1[,column1])
  mydata2 <- as_vector(df2[,column2])
  cohen.d(mydata1,mydata2)
}


stepRegress <- function(df = NA,
                        target = "rating",
                        entry.method = "forward",
                        target.type = "binary") {
  # set.seed(7)
  # if (remove.correlated == TRUE) {
  #   correlationMatrix <- cor(data[, 1:ncol(data)])
  #   highlyCorrelated <-
  #     caret::findCorrelation(correlationMatrix, cutoff = threshold)
  #   data <- data %>%
  #     select(-all_of(highlyCorrelated))
  # }
  # else
  #   NA
  
  frm <- formula(if (target.type == "binary") {
    as.formula(paste("glm(", target, "~ ., data = ", deparse(substitute(df)), ", family = binomial)")) #have to return the name of the dataframe in formula
  }
  else{
    as.formula(paste("lm(", target, "~ ., data = ", deparse(substitute(df)), ")"))
  })
  
  null.trimmed <-
    eval(parse(text = paste(
      "lm(", target, "~ 1, data = ", deparse(substitute(df)), ")"
    )))
  null.trimmed.b <-
    eval(parse(text = paste( 
      "glm(", target, "~ 1, data = ", deparse(substitute(df)), ", family = binomial)" # need to return the results of formula, not the formula itself
    )))
  
  results <- if (entry.method == "forward") {
    if (target.type == "binary") {
      step(null.trimmed.b, scope = frm, direction = entry.method)
    }
    else {
      step(null.trimmed, scope = frm, direction = entry.method)
    }
  }
  else {
    step(frm, frm, direction = entry.method)
  }
  
  return(results)
  
}

