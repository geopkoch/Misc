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
