#'Industry ranking
#'
#'This function ranks a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export ranks

ranks <- function(access_token, instance_url, object, depfield, indfield){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', depfield,', ',indfield,' FROM ', object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  data1 <- ranker(data1)
  data1 <- subset(data1, select = c("Id", "decile"))
  colnames(data1) <- c("strId", "dist")
  return(data1)
}
