#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@usage segment()
#'@import RForcecom
#'@import dplyr
#'@import stats

segment <- function(access_token, instance_url, object, field){

#inputs <- c(data)
# mytext <- c(inputs[['access_token']], inputs[['instance_url']],
#             '36.0', inputs[['object']], inputs[['field']])
# access_t <- inputs[1]
instance_u <- paste0(instance_url,'/')
api <- '36.0'
# obj <- inputs[3]
# field <- inputs[4]

myquery <- paste0('Select Id, ', field,' FROM ', object)
session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
data1 <- rforcecom.bulkQuery(session, myquery, object)
data1 <- na.omit(data1)

# Data Treatment starts Here
data2 <- subset(data1, select = c(2))
data2 <- slider(data2, 5)
data1 <- cbind(data1, data2) # Derived values are binded to the original data
data1 <- subset(data1, select = c("Id", "dist"))
return(data1)
}
