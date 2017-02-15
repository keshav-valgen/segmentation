library(RForcecom)
library(dplyr)
library(curl)
library(jsonlite)

segment <- function(data){

inputs <- c(data)
# mytext <- c(inputs[['access_token']], inputs[['instance_url']],
#             '36.0', inputs[['object']], inputs[['field']])
access_t <- inputs[1]
instance_u <- paste0(inputs[2],'/')
api <- '36.0'
obj <- inputs[3]
field <- inputs[4]

myquery <- paste0('Select Id, ', field,' FROM ', obj)
data1 <- myqueries(access_t, instance_u, api, myquery, obj)
data1 <- na.omit(data1)

# Data Treatment starts Here
data2 <- subset(data1, select = c(2))
data2 <- slider(data2, 5)
data1 <- cbind(data1, data2) # Derived values are binded to the original data
data1 <- subset(data1, select = c("Id", "dist"))
return(data1)
}
