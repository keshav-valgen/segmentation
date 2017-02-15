library(RForcecom)
library(dplyr)
library(curl)
library(jsonlite)

segment <- function(data){

inputs <- fromJSON(data, simplifyVector = FALSE)
mytext <- c(inputs[['access_token']], inputs[['instance_url']],
            '36.0', inputs[['object']], inputs[['field']])
access_t <- mytext[1]
instance_u <- paste0(mytext[2],'/')
api <- mytext[3]
obj <- mytext[4]
field <- mytext[5]

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
