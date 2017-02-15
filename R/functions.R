#'SegmentFunction
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export slider
#'@export data_clean


slider <- function(var, iter) # 2 inputs; The numeric variable to convert and Number of cuts
{
  fdata <- var
  fdata$dist <- "NA"

  # For continuous numeric variables, bsed on the specified split, percentiles are used to split the data into equal parts.
  if(iter == 2){

    cutoff <- quantile(fdata[[1]], 1/2)
    fdata$dist[fdata[1] <= cutoff] <- paste0("0 to ",cutoff )
    fdata$dist[fdata[1] > cutoff & fdata[1] <= max(var) ] <- paste0(cutoff," to ", max(var))
    return(fdata)
  }else if(iter == 3){

    cutoff <- quantile(fdata[[1]], c(1/3, 2*(1/3)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= max(var) ] <- paste0(cutoff[2]+1," to ", max(var))
    return(fdata)
  } else if(iter == 4){

    cutoff <- quantile(fdata[[1]], c(1/4, 2*(1/4), 3*(1/4)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(cutoff[2]+1," to ", (cutoff[3]))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= max(var) ] <- paste0(cutoff[3]+1," to ", max(var))
    return(fdata)
  } else if(iter == 5){

    cutoff <- quantile(fdata[[1]], c(1/5, 2*(1/5), 3*(1/5), 4*(1/5)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(cutoff[2]+1," to ", (cutoff[3]))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= cutoff[4] ] <- paste0(cutoff[3]+1," to ", (cutoff[4]))
    fdata$dist[fdata[1] > cutoff[4] & fdata[1] <= max(var) ] <- paste0(cutoff[4]+1," to ", max(var))
    return(fdata)
    # Upto 5 cuts can be done

  }
}

data_clean <- function(var){

  if(is.numeric(var) == TRUE){

    if(length(unique(var)) == 2 ) # If numeric data is only 1 or 0, we convert them directly to discreet variable
    {
      var<- as.factor(var)
      return(var)

    }else{
      # If Continuous, we cut the data to 2 levels using percentile
      fdata <- data.frame(var)
      fdata$dist <- "NA"
      cutoff <- quantile(fdata[[1]], 1/2)
      fdata$dist[fdata[1] <= cutoff] <- paste0("0 to ",cutoff )
      fdata$dist[fdata[1] > cutoff & fdata[1] <= max(var) ] <- paste0(cutoff," to ", max(var))
      fdata[1] <- NULL
      var <- fdata$dist
      return(as.factor(var))
    }
  }else{
    # If categorical, we reduce the number of levels to 2
    cat_name <- names(table(var)[which.max(table(var))])
    var <- as.character(var)
    var[var != cat_name] <- "OTHER"
    return(as.factor(var))
  }
}







