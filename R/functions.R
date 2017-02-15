#'SegmentFunction
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@import stats

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
