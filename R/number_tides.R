#' @title Numbers tidal cycles, flood tides, and ebb tides
#' @description Numbers tidal cycles, flood tides, and ebb tides in a set of 
#' water level data.
#' 
#' @details Numbers tidal cycles, flood tides, and ebb tides in a set of 
#' water level data.
#' 
#' @param data dataframe to modify (containing water levels, time stamps)
#' @param datetime date/time column from full dataset (used as 'time' argument 
#' in call to \code{HL})
#' @param hl output from \code{HL}
#' 
#' @return the dataframe noted in \code{data}, with additional columns 
#' assigning a number to each tidal cycle, ebb tide, and flood tide.
#' 
#' @seealso \code{\link{HL}}
#' 
#' @examples
#' # build high-low dataset
#' HL.NL <- HL(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
#' # number tides in original
#' nos <- number.tides(data = NL_6min_2013, datetime = NL_6min_2013[,1], HL.NL)
#' head(nos)
#' @export

number.tides <- function(data, datetime, hl) {
  
  data$ht.ind <- ifelse(datetime %in% hl$time[hl$tide == "H"], 1, 0)
  data$lt.ind <- ifelse(datetime %in% hl$time[hl$tide == "L"], 1, 0)
  
  data$tide.no <- cumsum(data$lt.ind)  # number tidal cycles
  
  # number the flood and ebb tides (include numbers *only* on flood and ebb tides)
  data$ht.temp <- cumsum(data$ht.ind)
  data$index <- c(1:nrow(data))
  
  
  if(data$index[data$ht.temp == "1"][1] - data$index[data$tide.no == "1"][1] > 0){        # positive value: low tide comes first
    data$ebb.fld <- ifelse(data$ht.temp == data$tide.no, 1, 0)                            # 1 indicates ebb tide, 0 indicates flood tide
  } else { data$ebb.fld <- ifelse(data$ht.temp > data$tide.no, 1, 0)   
  }
  
  data$fld.no <- ifelse(data$ebb.fld == 0, data$tide.no, NA)
  data$ebb.no <- ifelse(data$ebb.fld == 1, data$tide.no, NA)
  
  drop <- c("ht.temp", "index")
  data <- data[, !(names(data) %in% drop)]
  
  invisible(data)
}
