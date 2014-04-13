### function to number tides and insert tide numbers (for whole tidal cycles, flood, and ebb tides)
### in high-frequency dataset.
### Requires HL() function output, and initial dataset
### input arguments:
### "data": full water level dataframe
### "datetime": date/time column from full dataset (used as "time" argument in HL())
### "hl": HL() output

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
