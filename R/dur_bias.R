#' @title Calculates bias in flooding duration estimates
#' @description
#' Seasonality in water levels imparts  bias to flooding duration datasets built from short-term water 
#' level data. \code{dur.bias} calculates and presents these biases in visual and tabular form.
#' 
#' Using data from a nearby, user-defined NOAA station, bias is estimated by comparing the 
#' relationship between flooding duration and elevation in datasets covering two time periods: (1) the time period
#' of water level logger deployment, and (2) the reference time period (some number of years).
#' @details
#' Seasonality in water levels imparts  bias to flooding duration datasets built from short-term water 
#' level data. \code{dur.bias} calculates and presents these biases in visual and tabular form.
#' @usage \code{dur.bias(data, station = 8518750, ref.period = c("20120101", "20121231"), time = "GMT")} 
#' @param data water level dataset. must have a time stamp column named 'datetime' with the first ten digits following the format \code{YYYY-MM-DD}.
#' Different separators are fine, but the placement and number of digits for year, month, and day are critical.
#      date range must be continuous, or the "estimated" curve won't be accurate (datasets with gaps won't work well)
#' @param station name or number of NOAA station to be used for reference data. Ideally, use the station closest to where 
#' your water level data was collected. This is fed to \code{\link{noaa}} so must be compatible with 
#' \code{noaa}'s \code{station} argument.
#' @param ref.period downloads NOAA tide data to set reference flooding-elevation relationship. This is used to 
#' evaluate accuracy of flooding duration estimates determined by water level deployment.
#'      use of whole years is recommended. dates must be entered in format \code{YYYYMMDD}, in the form 
#'      (c(start_date, end_date))
#' @param time time zone to download reference data (\code{GMT} is default; the NOAA website does not have all data 
#' available in all time zones.)
#' @return (1) A summary of root mean square errors is printed, (2) a data frame of elevations, flooding duration estimates, and their associated bias 
#' estimates is saved as output, and (3) two plots are made, graphically showing the relationship between predicted and reference flooding duration curves. 
#' @seealso \code{\link{noaa}}, \code{\link{noaa.stations}}, \code{\link{fld.dur}}
#' @references http://wetlandsandr.wordpress.com/
#' @example
#' # example requires internet connection
#' # 



dur.bias <- function(data, station = 8518750, ref.period = c("20120101", "20121231"), time = "GMT") {
  # download 6-min data for period of logger deployment, and reference period
  start <- paste0(substr(data[1, "datetime"], 1, 4),  substr(data[1, "datetime"], 6, 7), substr(data[1, "datetime"], 9, 10))
  end <- paste0(substr(data[nrow(data), "datetime"], 1, 4),  substr(data[nrow(data), "datetime"], 6, 7), substr(data[nrow(data), "datetime"], 9, 10))
  deploy <- noaa(station = station, begindate = start, enddate = end, interval = "6 minute", time = time)
  ref <- noaa(station = station, begindate = ref.period[1], enddate = ref.period[2], interval = "6 minute", time = time)
  
  dur.data <- data.frame(elevs = seq(-3, 3.5, by = 0.001))
  dur.data$duration.est <- fld.dur(dur.data$elevs, deploy[, 2])
  dur.data$duration.ref <- fld.dur(dur.data$elevs, ref[, 2])
  dur.data$disparity <- dur.data$duration.ref - dur.data$duration.est 
  dur.data$RMSE <- sqrt((dur.data$disparity)^2)
  
  print(summary(dur.data$RMSE))
  invisible(dur.data)
  
  # diagnostic plots: elevation vs flooding, and disparity vs. estimated hydroperiod
  f <- 0.85 # cex.axis, cex.lab size
  g <- 0.5 # point size
  
  filename <- "dur_bias_output.png"
  png(filename, height = 200, width = 100, units = "mm", res = 300)
  par(mar = c(4, 4.5, 0.3, 1))
  par(fig = c(0, 0.53, 0, 1))
  
  plot(y = dur.data$duration.est * 100, x = dur.data$elevs, xlab = "elevation (m; MHW)", 
       ylab = "flooding duration (percent of time)", ylim = c(0, max(dur.data$duration.est * 100, na.rm = T)),
       xlim = c(min(dur.data$elevs[dur.data$duration.est < 0.9995], na.rm = T), max(dur.data$elevs[dur.data$duration.est > 0.0005], na.rm = T)),
       type = "n", yaxt = "n", bty="n", xaxs = "i", yaxs = "i", 
       cex.lab = f, cex.axis = f, tck = 0.3, tcl = 0.15
  )
  points(y = dur.data$duration.ref * 100, x = dur.data$elevs, pch = 19, cex = g) # "truth"
  points(y = dur.data$duration.est * 100, x = dur.data$elevs, pch = 19, cex = g, col = "red") # from deploy period
  text(-1, (1 - dur.data$duration.ref[dur.data$elevs == -1]) * 100 * 0.9,
       "reference values", cex = f)
  text(-1, (1 - dur.data$duration.ref[dur.data$elevs == -1]) * 100 * 0.5,
       "predicted values", col = "red", cex = f)
  abline(h = 0)
  axis(2, las = 1, at = axTicks(2), labels = sprintf("%1.0f%%", axTicks(2)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  
  par(new = T)
  par(fig = c(0.47, 1, 0, 1))
  plot(y = dur.data$RMSE * -100, x = dur.data$duration.est * 100, ylab = "root mean square error (percent of time)", 
       xlab = "estimated flooding duration (percent of time)", ylim = c(min(dur.data$RMSE * -100, na.rm = T), max(dur.data$RMSE * -100, na.rm = T)), 
       type = "n", yaxt = "n", xaxt = "n", bty="n", xaxs = "i", yaxs = "i", 
       cex.lab = f, cex.axis = f, tck = 0.3, tcl = 0.15
       )
  points(y = dur.data$RMSE * -100, x = dur.data$duration.est * 100, pch = 19, cex = g)
  abline(h = min(dur.data$RMSE * -100, na.rm = T))
  abline(v = 0)
  axis(2, las = 1,at = axTicks(2), labels = sprintf("%1.0f%%", axTicks(2)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  axis(1, las = 1,at = axTicks(1), labels = sprintf("%1.0f%%", axTicks(1)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  dev.off()
  
  print(paste0(filename, " saved to ", getwd()))
  
}