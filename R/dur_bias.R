#' @title Calculates bias in flooding duration estimates due to logger deployment time
#' 
#' @description Seasonality in water levels imparts  bias to flooding duration 
#' datasets built from short-term water level data. \code{dur.bias} calculates 
#' and presents these biases in visual and tabular form.
#' 
#' Using data from a nearby, user-defined NOAA station, bias is estimated by comparing the 
#' relationship between flooding duration and elevation in datasets covering two time periods: 
#' (1) the time periodof water level logger deployment, and (2) the reference time period 
#' (some number of years).
#' 
#' @details Seasonality in water levels imparts  bias to flooding duration datasets 
#' built from short-term water level data. \code{dur.bias} calculates and presents 
#' these biases in visual and tabular form.
#' 
#' @usage dur.bias(data, station = 8518750, ref.period = c("20120101", "20121231"), time = "GMT")
#' 
#' @param data water level dataset. must have a time stamp column named 'datetime' with the first ten 
#' digits following the format \code{YYYY-MM-DD}.Different separators are fine, but the 
#' placement and number of digits for year, month, and day are critical. Date range 
#' must be continuous, or the "estimated" curve won't be accurate (datasets with gaps won't 
#' work well)
#' 
#' @param dateCol index number of the column in `data` containing dates. Must be a date/POSIX datatype. 
#' 
#' @param station name or number of NOAA station to be used for reference data. Ideally, 
#' use the station closest to where your water level data was collected. This is fed 
#' to \code{\link{noaa}} so must be compatible with \code{noaa}'s \code{station} argument.
#' 
#' @param ref.period downloads NOAA tide data to set reference flooding-elevation relationship. This 
#' is used to evaluate accuracy of flooding duration estimates determined by water level 
#' deployment. Use of whole years is recommended. dates must be entered in format 
#' \code{YYYYMMDD}, in the form \code{(c(start_date, end_date))}.
#' 
#' @param time time zone to download reference data (\code{GMT} is default; the NOAA website 
#' does not have all data available in all time zones.)
#' 
#' @return (1) A summary of root mean square errors is printed, 
#' (2) a data frame of elevations, flooding duration estimates, and their associated bias 
#' estimates is saved as output, and (3) two plots are made, graphically showing the 
#' relationship between predicted and reference flooding duration curves. 
#' 
#' @seealso \code{\link{noaa}}, \code{\link{noaa.stations}}, \code{\link{fld.dur}}
#' 
#' 
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics par
#' @importFrom graphics text
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics axTicks
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' 
#' @examples 
#' \dontrun{
#' NL_extract <- NL_6min_2013[NL_6min_2013[, 1] < "2013-02-01", ]
#' dur.bias(data = NL_extract, station = 8518750, ref.period = c("20130101", "20131231"))
#' }
#' @export


dur.bias <- function(data, 
                     dateCol = 1,
                     station = 8518750, ref.period = c("20120101", "20121231"), 
                     time = "GMT") { # nocov start
  # download 6-min data for period of logger deployment, and reference period
  timeCol <- dateCol # datetime column in data
  
  start <- as.character(format(data[1, timeCol], format = "%Y%m%d"))
  end   <- as.character(format(data[nrow(data), timeCol], format = "%Y%m%d"))
  
  # start <- paste0(substr(data[1, timeCol], 1, 4),  substr(data[1, timeCol], 6, 7), 
  #                 substr(data[1, timeCol], 9, 10))
  # end <- paste0(substr(data[nrow(data), timeCol], 1, 4),  substr(data[nrow(data), timeCol], 6, 7), 
  #               substr(data[nrow(data), timeCol], 9, 10))
  deploy <- VulnToolkit::noaa(station = station, begindate = start, enddate = end, interval = "6 minute", 
                 time = time, continuous = "T")
  ref <- VulnToolkit::noaa(station = station, begindate = ref.period[1], enddate = ref.period[2], interval = "6 minute", 
              time = time, continuous = "T")
  
  dur.data              <- data.frame(elevs = seq(-3, 3.5, by = 0.001))
  dur.data$duration.est <- VulnToolkit::fld.dur(z = dur.data$elevs, level = deploy[, 2])
  dur.data$duration.ref <- VulnToolkit::fld.dur(z = dur.data$elevs, level = ref[, 2])
  dur.data$disparity    <- dur.data$duration.ref - dur.data$duration.est 
  dur.data$RMSE         <- sqrt((dur.data$disparity)^2)
  
  
  # diagnostic plots: elevation vs flooding, and disparity vs. estimated hydroperiod
  f <- 0.85 # cex.axis, cex.lab size
  g <- 0.5 # point size
  
  # filename <- "dur_bias_output.png"
  # grDevices::png(filename, height = 100, width = 200, units = "mm", res = 300)
  # graphics::par(mar = c(4, 4.5, 0.3, 1))
  # graphics::par(fig = c(0, 0.53, 0, 1))
  
  graphics::plot(y = dur.data$duration.est * 100, x = dur.data$elevs, xlab = "elevation (m; MHW)", 
       ylab = "flooding duration (percent of time)", ylim = c(0, max(dur.data$duration.est * 100, na.rm = TRUE)),
       xlim = c(min(dur.data$elevs[dur.data$duration.est < 0.9995], na.rm = T), 
                max(dur.data$elevs[dur.data$duration.est > 0.0005], na.rm = T)),
       type = "n", yaxt = "n", bty="n", xaxs = "i", yaxs = "i", 
       cex.lab = f, cex.axis = f, tck = 0.3, tcl = 0.15
  )
  graphics::points(y = dur.data$duration.ref * 100, x = dur.data$elevs, pch = 19, cex = g) # "truth"
  graphics::points(y = dur.data$duration.est * 100, x = dur.data$elevs, pch = 19, cex = g, col = "red") # from deploy period
  graphics::text(-1, (1 - dur.data$duration.ref[dur.data$elevs == -1]) * 100 * 0.9,
       "reference values", cex = f)
  graphics::text(-1, (1 - dur.data$duration.ref[dur.data$elevs == -1]) * 100 * 0.5,
       "estimated values", col = "red", cex = f)
  graphics::abline(h = 0)
  graphics::axis(2, las = 1, at = graphics::axTicks(2), labels = sprintf("%1.0f%%", graphics::axTicks(2)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  
  # graphics::par(new = TRUE)
  # graphics::par(fig = c(0.47, 1, 0, 1))
  graphics::plot(y = dur.data$disparity * 100, x = dur.data$duration.est * 100, ylab = "disparity (percent of time)", 
       xlab = "estimated flooding duration (percent of time)", 
       ylim = c(min(dur.data$disparity * 100, na.rm = T), max(dur.data$disparity * 100, na.rm = TRUE)), 
       type = "n", yaxt = "n", xaxt = "n", bty="n", xaxs = "i", yaxs = "i", 
       cex.lab = f, cex.axis = f, tck = 0.3, tcl = 0.15
       )
  graphics::points(y = dur.data$disparity * 100, x = dur.data$duration.est * 100, pch = 19, cex = g)
  graphics::abline(h = min(dur.data$disparity * 100, na.rm = T))
  graphics::abline(v = 0)
  graphics::axis(2, las = 1,at = graphics::axTicks(2), labels = sprintf("%1.0f%%", graphics::axTicks(2)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  graphics::axis(1, las = 1,at = graphics::axTicks(1), labels = sprintf("%1.0f%%", graphics::axTicks(1)), cex.axis = f, tck = 0.3, tcl = 0.15) 
  # grDevices::dev.off()
  
  # print(paste0(filename, " saved to ", getwd()))
  print(summary(dur.data$disparity))
  invisible(dur.data)
  
} # nocov end
