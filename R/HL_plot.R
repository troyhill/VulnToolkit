### function to visually examine high and low tides found by function HL()
HL.plot <- function(level, time, period = 13, phantom = TRUE, tides = "all") {
  hl <- HL(level, time, period, phantom, tides)
  wll.2 <- data.frame(1:length(level), level, time)
  
  plot(wll.2$level ~ wll.2$time, type = "l", ylab = "water level", 
       xlab = "", xaxt = "n", col = "darkgray")
  points(hl$level[hl$tide == "H"]  ~ hl$time[hl$tide == "H"], pch = 19, cex = 0.75, col="red")
  points(hl$level[hl$tide == "L"] ~ hl$time[hl$tide == "L"], pch = 19, cex = 0.75, col="cornflowerblue")
}
