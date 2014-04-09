\name{vuln.kit}
\alias{vuln.kit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vuln.kit(level, datetime, HL.calc = "calculated", HL.input = "NA", HL.output = "n", regime = "semi-diurnal", units = "meters", frq.dur.inc = 0.005, TV.inc = 0.1, MHW = "calculated", MLW = "calculated", MN = "calculated")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{datetime}{
%%     ~~Describe \code{datetime} here~~
}
  \item{HL.calc}{
%%     ~~Describe \code{HL.calc} here~~
}
  \item{HL.input}{
%%     ~~Describe \code{HL.input} here~~
}
  \item{HL.output}{
%%     ~~Describe \code{HL.output} here~~
}
  \item{regime}{
%%     ~~Describe \code{regime} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
  \item{frq.dur.inc}{
%%     ~~Describe \code{frq.dur.inc} here~~
}
  \item{TV.inc}{
%%     ~~Describe \code{TV.inc} here~~
}
  \item{MHW}{
%%     ~~Describe \code{MHW} here~~
}
  \item{MLW}{
%%     ~~Describe \code{MLW} here~~
}
  \item{MN}{
%%     ~~Describe \code{MN} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (level, datetime, HL.calc = "calculated", HL.input = "NA", 
    HL.output = "n", regime = "semi-diurnal", units = "meters", 
    frq.dur.inc = 0.005, TV.inc = 0.1, MHW = "calculated", MLW = "calculated", 
    MN = "calculated") 
{
    if (is.numeric(level) == FALSE) 
        stop("invalid entry: 'level' must be numeric")
    if (class(datetime)[1] != "POSIXlt" & class(datetime)[1] != 
        "POSIXct") 
        stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
    if (length(level) != length(datetime)) 
        stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
    if (HL.calc != "calculated" & HL.calc != "user") 
        stop("invalid entry: 'HL.calc' must be set to 'calculated' (determined by vuln.kit from the high-frequency data) or 'user' (if user provides a dataset of high and low tides)")
    if (HL.output != "n" & HL.output != "y" & HL.output != "N" & 
        HL.output != "Y") {
        stop("invalid entry: 'HL.output' must be set to 'y' or 'n'")
    }
    if (regime == "semi-diurnal" | regime == "mixed") {
        period <- 12
    }
    else if (regime == "diurnal") {
        period <- 24
    }
    else stop("invalid tidal regime entry: 'regime' must be 'semi-diurnal' or 'diurnal'")
    if (units == "feet") {
        level <- level * 0.3048
    }
    else if (units != "meters") {
        stop("invalid units: must be 'feet' or 'meters'")
    }
    if (is.numeric(frq.dur.inc) == FALSE) 
        stop("invalid entry: 'frq.dur.inc' must be numeric")
    if (units == "feet") {
        frq.dur.inc <- frq.dur.inc * 0.3048
    }
    if (is.numeric(TV.inc) == FALSE) 
        stop("invalid entry: 'TV.inc' must be numeric")
    if (units == "feet") {
        TV.inc <- TV.inc * 0.3048
    }
    if (MHW != "calculated" & is.numeric(MHW) == FALSE) {
        stop("invalid entry: if not calculated internally, 'MHW' must be a numeric input")
    }
    else if (is.numeric(MHW) == TRUE & units == "feet") {
        MHW <- MHW * 0.3048
    }
    if (MLW != "calculated" & is.numeric(MLW) == FALSE) {
        stop("invalid entry: if not calculated internally, 'MLW' must be a numeric input")
    }
    else if (is.numeric(MLW) == TRUE & units == "feet") {
        MLW <- MLW * 0.3048
    }
    if (MN != "calculated" & is.numeric(MN) == FALSE) {
        stop("invalid entry: if not calculated internally, 'MN' must be a numeric input")
    }
    else if (is.numeric(MN) == TRUE & units == "feet") {
        MN <- MN * 0.3048
    }
    if (HL.calc == "calculated") {
        partial.tide <- period * 60 * 60
        t.int <- as.numeric(datetime[2]) - as.numeric(datetime[1])
        wll.2 <- data.frame(1:length(level), level, datetime)
        width <- partial.tide/t.int
        t.x <- which.max(level[1:(1 + width)])
        for (i in 1:length(level)) {
            if (!is.na(t.x[i] + 0.5 * width)) {
                temp <- which.max(level[(t.x[i] + 0.5 * width):(t.x[i] + 
                  1.25 * width)])
                val <- temp - 1 + (t.x[i] + 0.5 * width)
                t.x <- rbind(t.x, val)
            }
            else t.x <- t.x
        }
        ht <- wll.2[t.x[, 1], ]
        ht$tide <- rep("H", nrow(ht))
        t.y <- which.min(level[1:(1 + width)])
        for (i in 1:length(level)) {
            if (!is.na(t.y[i] + 0.5 * width)) {
                temp <- which.min(level[(t.y[i] + 0.5 * width):(t.y[i] + 
                  1.25 * width)])
                val <- temp - 1 + (t.y[i] + 0.5 * width)
                t.y <- rbind(t.y, val)
            }
            else t.y <- t.y
        }
        lt <- wll.2[t.y[, 1], ]
        lt$tide <- rep("L", nrow(lt))
        HL.data <- rbind(ht, lt)
        HL.data <- HL.data[order(HL.data[, 1]), ]
        HL.data <- HL.data[, -1]
        rownames(HL.data) <- seq(length = nrow(HL.data))
        HL.data <- HL.data[-nrow(HL.data), ]
    }
    else if (HL.calc == "user") {
        if (class(HL.input[, 1])[1] != "POSIXlt" & class(HL.input[, 
            1])[1] != "POSIXct") {
            stop("invalid entry: user-defined high/low dataset must have POSIX* timestamps. see ?as.POSIXct")
        }
        else if (is.numeric(HL.input[, 2]) == FALSE) {
            stop("invalid entry: user-defined high/low dataset contains non-numeric water level data")
        }
        else HL.data <- HL.input
        HL.data[HL.data[, 3] == "HH", 3] <- "H"
        HL.data[HL.data[, 3] == "LL", 3] <- "L"
        names(HL.data) <- c("datetime", "level", "tide")
    }
    else stop("invalid entry: 'HL.calc' input must be either calculated internally ('calculated') or supplied by the user ('user')")
    if (HL.calc == "user" & units == "feet") 
        HL.input[, 2] <- HL.input[, 2] * 0.3048
    if (MHW == "calculated") {
        MHW <- mean(HL.data$level[HL.data$tide == "H"])
    }
    else (MHW == MHW)
    if (MLW == "calculated") {
        MLW <- mean(HL.data$level[HL.data$tide == "L"])
    }
    else (MLW == MLW)
    if (MN == "calculated") {
        MN <- MHW - MLW
    }
    else (MN == MN)
    mhw.level <- level - MHW
    HL.data$level.mhw <- HL.data$level - MHW
    elevs <- seq(from = -0.5, to = 0.5, by = frq.dur.inc)
    frq.ecdf <- ecdf(HL.data$level.mhw[HL.data$tide == "H"])
    dur.ecdf <- ecdf(mhw.level)
    durs <- 1 - dur.ecdf(elevs)
    frqs <- 1 - frq.ecdf(elevs)
    frq.dur.df <- data.frame(elevs, frqs, durs)
    par(mfrow = c(1, 2))
    par(mar = c(4, 4, 0.2, 0.2))
    plot(frqs ~ elevs, cex = 0.1, col = "black", type = "l", 
        xlim = c(-0.5, 0.5), yaxt = "n", ylim = c(0, 1 - frq.ecdf(-0.5)), 
        ylab = "", xlab = paste("Elevation (m; MHW)", sep = ""))
    axis(2, at = axTicks(2), labels = sprintf("\%1.0f\%\%", 100 * 
        axTicks(2)), las = 1)
    lines(durs ~ elevs, cex = 0.1, col = "red", type = "l")
    text(x = 0.4, y = 0.6, "frequency")
    text(x = 0.39, y = 0.5, "duration", col = "red")
    TFV <- (frq.dur.df$frqs[frq.dur.df$elevs == 0] - frq.dur.df$frqs[frq.dur.df$elevs == 
        paste("-", TV.inc, sep = "")])/(TV.inc * -100)
    TDV <- (frq.dur.df$durs[frq.dur.df$elevs == 0] - frq.dur.df$durs[frq.dur.df$elevs == 
        paste("-", TV.inc, sep = "")])/(TV.inc * -100)
    wl <- data.frame(datetime, level)
    wl <- wl[wl$datetime < wl$datetime[nrow(wl)] & wl$datetime > 
        (wl$datetime[nrow(wl)] - 60 * 60 * 24 * 365), ]
    frq.dur.df$wet.max <- as.numeric(rep(NA, times = nrow(frq.dur.df)))
    frq.dur.df$dry.max <- as.numeric(rep(NA, times = nrow(frq.dur.df)))
    for (i in 1:length(elevs)) {
        wl$zeroed <- wl[, 2] - MHW
        wl$ind <- ifelse(wl[, 3] >= elevs[i], 1, 0)
        wl.sub <- wl[wl$ind != 1, ]
        wl.sub$t2 <- c(wl.sub[-1, 1], NA)
        wl.sub$dt <- wl.sub$t2 - wl.sub[, 1]
        frq.dur.df$wet.max[i] <- max(wl.sub$dt, na.rm = T)/60
        wl.sub <- wl[wl$ind != 0, ]
        wl.sub$t2 <- c(wl.sub[-1, 1], NA)
        wl.sub$dt <- wl.sub$t2 - wl.sub[, 1]
        frq.dur.df$dry.max[i] <- max(wl.sub$dt, na.rm = T)/60
    }
    plot(frq.dur.df$dry.max ~ frq.dur.df$elevs, cex = 0.1, col = "black", 
        type = "l", xlim = c(-0.5, 0.5), yaxt = "n", ylim = c(min(frq.dur.df$wet.max), 
            max(frq.dur.df$dry.max)), ylab = "Hours", log = "y", 
        xlab = paste("Elevation (m; MHW)", sep = ""))
    axis(2, 10^c(-2:10), labels = 10^c(-2:10), las = 1)
    lines(frq.dur.df$wet.max ~ frq.dur.df$elevs, cex = 0.1, col = "red", 
        type = "l")
    text(x = 0.3, y = 10^1.6, "longest dry period")
    text(x = 0.3, y = 10^1.1, "longest wet period", col = "red")
    WPV <- (frq.dur.df$wet.max[frq.dur.df$elevs == 0] - frq.dur.df$wet.max[frq.dur.df$elevs == 
        paste("-", TV.inc, sep = "")])/(TV.inc * -100)
    DPV <- (frq.dur.df$dry.max[frq.dur.df$elevs == 0] - frq.dur.df$dry.max[frq.dur.df$elevs == 
        paste("-", TV.inc, sep = "")])/(TV.inc * -100)
    if (HL.output == "n" | HL.output == "N") {
        output <- list(frq.dur.df = frq.dur.df, TFV = TFV, TDV = TDV, 
            WPV = WPV, DPV = DPV, MHW = MHW, MLW = MLW, MN = MN)
    }
    else if (HL.output == "y" | HL.output == "Y") {
        output <- list(frq.dur.df = frq.dur.df, TFV = TFV, TDV = TDV, 
            WPV = WPV, DPV = DPV, MHW = MHW, MLW = MLW, MN = MN, 
            high.low = HL.data)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
