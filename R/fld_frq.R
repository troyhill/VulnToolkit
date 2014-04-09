### Functions to calculate flooding frequency and duration
### In the flooding frequency function, "x" is elevation of interest and "vec" is a dataset
### of *only* high tides
### In the flooding duration function, "x" is elevation of interest and "vec" is a dataset
### of water levels recorded at constant intervals
### All elevation and tide data must be relative to same datum

fld.frq <- function(x, vec = x) {  
  a <- ecdf(vec)
  1 - a(x)
}
