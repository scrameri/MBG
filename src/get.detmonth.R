get.detmonth <- function(detby) {
  
  # internal parameters
  mstring <- "^Jan|^jan|^Feb|^feb|^Fév|^Fev|^fev|^Mar|^mar|^Apr|^apr|^Avr|^avr|^May|^may|^Mai|^mai|^Jun|^jun|^Jui|^jui|^Jul|^jul|^Aug|^aug|^Aou|^aou|^Sep|^sep|^Oct|^oct|^Nov|^nov|^Dec|^dec|^Déc|^déc"
  mvec <- unlist(strsplit(mstring, split = "[|]"))
  mcorr <- c(1,1,2,2,2,2,2,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,8,8,8,8,9,9,10,10,11,11,12,12,12,12)
  
  # check
  stopifnot(is.character(detby),
            length(mvec) == length(mcorr))
  
  # split string
  l <- strsplit(detby, split = " ")
  detmo.string <- detmo.string.orig <- unlist(lapply(l, function(x) suppressWarnings(x[max(grep(mstring, x))])))
  
  # replace with numeric
  for (i in mvec) {
    detmo.string[grep(i, detmo.string)] <- mcorr[mvec == i]
  }
  detmo <- as.numeric(detmo.string)
  
  # return
  return(detmo)
}
