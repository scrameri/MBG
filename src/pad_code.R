pad_code <- function(x, pad = "0", n = 4) {
  xs <- strsplit(x, split = "-")
  x1 <- sapply(xs, "[", 1)
  x2 <- sapply(xs, "[", 2)
  x.num <- gsub("[A-Za-z ]+", "", x1)
  x.char <- gsub("[0-9 ]+", "", x1)
  x.suf <- if (is.na(x2[1])) "" else paste0("-", x2[1])
  paste0(x.char, stringr::str_pad(x.num, width = n, pad = pad), x.suf)
}
