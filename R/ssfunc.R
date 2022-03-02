## read all rds files in a folder and load them as objects with the file names
readRDS_dir <- function(path) {
  filenames <- list.files(path = path, pattern = ".rds")
  objnames <- substring(filenames, 1, nchar(.)-4)
  eval(parse(text = glue::glue("`{objnames}` <- readRDS('{path}{filenames}')")), envir = .GlobalEnv)
  return(objnames)
}

## Create recursive list
rec_list <- function(len){
  if(length(len) == 1){
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec_list(len[-1]))
  }
}

## AR1 Var-Cov matrix
ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  rho^exponent
}

## Evaluate a expression text
eval_text <- function(text) {
  eval(parse(text = text), envir = .GlobalEnv)
}

