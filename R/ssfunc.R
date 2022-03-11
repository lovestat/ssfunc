## read all rds files in a folder and load them as objects with the file names
readRDS_dir <- function(path) {
  filenames <- list.files(path = path, pattern = ".rds")
  objnames <- substring(filenames, 1, nchar(filenames)-4)
  eval(parse(text = glue::glue("`{objnames}` <- readRDS('{path}{filenames}')")), envir = .GlobalEnv)
  return(objnames)
}
 
## Create recursive list with desired a name list
## len is an integer vector
## nam is a list containing names
rec_list <- function(len, nam = NULL){
  if(length(len) == 1){
    out1 <- vector("list", len);
    names(out1) <- nam[[1]]
    out1
  } else {
    out2 <- lapply(1:len[1], function(...) rec_list(len[-1], nam = nam[-1]))
    names(out2) <- nam[[1]]
    out2
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

