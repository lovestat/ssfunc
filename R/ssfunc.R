## read all rds files in a folder and load them as objects with the file names
readRDS_dir <- function(path) {
  filenames <- list.files(path = path, pattern = ".rds")
  objnames <- substring(filenames, 1, nchar(filenames)-4)
  eval(parse(text = glue::glue("`{objnames}` <- readRDS('{path}{filenames}')")), envir = .GlobalEnv)
  return(objnames)
}

## read rds files in vectorized style
### objnames:  a character vector, the object names to assign
### filepaths: a character vector, the rds file paths
### out:       'objnames' will output the objnames
###            'list' will store all the objects into a names list
### reload:    logical value, if reload the object
readRDS_vec <- function(objnames, filepaths, out = c("objnames", "list"), reload = TRUE) {
  out <- match.arg(out)
  stopifnot(length(objnames) == length(filepaths))
  if (!reload){
    l <- objnames %in% ls(envir = .GlobalEnv)
    objnames <- objnames[!l]
    filepaths <- filepaths[!l]
  }
  eval(parse(text = glue::glue("`{objnames}` <- readRDS('{filepaths}')")), envir = .GlobalEnv)
  if (out == "objnames") return(objnames)
  if (out == "list") return(mget(objnames, envir = .GlobalEnv))
}

## Delete files with specific pattern in a directory
delete_files <- function(path, pattern = ".Rout"){
  filenames <- list.files(path = path, pattern = pattern)
  filepaths <- glue::glue("{path}/{filenames}")
  o <- map_lgl(filepaths, ~ if (file.exists(.x)) file.remove(.x) else FALSE)
  list(deleted.files = filenames[o],
       deleted.paths = filepaths[o],
       undeleted.files = filenames[!o],
       undeleted.paths = filepaths[!o])
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

## Replacement functional `subset()`
`subset<-` <- function (x, subset, select, value, ...)
{
  # Use tibble's feature to store and visualize list as cell elements
  x <- tibble::as_tibble(x)
  r <- if (missing(subset))
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'subset' must be logical")
    r & !is.na(r)
  }
  vars <- if (missing(select))
    stop("'select' must be provided")
  else {
    deparse(substitute(select))
  }
  x[r, vars] <- value
  x
}

## Transpose a data.frame
t_df <- function(x) {
  x %>%
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>% 
  pivot_wider(names_from = "rowname", values_from = "col2")
}
                   
