# aq library of commonly used functions #

# Library loading functions #
lib <- function(type=c("dat", "ggp2", "mgcv")){
  libs_ <- c()
  if(any(type %in% "dat")) libs_ <- c(libs_, "plyr", "dplyr", "tidyr")
  if(any(type %in% "ggp2")) libs_ <- c(libs_, "ggplot2", "grid", "gridExtra")
  if(any(type %in% "mgcv")) libs_ <- c(libs_, "mgcv", "visreg")
  cat("attaching packages::", paste(libs_, collapse=", "))
  invisible(lapply(libs_, require, character.only=T))
}

# converting numbers into percentage format
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# calculating standard error
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# find specific term across scripts/files
findTerm <- function(x, file.list, full="n"){
  line.ret <- lapply(file.list, function(vv)grep(x, readLines(vv, warn=FALSE), ignore.case=T))
  names(line.ret) <- file.list
  f0 <- switch(full,
               "y" = function(ww){
                 obg <- unlist(line.ret[ww])
                 if(length(obg)){
                   obh <- readLines(names(line.ret[ww]), warn=FALSE)[obg]
                   paste(paste0("line:", obg), obh, sep="   ")  
                 }else{
                   paste('search term:', x, 'does not appear in', ww)
                 }},
               "n" = function(ww){
                 obg <- unlist(line.ret[ww])
                 if(length(obg)){
                   obh <- readLines(names(line.ret[ww]), warn=FALSE)[obg]
                   paste(paste0("line:", obg), obh, sep="   ")
                 }
               })
  line.ret2 <- lapply(names(line.ret), f0)
  names(line.ret2) <- file.list
  line.ret2[!sapply(line.ret2, is.null)]
}