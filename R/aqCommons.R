# aq library of commonly used functions #

# Library loading functions #
lib <- function(type=c("dat", "ggp2", "mgcv")){
  libs_ <- c()
  if(any(type %in% "dat")) libs_ <- c(libs_, "plyr", "dplyr", "tidyr", "broom")
  if(any(type %in% "ggp2")) libs_ <- c(libs_, "ggplot2", "grid", "gridExtra")
  if(any(type %in% "mgcv")) libs_ <- c(libs_, "mgcv", "visreg")
  cat("attaching packages::", paste(libs_, collapse=", "))
  invisible(lapply(libs_, require, character.only=T))
}

# converting numbers into percentage format
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# calculating standard error
stdErr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

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

# return summary statistics from specified/named variable (`varname`) of a data.frame (`data`)

checkVar <- function(varname, data){
  val <- data[, varname]
  tmp <- data.frame(mean = mean(val),
                    sd = sd(val),
                    IQR = IQR(val),
                    `0%`= min(val),
                    `25%` = quantile(val, 0.25),
                    `50%` = median(val),
                    `75%` = quantile(val, .75),
                    `100%` = max(val),
                    n = length(val))
  names(tmp) <- c("mean", "sd", "IQR", "`0%`", "`25%`", "`50%`", "`75%`", "`100%`", "n")
  rownames(tmp) <- varname
  return(tmp)
}
