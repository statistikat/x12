x12env <- new.env()
putd <- function(x, value) {
  assign(x, value, envir=x12env) # add () to sdcGUIenv
}

rmd <- function(x) {
  rm(list=x, envir=x12env) # rm () from sdcGUIenv
}

getd <- function(x, mode="any") {
  get(x, envir=x12env, mode=mode, inherits=FALSE) # add () to sdcGUIenv
}

existd <- function(x, mode="any") {
  exists(x, envir=x12env, mode=mode, inherits=FALSE) # add () to sdcGUIenv
}
x12path <- function(path=NULL){
  if(is.null(path)){
    x13p <- x13binary::x13path()
    files <- list.files(x13p)
    path <- file.path(x13p,files[head(grep("x13as",files),1)])
  }
  pathWork("x12path",path)  
  invisible(path)
}
#x13path <- function(path=NULL){
#  pathWork("x13path",path)
#}

pathWork <- function(name,path){
  if(is.null(path)||length(path)!=1){
    if(existd(name))
      return(getd(name))
    else
      cat("Not defined!\n")
  }else{
    if(!file.exists(path))
      stop(paste(path," - does not exists.",sep=""))
    putd(name,path)
  }
  
}