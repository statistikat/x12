.onAttach <- function(...){
  #data(AirPassengers)
  if(x13binary::supportedPlatform()){
    packageStartupMessage("x12 is ready to use.")
    packageStartupMessage("Use the package x12GUI for a Graphical User Interface. \n")
    x12path()  
    packageStartupMessage("By default the X13-ARIMA-SEATS binaries provided by the R package x13binary\n")
    packageStartupMessage("are used but this can be changed with x12path(validpath) \n")
    packageStartupMessage("---------------\n")
    packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/statistikat/x12/issues")
  }else{
    packageStartupMessage("The X13-ARIMA-SEATS binaries from R package x13binary is not available.\n")
    packageStartupMessage("Please provide the path to your own binary through the function x12path.\n")
  }
  
}