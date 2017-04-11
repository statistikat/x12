.onAttach <- function(...){
  #data(AirPassengers)
  packageStartupMessage("x12 is ready to use.")
  packageStartupMessage("Use the package x12GUI for a Graphical User Interface. \n")
  x12path()
  packageStartupMessage("By default the X13-ARIMA-SEATS binaries provided by the R package x13binary\n")
  packageStartupMessage("are used but this can be changed with x12path(validpath) \n")
  packageStartupMessage("---------------\n")
  packageStartupMessage("Suggestions and bug-reports can be submitted at: https://github.com/statistikat/x12/issues")
}