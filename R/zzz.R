.onAttach <- function(...){
  #data(AirPassengers)
  packageStartupMessage("x12 is ready to use.")
  packageStartupMessage("Load the package x12GUI for a Graphical User Interface. \n")
  packageStartupMessage("It is advised to set the path to the X12 or X13 executables\n")
  packageStartupMessage("with x12path(validpath) or x13path(validpath)!")
}