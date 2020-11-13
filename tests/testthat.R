library(testthat)
library(x12)
options(x12.delete = TRUE)
if(x13binary::supportedPlatform()){
  test_check("x12")
}
