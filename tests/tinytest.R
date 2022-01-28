if ( requireNamespace("tinytest", quietly=TRUE) ){
  options(x12.delete = TRUE)
  setwd(tempdir())
  tinytest::test_package("x12")
}
