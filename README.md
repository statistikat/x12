[![R-CMD-check](https://github.com/statistikat/x12/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/statistikat/x12/actions)
[![Codecov test coverage](https://codecov.io/gh/statistikat/x12/branch/master/graph/badge.svg)](https://app.codecov.io/gh/statistikat/x12?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/x12)](https://CRAN.R-project.org/package=x12)
[![Downloads](http://cranlogs.r-pkg.org/badges/x12)](https://CRAN.R-project.org/package=x12)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software)

x12
===
The R package x12 is used in combination with the X-12 ARIMA/X13-ARIMA-SEATS Seasonal Adjustment Program.

By default (since version 1.7.0) the x13binary R package is used to provide the binaries.

But a local/alternative version could also be used which can be downloaded from https://www.census.gov/srd/www/x13as/
for Windows, Linux and as Source Code.
After the installation the x13as.exe or the x13as executable should be on your system. The path to the executable is
important it is set with the function x12path, e.g., x12path("d:/X12InstallationPath/x13as.exe") or x12path("/usr/bin/x13a").

X-13ARIMA-SEATS (http://www.census.gov/srd/www/x13as/) is the current version of the seasonal adjustment program of the
US Census Bureau, it can also be used in connection with the R package x12, but at the moment the SEATS Spec is not
included in the package at the moment, it is planned to include it in a future release.

On Mac OS X and Linux it is possible to compile x12a or x13as, the Makefile template 'makefile.lnx' has to be
adapted and copied to Makefile afterwards 'make' should work to create the exexcutables.


