## instead of being smart, I'll be quick.
## all the packages I'll need to re-install after ever updating r:

## btw:
## brew update
## brew upgrade r

packages <- c("ggplot2","dplyr","plyr","gridExtra","lme4", "MCMCglmm", "lazyeval", "rjags", "coda", "digest", "xlsx", "stringdist", "DBI", "devtools", "data.table", "e1071", "reshape", "foreach", "rJava", "RSQLite", "sqldf", "data.table", "rmarkdown", "stargazer", "pryr", "chron", "readr", "R.matlab", "stringr", "orthopolynom")

lapply(packages, install.packages)

## Notes:
## if rJava is giving you problems, make sure the dev kit is installed:
## run: R CMD javareconf
