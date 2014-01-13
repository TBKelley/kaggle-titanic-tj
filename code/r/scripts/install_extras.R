install.not.exists <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep=TRUE)
  }
}
install.not.exists('data.table')

# Install Rtools before running this
install.packages("devtools")
library("devtools")
install.packages("microbenchmark")
install.packages("Lahman")
install.packages('hflights')
install.packages('RSQLite')
install.packages('RSQLite.extfuns')
devtools::install_github("dplyr")
vignette("introduction", package = "dplyr")

devtools::install_github("plotly/R-api")

# library(dplyr)
# library(data.table)
# library(ggplot2)
# library(reshape2)
# library(plotly)