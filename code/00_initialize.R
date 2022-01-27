### these are all the packages we use throughout the project (there may be extra here)

library(readxl)
library(SearchTrees)
library(gstat)
library(rgeos)
library(rdrobust)
library(cowplot)
library(scales)
library(stargazer)
library(did)
library(plm)
library(raster)
library(ebal)
library(miceadds)
library(vroom)
library(ggeffects)
library(tidycensus)
library(fixest)
library(splitstackshape)
library(spdep)
library(AER)
library(maptools)
library(tigris)
library(rgdal)
library(modelsummary)
library(spatialreg)
library(sqldf)
library(lubridate)
library(rgdal)
library(tidyverse)
library(data.table)
library(kevostools)

### this is a quick function that allows us to clear memory without losing things we
### want to keep from one script to the next

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")
options("modelsummary_format_numeric_latex" = "plain")

cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
  gc()
}

source("code/user_functions/clean_names.R")
