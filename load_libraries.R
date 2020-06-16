list.of.packages <- c("ggplot2",
                      "reshape2",
                      "dplyr",
                      "rgdal",
                      "rgeos",
                      "tmap",
                      "leaflet",
                      "RColorBrewer",
                      "sp",
                      "raster",
                      "dismo",
                      "adehabitatHR",
                      "spdep",
                      "spgwr",
                      "grid",
                      "gridExtra",
                      "spatstat",
                      "maptools",
                      "gstat",
                      "xts",
                      "rgl",
                      "automap",
                      "olsrr",
                      "tidyverse",
                      "corrplot"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")


library(ggplot2)
library(reshape2)
library(dplyr)
library("rgdal")
library("rgeos")
library(tmap)
library(leaflet)
library(RColorBrewer)
library("sp")
library(raster)
library(dismo)
library(adehabitatHR)
library(spdep)
library("spgwr")
library(grid)
library(gridExtra)
library(spatstat)
library(maptools)
library(gstat)
library(xts)
library(rgl)
library(automap)
library(olsrr)
library(tidyverse)
library(corrplot)