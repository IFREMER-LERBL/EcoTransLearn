## Clean environment and close all figures
rm(list = ls(all=TRUE))
graphics.off()

## Set the working directory
setwd("C:/Users/Administrateur/Desktop/ImgTLClass/")

## Required packages
required.packages <- c("colorRamps","ggplot2","grid","jpeg","mapplots", "maps",
                       "randomForest","reticulate","SDMTools","shapefiles",
                       "stringr","svDialogs","svMisc","tcltk2","tiff","zooimage")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(required.packages, require, character.only = TRUE))

## Import images used in GUI
barplt <- tcl("image", "create", "photo", "barplt", file = "images/barplot.gif")
csv <- tcl("image", "create", "photo", "csv", file = "images/csv.gif")
fish <- tcl("image", "create", "photo", "fish", file = "images/fish.gif")
folder <- tcl("image", "create", "photo", "folder", file = "images/folder.gif")
ifremerLogo <- tcl("image", "create", "photo", "ifremerLogo", file = "images/ifremer_logo_resized_small.gif")
map <- tcl("image", "create", "photo", "map", file = "images/map.gif")
phyto <- tcl("image", "create", "photo", "phyto", file = "images/phyto_flowcam.gif")
plus <- tcl("image", "create", "photo", "plus", file = "images/plus.gif")
python <- tcl("image", "create", "photo", "python", file = "images/python.gif")
settingsFile <- tcl("image", "create", "photo", "settingsFile", file = "images/settings.gif")
update <- tcl("image", "create", "photo", "update", file = "images/update.gif")

## Source all R functions
source("ClassSetting.R")
source("ExportRes.R")
source("ImgTLClass.R")
source("ImgProcess.R")
source("MoreTools.R")
source("NorthArrow.R")

## Function to launch GUI
cat("Execute 'ImgTLClass()' function to launch GUI...\n")