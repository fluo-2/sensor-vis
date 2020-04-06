#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

library(gdata)

extract_categorical_data <- function(subtype="WL_FWHM"){
  if(subtype == "WL_FWHM"){
    # read main data source for spaceborne sheet
    data <- read.xls("./data/SIF_Sensors.xlsx",sheet="spaceborne")
    # extract satellite data where WL center and FWHM are clearly defined
    data <- data[c("Mission","Sensor","WL.Center","FWHM")]
    # extract subset where WL-Center and FWHM are clearly defined
    data <- data[which(data[,c("WL.Center")] != ""),]
    data <- data[which(data[,c("FWHM")] != ""),]
    # clean up data and convert to numeric
    clean_data <- data.frame()
    for(i in 1:nrow(data)){
      if(length(grep(",\\s?",data[i,c("WL.Center")]))){
        wl_center <- as.numeric(strsplit(as.character(data[i,c("WL.Center")]),
                                         ",\\s?")[[1]])
        fwhm <- as.numeric(strsplit(as.character(data[i,c("FWHM")]),
                                    ",\\s?")[[1]])
        clean_data <- rbind(clean_data,
                            data.frame(cbind(data[i,c("Mission","Sensor")],
                                             wl_center,fwhm,row.names=NULL)))
      } else if(length(grep("\n",data[i,c("WL.Center")]))){
        wl_center <- as.numeric(strsplit(as.character(data[i,c("WL.Center")]),
                                         "\n")[[1]])
        fwhm <- as.numeric(strsplit(as.character(data[i,c("FWHM")]),
                                    "\n")[[1]])
        clean_data <- rbind(clean_data,
                            data.frame(cbind(data[i,c("Mission","Sensor")],
                                             wl_center,fwhm,row.names=NULL)))
      }
    }
  }
  return(clean_data)
}

# source: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x/6468532#6468532
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}
