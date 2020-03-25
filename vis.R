#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

library(gdata)
library(ggplot2)
library(tikzDevice)
library(reshape2)

data <- read.xls("./data/SIF_Sensors.xlsx",sheet=1)
data <- data[c(1,9,10)]
data <- data[grep("multispectral",data[,2]),]

# TODO FWHM stands for diameter type width, not radius, so +- FWHM/2
# plot the initial ones for now
