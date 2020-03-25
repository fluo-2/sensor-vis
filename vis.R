#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

library(tools)
library(gdata)
library(ggplot2)
library(tikzDevice)
library(reshape2)

# read and filter
data <- read.xls("./data/SIF_Sensors.xlsx",sheet=1)
data <- data[c(1,9,10)]
data <- data[grep("multispectral",data[,2]),]
names(data)[2] <- "Spectral_Bands"
# filter-again and recompose
filter <- lapply(1:nrow(data),function(i) {
  range <- as.numeric(strsplit(as.character(data[i,2]),"\\,?\\s")[[1]])
  range <- range[which(!is.na(range))]
  return(cbind(data[i,c(1,3)],range))
})
filter <- do.call(rbind,filter)
filter[,2] <- as.numeric(as.character(filter[,2]))
# proceed to plot data
tikz("basic_spectral.tex", width=20, height=15, standAlone = TRUE)
g <- ggplot(filter) +
  geom_rect(aes(xmin=range-(FWHM/2),xmax=range+(FWHM/2),
                ymin=0, ymax=0.49),color="black",fill="red",alpha=0.7,size=0.9) +
  ylab("") +
  xlab("\n Wavelength $\\lambda$ [nm]") +
  ylim(c(0,0.5)) +
  theme_bw() +
  theme(text = element_text(size=30, family="CM Roman"),
        legend.position = "none",
        plot.title = element_text(hjust=0.5),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  scale_x_continuous(breaks = round(seq(min(filter$range), max(filter$range), by = 50),1)) +
  facet_wrap(Instrument~.,nrow=4)
print(g)
dev.off()
texi2pdf("basic_spectral.tex",clean=TRUE)
file.remove("basic_spectral.tex")
file.rename("basic_spectral.pdf",
            "./img/basic_spectral.pdf")
