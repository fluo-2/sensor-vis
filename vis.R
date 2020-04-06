#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

library(tools)
library(gdata)
library(ggplot2)
library(tikzDevice)
library(reshape2)
library(ggspectra)
source("./handle_data.R")

# read main data source for sheet 1
clean_data <- extract_categorical_data("WL_FWHM")
# create dummy dataframe to initialize wavelength range
wave <- data.frame(wl_center = 100:15000)
# proceed to plot data
tikz("basic_spectral.tex", width=20, height=12, standAlone = TRUE)
g <- ggplot(wave,aes(x=wl_center)) +
  ## wl_guide(alpha=0.8) +
  geom_rect(data=clean_data,aes(xmin=wl_center-(fwhm/2),xmax=wl_center+(fwhm/2),
                ymin=0, ymax=0.49),color="black",fill="red",alpha=0.5,size=1.1) +
  ## geom_vline(xintercept = 685, linetype="dashed", size=1.2) +
  ylab("") +
  xlab("\n Wavelength $\\lambda$ [nm]") +
  ylim(c(0,0.5)) +
  theme_bw() +
  theme(text = element_text(size=30),
        legend.position = "none",
        plot.title = element_text(hjust=0.5),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        plot.margin = margin(10, 50, 10, 10)) +
  scale_x_continuous(breaks = round(seq(roundUp(min(wave$wl_center),500),
                                        roundUp(max(wave$wl_center),500),
                                        by = 1000),1),
                     expand=expand_scale(mult=c(0,0))) +
  facet_wrap(Sensor~.,ncol=1)
print(g)
dev.off()
texi2pdf("basic_spectral.tex",clean=TRUE)
file.remove("basic_spectral.tex")
file.rename("basic_spectral.pdf",
            "./img/basic_spectral.pdf")
