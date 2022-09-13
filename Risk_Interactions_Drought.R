##==============================================================================
##
## Script creates conceptual graphs to show interactions among drought hazard, 
## exposure, and risk
## All the values in this plot such as drought levels, adaptation coefficient, exponential function
## and its parameters, etc. are hypothetical and to convey a concept.
##
## Authors:Iman Hosseini-Shakib (ishakib@gmail.com)
##         Atieh Alipour (atieh.aalipour@gmail.com)
##         Klaus Keller (klaus_Keller@dartmouth.edu)
##==============================================================================
## Copyright 2022 Iman Hosseini-Shakib, Atieh Alipour, and Klaus Keller
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================

rm(list=ls())
graphics.off()

# Packages used
if (!require('VGAM')) {install.packages('VGAM')}
if (!require('ggplot2')) {install.packages('ggplot2')}
if (!require('ggpubr')) {install.packages('ggpubr')}
if (!require('ggExtra')) {install.packages('ggExtra')}
library(VGAM)
library(ggplot2)
library(ggpubr)
library(ggExtra)
nsamp <- 1e6 # how many samples per distribution?

exp_function <- function(DL,k,a){100-(exp(k*(DL)))^a} #An exponential function to mimic crop yield curve with Drought Level (DL) and coefficients a and k 

                                    
# Drought level distribution characteristics
DL=1:1000   #hypothetical Drought levels
meanDL=525 #hypothetical mean drought level for the stationary pdf
sdDL=65 #hypothetical standard deviation of the stationary pdf
meanDL_nonS=550 #hypothetical mean drought level for the non-stationary pdf
sdDL_nonS=150 #hypothetical standard deviation of the non-stationary pdf
shapeDL_nonS=5 #hypothetical shape factor of the non-stationary pdf

# Hazard
DLbest=DL[meanDL] #hypothetical best guess stationary drought level
# obtain samples from distributions
dl_stat_samp <- rnorm(nsamp,meanDL,sdDL) #hypothetical distribution of drought levels - stationary with uncertainty
dl_nonstat_samp <- rskewnorm(nsamp,location=meanDL_nonS,scale=sdDL_nonS,shape=shapeDL_nonS) #hypothetical distribution of drought levels - non-stationary with uncertainty

# Vulnerability
CY_adaptation <- exp_function((DL-200),0.2,0.03) #hypothetical crop yield - with adaptation
CY_noadaptation <- exp_function((DL-200),0.2,0.04) #hypothetical crop yield - without adaptation

# Risk
rsk_nounc_noadaptation <- CY_noadaptation[DLbest] # hypothetical risk with stationary best guess hazard and crop yield function without adaptation 
rsk_stat_noadaptation <- exp_function((dl_stat_samp)-200, 0.2, 0.04) # hypothetical risk pdf with stationary with uncertainty pdf and crop yield function without adaptation
rsk_nonstat_noadaptation <- exp_function((dl_nonstat_samp)-200, 0.2, 0.04) # hypothetical risk pdf with non-stationary with uncertainty pdf and crop yield function without adaptation
rsk_nonstat_adaptation <- exp_function((dl_nonstat_samp)-200,0.2, 0.03) # hypothetical risk pdf with non-stationary with uncertainty pdf and adaptation

dat_stat_noadaptation <- data.frame(dl=dl_stat_samp, CY=rsk_stat_noadaptation, group='stat_noadaptation')
dat_nonstat_noadaptation <- data.frame(dl=dl_nonstat_samp, CY=rsk_nonstat_noadaptation, group='nonstat_noadaptation')
dat_nonstat_adaptation <- data.frame(dl=dl_nonstat_samp, CY=rsk_nonstat_adaptation, group='nonstat_adaptation')
dat_all <- rbind(dat_stat_noadaptation, dat_nonstat_noadaptation, dat_nonstat_adaptation)

dat_CY_noadaptation <- data.frame(dl=DL, CY=CY_noadaptation, group="noadaptation")
dat_CY_adaptation <- data.frame(dl=DL, CY=CY_adaptation, group="adaptation")
dat_CY_all <- rbind(dat_CY_noadaptation, dat_CY_adaptation)

# diagnostic plot
dat_CY <- data.frame(dl=DL, CY=CY_noadaptation[DL])
p <- ggplot() + geom_line(dat=dat_CY, aes(x=dl, y=CY)) + 
  geom_point(dat=dat_stat_noadaptation, aes(x=dl, y=CY), col='red') +
  geom_vline(aes(xintercept=DLbest), linetype='dashed') +
  geom_hline(aes(yintercept=rsk_nounc_noadaptation), linetype='dashed', col='red') +
  theme_classic() +
  scale_x_continuous(limits=c(0, 1000), expand=c(0, 0)) + 
  scale_y_continuous(expand=c(0, 0))
p1 <- ggMarginal(p, type='histogram', xparams=list(bins=49), yparams=list(bins=49, fill='red'))

# rebuild original plot
# hazard (drought levels)
dens_dl <- density(dl_stat_samp)
a<-ggplot(dat_all[dat_all$group %in% c('stat_noadaptation', 'nonstat_noadaptation'),], aes(x=dl,group=group)) +
  geom_density(aes(color=group),size=1.5)+
  geom_segment(aes(x = DLbest, xend=DLbest, y=0, yend=max(dens_dl$y)), size=1, linetype="solid", color="black") + #hypothetical best guess stationary drought levels
  scale_colour_manual(name = '', 
                      values =c(nonstat_noadaptation='darkblue',stat_noadaptation='cornflowerblue'))+
  scale_x_continuous("Drought Levels", expand = c(0, 0), limits=c(0, 1100)) + 
  scale_y_continuous("Probability Density", expand = c(0, 0)) + 
  annotate(x=580,y=0.006, geom="text",
           label="Neglecting\nclimate change\nand uncertainty",
           color="black",size=6,lineheight = .75,hjust="left")+
  annotate(x=725,y=0.004,geom="text",
           label="Considering\nclimate change\nand uncertainty",
           color="darkblue",size=6,lineheight = .75,hjust="left")+
  annotate(x=450,y=0.0055,geom="text",
           label="Neglecting\nclimate change\nconsidering\nuncertainty", 
           color="cornflowerblue",size=6,lineheight = .75,hjust="right")+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),type = "closed"))) +
  coord_cartesian(ylim=c(0, 0.007))

# Plot b - Vulnerability
b<-ggplot(dat_CY_all,aes(x=dl, y=CY, group=group))+
  geom_line(aes(color=group),size=1.5)+
  scale_colour_manual(name = '', 
                      values =c(noadaptation='darkkhaki',adaptation='darkgreen'))+
  scale_x_continuous("Drought Levels", expand = c(0, 0), limits = c(0, 1100)) + 
  scale_y_continuous("Crop Yield", expand = c(0, 0), limits = c(0, 1.1*max(dat_CY_adaptation$CY))) + 
  annotate(x=880,y=0.78*max(dat_CY_adaptation$CY), geom="text",
           label="Considering\nadaptation", color="darkgreen",
           size=6,lineheight = .75)+
  annotate(x=370,y=0.65*max(dat_CY_adaptation$CY), geom="text",
           label="Neglecting\nadaptation", color="darkkhaki",
           size=6,lineheight = .75)+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position =  "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),type = "closed")))

# Plot c - Risk
c<-ggplot(dat_all,aes(x=CY, group=group))+
  geom_density(aes(color=group),size=1.5)+
  geom_segment(aes(x=rsk_nounc_noadaptation, xend=rsk_nounc_noadaptation, y=0, yend=0.13),size=1,linetype="solid", color="black")+ #hypothetical best guess stationary drought levels
  annotate(x=mean(rsk_nonstat_adaptation),y=0.13, geom="point",size=4,color="darkred")+
  annotate(x=mean(rsk_nounc_noadaptation),y=0.13, geom="point",size=4,color="black")+
  annotate(x=mean(rsk_nonstat_noadaptation),y=0.13, geom="point",size=4,color="orangered1")+
  annotate(x=mean(rsk_stat_noadaptation),y=0.13, geom="point",size=4,color="orange")+
  scale_colour_manual(name = '', 
                      values =c(nonstat_adaptation='darkred',nonstat_noadaptation='orangered1',stat_noadaptation='orange')) +
  scale_x_continuous("Crop Yield", expand = c(0, 0), limits = c(0, 1.1*max(dat_CY_adaptation$CY))) + 
  scale_y_continuous("Probability Density", expand = c(0, 0), limits = c(0, 0.14)) + 
  geom_segment(x=80,y=0.125,xend = 80,yend = 0.13,
               size=0.5,arrow = arrow(angle = 15, length = unit(0.1, "inches")))+  
  annotate(x=75,y=0.11, geom="label", label="Expected\nvalues",
           size=6,hjust="center",lineheight = .75,fill="lightgray")+
  annotate(x=91.5,y=0.125, geom="text",
           label="Neglecting all",
           color="black",size=6,hjust="right",lineheight = .75)+
  annotate(x=40,y=0.03, geom="text",
           label="Considering all",
           hjust="left",color="darkred",size=6,lineheight = .75)+
  geom_segment(x=40,y=0.029,xend = 40,yend = 0.004,
               size=0.5,color="darkred",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  annotate(x=20,y=0.05, geom="text",
           label="Neglecting adaptation,\nconsidering climate change\nand uncertainty", 
           hjust="left",color="orangered1",size=6,lineheight = .75)+
  geom_segment(x=20,y=0.049,xend = 20,yend = 0.006,
               size=0.5,color="orangered1",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  annotate(x=60,y=0.03, geom="text",
           label="Neglecting adaptation\nand climate change,\nconsidering uncertainty", 
           hjust="left",color="orange",size=6,lineheight = .75)+
  geom_segment(x=70,y=0.04,xend = 80,yend = 0.04,
               size=0.5,color="orange",arrow = arrow(angle = 15, length = unit(0.1, "inches")))+
  theme_classic()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        legend.text=element_text(size=18),
        legend.position =  "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                                               type = "closed")))+
  coord_flip()

# Organizing plots
figure1 <- ggarrange(b,c,a,
                     labels = c("b) Exposure Effect","   c) Risk","  a) Hazard"),
                     font.label = list(size = 18),
                     vjust =1.5, hjust =-.3,
                     ncol = 2, nrow = 2) 

# Coordinates of the start and end points of the leader line
x1=0.254; y1=0.40
x2=0.53; y2=0.895
# Adding leader lines and arrowheads 
figure<-figure1+geom_segment(aes(x=x1,xend=x1,y=y1,yend=y2),size=1,linetype="dotted")+
  geom_segment(aes(x=x1,xend=x1,y=y2-0.01,yend=y2),arrow = arrow(angle = 15, length = unit(0.2, "inches"),type="closed"))+
  geom_segment(aes(x=x1,xend=x2,y=y2,yend=y2),size=1,linetype="dotted")+
  geom_segment(aes(x=x2-0.01,xend=x2,y=y2,yend=y2),arrow = arrow(angle = 15, length = unit(0.2, "inches"),type="closed"))

# Saving a JPG and PDF file of the plot
ggsave("Risk_Interactions_Drought.jpg",width = 11, height = 8.5, units = c("in"),dpi = 600)
ggsave("Risk_Interactions_Drought.pdf",width = 11, height = 8.5, units = c("in"),dpi = 600)
# Time to clean up!
rm(list=ls())

