## Sources and libraries
## https://johnbhorne.wordpress.com/2016/09/15/still-making-haplotype-networks-the-old-way-how-to-do-it-in-r/
## also
## https://jimmyodonnell.wordpress.com/2014/06/16/legend-for-haplotype-networks-in-r/

library(ape)
library(pegas)

## read data
Omm_pop <- read.dna("../Supplementary_Materials/File S4 OmmPop_608_bp.fa", format = "fasta")

## convert data to the necessary format
OmmHaps <- haplotype(Omm_pop)
OmmNet <- haploNet(OmmHaps)
plot(OmmNet)
ind.hap<-with(
  stack(setNames(attr(OmmHaps, "index"), rownames(OmmHaps))),
  table(hap=ind, individuals=rownames(Omm_pop)[values]))

mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

## Get names from the data
#names <- c(rep("Omm_c_sp",33), rep("O_flavus",4), rep("O_albinus",3), rep("Om_c_m", 2))
#names <- c(rep("O_albinus",3), rep("O_flavus",4), rep("Omm_c_m", 2), rep("Om_c_sp",33))

#names <- c(rep("Ayaya_O_flavus",12), rep("Ayaya_O_albinus",4), rep("Bay_O_flavus", 5), rep("Bay_O_albinus",9), rep("BG_O_flavus",10), rep("BG_O_albinus",11), 
#rep("BK_O_flavus",27), rep("BK_O_albinus",20), rep("Br_O_flavus",2), rep("Br_O_albinus",2), 
#rep("Bug_O_flavus",4), rep("Bug_O_albinus",3), rep("List_O_flavus",2), rep("List_O_albinus",2), rep("Pos_O_albinus",2), 
#rep("Slud_O_flavus",4), rep("Slud_O_albinus",3))

names <- c(rep("Ayaya", 16), rep("Bay", 14), rep("BG", 21), 
           rep("BK",47), rep("Br",4), rep("Bug",7), rep("List",4), rep("Pos",2), 
           rep("Slud",8))

new.hap <- table(good$hap, names)
good[order(good$hap), ]

plot(OmmNet)
par(xpd = TRUE)
plot(OmmNet, size=attr(OmmNet, "freq"), scale.ratio = 1, cex = 0.8, labels=TRUE, 
     pie = new.hap, font=2, fast=TRUE, threshold=0, show.mutation = , legend = c(10, 0))
replot()
svg("Omm_Pop.svg")


dev.off() ## close and save the figure file
