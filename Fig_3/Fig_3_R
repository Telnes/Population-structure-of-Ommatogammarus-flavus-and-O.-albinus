library(ape)

## Sources and libraries
## https://johnbhorne.wordpress.com/2016/09/15/still-making-haplotype-networks-the-old-way-how-to-do-it-in-r/
## also
## https://jimmyodonnell.wordpress.com/2014/06/16/legend-for-haplotype-networks-in-r/

#install.packages("pegas")
library(pegas)

## read data
OmmCOI <- read.dna("BK Omm aln trim del 2 sample.fa", format = "fasta")


## convert data to the necessary format
COIHaps <- haplotype(OmmCOI)
COINet <- haploNet(COIHaps)

ind.hap<-with(
  stack(setNames(attr(COIHaps, "index"), rownames(COIHaps))),
  table(hap=ind, individuals=rownames(OmmCOI)[values]))

plot(COINet, size = attr(COINet, "freq"), fast = FALSE,
     legend = c(-30, 0),
     scale.ratio = 0.01, label = F) ## pie = new.hap


mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

depthlst <- strsplit(as.character(good$individuals), "_")
depth <- sapply(depthlst, "[", 5)
#length(depth)

new.hap <- table(good$hap, depth)

png("Omm_COI_haplotype_network_eyes_new.png", width = 800, height = 800)
svg("Omm_COI_haplotype_network_eyes_new.svg")

par(mar=c(0,0,0,0))
plot(COINet, size = attr(COINet, "freq"), fast = FALSE,
     pie = new.hap, legend = c(-40, 20),
     scale.ratio = 0.5, label = F) ##

dev.off() ## close and save the figure file




