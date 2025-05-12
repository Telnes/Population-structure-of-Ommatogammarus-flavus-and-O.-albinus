library(ape) ## for reading alignment
library(pegas) ## for plotting haplotype

## read data
OmmLWS <- read.dna("../Supplementary_Materials/File S5 Ofa_Oalb_LWS_aln_trim.fa", format = "fasta")

## convert data to the necessary format (haploNet)
OmmLWSHaps <- haplotype(OmmLWS)
OmmLWSNet <- haploNet(OmmLWSHaps)

## get haplotypes
ind.hap<-with(
  stack(setNames(attr(OmmLWSHaps, "index"), rownames(OmmLWSHaps))),
  table(hap=ind, individuals=rownames(OmmLWS)[values]))

mydata <- as.data.frame(ind.hap)
good <- mydata[mydata$Freq == 1,]

depthlst <- strsplit(as.character(good$individuals), "_")
depth <- sapply(depthlst, "[", 4)
new.hap <- table(good$hap, depth)

## save to figure (uncomment png / comment svg if needed)
#png("Omm_haplotype_network_LWS.png", width = 800, height = 800)
svg("Omm_haplotype_network_LWS.svg")
plot(OmmLWSNet, size = attr(OmmLWSNet, "freq"), fast = FALSE,
     pie = new.hap, legend = c(-15, 3),
     scale.ratio = 1, label = F)##
dev.off() ## close and save the figure file
