library(ggplot2)
library(openxlsx)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(data.table)
setwd ("C:/Users/Simba/Documents/Популяции O_flavus_albinus/Carotenoides")

#g<-substr("abcdef", 2, 4)
#Try_read_qPCR_data_Ommatogammarus_eyes_from_deff_depth
es<-read.xlsx("qPCR.xlsx", sheet=2, cols=2:6)
alldat <- es[es$Control < 30,] #all pass!
#alldat$Group <- substr(alldat$Sample, 5, 5)

ch <- ifelse(alldat$Exp > 30, 4, 19)
alldat$Species <- substr(alldat$Sample, 1, 5)

Ofladat <- alldat[alldat$Species == "O_fla",]
alldat$Depth
alldat$Species <- factor(alldat$Species, levels = c("O_fla", "O_alb"))


##for plot2
alldat$group2 <- ifelse(alldat$"Depth"<300, "Shallow", "Deep") ##for plot2
alldat$group2 <- factor(alldat$group2, levels = c("Shallow", "Deep"))

library("ggplot2")

Ofla <-  ggplot(data = Ofladat, aes(x=factor(Depth),y=deltaCt))+
  geom_boxplot() + geom_point()
  
Ofla

Oalbdat <- alldat[alldat$Species == "O_alb",]
alldat$Depth

library("ggplot2")

Oalb <-  ggplot(data = Oalbdat, aes(x=factor(Depth),y=deltaCt))+
  geom_boxplot() + geom_point() 
Oalb

species <- c("O. flavus", "O. albinus")
names(species) <- c("O_fla", "O_alb")

plot<-ggplot(data = alldat, aes(x=factor(Depth),y=deltaCt, fill=Species, col=Species))+
  geom_boxplot() + geom_point() + 
  facet_grid(. ~ Species, labeller = labeller(Species = species)) + 
  scale_fill_manual(values = c("orange", "beige")) +
  scale_color_manual(values = c("black", "red4")) + 
  theme_bw(base_size = 12) + 
  labs(title = NULL, x = "Depth, m",  y = "deltaCt") +  
  theme(legend.position = "none", strip.text.x = element_text(face = "italic")) +
  expand_limits(y=5)
plot

ggsave("Opsins_square.svg", width = 7, height = 5)

ggplot(data = alldat, aes(x=Species, y=deltaCt, fill=Species, col=Species))+
  geom_boxplot() + geom_point() +
xlab("") + ylab("deltaCt") +
 
  scale_fill_manual(values = c("orange", "beige")) +
  scale_color_manual(values = c("black", "red4")) +
  theme(legend.position="none")
ggsave("Opsins_two_species.svg")

library(coin)
wilcox_test(alldat$deltaCt ~ alldat$Species)

