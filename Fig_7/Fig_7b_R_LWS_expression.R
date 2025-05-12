library(openxlsx) ## read.xlsx
library(ggplot2) ## plots (ggplot)
library(coin) ## wilcox_test

## read raw data
es <- read.xlsx("../Supplementary_Materials/Supplementary_tables.xlsx", sheet=6, cols=2:6)

## sample quality control: all Ct values for the reference gene below 30 cycles
alldat <- es[es$Control < 30,] #all pass!

## add `Species` column from Sample IDs
alldat$Species <- substr(alldat$Sample, 1, 5)

Ofladat <- alldat[alldat$Species == "O_fla",]
alldat$Depth
alldat$Species <- factor(alldat$Species, levels = c("O_fla", "O_alb"))


##for plot2
alldat$group2 <- ifelse(alldat$"Depth"<300, "Shallow", "Deep") ##for plot2
alldat$group2 <- factor(alldat$group2, levels = c("Shallow", "Deep"))

## species names to label subpanels
species <- c("O. flavus", "O. albinus")
names(species) <- c("O_fla", "O_alb")

## plot
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

## save plot to file
ggsave("Opsins_square.svg", width = 7, height = 5)


## plot combined data for each specise
ggplot(data = alldat, aes(x=Species, y=deltaCt, fill=Species, col=Species))+
  geom_boxplot() + geom_point() +
  xlab("") + ylab("deltaCt") +
  scale_fill_manual(values = c("orange", "beige")) +
  scale_color_manual(values = c("black", "red4")) +
  theme(legend.position="none")
ggsave("Opsins_two_species.svg")


wilcox_test(alldat$deltaCt ~ alldat$Species)

