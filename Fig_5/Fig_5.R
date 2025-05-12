library(ggplot2)
library(openxlsx)

## read xlsx
cr<-read.xlsx ("../Supplementary_Materials/Supplementary_tables.xlsx", sheet = "Table S5. Carotenoids", startRow = 2) #cols=c(1:3)

cr$Species <- substr(cr$Species, 1, 9)
cr$Species <- factor(cr$Species, levels = c("O_flavus_", "O_albinus"))

cr$Carotenoids_ppm <- cr$`Carotenoids,.ppm_corrected`
cr$Depth

Ofladat <- cr[cr$Species == "O_flavus_",]
Ofla <-  ggplot(data = Ofladat, aes(x = factor(Depth),y = Carotenoids_ppm))+
  geom_boxplot() + geom_point() 
Ofla


Oalbdat <- cr[cr$Species == "O_albinus",]

Oalb <-  ggplot(data = Oalbdat, aes(x = factor(Depth),y = Carotenoids_ppm))+
  geom_boxplot() + geom_point() 
Oalb


species <- c("O. flavus", "O. albinus")
names(species) <- c("O_flavus_", "O_albinus")

plot<-ggplot(data = cr, aes(x=factor(Depth),y=Carotenoids_ppm, fill=Species, col=Species))+
  geom_boxplot() + geom_point() + 
  facet_grid(. ~ Species, labeller = labeller(Species = species)) + 
  scale_fill_manual(values = c("orange", "beige")) +
  scale_color_manual(values = c("black", "red4")) + 
  theme_bw(base_size = 12) + 
  labs(title = NULL, x = "Depth, m",  y = "Carotenoids/ppm") +  
  theme(legend.position = "none", strip.text.x = element_text(face = "italic")) +
  expand_limits(y=55)
plot


plot
ggsave("Carot_new.svg", width = 7.4, height = 5.3)


plot3<-ggplot(data = cr, aes(x=Depth,y=Carotenoids_ppm)) +
  geom_point() + geom_smooth(method = lm) +
  facet_grid(. ~ Species) +
  expand_limits(y=0) + xlab("Depth, m") + ylab("Carotenoids/ppm") +
  theme(legend.position="none") +
  labs(title = NULL, 
       x = "Depth, m", 
       y = "Carotenoids/ppm") +
  theme_bw(base_size = 12)
plot3



ggsave("Ceroteenoides point lm new.svg", width = 7.4, height = 3) #4.8 good
