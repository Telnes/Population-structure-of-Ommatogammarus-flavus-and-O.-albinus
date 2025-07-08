library(openxlsx)

color_changes <- read.xlsx("./2025-06-18_Omm_colors.xlsx", detectDates = FALSE)

## Calculate color index (it's actually R/B)
color_changes$Red <- strtoi(as.hexmode(substr(color_changes$Pereon_color, 1, 2)), base = 16)
color_changes$Green <- strtoi(as.hexmode(substr(color_changes$Pereon_color, 3, 4)), base = 16)
color_changes$Blue <- strtoi(as.hexmode(substr(color_changes$Pereon_color, 5, 6)), base = 16)
color_changes$RtoB <- color_changes$Red/color_changes$Blue
color_changes$RtoG <- color_changes$Red/color_changes$Green
color_changes$GtoB <- color_changes$Green/color_changes$Blue

library(ggplot2)
ggplot(color_changes, aes(x=factor(Depth), y=RtoB))  + 
  #geom_boxplot() + 
  geom_violin(draw_quantiles = 0.5) + 
  geom_jitter(width = .2) + 
  expand_limits(y=0) + 
  ggtitle("O. flavus body color")


#ggplot(color_changes, aes(x=Length, y=RtoB))  + 
#  geom_point() + 
#  geom_smooth(method='lm') + 
#  facet_grid(~Depth)

pB <- 
ggplot(color_changes[color_changes$Depth == 1000,], aes(x=Length, y=RtoB))  + 
  geom_point() + 
  geom_smooth(method='lm') +
  ylab("R/B color index") + xlab("Body length, mm") + 
  expand_limits(x=5, y=0) + 
  ggtitle("*O. flavus* body color *vs.* body length, sample from 1000 m") + 
  theme_bw(base_size = 12) + 
  theme(plot.title = ggtext::element_markdown(size = 12, hjust = .5))
pB
  

color_changes$tocolor <- paste0("#", color_changes$Pereon_color)

ggplot(color_changes, aes(x=Length, y=RtoB))  + 
  geom_point(color=color_changes$tocolor, size=3) + 
  #geom_smooth(method='lm') + 
  facet_grid(~Depth) + 
  theme_bw()


color_changes_adults <- color_changes[color_changes$Length > 15, ]
table(color_changes_adults$Depth)


library(dplyr)
color_changes_adults %>% group_by(Depth) %>% summarise(mediancol = median(RtoB)) -> median_RtoB

pA <- 
ggplot(color_changes_adults, aes(x=factor(Depth), y=RtoB))  + 
  #geom_boxplot() + 
  geom_violin(draw_quantiles = 0.5, aes(color=factor(Depth))) + #fill="orange") + 
  geom_jitter(width = .1) + 
  expand_limits(y=0) + 
  ylab("R/B color index") + xlab("Depth, m") + 
  ggtitle("*O. flavus* body color, adults (>15 mm)") + 
  theme_bw(base_size = 12) + 
  theme(plot.title = ggtext::element_markdown(size = 12, hjust = .5)) + 
  ## add mean color values for this depth as color for the violin plots
  scale_color_manual(values = c("#a35b26", "#ab6024", "#9c5e24", "#9b6f35", "#927d57"), guide='none')
pA


## this is 
#ggplot(color_changes_adults, aes(x=Length, y=RtoB))  + 
#  geom_point(color=color_changes_adults$tocolor, size=3) + 
#  #geom_smooth(method='lm') + 
#  facet_grid(~Depth) + 
#  theme_bw()

library(ggpubr)
pcol <- ggarrange(pA, pB, nrow=2)

ggsave("Fig6new.png", pcol, device = png, width=175, height=175, units = "mm")

