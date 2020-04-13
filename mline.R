#TESTING my mean line legend

library(ggplot2)
library(smonitr)

mline = ggplot() + 
  annotate("segment", color = "red", linetype = "dashed", 
             size = 0.9, x = 0, xend = 1, y = 0, yend = 0)+
  annotate("segment", color = "white", 
           size = 0.9, x = 1, xend = 2, y = 0, yend = 0)+
  annotate("text", label = "Long-term Average", x = 1.1, y = 0, hjust = 0)+
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        panel.background = element_rect(fill = "white"), axis.ticks=element_blank())

ggsave("mline.png", mline, dpi=300, units="cm", width=9.3, height=1)
