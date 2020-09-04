#TESTING my mean line legend

library(ggplot2)

mline = ggplot() + 
  annotate("segment", color = "red", linetype = "dashed", 
             size = 0.9, x = 0, xend = 1, y = 0, yend = 0)+
  annotate("segment", color = "white", 
           size = 0.9, x = 1, xend = 2, y = 0, yend = 0)+
  annotate("text", label = "Long-term Average", x = 1.1, y = 0, hjust = 0)+
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        panel.background = element_rect(fill = "white"), axis.ticks=element_blank())

ggsave("mline.png", mline, dpi=300, units="cm", width=9.3, height=1)


#also do a missing-data-point 
mpoint = ggplot() + 
  annotate("segment", color = "white", 
           size = 0.9, x = 1.5, xend = 2, y = 0, yend = 0)+
  annotate("point", shape = 24, size = 3, 
           fill = "tan2", color = "gray10", x = 1.05, y = 0)+
  
  annotate("text", label = "No Survey", x = 1.1, y = 0, hjust = 0)+
  theme(axis.text = element_blank(), axis.title = element_blank(), 
        panel.background = element_rect(fill = "white"), axis.ticks=element_blank())

mpoint

ggsave(file.path(fig_root_static,"mpoint.png"), mpoint, dpi=300, units="cm", width=9.3, height=1)
