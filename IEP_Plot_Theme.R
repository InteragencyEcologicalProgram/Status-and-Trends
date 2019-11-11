
## Create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 10, face = "plain"),
      axis.title.y = element_text(size = 10, face = "plain",
                                  margin=margin(t = 0, r = 10, b = 0, l = 0)), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),  
      #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
      plot.margin = unit(c(0.25, 0.6, 0.1, 0.4), units = "cm"), #adjusted the "top" and "right" values so nothing is cut off
      plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
      legend.text = element_text(size = 9, face = "plain"),
      legend.title=element_text(size=10)
    )
}
