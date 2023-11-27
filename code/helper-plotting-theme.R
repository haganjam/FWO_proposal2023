
# customised plotting theme
theme_meta <- 
  function(base_family = "") {
    theme(panel.background=element_rect(fill="white", colour="black", linetype="solid", 
                                        linewidth = 0.85),
          panel.grid = element_blank(),
          axis.text = element_text(colour="black",size=9.5),
          axis.title = element_text(colour = "black", size = 10.5),
          legend.text = element_text(colour = "black", size = 9.5),
          legend.key = element_rect(fill = NA))
  }

### END