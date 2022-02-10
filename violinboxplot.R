ggplot.viobox <- function( data, target, caption = NULL){
  
  #Specified a name for category column
  names(data)[2] = 'Group'
  
  viobox = 
    ggbetweenstats(data = data, 
                   x = 'Group', 
                   y = !!as.symbol(target), 
                   #p.adjust.method = "none",
                   pairwise.display = "everything",
                   messages = FALSE) +
    labs( title = target, caption = caption ) +
    
    theme( 
      plot.title = element_text(size = 30, vjust = 1.2, hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, face = "italic", color = "red"),
      axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5), #20
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size = 15, vjust = 0.5, hjust = 0.5), #20
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    rremove('x.grid')
  return(viobox)
}





