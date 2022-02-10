
lineplot.all <- function(data, x.axis.label.rotate = T){
  
  #rename
  names(data)[1:2] = c("Sample_ID", "category")
  
  #data tranpose
  input = gather(data, key = "Key", value = "Value", -Sample_ID, -category)
  
  res = ggplot(data = input, aes(x = Key, y = Value, group = Sample_ID, color = category) )+
      theme_light() + 
      geom_line(size = 0.5)+
      geom_point(size = 2) +
      {if(x.axis.label.rotate == T) theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5, hjust = 1)) else {theme(axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5))}} +
    
      theme( 
        plot.title = element_text(size = 33, vjust = 1.2, hjust = 0.5),
        #axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5), #20
        axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #legend.position = "top",
        legend.position = "top",
        legend.key.size = unit(2, "cm"),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        #legend.title = element_text(color = "black", size = 16),
        legend.background = element_rect(colour = "white"),
        legend.text = element_text( size = 15 )
      )
  return(res)
  
}


lineplot.mean <- function(data){
  
  #rename
  names(data)[1:2] = c("Sample_ID", "category")
  
  ##data tranpose
  input = gather(data, key = "Key", value = "Value", -Sample_ID, -category) %>% 
    group_by(category, Key) %>% summarise(Value = mean(Value, na.rm = T))
  
  res = ggplot(data = input, aes(x = Key, y = Value, group = category, color = category) )+
    theme_light() + 
    geom_line(size = 0.5)+
    geom_point(size = 2) +
    theme( 
      plot.title = element_text(size = 33, vjust = 1.2, hjust = 0.5),
      axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5), #20
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #legend.position = "top",
      legend.position = "top",
      legend.key.size = unit(2, "cm"),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      #legend.title = element_text(color = "black", size = 16),
      legend.background = element_rect(colour = "white"),
      legend.text = element_text( size = 15 )
    )
  return(res)
  
}

