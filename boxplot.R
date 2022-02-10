ggplot.box = function(data, target, rotate.plot = FALSE, jitter = FALSE, test = 'default', adj.bin = T, bin = 1, x.axis.label.rotate = F){
  
  names(data)[2] = "Group" #Specified a variable name to ggplot
  
  #Range for bin
  min.y = round(min(as.numeric(unlist(data[target]))))
  max.y = round(max(as.numeric(unlist(data[target]))))
  
  pp = 
    ggboxplot(data, x = "Group", y = target, fill = "Group") +
    #geom_text(aes(label = outlier_sample), na.rm=TRUE, nudge_x = 0.1, nudge_y = 0.03, size = 5)+
    labs( title = target ) +
    stat_n_text(size = 5) + 
    theme_ipsum() + 
    {if(rotate.plot == TRUE) coord_flip() } +
    {if(rotate.plot == FALSE) {rremove('x.grid')} else {rremove('y.grid')}} +
    {if(rotate.plot == TRUE){ if(test == 'default'){stat_compare_means(label.x.npc = "right",label.y.npc = "bottom")} else {stat_compare_means(method = 'anova',label.x.npc = "right",label.y.npc = "bottom")} }else{ if(test == 'default'){stat_compare_means()} else{stat_compare_means(method = 'anova')}} }+
    {if(adj.bin == T) scale_y_continuous(breaks = seq(min.y, max.y, bin))} +
    #scale_y_continuous(limits = c(0, max(data.MSE$RMSE)+0.2), breaks = seq(0, 1, by = 0.1)) +
    {if(jitter == TRUE) geom_jitter(alpha = 0.6)} +
    {if(x.axis.label.rotate == T) theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) else {theme(axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5, family = 'serif'))}} +
    theme(
      plot.title = element_text(size = 33, vjust = 1.2, hjust = 0.5, family = 'serif'),
      #axis.text.x = element_text(size = 15, vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5, family = 'serif'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #legend.position = "top",
      legend.position = "none",
      legend.key.size = unit(1, "cm"),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      #legend.title = element_text(color = "black", size = 16),
      legend.background = element_rect(colour = "white"),
      legend.text = element_text( size = 15, family = 'serif')
    )
  
  return(pp)
}
