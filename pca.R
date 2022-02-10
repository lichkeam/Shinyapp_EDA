PCA1 <- function( data, scale_ = TRUE, labelsize_ = 5, loadings.label_ = F, na.replace = 'mean' ) {
  
  names(data)[2] = 'category'
  #Interpret X matrix and replace NAs by mean
  x.mat = data[-c(1,2)] %>% as.data.frame() %>% sapply(., as.numeric)
  if(apply(x.mat, 2,is.na) %>% any()){
    x.mat = if(na.replace == 'mean') apply(x.mat, 2, replace.mean) else apply(x.mat, 2, replace.median)
    
  }
  #comp.x.mat = apply(x.mat, 2, mean_replace)

  #Principal component
  pri = prcomp(x.mat, scale. = scale_)
  
  PCAplot = 
    autoplot(pri, data = data, colour = names(data)[2], size = 4, loadings.label = loadings.label_,
             loadings.colour = 'navyblue', loadings.label.size = labelsize_ )+
    theme_bw()+
    geom_text_repel(label = data[,1])+
    theme(
      legend.position = "top",
      axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      #legend.background = element_rect(fill = "wheat1"),
      legend.text = element_text( size = 20 ),
      axis.title.x = element_text( size = 25 ),
      axis.title.y = element_text( size = 25 ),
      #legend.title = element_text( color = "deepskyblue4", size = 20)
      legend.title = element_blank() 
    )
  
  #open pdf
  #pdf(file = paste(OUTPUT.PATH_, "/", "PCA", ".pdf", sep = ""), width=13.66, height=7.68*1.5)
  return(PCAplot)
  #dev.off()
  
}


PCA2 <- function(data, classname, scale_ = T, na.replace = 'mean', axes_ = c(1,2), ellipses = T){
  
  names(data)[c(1,2)] = c('Sample_ID', 'category')
  #Interpret X matrix and replace NAs by mean
  x.mat = data[-c(1,2)] %>% as.data.frame() %>% sapply(., as.numeric)
  if(apply(x.mat, 2,is.na) %>% any()){
    x.mat = if(na.replace == 'mean') apply(x.mat, 2, replace.mean) else apply(x.mat, 2, replace.median)
    
  }
  #comp.x.mat = apply(x.mat, 2, mean_replace)
  
  #Principal component
  pri = prcomp(x.mat, scale. = scale_)
  
  PCAplot = fviz_pca_ind(
    
    pri,axes = axes_, 
    label = "none",
    addEllipses = ellipses, 
    alpha.ind = 0.7,
    geom = 'point',
    #pointsize = 2.2,
    #ellipse.level = 0.7, #palette = c("red", "blue", "green"),
    #select.ind = list(name = example$Sample_ID), 
    habillage = data$category, 
    title = ""#, xlab = "PC1", ylab = "PC2" 
  ) +
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.title.x = element_text( size = 25 ),
          axis.title.y = element_text( size = 25 ))+
    scale_shape_manual(values = rep(19, ncol(data)))
  print(PCAplot)
}

replace.mean <- function(vec) {
  
  mea <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- mea 
  return(vec)
  
}

replace.median <- function(vec) {
  
  med <- median(vec, na.rm = TRUE)
  vec[is.na(vec)] <- med 
  return(vec)
  
}