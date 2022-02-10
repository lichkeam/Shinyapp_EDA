heat_map <- function(data, transform.method = 'normalize'){
  
  #data transformation
  if(transform.method == 'normalize') data[,-c(1,2)] = normalize(data[,-c(1,2)]) else data[,-c(1,2)] = scale(data[,-c(1,2)]) 
  
  #remove sampleID
  names(data)[1] = 'Sample_ID'
  data = data %>% `rownames<-`(NULL) %>% column_to_rownames('Sample_ID')
  
  #heatmap
  res = heatmaply(data,
                  column_text_angle = 90,
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                    low = "red", 
                    mid = "black",
                    midpoint = 0.5,
                    high = "green"
                  ))
  return(res)
  
}

