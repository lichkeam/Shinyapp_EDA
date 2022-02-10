descript <- function( DATA_) {
  
  #read data table
  
  if(!is.null(DATA_$class)) DATA_ %<>% select(-class)
  
  gath.data = 
    DATA_[,-1] %>% gather(., key = "KEY", value = "value", -names(DATA_)[2]) %>% 
    transform( value = as.numeric(value)) %>%
    group_by_(names(DATA_)[2], "KEY") %>% # Group by KEY
    rename(MEAN = value) %>%
    summarise( 
      
      N = sum( !is.na(MEAN) ),
      Min = min( MEAN, na.rm = T ),
      Q1 = quantile( MEAN, 0.25, na.rm = T ),
      Med = median( MEAN,na.rm = T ),
      Mean = mean( MEAN, na.rm = T ),
      Q3 = quantile( MEAN, 0.75, na.rm = T ),
      Max = max( MEAN, na.rm = T ),
      IQR = ( Q3 - Q1 ),
      Range = ( Max - Min ),
      Std = sd( MEAN, na.rm = T ),
      CV = ( Std/Mean * 100 )
      
    ) %>% 
    
    mutate_if(is.numeric, round, digits = 4) %>%
    
    # replace NA as 0
    mutate_all(funs(ifelse(is.na(.), 0, .)))
  
  #gath.data$miRNA = gath.data$miRNA #%>% gsub("^[[:digit:]].*_", "", .)
  
  type = unique(gath.data[1]) %>% unlist %>% as.vector
  
  report = list()
  for(i in 1 : length(type)){
    
    output = gath.data %>% filter(!!as.symbol(names(gath.data)[1]) == type[i])
    report[[i]] = output
    #write.csv(output, paste0(OUTPUT.PATH_,"/", type[i], "_Descriptive_statistics.csv"), row.names = F)
    
  }
  names(report) = type
  #write_xlsx(report, paste0(OUTPUT.PATH_,"/", "_Descriptive_statistics.xlsx"))
  
  cat(paste0("file ----", "Descriptive_statistics ", "created successfully"))
  return(gath.data)
}

