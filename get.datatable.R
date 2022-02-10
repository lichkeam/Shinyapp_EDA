get.datatable <- function(data, caption = '', scrollY = 350) {
  return(
      
    DT::datatable(
      data,
      escape = F,
      width = "100%",
      extensions = c('Buttons','Scroller', 'RowReorder', 'ColReorder'),
      # extensions = c('Buttons', 'FixedColumns'),
      options = list(
        
        scrollX = TRUE,
        scrollY = scrollY,
        scroller = TRUE,
        searchHighlight = TRUE,
        
        # rowReorder = TRUE,
        # colReorder = TRUE,
        
        dom = c('Bfrtip'),
        buttons = list(c('copy', 'csv', 'excel', 'pdf', 'print')),
        # autoWidth = TRUE,
        # fixedColumns = TRUE,
        columnDefs = list(
          # list(width = '100px', targets = 0),
          list(className = 'dt-center', targets = seq(ncol(data)+1)[-1]-1 )
        ),
        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#527e72', 'color': '#FFFFFF'});",
                            "}")
        
      )
    )
      

  )
}