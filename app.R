#Import dependency source code and data
source("requirment.R")
source("boxplot.R")
source('violinboxplot.R')
source("pca.R")
source("descriptive_stat.R")
source("heatmap.R")
source("lineplot.R")
source("get.datatable.R")
example = readRDS('example_data.rds')


#load fonts
if(is.null(fonts())) font_import(prompt = F)

##UI
ui <- fluidPage(
  titlePanel(img(src = 'header.png', height = '100px', width = '1900px'), windowTitle = "EDA"),

  theme = shinytheme("flatly"),

  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$body(tags$link(rel = 'stylesheet', type = 'text/css', href = 'customized.css')),
      h3(strong("File upload")),
      fileInput('target_upload', 'Choose File in Local Device',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             '.csv')
                           ),
      hr(),
      h3(strong("Boxplot selection")),
      
      fluidRow(column(3,
                      radioButtons("rotate", h4("Rotate"), choiceNames = list( icon("times-circle"), icon("check-circle")),
                                     choiceValues = list( FALSE, TRUE), inline = T)),
               column(3,
                      radioButtons("jitter", h4("Jitter"), choiceNames = list( icon("times-circle"), icon("check-circle")),
                                     choiceValues = list( FALSE, TRUE), inline = T)),
               column(6,
                      radioButtons("test", h4("Test"), choiceNames = list( "Kruskal-Wallis", "Anova"),
                                   choiceValues = list( 'default', 'anova'), inline = T)
                      ),
               column(3,
                      numericInput("bin", h4("Bin"), value = 4))
               ),
      hr(),
      h3(strong("PCA selection(Advance only)")),
      fluidRow(
        column(6,
               sliderInput("pc", h4("Component axes"),
                  min = 1, max = 10, value = c(1, 2))), 
        column(6,
               radioButtons("pcaellipse", h4("Show ellipse"), choiceNames = list( icon("check-circle"), icon("times-circle") ),
                            choiceValues = list( TRUE, FALSE), inline = T))
        ),
      
      hr(),
      h3(strong("HeatMap selection")),
      fluidRow(column(6,
                      radioButtons("heatmap_scale", h4("Scale Method"), choiceNames = list( "Normalize", "Standardize"),
                                   choiceValues = list( 'normalize', 'scale'), inline = T))
               ),
      
      hr(),
      h3(strong("Download selection")),
      fluidRow(
        column(6,
               numericInput("width", h5("PDF-Width"), value = 13.66*0.45)
               ),
        column(6,
               numericInput("height", h5("PDF-Height"), value = 7.68*1.5*0.7)
               )
        ),
      
  
      fluidRow(
        column(4,
               downloadButton('downloadboxplot', 'BoxPlot_adj-bin')),
        column(4,
               downloadButton('downloadboxplot2', 'BoxPlot_default')),
        column(4,
               downloadButton('downloadvioboxplot', 'VioboxPlot')
               )
        ),
      hr(),
                 ),
  
      mainPanel(
        navbarPage(title = "View",
                   tabPanel(
                     strong("Guidance"),
                     mainPanel(
                       h1(strong("Hello and Welcome!")),
                       p(h3("This app provides an interactive EDA. There are some example outputs on those panels. Refresh the page if new data is imported")),
                       
                       div(p(h4("!-Please import the data with following format; otherwise, there will be ERRORS.")), class = 'alert'),
  
                       img(src = 'abstract.png'),
                       h3("Note: There are no limit of sample sizes, number of variables, and number of categories. Violin&Boxplot is time consuming, please wait.")
                       
                     )
                   ),
                   tabPanel(
                    strong("DataTable"),
                    mainPanel(
                      h2(strong("Data Overview")),
                      DT::dataTableOutput("sample_table", height = 800, width = 1000)
                    )
                  ),
                  tabPanel(
                    strong("Box Plot(Default)"),
                    mainPanel(
                      h2(strong("Individual Features")),
                      uiOutput("boxplots2")
                    )
                  ),
                  tabPanel(
                    strong("Box Plot(adjustable bin)"),
                    mainPanel(
                      h2(strong("Individual Features")),
                      uiOutput("boxplots")
                    )
                  ),
                  tabPanel(
                    strong("Violin&Box Plot"),
                    mainPanel(
                      h2(strong("Individual Features")),
                      uiOutput("vioboxplots")
                    )
                  ),
                  tabPanel(
                    strong("Expression Profile"),
                    mainPanel(
                      h2(strong("Individual Sample")),
                      plotOutput("lineplot1", height = 550, width = 1200),
                      h2(strong("Mean(group by categorical variable)")),
                      plotOutput("lineplot2", height = 550, width = 1200)
                    )
                  ),
                  tabPanel(
                    strong("Descriptive Statistics"),
                    mainPanel(
                      h2(strong("Group by category")),
                      DT::dataTableOutput("desc_stat", height = 800, width = 1000)
                    )
                  ),
                  tabPanel(
                    strong("PCA"),
                    mainPanel(
                      h2(strong("Classic")),
                      uiOutput("pcaplots1"),
                      h2(strong("Advance")),
                      uiOutput("pcaplots2")
                      
                    )
                  ),
                  tabPanel(
                    strong("Heat Map"),
                    mainPanel(
                      h2(strong("Agglomerative Hierarchical Clustering")),
                      plotlyOutput("heatmap",
                                   width = "1200px",
                                   height = "1000px",)
                      
                    )
                  )
                  
        )
      )
      
    )
    
  
  
)

##Backend
server <- function(input, output, session) {
  
  ##reactive : upload dataframe 
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(example)
    df <- read.csv(inFile$datapath)
    return(df)
  })
  
  
  ##Boxplot start
  
  upload.boxplot <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df = read.csv(inFile$datapath)
    feat.names = names(df)[-c(1,2)]
    for (i in 1 : length(feat.names) ) {
      local({
        
        plotname <- paste("plot", i, sep="")
        y.target = feat.names[i]
        output[[plotname]] <- renderPlot({
          
          ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, bin = input$bin, test = input$test)
          
        })
        
      })
    }  
    return(output)
    
  })

  output$boxplots <- renderUI({
    if(is.null(input$target_upload)){
      df = example
      feat.names = names(df)[-c(1,2)]
      for (i in 1 : length(feat.names) ) {
        local({
          
          plotname <- paste("plot", i, sep="")
          y.target = feat.names[i]
          output[[plotname]] <- renderPlot({
            
            ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, bin = input$bin, test = input$test)
            
          })
          
        })
      } 
    }
    upload.boxplot()
    y = ncol(df_products_upload())
    if(!is.null(y)){
      plot_output_list <- lapply(1 : y, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 550, width = 900)
      })
    }
    
  })
  
  ##Boxplot End
  
  
  ##Boxplot2 start
  
  upload.boxplot2 <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df = read.csv(inFile$datapath)
    feat.names = names(df)[-c(1,2)]
    for (i in 1 : length(feat.names) ) {
      local({
        
        plotname <- paste("plot2", i, sep="")
        y.target = feat.names[i]
        output[[plotname]] <- renderPlot({
          
          ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, test = input$test, adj.bin = F)
          
        })
        
      })
    }  
    return(output)
    
  })
  
  output$boxplots2 <- renderUI({
    if(is.null(input$target_upload)){
      df = example
      feat.names = names(df)[-c(1,2)]
      for (i in 1 : length(feat.names) ) {
        local({
          
          plotname <- paste("plot2", i, sep="")
          y.target = feat.names[i]
          output[[plotname]] <- renderPlot({
            
            ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, test = input$test, adj.bin = F)
            
          })
          
        })
      } 
    }
    upload.boxplot2()
    y = ncol(df_products_upload())
    if(!is.null(y)){
      plot_output_list <- lapply(1 : y, function(i) {
        plotname <- paste("plot2", i, sep="")
        plotOutput(plotname, height = 550, width = 900)
      })
    }
    
  })
  
  ##Boxplot2 End
  
  ############################
  ##Vioboxplot start
  
  upload.vioboxplot <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df = read.csv(inFile$datapath)
    feat.names = names(df)[-c(1,2)]
    for (i in 1 : length(feat.names) ) {
      local({
        
        plotname <- paste("vioboxplot", i, sep="")
        y.target = feat.names[i]
        output[[plotname]] <- renderPlot({
          
          ggplot.viobox(df, y.target)
          
        })
        
      })
    }  
    return(output)
    
  })
  
  output$vioboxplots <- renderUI({
    if(is.null(input$target_upload)){
      df = example
      feat.names = names(df)[-c(1,2)]
      for (i in 1 : length(feat.names) ) {
        local({
          
          plotname <- paste("vioboxplot", i, sep="")
          y.target = feat.names[i]
          output[[plotname]] <- renderPlot({
            
            ggplot.viobox(df, y.target)
            
          })
          
        })
      } 
    }
    upload.vioboxplot()
    y = ncol(df_products_upload())
    if(!is.null(y)){
      plot_output_list <- lapply(1 : y, function(i) {
        plotname <- paste("vioboxplot", i, sep="")
        plotOutput(plotname, height = 550, width = 900)
      })
    }
    
  })
  
  ##Vioboxplot End
  
  
  ## Download Start ##
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('descriptive_statistics-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(path) {
  #     df <- df_products_upload()
  #     result = descript(df)
  #     write.csv(result, path, row.names = F)
  #   }
  # )
  
  output$downloadboxplot <- downloadHandler(
    filename = function() {
      paste('boxplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(path) {
      pdf(path, width = input$width, height = input$height)
      inFile <- input$target_upload
      if (is.null(inFile)){
        
        df = example
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, bin = input$bin, test = input$test))
            
          })
        }
        dev.off()
        
      }else{
        df <- read.csv(inFile$datapath)
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, bin = input$bin, test = input$test))
            
          })
        }
        dev.off()
      }
    }
  )
  
  
  output$downloadboxplot2 <- downloadHandler(
    filename = function() {
      paste('boxplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(path) {
      pdf(path, width = input$width, height = input$height, family = 'serif', fonts = 'serif')
      inFile <- input$target_upload
      if (is.null(inFile)){
        
        df = example
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, adj.bin = F, test = input$test))
            
          })
        }
        dev.off()
      }else{
        df <- read.csv(inFile$datapath)
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.box(df, target = y.target, rotate.plot = input$rotate, jitter = input$jitter, adj.bin = F, test = input$test))
            
          })
        }
        dev.off()
      }
    }
  )
  
  
  output$downloadvioboxplot <- downloadHandler(
    filename = function() {
      paste('vioboxplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(path) {
      pdf(path, width = input$width, height = input$height)
      inFile <- input$target_upload
      if (is.null(inFile)){
        
        df = example
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.viobox(df, y.target))
            
          })
        }
        dev.off()
        
      }else{
        df <- read.csv(inFile$datapath)
        names(df)[2] = "Group" #easy to specified a variable name to ggplot
        feat.names = names(df)[-c(1,2)]
        for (i in 1 : length(feat.names) ) {
          local({
            
            plotname <- paste("plot", i, sep="")
            y.target = feat.names[i]
            print(ggplot.viobox(df, y.target))
            
          })
        }
        dev.off()
      }
    }
  )
  
  
  ## Download End ##
  
  #Lineplot1(all sample)
  upload.lineplot1 <- reactive({

    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile)) df = example
    #df = read.csv(inFile$datapath)
    df <- df_products_upload()
    output[['lineplot1']] = renderPlot({
      
      lineplot.all(df)
      
    })
    return(output)
    
  })

  
  output$lineplot1 = renderUI({
    if(is.null(input$target_upload)){
      df = example
      output[['lineplot1']] = renderPlot({
        
        lineplot.all(df)
        
      })
      plotOutput("lineplot1", height = 550, width = 700)

    }else{
      upload.lineplot1()
      plotOutput("lineplot1", height = 550, width = 700)

    }
    
  })
  
  #Lineplot2(mean of category)
  upload.lineplot2 <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile)) df = example
    df = read.csv(inFile$datapath)
    output[['lineplot2']] = renderPlot({
      
      lineplot.mean(df)
      
    })
    return(output)
    
  })
  
  
  output$lineplot2 = renderUI({
    if(is.null(input$target_upload)){
      df = example
      output[['lineplot2']] = renderPlot({
        
        lineplot.mean(df)
        
      })
      plotOutput("lineplot2", height = 550, width = 700)

    }else{
      upload.lineplot2()
      plotOutput("lineplot2", height = 550, width = 700)

    }
    
  })
  
  #PCA1
  upload.pcaplot1 <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile)) df = example
    df = read.csv(inFile$datapath)
    output[['PCA1']] = renderPlot({

      PCA1(df)

    })
    return(output)

  })
  
  
  output$pcaplots1 = renderUI({
    if(is.null(input$target_upload)){
      df = example
      output[['PCA1']] = renderPlot({
        
        PCA1(df)
        
      })
      plotOutput("PCA1", height = 550, width = 700)
    }else{
      upload.pcaplot1()
      plotOutput("PCA1", height = 550, width = 700)
    }

  })
  
  #PCA2
  upload.pcaplot2 <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile)) df = example
    df = read.csv(inFile$datapath)
    output[['PCA2']] = renderPlot({
      
      PCA2(df, axes_ = c(as.numeric(input$pc[1]), as.numeric(input$pc[2])), ellipses = input$pcaellipse)
      
    })
    return(output)
    
  })
  
  
  output$pcaplots2 = renderUI({
    if(is.null(input$target_upload)){
      df = example
      output[['PCA2']] = renderPlot({
        
        PCA2(df, axes_ = c(as.numeric(input$pc[1]), as.numeric(input$pc[2])), ellipses = input$pcaellipse)
        
      })
      plotOutput("PCA2", height = 550, width = 700)
    }else{
      
      upload.pcaplot2()
      plotOutput("PCA2", height = 550, width = 700)
      
    }
    
  })
  
  #HeatMap
  upload.heatmap <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    if (is.null(inFile)) df = example
    df = read.csv(inFile$datapath)
    output[['heatmap']] = renderPlotly({
      
      heat_map(df, transform.method = input$heatmap_scale)
      
    })
    return(output)
    
  })
  
  
  output$heatmap = renderUI({
    if(is.null(input$target_upload)){
      df = example
      output[['heatmap']] = renderPlotly({
        
        heat_map(df, transform.method = input$heatmap_scale)
        
      })
      plotly::renderPlotly("heatmap")
      #plotOutput("heatmap")
    }else{
      upload.heatmap()
      plotly::renderPlotly("heatmap")
      #plotOutput("heatmap")
    }
    
  })  
  
  #sample_table: show data output
  output$sample_table<- DT::renderDataTable({
    
    df <- df_products_upload()
    get.datatable(df)
    
  })
  
  output$desc_stat<- DT::renderDataTable({
    
    df <- df_products_upload()
    result = descript(df)
    get.datatable(result)
    
    # descriptive_statistic()
    
  })
  
}
shinyApp(ui, server)
