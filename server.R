# .libPaths("/srv/shiny-server/cellplot/libs")

gitversion <- function(){ 
  return('no-version')
}

library(tercen)
library(dplyr)

library(shiny)
library(CellPlot)
futile.logger::flog.threshold(futile.logger::ERROR, name = "cellplotLogger")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  getCtx = reactive({
    # retreive url query parameters provided by tercen
    query = parseQueryString(session$clientData$url_search)
    
    token = query[["token"]]
    taskId = query[["taskId"]]
    
    # create a Tercen context object using the taskId and token
    tercenCtx(taskId=taskId, authToken=token)
    
    # for testing
    # options("tercen.workflowId"= "a65c601243e295f3b2e2d15dc1070185")
    # options("tercen.stepId"= "32-8")
    # tercenCtx()
  })
  
  getMode = reactive({
    query = parseQueryString(session$clientData$url_search)
    query[["mode"]]
  })

  plot.data <- reactive({
    ctx = getCtx()
    
    row.tbl = ctx %>% rselect() %>%
      mutate(.ri=seq(from=0, to=nrow(.)-1))
    
    # need some validation here
    colnames(row.tbl) = c("PValue", "Count", "LogEnrich", "Term", ".ri" )
    
    ctx %>% select(.ri, .y, .x , GenesSignificant) %>% 
      group_by(.ri) %>%
      summarise(log2FoldChange = list(.y), 
                padj = list(.x), 
                GenesSignificant = list(GenesSignificant)) %>%
      inner_join(row.tbl, by=".ri") %>%
      slice(1:input$nterms)
  })
  
  plot.cellplot<-reactive({
    
    x<-plot.data()
    
    cell.plot(x = setNames(x$LogEnrich, x$Term), 
              cells = x$log2FoldChange, 
              main ="GO enrichment", 
              x.mar = c(.5, 0.1),
              key.n = 7, 
              y.mar = c(.1, 0), 
              cex = 1.6, 
              cell.outer = 3, 
              bar.scale = .7, 
              space = .2)
  })
  
  plot.symplot<-reactive({
    
    x<-plot.data()
    sym.plot(x = setNames(x$LogEnrich, x$Term), 
             cells = x$log2FoldChange, 
             x.annotated = x$Count, 
             main = "GO enrichment",
             x.mar = c(.7, 0.1),
             y.mar = c(0.1,0.1),
             key.n = 7, 
             cex = 1.6, 
             axis.cex = .8, 
             group.cex = .7) 
  })
  
  plot.arclot<-reactive({
    
    x<-plot.data()
    x$up <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i>0] })
    x$dwn <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i<0] })
    arc.plot(x = setNames(x$LogEnrich, x$Term), 
             up.list = x$up, 
             down.list = x$dwn, 
             x.mar = c(0.7, 0.1), # c(0.7, 0.1)
             y.mar = c(0.3, 0.1)) # c(0.1, 0.1)
    
  })
  
  #plot.histogram<-reactive({
  #  x<-plot.data()
  #  y <- lapply(x, function (x) {
  #    x$Upregulated <- sapply(x$log2FoldChange, function (z) sum(z>0))
  #    x$Downregulated <- sapply(x$log2FoldChange, function (z) sum(z<0))
  #    x
  #  })
  #  yterms <- unique(unlist(lapply(y, function(x){
  #    x <- subset(x, pvalCutOff <= 0.05)
  #    x <- x[order(x$LogEnrich),]
  #    head(x, 9)$GO.ID
  #  })))
  #  
  #  par(mar = c(0,.5,2.5,8))
  #  go.histogram(y, go.alpha.term = "pvalCutOff", gene.alpha.term = "padj", 
  #               min.genes = 5, max.genes = 1e10, go.selection = yterms, show.ttest = T,
  #               main = "GO enrichment", 
  #               axis.cex = 1, lab.cex = 1.5, main.cex = 1.5)
  #})

  output$cellplot <- renderPlot({
    plot.cellplot()
  })
  
  output$downloadcellPlot <- downloadHandler(
    # specify file name
    filename = function(){
      paste0(input$outfile,".cellplot.",gitversion(),'.pdf')
    },
    content = function(filename){
      pdf(filename,height = 12.75, width = 13.50)
      x<-plot.data()
      cell.plot(x = setNames(x$LogEnrich, x$Term), 
                cells = x$log2FoldChange, 
                main ="GO enrichment", 
                x.mar = c(.5, 0.1),
                key.n = 7, 
                y.mar = c(.1, 0.1), 
                cex = 1.6, 
                cell.outer = 3, 
                bar.scale = .7, 
                space = .2)
      dev.off()
    }
    
  )
  
  output$symplot <- renderPlot({
    plot.symplot()
  }) 
  
  output$downloadsymPlot <- downloadHandler(
    # specify file name
    filename = function(){
      paste0(input$outfile,".symplot.",gitversion(),'.pdf')
    },
    content = function(filename){
      pdf(filename,height = 12.75, width = 13.50)
      x<-plot.data()
      sym.plot(x = setNames(x$LogEnrich, x$Term), 
               cells = x$log2FoldChange, 
               x.annotated = x$Count, 
               main = "GO enrichment",
               x.mar = c(.7, 0.1),
               y.mar = c(0.1,0.1),
               key.n = 7, 
               cex = 1.6, 
               axis.cex = .8, 
               group.cex = .7) 
      dev.off()
    }  
    )
  
  
  output$arcplot <- renderPlot({
    plot.arclot()
  })
  
  output$downloadarcPlot <- downloadHandler(
    # specify file name
    filename = function(){
      paste0(input$outfile,".arcplot.",gitversion(),'.pdf')
    },
    content = function(filename){
      pdf(filename,height = 12.75, width = 13.50)
      x<-plot.data()
      x$up <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i>0] })
      x$dwn <- lapply(Map(setNames, x$log2FoldChange, x$GenesSignificant), function (i) { i[i<0] })
      arc.plot(x = setNames(x$LogEnrich, x$Term), 
               up.list = x$up, 
               down.list = x$dwn, 
               x.mar = c(0.7, 0.1), # c(0.7, 0.1)
               y.mar = c(0.3, 0.1)) # c(0.1, 0.1)
      dev.off()
    }  
  )
  
  #output$histogram <- renderPlot({
  #  plot.histogram()
  #})
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  }
  )
})