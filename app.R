library(shiny)
library(DT)
library(rvest)
library(dplyr)

get_adp <- function(inj , ppr , keeper , size){
  url <- sprintf('http://www03.myfantasyleague.com/2016/adp?COUNT=500&POS=*&INJURED=%s&CUTOFF=20&FRANCHISES=%s&IS_PPR=%s&IS_KEEPER=%s&IS_MOCK=0&TIME=',inj, size,ppr,keeper)
  adp <- url %>%
    html() %>%
    html_nodes('.report') %>%
    html_table()
  
  adp <- adp[[1]]
  
  pl_split <- data.frame(do.call('rbind', strsplit(as.character(adp$Player),',',fixed=TRUE)), stringsAsFactors=FALSE)
  pl_split$Lastname = gsub(",","",pl_split$X1)
  pl_split2 <-data.frame(do.call('rbind', strsplit(as.character(pl_split$X2),' ',fixed=TRUE)), stringsAsFactors=FALSE)
  for (i in 1:(nrow(pl_split2)-1)){
    if(pl_split2$X5[i] == "Def"){
      pl_split2$X2[i] <- paste(pl_split2$X2[i], pl_split2$X3[i], sep = " ")
      pl_split2$X3[i] <- pl_split2$X4[i]
      pl_split2$X4[i] <- pl_split2$X5[i]
      
    }else{
      pl_split2$X2[i] <- pl_split2$X2[i]
    }
  }
  pl_split2$X5 <- NULL
  
  pl <- cbind(pl_split, pl_split2)
  pl <-pl[c(-1,-2,-4,-8)]
  
  colnames(pl) <- c( "Lastname","Firstname","Team", "Pos")
  
  adp$Lastname <- pl$Lastname
  adp$Firstname <- pl$Firstname
  adp$Team <- pl$Team
  adp$Pos <- pl$Pos
  adp$Player <- NULL
  adp <- adp[c("Firstname","Lastname","Team","Pos","Avg. Pick","Min. Pick", "Max. Pick", "# Drafts Selected In")]
  
  return (adp)
  
}

adp <- get_adp(0,1,0,12)

shinyApp(
  ui = fluidPage(
    tags$head(
          tags$style(HTML("tr.selected {
                  text-decoration: line-through;
                  color: gray;
                }")
             )
    ),
    titlePanel("ADP Draft Tracker"),
    
    fluidRow(
      column(12, helpText("Fill in your league settings and click 'Update' to create 
                             the chart of available players. As players are drafted select 
                             their row in the chart then click 'Remove Selected Players'. Enjoy and good luck!")
             )),
      
    fluidRow(column(6,
                    h3("League Settings"),
                    fluidRow(column(3,checkboxInput("injured", label = "Include Injured Players", value = FALSE)),
                    column(2,checkboxInput("ppr", label = "PPR Scoring", value = FALSE)),
                    column(3, checkboxInput("keeper", label = "Keepers Allowed", value = FALSE)),
                    selectInput("size", label = "Size of League", 
                                choices = list(8,10,12,14,16), selected = 12),
                    actionButton("update", label = "Update Table"
                    ))),
              column(6,
                     h3("Filter Positions"),checkboxGroupInput("positions", 
                                                                 label = "Positions", 
                                                               inline = TRUE,
                                                                 choices = list("QB", "RB","WR","TE","Def")),
                     
                     actionButton("posBtn", label = "Filter Positions")
             
                )
    ),
             
        
      fluidRow(column(10, DT::dataTableOutput('tbl')),
               column(2, actionButton("selected", label = "Remove Selected Players"))
                      
    )
  ),
  
  server = function(input, output) {
    #inputs --> url for leage settings
    settings <- observeEvent(input$update,{
      inj <- input$injured
      ppr <- input$ppr
      keeper <- input$keeper
      size <- input$size
      

      if (inj){inj <- 1}else{inj<-0}
      if (ppr){ppr <- 1}else{ppr<-0}
      if (keeper){keeper <- 1}else{keeper<-0}
      
      values$dfWorking <- get_adp(inj, ppr, keeper, size)
      
      
      output$tbl <- renderDataTable(
        values$dfWorking,
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100)
          
        )
      )
    })

    positionsSettings <- observeEvent(input$posBtn,{
      pos <- input$positions
      
      pos <- c(pos)
      if (length(pos) > 0) {
        new_adp <- values$dfWorking[values$dfWorking$Pos %in% pos,]
      }else{
        new_adp <- values$dfWorking
      }
      
    
      output$tbl <- renderDataTable(
        new_adp,
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100)
          
        )
      )
      
    })
    
    
    values <- reactiveValues(dfWorking = adp)
    
    observeEvent(input$selected,{
      
      if (!is.null(input$tbl_rows_selected)) {
        
        values$dfWorking <- values$dfWorking[-as.numeric(input$tbl_rows_selected),]
      }
      output$tbl <- renderDataTable(
        values$dfWorking,
        options = list(
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100)
          
        )
      )
    })
    
    output$tbl <- renderDataTable(
      values$dfWorking,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100)
        
      )
    )
    
   
  }
)
