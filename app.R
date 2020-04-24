ui <- fluidPage(
  #adding radio button
  radioButtons("radio", label = h4("CHOOSE PAPER NAME"),
               choices = list("The Anselm Corpus: Methods and Perspectives" = 1, "Improving historical spelling normalization with bi-directional LSTMs
                              and multi-task learning" = 2), 
               selected = 1),
  hr(),
  
  #adding button
  actionButton("btn", h4("FIND CITATIONS SENTIMENT")),
  
  #browsing text file
  fileInput("file", h4("Select the text file from folder")),
  
  #output field
  verbatimTextOutput("value"),
  verbatimTextOutput("text"),
  verbatimTextOutput("text1"),
  sidebarLayout(
    numericInput("state", h4("Choose a author serial number:"),c(0,1,2)),
    verbatimTextOutput("text3")
    
  )
  
  )


server <- function(input, output, session) {
  #adding function file
  source("fun.R")
  
  #calculate function when button is clicked
  observeEvent(input$btn, {
    #new.function(6)
    output$text<-renderText(onerun<-Citation_sentiment_analysis(as.integer(input$radio)))
  })
  
  #displaying of output file
  output$text1<- renderText({
    filePath <- input$file$datapath
    fileText <- paste(readLines(filePath), collapse = "\n")
    fileText
  })
  output$text3 <- renderText({fun1(input$state)})
  
  
}


        
# Run the application 
shinyApp(ui = ui, server = server)

