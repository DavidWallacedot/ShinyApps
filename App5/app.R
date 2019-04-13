#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
votes = read.csv("house-votes-84-numeric.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bill table"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("bill",
                     "Select bill:",
                     choices =c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16") 
                       )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("table"),
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  demFor = 0
  demAgainst = 0
  repFor = 0
  repAgainst = 0
  didNotVote  =0
  rows = nrow(votes)
  party = c()
  x = data.frame()
  
  observeEvent(input$bill,output$plot <- renderPlot ({for(i in 1:435)
  {
    party = c(party,votes[i,1])
  }
    for (i in 1:435)
    {
      if(is.null(party[i]) || is.na(party[i]))
      {
        return()
      }
      if(is.null(votes[i,input$bill]) || is.na(votes[i,input$bill]))
      {
        return()
      }
      
      if(party[i]==1)# if party affiliation is democrat
      {
        if(votes[i,input$bill] == 1){demFor = demFor+1}
        else if(votes[i,input$bill]==-1){demAgainst = demAgainst+1}
        else {didNotVote = didNotVote+1}
      }
      else #else party affiliation is republican 
      {
        
        if(votes[i,input$bill] == 1){repFor = repFor+1}
        else if(votes[i,input$bill]==-1){repAgainst = repAgainst+1}
        else {didNotVote = didNotVote+1}
        
        
      }
      
      
      
    }
    pie(c(demFor,demAgainst,repFor,repAgainst,didNotVote),labels = c("demFor","demAgainst","repFor","repAgainst","didNotVote"))
    
  })
  )
  
  observeEvent(input$bill,output$table <- renderTable ({for(i in 1:435)
  {
    party = c(party,votes[i,1])
  }
  for (i in 1:435)
  {
    if(is.null(party[i]) || is.na(party[i]))
    {
      return()
    }
    if(is.null(votes[i,input$bill]) || is.na(votes[i,input$bill]))
    {
      return()
    }
    
    if(party[i]==1)# if party affiliation is democrat
    {
      if(votes[i,input$bill] == 1){demFor = demFor+1}
      else if(votes[i,input$bill]==-1){demAgainst = demAgainst+1}
      else {didNotVote = didNotVote+1}
    }
    else #else party affiliation is republican 
    {
      
      if(votes[i,input$bill] == 1){repFor = repFor+1}
      else if(votes[i,input$bill]==-1){repAgainst = repAgainst+1}
      else {didNotVote = didNotVote+1}
      
      
    }
    
    
    
  }
      x = data.frame(names = c("demFor","demAgainst","repFor","repAgainst","didNotVote"),c(demFor,demAgainst,repFor,repAgainst,didNotVote))
      
    })
  )
   output$table <- renderTable({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      iris
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

