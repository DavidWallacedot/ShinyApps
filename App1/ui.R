library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Congressional Voting Records kNN Misclassication Rate by k"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
    
    	selectInput("var", 
        	label = "Choose a party to display",
        	choices = c("Both", "Democrat","Republican"),
        	selected = "Both"),


      # Input: Slider for the number of bins ----
      sliderInput(inputId = "range",
                  label = "Range of number of neighbors:",
                  min = 1,
                  max = 20,
                  value = c(1,20)),
				  
	 actionButton("ReRun", "", icon = icon("play-circle"))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )#sideBarPanel
)
