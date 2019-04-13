library(class)

votes <- read.csv("house-votes-84-numeric.csv", header=TRUE)

n = nrow(votes)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = votes[trainIndex ,]
validation = votes[-trainIndex ,]

cl_train<-train$party
train<-train[-c(1)]
cl_valid<-validation$party
validation<-validation[-c(1)]

fullMCRate <- c() 
demMCRate <- c()
repMCRate <- c()
for (k in 1:20) {
	predicted.labels <- knn(train, validation, cl_train, k)
	
	# compute mc rates
	fullCnt <- 0
	fullMC <- 0
	demCnt <- 0
	demMC <- 0
	repCnt <- 0
	repMC <- 0
	for (i in 1:nrow(validation)) {
		wrong <- FALSE
		if (predicted.labels[i] != cl_valid[i]) wrong <- TRUE
		fullCnt <- fullCnt + 1
		if (wrong) fullMC <- fullMC + 1
		if (cl_valid[i] == "democrat") {
			demCnt <- demCnt + 1
			if (wrong) demMC <- demMC + 1
		} else {
			repCnt <- repCnt + 1
			if (wrong) repMC <- repMC + 1
		}

	}
	fullMCRate[k] <- fullMC / fullCnt
	if (demCnt != 0) demMCRate[k] <- demMC / demCnt
	else demMCRate[k] <- 0 # is this correct?
	if (repCnt != 0) repMCRate[k] <- repMC / repCnt
	else repMCRate[k] <- 0 # is this correct?

}
kValues <- c(1:20)

server <- function(input, output) {

output$distPlot <- renderPlot({
 
   	min <- input$range[1]
  	max <- input$range[2]
       
  	if (input$var == "Both") {
	    barplot(fullMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="purple",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
  	} else if (input$var == "Democrat") {
	    barplot(demMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="blue",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
  	} else if (input$var == "Republican") {
	    barplot(repMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="red",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
	}
  	
  })

   observeEvent(input$ReRun,output$distPlot <- renderPlot({
n = nrow(votes)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = votes[trainIndex ,]
validation = votes[-trainIndex ,]

cl_train<-train$party
train<-train[-c(1)]
cl_valid<-validation$party
validation<-validation[-c(1)]

fullMCRate <- c() 
demMCRate <- c()
repMCRate <- c()
for (k in 1:20) {
	predicted.labels <- knn(train, validation, cl_train, k)
	
	# compute mc rates
	fullCnt <- 0
	fullMC <- 0
	demCnt <- 0
	demMC <- 0
	repCnt <- 0
	repMC <- 0
	for (i in 1:nrow(validation)) {
		wrong <- FALSE
		if (predicted.labels[i] != cl_valid[i]) wrong <- TRUE
		fullCnt <- fullCnt + 1
		if (wrong) fullMC <- fullMC + 1
		if (cl_valid[i] == "democrat") {
			demCnt <- demCnt + 1
			if (wrong) demMC <- demMC + 1
		} else {
			repCnt <- repCnt + 1
			if (wrong) repMC <- repMC + 1
		}

	}
	fullMCRate[k] <- fullMC / fullCnt
	if (demCnt != 0) demMCRate[k] <- demMC / demCnt
	else demMCRate[k] <- 0 # is this correct?
	if (repCnt != 0) repMCRate[k] <- repMC / repCnt
	else repMCRate[k] <- 0 # is this correct?

}
kValues <- c(1:20)
 
   	min <- input$range[1]
  	max <- input$range[2]
       
  	if (input$var == "Both") {
	    barplot(fullMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="purple",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
  	} else if (input$var == "Democrat") {
	    barplot(demMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="blue",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
  	} else if (input$var == "Republican") {
	    barplot(repMCRate[min:max],names.arg=kValues[min:max],xlab="Number of Neighbors",
    	ylab="Misclassification Rate",col="red",
    	main="Misclassification Rate by Number of Nearest Neighbors",border="black")
	}
  	
  }))

}