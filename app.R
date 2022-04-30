# Load R packages
library(shiny)
library(shinythemes)
library(mclust)
library(e1071) 

# UI
ui <- fluidPage(theme = shinytheme("yeti"),navbarPage("Reveals The Scams",
                             tabPanel("Calculator",
                             sidebarLayout(
                                 # Sidebar panel for inputs ----
                                 sidebarPanel(
                                     fileInput("file1", "Please select only csv 
                                               files with following column order: 
                                               reserved price and winning price",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",".csv")),
                                     tags$hr(),
                                     radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),
                                                  selected = "head"),
                                     tags$hr(),
                                     actionButton("submitbutton","Submit",icon("arrow-down"),width = 100),
                                     p("Click the button to check the bids"),
                                     tags$hr(),
                                     downloadButton("downloadData","Download", icon("download"), width = 200),
                                 ),
                             # Output:
                             mainPanel(
                             tableOutput("contents"),
                             tags$label(h3('Status')),
                             verbatimTextOutput('txt'),
                             tags$label(h3('Output')),
                             verbatimTextOutput('outputTxt'),
                             imageOutput('imageOutput')),),icon=icon("fa-solid fa-calculator")),
                tabPanel("About",
                         titlePanel("About"),
                         div(includeMarkdown("about.rmd")),
                         icon = icon("fa-solid fa-info")),
                tabPanel("Contact Us",
                         titlePanel("Contact Us"),
                         div(includeMarkdown("ContactUs.rmd")),
                         icon = icon("fa-regular fa-envelope")))
)# fluidPage

# Define server function  
server <- function(input, output,session) {

    output$contents <- renderTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = TRUE,sep = ",",quote = '"')
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        if(input$disp == "head") {
            return(head(df))}
        else {
            return(df)}
        })
    
    # Background Image plot
    output$imageOutput <- renderImage ({
        list(src = "Pics/auction1.jpg",
             contentType="auction1/jpg", width = 675, height = 400,
             alt = "auction")} ,deleteFile = FALSE)
    
    # Status plot
    output$txt <- renderText({
        if(input$submitbutton>0){
            isolate("The calculation was performed successfully")}
        else {
            return ("Waiting for submit")}
    })
    
    # Result plot
    output$outputTxt <- renderText({
        counter <- 0
        req(input$file1)
        data2 <- read.csv(input$file1$datapath,
                       header = TRUE,sep = ",",quote = '"')
        colnames(data2) <- c('reserved_price','winning_price')
        data2['ratio'] <- data2$winning_price/data2$reserved_price
        
        #Prepering data for clustering
        NData <- na.omit(data2)
        
        #Model based clustering
        clus1 <- Mclust(NData$ratio,G=2)
        
        #adding cluster to table
        t1 <- cbind(NData,Classification = clus1$classification)
        
        # creating data frame for each cluster
        NClus1 <- subset(t1,Classification == 1)
        NClus2 <- subset(t1,Classification == 2)
        
        # KS test for each cluster
        pclus1<- ks.test(NClus1$ratio,"pnorm")$p.value
        pclus2<- ks.test(NClus2$ratio,"pnorm")$p.value
        
        # checking KS test result
        if(pclus1 >= 0.01 && pclus2 < 0.01)
        {
            counter <- counter+1
        }
        
        # creating median for each cluster
        m1 <- median(NClus1$ratio)
        m2 <- median(NClus2$ratio)
        
        # T test
        tpvalue <- t.test(NClus1$ratio,NClus2$ratio,alternative = "less")$p.value
        # checking T test result
        if(tpvalue < 0.05 && m1!=m2)
        {
            counter <- counter+1
        }
        
        #Skewness Test
        s1 <- skewness(NClus1$ratio)  
        s2 <- skewness(NClus2$ratio)
        
        # checking Skewness test result
        if(s1 > 0 && s2 < 0)
        {
            counter <- counter+1
        }
        
        #F Test
        v<- var.test(NClus1$ratio,NClus2$ratio,alternative = "less",conf.level = 0.95)$p.value
        
        # checking Skewness test result
        if(v < 0.05)
        {
            counter <- counter+1
        }
        
        if(counter == 4 && input$submitbutton>0){
            output$imageOutput <- renderImage ({
                list(src = "Pics/approved.jpg",
                     contentType="approved/jpg", width = 250, height = 250,
                     alt = "approved")} ,deleteFile = FALSE)
            return("Approved")}
        
        if(counter != 4 && input$submitbutton>0){
            output$imageOutput <- renderImage ({
                list(src = "Pics/rejected.jpg",
                     contentType="rejected/jpg", width = 250, height = 250,
                     alt = "rejected")} ,deleteFile = FALSE)
            return("Warning")}
        
    }) # End of Result plot
    
    # Creating again cluster 2 for downloading
    datasetInput <- reactive({
      req(input$file1)
      data2 <- read.csv(input$file1$datapath,
                        header = TRUE,sep = ",",quote = '"')
      colnames(data2) <- c('reserved_price','winning_price')
      data2['ratio'] <- data2$winning_price/data2$reserved_price
      NData <- na.omit(data2)
      clus1 <- Mclust(NData$ratio,G=2)
      t1 <- cbind(NData,Classification = clus1$classification)
      NClus2 <- subset(t1,Classification == 2)
      return(NClus2)
    })
    # Downloading cluster 2 into a csv file
    output$downloadData <- downloadHandler(
      filename = function() { paste0("cluster2_final.csv")},
      content = function(file) {write.csv(datasetInput(), file,row.names = FALSE)})
} # end of server
    
# Create Shiny object
shinyApp(ui = ui, server = server)
