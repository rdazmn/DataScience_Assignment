
library(shiny)
library(markdown)
library(ggplot2)


#setwd('C:/Users/Admin/Desktop/UM FILE/Semester 4/Intro Data Science/R/Lifeline/')
cty<-read.csv(file='clean data/asean.csv')
me<-read.csv(file='clean data/asean_meow.csv')
bye<-read.csv(file='clean data/asean_bye.csv')
file4<-read.csv(file='clean data/asean_4.csv')
gender<-cty$Sex
ctry<-cty$Country

ui <- fluidPage(
    navbarPage("Lifeline",
               tabPanel("Plot",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = 'countryInput','Country',choices =colnames(bye),selected ='Malaysia'),
                                
                                downloadButton('report','Generate report')
                               
                            ),
                            mainPanel(
                                plotOutput("plot")
                                
                                
                            )
                        )
               ),
               tabPanel("Summary","Overall Suicide rates in ASEAN countries",plotOutput('overallplot'),
                        "Overall Average of suicide rates in asean",
                        verbatimTextOutput('avg'),hr(),
                        plotOutput('plot2'),
                        plotOutput('plot3'),hr(),
                        "Summary",
                        verbatimTextOutput("summary")
                        
               ),
               navbarMenu("More",
                          tabPanel("Table",
                                   DT::dataTableOutput("table")
                          ),
                          tabPanel("About",
                                   fluidRow(
                                       column(6,
                                              includeMarkdown("about.md"),
                                              tags$h4(a(href='https://ourworldindata.org/suicide#measurement-data-quality','Learn More'))
                                             
                                              
                                       )
                                      
                                   )
                          )
                          
               )
    )
)


#plotthis<-function(countryInput,yearInput){
#if(countryInput==ctry){
#    plot(x=ctry,dfyear(yearInput))
#}
#}

#dfyear<-function(yearInput){
#if(yearInput=='X2016'){
#    cty$X2016
#}
#else if(yearInput=='X2015'){
#    cty$X2015
#}
#else if(yearInput=='X2010'){
#    cty$X2010
#}else{
#   cty$X2000
#}
#}



server <- function(input, output) {
   
    #output$plot<-renderPlot({
    #    plotthis(input$countryInput,input$yearInput)
    #})
    
    output$plot<-renderPlot({
        barplot(bye[,input$countryInput],
                main=input$countryInput,ylab='Rate',xlab = 'Year',names.arg =c('2000','2005','2010','2015'),ylim=c(0,20),col = c("mistyrose", "lightcyan", 
                                                                                                                                 "lavender", "cornsilk"))
    })
    
   
    output$plot2<-renderPlot({
       
        ggplot(file4,aes(x=file4$Male))+geom_density()+labs(y='Rate',x='2000-2015')+ggtitle('Suicide rate in ASEAN by male')
    })
    
    output$plot3<-renderPlot({
        
        ggplot(file4,aes(x=file4$Female))+geom_density()+labs(y='Rate',x='2000-2015')+ggtitle('Suicide rate in ASEAN by female')
    })
    
    
    output$summary <- renderPrint({
        summary(cty)
        
    })
    
    output$overallplot <- renderPlot({
        colour<-c('red','light yellow','blue','green','orange','violet','pink','light green','light blue','brown')
        barplot(me$Overall,names.arg = me$Country,col=colour,xlab = "Countries",ylab = 'Suicide rates',ylim=c(0,60))
    })
    
    output$avg<-renderPrint({
        mean(cty$X2005)+mean(cty$X2015)+mean(cty$X2010)+mean(cty$X2000)
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(cty)
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(n = input$countryInput)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
