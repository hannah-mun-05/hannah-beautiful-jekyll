
ui <- fluidPage(

  
  # app title  
titlePanel("Viral load and CD4 cell changes on ART in HIV"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # selector for gender
      selectInput(
        inputId = "select_gender",
        label = "Gender",
        choices = c(
          "All" ,
          "Male" = "1",
          "Female" = "2"
        ),
        selected = "All",
        multiple = FALSE
      ),
      
      # selector for ethnicity
      selectInput(
        inputId = "select_ethnic",
        label = "Ethnicity",
        choices = c(
          "All",
          "African" = "2",
          "Cacausian" = "3",
          "Other" = "4"
        ), 
        selected = "All",
        multiple = FALSE
      ),
      
      # selector for ethnicity
      selectInput(
        inputId = "select_drug",
        label = "Base Drug Combination",
        choices = c(
          "All",
          "FTC + TDF" = "0",
          "3TC + ABC" = "1",
          "FTC + TAF" = "2",
          "DRV + FTC + TDF" = "3",
          "FTC + RTVB + TDF" = "4",
          "Other" = "5"
        ), 
        selected = "All",
        multiple = FALSE
      ),
      
      # add a horizontal line
      hr(),
      # add a download button
      downloadButton(
        outputId = "download_plot",
        label = "Download plot"
      )
   ),

      
      # set main panel to display outputs
      mainPanel(
        
        # set output tabs
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("About", tableOutput("about"))
        )
      
    )
   )
   
)


