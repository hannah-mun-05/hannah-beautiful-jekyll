

server <- function(input, output, session) {
  
  download_plot <- reactive({
    p(df_sub, g = input$select_gender, e = input$select_ethnic, d = input$select_drug)
  })
  
  output$plot <- renderPlot(
    p(df_sub, g = input$select_gender, e = input$select_ethnic, d = input$select_drug)
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      stringr::str_glue("Plot_Gender{input$select_gender}_Ethnic{input$select_ethnic}_Drug{input$select_drug}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             p(df_sub, g = input$select_gender, e = input$select_ethnic, d = input$select_drug),
             width = 8, height = 5, dpi = 300
      )
    }
    
  )
  
  output$about <- renderText({paste(br(),
                                    "This is an interactive visualization on synthetic ART in HIV data.",
                                    br(),
                                    "Output displays average viral load and CD4 cell counts of patients on each timepoint of the treatment.",
                                    br(),
                                    "Blue line indicates average viral load which value is on the left y axis, and red line indicated average CD4 cell counts which value is on the right y axis.",
                                    br(),
                                    "By default, the graph shows viral load and CD4 cell counts of all patients. You can adjust 3 types of input data which are gender, ethnicity and type of base drug combination applied.",
                                    br(),
                                    "To navigate this plot, click drop down menu under each input option, and select the option, then the plot will display the result of selected patient group.",
                                    br(),
                                    "To save the plot, click Download plot button below input selection menu.",
                                    br(),
                                    br(),
                                    "Note. Plot returns error message when selected input has no data.",
                                    br(),
                                    "i,e. There is no data input of [Female] + [Caucasian] + [FTC + RTVB + TDF]. This selection returns error message 'replacement has length zero'."
                                    )
    
  })


}




