shinyUI(fluidPage(
  
 # titlePanel("Applied Research Collaborations (ARC): Yorkshire and Humber"),
  
  #h5("Add text"),
  

    div(h2("Informing activity in emergency departments"),
        h4("Applied Research Collaborations (ARC): Yorkshire and Humber"),
        HTML("<div style='height:0.5px;width:700'>"),
        imageOutput("line"),
        HTML("</div>"),
        hr(),
        style='padding-left:50px;'),
    
    div(
      #"Hello"
      uiOutput("intro_text")
      )
  
))