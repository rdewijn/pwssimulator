#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("PWS simulator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("nl", "leerlingen per groep", 8, min = 0, step = 1),
      sliderInput("A", "gemiddelde score A", min = 0, max = 100, value = 50),
      sliderInput("B", "gemiddelde score B", min = 0, max = 100, value = 50),
      sliderInput("C", "gemiddelde score C", min = 0, max = 100, value = 50),
      sliderInput("D", "gemiddelde score D", min = 0, max = 100, value = 50),
      sliderInput("S", "spreiding binnen groep (standard deviatie)", min = 0, max = 100, value = 10)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("boxplot",
                 checkboxInput("pbox", "boxplot", TRUE),
                 
                 checkboxInput("ppoints", "points", TRUE),
                 plotOutput("box"),
                 h3("p-waarde uit t-test"),
                 tableOutput('ttest')
        ),
        tabPanel("table",
                 tableOutput("table")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ttfun = function(df1, df2){
   # browser()
    tt = df1 %>%
      bind_rows(df2) %>%
      mutate(groep = droplevels(groep)) %>%
      t.test(gesimuleerde_scores ~ groep, data = .)
    data.frame(p.value = tt$p.value)
  }
  
  
  observe({
    simulate = reactive({
      sdata = expand.grid(groep = c("A", "B", "C", "D"), leerling = 1:input$nl) %>%
        mutate(groep = groep %>% as.factor) %>%
        mutate(mscore = case_when(groep == "A" ~ input$A,
                                  groep == "B" ~ input$B,
                                  groep == "C" ~ input$C,
                                  groep == "D" ~ input$D)) %>%
        mutate(gesimuleerde_scores = mscore + input$S * rnorm(n()) ) %>%
        mutate(gesimuleerde_scores = ifelse(gesimuleerde_scores < 0, 0, gesimuleerde_scores)) %>%
        mutate(gesimuleerde_scores = ifelse(gesimuleerde_scores > 100, 100, gesimuleerde_scores)) 
        
    })
    
    analyse = reactive({
      df = simulate() %>%
        filter(groep != levels(groep)[1])
      
      control = simulate() %>%
        filter(groep == levels(groep)[1])
      
      df %>%
        group_by(groep) %>%
        do(ttfun(., control)) %>%
        ungroup()
      
    })
    
    output$table = renderTable({
      simulate() %>%
        arrange(groep)
    })
    
    output$ttest = renderTable({
      analyse()
    })
    
    output$box = renderPlot({
      p = simulate() %>%
        ggplot(mapping = aes(x = groep, y = gesimuleerde_scores, fill = groep))
 
      if(input$pbox){
        p = p + geom_boxplot(colour = "darkblue")
      }
      if (input$ppoints){
        p = p + geom_point()
      }
      p + theme_bw() + ylim(c(0, 100))
        
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
