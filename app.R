library(pacman)

p_load(tidyverse, shiny,DT, shinythemes, plotly, ggthemes)

options(scipen=999)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #shinythemes::themeSelector(),
  theme = shinytheme("cerulean"),

    # Application title
    titlePanel("MEXICO´S PUBLIC REVENUE"),

    navbarPage("Sections", 
               tabPanel("Database",
                        sidebarLayout(
                          sidebarPanel(
                            
                            h2("This Shiny App Goal"),
                              br(),
                            h4("This Shiny Apps tries to show the relationship
                               between the Mexico´s public revenue and the number of
                               workers who are enrolled at IMSS. Theoretically, 
                               there is a directly proportional relationship between them:
                               the more enrolled workers at IMSS, more public revenue 
                               the state catches."),
                            br(),
                            h4("It´s important to mention that this is app is only an ilustrative exercise
                               about how shiny works and it")
                            
                          ), 
                          mainPanel(DTOutput("uno"))
                          
                          
                          
                        )
                 
               ),
               tabPanel("Data Visualization",
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("x_axis","Select x variable", c("ingreso","deflated_revenue", "trabajadores", "fecha"), 
                                        selected="fecha", multiple = F),
                            br(),
                            selectInput("y_axis","Select y variable", c( "ingreso","deflated_revenue", "trabajadores"), 
                                        selected="ingreso", multiple = F),
                            br(),
                            br(),
                            numericInput("year_deflated","Pick a base year to deflate the revenue",
                                        min=1997,max=2021, value=2018)
                          ),
                          mainPanel(plotlyOutput("grafica"))
                          
                          
                        )
                        
                        ),
               tabPanel("Data Model",
                        sidebarLayout(
                          sidebarPanel(
                            
                            h1("REGRESSION MODEL"),
                            br(),
                            br(),
                            h4("How the theory says, the public revenue rise up
                               per each worker enrolled at IMSS. The logical 
                               explanation of this outcome is that one important 
                               element of the public revenue is the income tax which 
                               is paid by the worker: the more enrolled workers at IMSS,
                               the more income tax and public revenue the government catches.")
                      
                          ),
                          mainPanel(verbatimTextOutput("modelo_resultado")  
                                      )
                        ))
        
)

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
    data <- reactive({
      
      data <- read_delim("data.csv", delim = ",")
      
                })
  
    output$uno <- renderDT({
      data()
  
    })

    
    
    datos_v <- reactive({
      
      divisores <- data() %>% mutate(año = year(as_date(fecha,format ="%d/%m/%Y"))) %>% 
        group_by(año) %>% summarise(n = mean(inp))
      
      divisor <- divisores %>% filter(año== paste0(input$year_deflated) ) %>% 
        select(n) %>% 
        pull()
      
       data() %>% mutate(deflated_revenue= (ingreso) /(inp/divisor))
      
    })
    
    output$grafica <- renderPlotly({

      if(input$x_axis == "fecha"){
        
        ggplotly( ggplot(datos_v(),aes_string(x= input$x_axis, y= input$y_axis ))+
                    geom_line(aes(group=1), colour="blue")+ theme_classic()  )
        
      }else{
        
        ggplotly( ggplot(datos_v(),aes_string(x= input$x_axis, y= input$y_axis ))+
                    geom_point(colour="blue") + theme_classic() )
        
      }
      
    })
    
    modelo <- reactive({
      
        lm(deflated_revenue ~ trabajadores, datos_v() )
      
    })
    
    
    output$modelo_resultado <- renderPrint({
      
    
        summary(modelo())
    
      
      
    })
    
}

  
# Run the application 
shinyApp(ui = ui, server = server)
