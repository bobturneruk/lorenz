#Required libraries

library(rgl)
library(car)
library(shiny)
library(deSolve)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(viridis)

#The lorenz functions in a for deSolve can understand
Lorenz<-function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
        # rate of change
        dX <- a*X + Y*Z
        dY <- b * (Y-Z)
        dZ <- -X*Y + c*Y - Z
        
        # return the rate of change
        list(c(dX, dY, dZ))
    })
}

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Lorenz Attractor"),
    

    # Sidebar with a slider inputs for paramters and initial conditions 
    sidebarLayout(
        sidebarPanel(
            
                sliderInput("param_a", label = "Parameter a:",
                            min = -100, max = 100, value = -8/3, step=0.001),
                sliderInput("param_b", label = "Parameter b:",
                            min = -100, max = 100, value = -10, step=0.001),
                sliderInput("param_c", label = "Parameter c:",
                            min = -100, max = 100, value = 28, step=0.001),
                sliderInput("start_X", label = "Initial X:",
                            min = -100, max = 100, value = 1, step=0.001),
                sliderInput("start_Y", label = "Initial Y:",
                            min = -100, max = 100, value = 1, step=0.001),
                sliderInput("start_Z", label = "Initial Z:",
                            min = -100, max = 100, value = 1, step=0.001),
                sliderInput("time", label = "Duration:",
                            min = 1, max = 200, value = c(1,100), step=0.001)
            
        ),

        # Tabs containing plots and information
        mainPanel(
            tabsetPanel(
                tabPanel("2D Plots",plotOutput("plots_2d")),
                tabPanel("2D Time Series",plotOutput("time_series")),
                tabPanel("3D Interactive",rglwidgetOutput("attractor",width="100%")),
                tabPanel("Information",withMathJax(includeHTML("lorenz_text_snippet.html")))
            )
        )
    )
)

# Define server logic required to generate plots
server <- function(input, output) {

    vals <- reactiveValues()
    observe({
        #Model inputs are put into a form that deSolve can understand
        parameters <- c(a = input$param_a, b = input$param_b, c = input$param_c)
        state <- c(X = input$start_X, Y = input$start_Y, Z = input$start_Z)
        times <- seq(input$time[[1]], input$time[[2]], by = 0.01)
        
        #Solve ODEs
        vals$out <- ode(y = state, times = times, func = Lorenz, parms = parameters) %>%
            as.data.frame()
    })
    
    output$attractor <- renderRglwidget({
        
        #Plot the output in interactive 3D
        out=vals$out
        rgl.open(useNULL=T)
        bg3d("white")
        plot3d(out$X, out$Y, out$Z, 
               col=viridis(nrow(out)), type="l",
               xlab="X", ylab="Y", zlab="Z")
        
        #Set up a legend using the viridis colour table
        timerange<-max(out$time)-min(out$time)
        legendlist<-seq(min(out$time),max(out$time),timerange/10)
        #The legend looks super ugly, so it's hidden
        #legend3d("left",legend=legendlist,fill=viridis(10),title="time",cex = 0.5)
        
        rglwidget()
    })
    
    output$time_series <- renderPlot({
        out=vals$out
        #Create output time_series
        tX<-out %>%
          as.data.frame() %>%
          ggplot(aes(x=time,y=X)) +
          geom_path()
        
        tY<-out %>%
          as.data.frame() %>%
          ggplot(aes(x=time,y=Y)) +
          geom_path()
        
        tZ<-out %>%
          as.data.frame() %>%
          ggplot(aes(x=time,y=Z)) +
          geom_path()
        
        grid.arrange(tX, tY, tZ, ncol=1)
    })
    
    output$plots_2d <- renderPlot({
        out=vals$out
        #Create 2d plots of output
        XY<-out %>%
          as.data.frame() %>%
          ggplot(aes(x=X,y=Y,colour=time)) +
          geom_path() +
          coord_fixed() +
          scale_colour_viridis_c()
        
        XZ<-out %>%
          as.data.frame() %>%
          ggplot(aes(x=X,y=Z,colour=time)) +
          geom_path() +
          coord_fixed() +
          scale_colour_viridis_c()
        
        grid.arrange(XY, XZ, ncol=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
