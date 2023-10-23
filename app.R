### shiny app
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
# read in the data
df <- read.csv("Plotting_Mortality_0923.csv", header=T)
df$Treatment = factor(as.character(df$Treatment))
df_2 <-read.csv("PercentError_0923.csv",header=T)
df_2[1]<-NULL
df_2$Treatment = factor(as.character(df_2$Treatment))
head(df_2)

# need a user interface object
ui <- fluidPage(
  # this is the overall title of the page
  titlePanel("ProMo Visualization"),
  #these elements are placed with the fluid Page
  sidebarLayout(
    sidebarPanel(
      # from the internet, this will allow me to create an output based on the previous options
      h4("Plot variables"),
      varSelectInput("xvar", "X variable", df, selected = "ln_Pro"),
      varSelectInput("yvar", "Y variable", df, selected = "Mortality"),
      selectInput("columnVar","Color by:",c("Virus_Presence","Simple_Treatments","Series","Treatment","Experiment_Name"), selected="Virus_Presence"),
      checkboxInput("facet", label = "Facet plot?", value = TRUE),
      selectInput("facet_by", "Facet by", c("Experiment_Type","Experiment_Method","Virus_Presence","Simple_Treatments","Series","Treatment"), selected="Experiment_Type"),
      tags$hr(style="border-color: purple;"),
      h4("Plot percent error"),
      selectInput("xvar2", "X variable", c("ln_Pro","ln_Grazer"), selected = "ln_Pro"),
      selectInput("yvar2", "Percent Error of:", c("flb_pro_ratio","dln_pro_ratio"), selected = "flb_pro_ratio"),
      selectInput("columnVar2","Color by:",c("Virus_Presence","Simple_Treatments","Series","Treatment","Experiment_Name"), selected="Virus_Presence"),
      sliderInput("sliderx", "X-axis limits", min=6, max=17, value=c(8, 17)),
      sliderInput("slidery", "Y-axis limits", min=-4300, max=4300, value=c(-100, 100)),
      checkboxInput("smooth", label = "Add trend line?", value = FALSE),
      tags$hr(style="border-color: purple;"),
      h4("3D plot"),
      selectInput("plot3_var", "Plot variable", c("Mortality","Percent Error"), selected="Mortality"),
      selectInput("columnVar3","Color by:",c("Virus_Presence","Simple_Treatments","Series","Treatment","Experiment_Name", "Experiment_Method", "Experiment_Type"), selected="Virus_Presence"),
      
    ),
    mainPanel(
      plotlyOutput("plot"),
      tags$hr(style="border-color: purple;"),
      plotlyOutput("plot2"),
      tags$hr(style="border-color: purple;"),
      plotlyOutput("plot3"),
      
    )
  )  
)



# need a server function
server <- function (input, output, session){
  
  color_fill = reactive({
    switch(input$columnVar,
           "Virus_Presence" = c("Grazer Only"="#a1d76a", "Grazer and Virus"="#e9a3c9"),
           "Simple_Treatments" = c("HG"="#3b7741","HG+V"="#c04281","LG"="#99b24d","LG+V"="#ea7db0"),
           "Series" = c("A"="#d7191c","B"="#f9a660","C"="goldenrod","D"="#abdda4","E"="#2b83ba"),
           "Treatment" = c("2"="#1b9e77","3"="#d95f02","4"="#7570b3","5"="#e7298a","6"="#66a61e","7"="#e6ab02","8"="#2b83ba"),
           "Experiment_Name" = c("FLB1"="#a3520f","FLB2"="#f0750f", "FLB3"="#fac676", "FLB4"="#f0a150", "FLB5"="#f09537", "FLB6"="#f48020","DLN1"="#20d4f4","DLN2"="#1f78b4"),
           "Experiment_Method"= c("FLB"="#f9a660", "DLN"="#72b8ea"),
           "Experiment_Type" = c("Pro_culture" ="#999999","FLB" = "#f9a660","DLN"="#72b8ea")
    )
  })
  
  
  # convert renderPlot to renderPlotly
  output$plot <- renderPlotly({
    # create ggplot object
    if (input$xvar=="ln_Pro"){
      p<-ggplot(df, aes(!!input$xvar, !!input$yvar, fill=get(input$columnVar),shape=Experiment_Method))+
        geom_point(size=3, stroke=0.25, na.rm=TRUE)+
        scale_x_reverse()+
        theme_bw()+
        scale_shape_manual(values=c("Pro" =21,"FLB" = 23,"DLN"=24),name="")+
        scale_fill_manual(values=color_fill(),name="")
    }
    if (input$xvar !="ln_Pro"){
      p<-ggplot(df, aes(!!input$xvar, !!input$yvar, fill=get(input$columnVar),shape=Experiment_Method))+
        geom_point(size=3, stroke=0.25, na.rm=TRUE)+
        theme_bw()+
        scale_shape_manual(values=c("Pro" =21,"FLB" = 23,"DLN"=24),name="")+
        scale_fill_manual(values=color_fill(),name="")
    }
    
    if (input$facet) {
      p_2 <-p+
        facet_wrap(input$facet_by)
    }
    if (!input$facet){
      p_2<-p
    }
    ggplotly(p_2) 
    
  })
  
  df_2_update = reactive({
    a <-subset(df_2,variable==input$yvar2)
    return(a)
  })
  color_fill2 = reactive({
    switch(input$columnVar2,
           "Virus_Presence" = c("Grazer Only"="#a1d76a", "Grazer and Virus"="#e9a3c9"),
           "Simple_Treatments" = c("HG"="#3b7741","HG+V"="#c04281","LG"="#99b24d","LG+V"="#ea7db0"),
           "Series" = c("A"="#d7191c","B"="#f9a660","C"="goldenrod","D"="#abdda4","E"="#2b83ba"),
           "Treatment" = c("2"="#1b9e77","3"="#d95f02","4"="#7570b3","5"="#e7298a","6"="#66a61e","7"="#e6ab02","8"="#2b83ba"),
           "Experiment_Name" = c("FLB1"="#a3520f","FLB2"="#f0750f", "FLB3"="#fac676", "FLB4"="#f0a150", "FLB5"="#f09537", "FLB6"="#f48020","DLN1"="#20d4f4","DLN2"="#1f78b4"),
           "Experiment_Method"= c("FLB"="#f9a660", "DLN"="#72b8ea"),
           "Experiment_Type" = c("Pro_culture" ="#999999","FLB" = "#f9a660","DLN"="#72b8ea")
    )
  })
  output$plot2 <- renderPlotly({
    # create ggplot object
    if (input$xvar2=="ln_Pro"){
      p <-ggplot(df_2_update(), aes(x=get(input$xvar2), y = Percent_Error*100, fill=get(input$columnVar2),shape=Experiment_Method)) +
        geom_hline(yintercept=0,linetype=1, alpha=0.8) +
        geom_hline(yintercept=c(-200,200),linetype=2)+
        geom_point(size=3, stroke=0.25, na.rm=TRUE)+
        scale_shape_manual(values=c("Pro" =21,"FLB" = 23,"DLN"=24),name="")+
        theme_bw()+
        labs(x=input$xvar2, y ="Percent Error", title="")+
        scale_fill_manual(values=color_fill2(),name="")+
        scale_y_continuous(limits=c(input$slidery[1],input$slidery[2]), breaks=c(-4000,-2000,-1000,-500,-200,-100,0,100,200,500,1000,2000,4000)) +
        scale_x_continuous(limits = c(input$sliderx[2],input$sliderx[1]),trans=scales::reverse_trans())
    }
    if (input$xvar2 !="ln_Pro"){
      p <-ggplot(df_2_update(), aes(x=get(input$xvar2), y = Percent_Error*100, fill=get(input$columnVar2),shape=Experiment_Method)) +
        geom_hline(yintercept=0,linetype=1, alpha=0.8) +
        geom_hline(yintercept=c(-200,200),linetype=2)+
        geom_point(size=3, stroke=0.25, na.rm=TRUE)+
        scale_shape_manual(values=c("Pro" =21,"FLB" = 23,"DLN"=24),name="")+
        theme_bw()+
        labs(x=input$xvar2, y ="Percent Error", title="")+
        scale_fill_manual(values=color_fill2(),name="")+
        scale_y_continuous(limits=c(input$slidery[1],input$slidery[2])) +
        scale_x_continuous(limits = c(input$sliderx[1],input$sliderx[2]))
    }
    
    
    if (input$smooth) {
      p_2 <-p +
        geom_smooth(method=lm, aes(group=get(input$columnVar2), color=get(input$columnVar2)),alpha=0.5)+
        scale_color_manual(values=color_fill2(),name="")
    }
    if (!input$smooth) {
      p_2 <- p
    }
    ggplotly(p_2) 
    
    
  })
  color_fill3 = reactive({
    switch(input$columnVar3,
           "Virus_Presence" = c("Grazer Only"="#a1d76a", "Grazer and Virus"="#e9a3c9"),
           "Simple_Treatments" = c("HG"="#3b7741","HG+V"="#c04281","LG"="#99b24d","LG+V"="#ea7db0"),
           "Series" = c("A"="#d7191c","B"="#f9a660","C"="goldenrod","D"="#abdda4","E"="#2b83ba"),
           "Treatment" = c("2"="#1b9e77","3"="#d95f02","4"="#7570b3","5"="#e7298a","6"="#66a61e","7"="#e6ab02","8"="#2b83ba"),
           "Experiment_Name" = c("FLB1"="#a3520f","FLB2"="#f0750f", "FLB3"="#fac676", "FLB4"="#f0a150", "FLB5"="#f09537", "FLB6"="#f48020","DLN1"="#20d4f4","DLN2"="#1f78b4"),
           "Experiment_Method"= c("FLB"="#f9a660", "DLN"="#72b8ea"),
           "Experiment_Type" = c("Pro_culture" ="#999999","FLB" = "#f9a660","DLN"="#72b8ea")
    )
  })
  output$plot3 <- renderPlotly({
    # create plot based on the mortality rates
    # create plot based on percent error
    if (input$plot3_var=="Mortality"){
      p<-plot_ly(df, x=~ln_Pro, y=~ln_Grazer, z=~Mortality, color=~get(input$columnVar3), colors=color_fill3(),type="scatter3d",mode="markers")
    }
    if (input$plot3_var=="Percent Error"){
      p<-plot_ly(df_2, x=~ln_Pro, y=~ln_Grazer, z=~Percent_Error*100, color=~get(input$columnVar3), colors=color_fill3(),type="scatter3d",mode="markers")
    }
    p
  })
  
}
# need a call to the shinyApp function
shinyApp(ui = ui, server = server)
# to run
# runApp("directory of app")
