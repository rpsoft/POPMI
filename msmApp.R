library(shiny)
library(plotly)
library(collapsibleTree) 
library(RColorBrewer)
# library(shinycssloaders)

source("readyData.R")

ui <- fluidPage(style = "background-color: #fff; height:100vh; ", 

  titlePanel(div(style = "background-color: #fff; border-radius:10px; padding:10px", h1("Multi State Modelling"))),
  
  tabsetPanel(type = "tabs",
      tabPanel(style = "border-left: 1px solid #d4d4d4;", div(style="font-size:20px;font-weight:bold;","Joint"), 
               # Sidebar with a slider input for number of years follow-up 
               sidebarLayout(
                 sidebarPanel(width = 3, style="margin-top:5px; margin-left:5px;",
                              radioButtons("precond_sb", label = "Pre-condition", c("Cerebro-Vascular Disease (CVD)" = "cvd", "Myocardial Infarction (MI)" = "mi"), inline = T, selected = "cvd"),
                              selectInput('year_cat_sb', 'Year Category', c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]"), c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]")),
                 ),
                 mainPanel(width = 9,
                           plotlyOutput("sunburst", width = "auto", height = "800px") ) 
               )
      ),
      tabPanel(style = "border-left: 1px solid #d4d4d4;",div(style="font-size:20px;font-weight:bold;","Conditional"), 
          # Sidebar with a slider input for number of years follow-up 
          sidebarLayout(
              sidebarPanel(width = 3, style="margin-top:5px; margin-left:5px;",
                 radioButtons("precond", label = "Pre-condition", c("Cerebro-Vascular Disease (CVD)" = "cvd", "Myocardial Infarction (MI)" = "mi"), inline = T, selected = "cvd"),
                 selectInput('year_cat', 'Year Category', c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]"), c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]")),
                 hr(),
                 radioButtons("sex", label = "Sex", c("Female" = 0, "Male" = 1), inline = T, selected = 1),
                 selectInput('start_cond', 'First event', c("hf","mi","stroke","bleeding"), c("hf","mi","stroke","bleeding")),
                 sliderInput("age", "Age (In years)", min = 30, max = 110, value = 50, step = 5),
                 sliderInput("simd", "Deprivation score (SIMD)", value = 1, min = 1, max = 5, step = 1),
                 radioButtons("prev_hf", label = "Previous Heart Failure (HF)", c("no" = 0, "yes" = 1), inline = T, selected =0),
                 radioButtons("cerebro_or_mi", label = "cerebro_or_mi", c("no" = 0, "yes" = 1), inline = T, selected =0),
            ),
            mainPanel(width = 9,
               collapsibleTreeOutput("dendo", width = "100%", height = "90vh")
            )
          )
      )
      # tabPanel(style = "border-left: 1px solid #d4d4d4;","legacy_sankey", plotlyOutput("sankey"))
  ),
)

server <- function(input, output,session) {
 
  observe({
    
    output$selec <- renderText(input$start_cond)
    # source, sex_in, simd, hf_in, cerebro_or_mi, year_cat_in, age_in 
    final_data <- filterMSMData( input$precond, input$sex, input$simd, input$prev_hf, input$cerebro_or_mi, input$year_cat, input$age )
    

    wprb <- filterWeightedMSMData( input$precond_sb, input$year_cat_sb) %>% arrange(event_1,event_2,event_3,event_4)
    
    theData <- (final_data$data %>% filter( event_1 == input$start_cond) ) 
    theData <- theData %>% mutate( event_n = which(unique( theData$event_id ) == event_id)[1] ) 

    # updateSliderInput(
    #   session,
    #   "simd",
    #   min = 1, max = 9
    # )
    # 
    
    updateRadioButtons(
      session,
      "cerebro_or_mi",
      label = ifelse( input$precond == "mi", "Previous Cerebro-Vascular Disease (CVD)", "Previous Myocardial Infarction (MI)" )
    )
  
    dendo_data <- theData %>% mutate( target_label = final_data$labels[target_n], source_label = final_data$labels[source_n] ) %>% 
      mutate( ss = str_split(target,"_") ) %>% 
      rowwise %>% mutate( node_label = paste0(ss[length(ss)])) %>%
      select(source_label,target_label,cond_prob,event_n,node_label) %>%
      mutate( cond_prob = as.numeric(cond_prob) ) %>%
      add_row(source_label = NA, target_label = input$start_cond, cond_prob=1,event_n=0, node_label= input$start_cond, .before = 1)
  
    labels <- c( "bleeding", "death", "hf", "mi", "none", "stroke" )
    label_colors <- c( "#F44336", "#273746", "#9C640C", "#03A9F4", "#E0E0E0 ", "#FFC107" )
    
    dendo_data <- dendo_data %>% rowwise %>% mutate(Color = label_colors[ which(labels == node_label)[1]]) %>% ungroup

    dendo_data$nsize <- (1+nrow(dendo_data)) - 1:nrow(dendo_data)
    
    dendo_data$order <- round(((max(dendo_data$event_n) - dendo_data$event_n)*20) + (dendo_data$cond_prob*10))*10
    
    dendo_data <- dendo_data %>% arrange(desc(order))
    
    output$dendo <- renderCollapsibleTree ( 
    
      collapsibleTreeNetwork(
        dendo_data,
        fill = "Color",
        attribute = "cond_prob",
        collapsed = FALSE,
        height = 800,
        width = 1000,
        zoomable = TRUE,
        aggFun = identity,
        tooltip = TRUE,
        linkLength = 500,
        # nodeSize = "nsize",
        fontSize = 20,
      )
      )
    
    output$sunburst <-  renderPlotly({
      plot_ly(wprb,
        width = 1500,
        height = 1500,
        parents = wprb$source,
        labels = wprb$target,
        values = wprb$wr,
        type = 'sunburst',
        
        hoverinfo = 'text',
        text = ~paste(wr," %")
      )
    })
    
    # output$sankey <- renderPlotly({
    #   p <- plot_ly(
    #     height = 800, 
    #     width = 1800,
    #     type = "sankey",
    #     orientation = "h",
    #   
    #     node = list(
    #       label = final_data$labels,
    #       # color = dat_node$color,
    #       pad = (theData$cond_prob * 100),
    #       thickness = 40,
    #       line = list(
    #         color = "black",
    #         width = 0.9
    #       )
    #     ),
    #     
    #     link = list(
    #       source = theData$source_n-1,
    #       target = theData$target_n-1,
    #       value = theData$cond_prob
    # 
    #     )
    #   ) %>% 
    #     layout(
    #       title = "",
    #       font = list(
    #         size = 10
    #       )
    #     )
    # })
  })
  
}

shinyApp(ui = ui, server = server)
