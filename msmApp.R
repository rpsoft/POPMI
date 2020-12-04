library(shiny)
library(plotly)
library(collapsibleTree) 
library(RColorBrewer)
library(shinycssloaders)

source("readyData.R")

labels <- c( "bleeding", "death", "hf", "mi", "none", "stroke", "cvd" )
label_colors <- c( "#F44336", "#273746", "#9C640C", "#03A9F4", "#E0E0E0 ", "#FFC107", "#FFFFFF")

prepareDendoData <- function(precond, sex, simd, prev_hf, cerebro_or_mi, year_cat, age){
  final_data <- filterMSMData( precond, sex, simd, prev_hf, cerebro_or_mi, year_cat, age )
  theData <- final_data$data
  theData <- theData %>% mutate( event_n = which(unique( theData$event_id ) == event_id)[1] )
  
  dendo_data <- theData %>% mutate( target_label = final_data$labels[target_n], source_label = final_data$labels[source_n] ) %>% 
    mutate( ss = str_split(target,"_") ) %>% 
    rowwise %>% mutate( node_label = paste0(ss[length(ss)])) %>%
    select(source_label,target_label,cond_prob,event_n,node_label) %>%
    mutate( cond_prob = as.numeric(cond_prob) ) %>%
    add_row(source_label = NA, target_label = precond, cond_prob=1,event_n=0, node_label= precond, .before = 1)
  
  dendo_data <- dendo_data %>% rowwise %>% mutate(Color = label_colors[ which(labels == node_label)[1]]) %>% ungroup
  
  dendo_data$nsize <- (1+nrow(dendo_data)) - 1:nrow(dendo_data)
  
  dendo_data$order <- round(((max(dendo_data$event_n) - dendo_data$event_n)*20) + (dendo_data$cond_prob*10))*10
  
  dendo_data <- dendo_data %>% arrange(desc(order))
}

ui <- fluidPage(style = "background-color: #fff; height:100vh; ", 

  titlePanel(div(style = "background-color: #fff; border-radius:10px; padding:10px", h1("Multi State Modelling"))),
  
  tabsetPanel(type = "tabs",
      tabPanel(style = "border-left: 1px solid #d4d4d4;", div(style="font-size:20px;font-weight:bold;","Joint"), 
               # Sidebar with a slider input for number of years follow-up 
               sidebarLayout(
                 sidebarPanel(width = 3, style="margin-top:5px; margin-left:5px;",
                   selectInput('year_cat_sb', 'Year Category', c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]"), c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]")),
                   radioButtons("precond_sb", label = "Pre-condition", c("Cerebro-Vascular Disease (CVD)" = "cvd", "Myocardial Infarction (MI)" = "mi"), inline = T, selected = "cvd"),
                 ),
                 mainPanel(width = 9,
                   plotlyOutput("sunburst", width = "auto", height = "800px") ) %>% withSpinner(color="#0dc5c1")
               )
      ),
      tabPanel(style = "border-left: 1px solid #d4d4d4;",div(style="font-size:20px;font-weight:bold;","Conditional - MI"), 
          # Sidebar with a slider input for number of years follow-up 
          sidebarLayout(
              sidebarPanel(width = 3, style="margin-top:5px; margin-left:5px;",
                 # radioButtons("precond", label = "Pre-condition", c("Cerebro-Vascular Disease (CVD)" = "cvd", "Myocardial Infarction (MI)" = "mi"), inline = T, selected = "cvd"),
                 selectInput('year_cat_mi', 'Year Category', c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]"), c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]")),
                 hr(),
                 radioButtons("sex_mi", label = "Sex", c("Female" = 0, "Male" = 1), inline = T, selected = 1),
                 # selectInput('start_cond', 'First event', c("hf","mi","stroke","bleeding"), c("hf","mi","stroke","bleeding")),
                 sliderInput("age_mi", "Age (In years)", min = 30, max = 110, value = 50, step = 5),
                 sliderInput("simd_mi", "Deprivation score (SIMD)", value = 1, min = 1, max = 5, step = 1),
                 radioButtons("prev_hf_mi", label = "Previous Heart Failure (HF)", c("no" = 0, "yes" = 1), inline = T, selected =0),
                 radioButtons("prev_cvd_mi", label = "Previous Cerebro-Vascular Disease (CVD)", c("no" = 0, "yes" = 1), inline = T, selected =0),
            ),
            mainPanel(width = 9,
               collapsibleTreeOutput("dendo_mi", width = "100%", height = "90vh") %>% withSpinner(color="#0dc5c1")
            )
          )
      ),
      tabPanel(style = "border-left: 1px solid #d4d4d4;",div(style="font-size:20px;font-weight:bold;","Conditional - CVD"), 
           # Sidebar with a slider input for number of years follow-up 
           sidebarLayout(
             sidebarPanel(width = 3, style="margin-top:5px; margin-left:5px;",
                          # radioButtons("precond", label = "Pre-condition", c("Cerebro-Vascular Disease (CVD)" = "cvd", "Myocardial Infarction (MI)" = "mi"), inline = T, selected = "cvd"),
                          selectInput('year_cat_cvd', 'Year Category', c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]"), c("(1994,1999]", "(1989,1994]", "(2004,2009]", "(1999,2004]", "(2009,2014]")),
                          hr(),
                          radioButtons("sex_cvd", label = "Sex", c("Female" = 0, "Male" = 1), inline = T, selected = 1),
                          # selectInput('start_cond', 'First event', c("hf","mi","stroke","bleeding"), c("hf","mi","stroke","bleeding")),
                          sliderInput("age_cvd", "Age (In years)", min = 30, max = 110, value = 50, step = 5),
                          sliderInput("simd_cvd", "Deprivation score (SIMD)", value = 1, min = 1, max = 5, step = 1),
                          radioButtons("prev_hf_cvd", label = "Previous Heart Failure (HF)", c("no" = 0, "yes" = 1), inline = T, selected =0),
                          radioButtons("prev_mi_cvd", label = "Previous Myocardial Infarction (MI) ", c("no" = 0, "yes" = 1), inline = T, selected =0),
             ),
             mainPanel(width = 9,
                       collapsibleTreeOutput("dendo_cvd", width = "100%", height = "90vh") %>% withSpinner(color="#0dc5c1")
             )
           )
      )
      # tabPanel(style = "border-left: 1px solid #d4d4d4;","legacy_sankey", plotlyOutput("sankey"))
  ),
)

server <- function(input, output,session) {
 
  # output$selec <- renderText(input$start_cond)
  # source, sex_in, simd, hf_in, cerebro_or_mi, year_cat_in, age_in 

  observeEvent(c(
    input$precond_sb, 
    input$year_cat_sb
  ),{
    
    wprb <- filterWeightedMSMData( input$precond_sb, input$year_cat_sb) %>% arrange(event_1,event_2,event_3,event_4)
    # wprb <- wprb %>% rbind (filterWeightedMSMData( "cvd", input$year_cat_sb) %>% arrange(event_1,event_2,event_3,event_4))
    
    # wprb <- wprb %>% rowwise %>% mutate(Color = label_colors[ which(str_detect(labels, target))[1] ]) %>% ungroup
    # labels <- c( "bleeding", "death", "hf", "mi", "none", "stroke" )
    # label_colors <- c( "#F44336", "#273746", "#9C640C", "#03A9F4", "#E0E0E0 ", "#FFC107" )
    
    wprb <- wprb %>%  mutate( Color = "#E0E0E0" ) %>% 
      mutate( Color = ifelse( str_detect(target, "death"), "#273746", Color  )) %>%
      mutate( Color = ifelse( str_detect(target, "cvd"), "#ead8d8", Color  )) %>%
      mutate( Color = ifelse( str_detect(target, "stroke"), "#FFC107", Color  )) %>%
      mutate( Color = ifelse( str_detect(target, "mi"), "#03A9F4", Color  )) %>%
      mutate( Color = ifelse( str_detect(target, "hf"), "#9C640C", Color  )) %>%
      mutate( Color = ifelse( str_detect(target, "bleeding"), "#F44336", Color  ))
     
    output$sunburst <- renderPlotly({
      plot_ly(wprb,
              width = 1300,
              height = 1300,
              parents = wprb$source,
              labels = wprb$target,
              values = wprb$wr,
              type = 'sunburst',
              marker = list(colors =  wprb$Color), # color = wprb$Color,
              hoverinfo = 'text',
              text = ~paste(wr," %")
      ) %>%
        config(displayModeBar = FALSE, displaylogo = FALSE)#%>% layout(sunburstcolorway = ~Color)

    })
  })
  
  observeEvent(c( 
    input$sex_mi, input$simd_mi, input$prev_hf_mi, input$prev_mi_cvd, input$year_cat_mi, input$age_mi 
  ),{
    # withProgress(message = 'Making plot', value = 0, {
      dendo_data_mi <- prepareDendoData("mi", input$sex_mi, input$simd_mi, input$prev_hf_mi, input$prev_mi_cvd, input$year_cat_mi, input$age_mi)
      # incProgress(1, detail = paste("Doing part", i))
    # })
    output$dendo_mi <- renderCollapsibleTree ( 
      collapsibleTreeNetwork(
        dendo_data_mi,
        fill = "Color",
        attribute = "cond_prob",
        collapsed = TRUE,
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

  })
  
  observeEvent( c(
    input$sex_cvd, input$simd_cvd, input$prev_hf_cvd, input$prev_cvd_mi, input$year_cat_cvd, input$age_cvd
  ),{
      dendo_data_cvd <- prepareDendoData("cvd", input$sex_cvd, input$simd_cvd, input$prev_hf_cvd, input$prev_cvd_mi, input$year_cat_cvd, input$age_cvd)
 
      output$dendo_cvd <- renderCollapsibleTree ( 
        collapsibleTreeNetwork(
          dendo_data_cvd,
          fill = "Color",
          attribute = "cond_prob",
          collapsed = TRUE,
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
  })
}

shinyApp(ui = ui, server = server)
