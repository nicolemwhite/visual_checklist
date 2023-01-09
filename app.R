
## app.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(RColorBrewer)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

colourschemes <- list('Greyscale' = c("#000000","#737373","#BDBDBD","#D9D9D9","#F0F0F0"),#"Greys",
                      'Accent' = c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#666666"),
                      'Dark2'= brewer.pal(n=8,name="Dark2"),
                      'Paired'=brewer.pal(n=8,name="Paired"),
                      "Set1" = brewer.pal(n=8,name="Set1"),
                      "Set2"=brewer.pal(n=8,name="Set2"),
                      "Set3"=brewer.pal(n=8,name="Set3"),
                      'Colour-blind friendly'=  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
themes <- list("Light" = theme_light(),"Minimal" = theme_minimal(),"Black/White" = theme_bw(),"Classic" = theme_classic(),"Gray"=theme_gray())
legend_positions <- list("No" = 'none',"Yes" = 'top')


header<- dashboardHeader(title = "Visual reporting checklists", disable = FALSE, titleWidth  = 600)
  ## Sidebar content

sidebar<- dashboardSidebar(
    useShinyjs(),
    width=600,
    sidebarMenu(
      id = 'sidebar',
      menuItem("Application", tabName = "setup", icon = icon("list"),startExpanded = F),
      menuItem("Customisation", tabName = "customise", icon = icon("wrench"),startExpanded=T),
      
      div(id = 'sidebar_setup',
          conditionalPanel("input.sidebar == 'setup'",
                           fluidRow(
                             column(7,selectInput(inputId="template",label='Select template (TODO)',choices=c("CHEERS","STROBE","TRIPOD","PROBAST",selected="CHEERS"))),
                             column(4,downloadButton("download_template"),style = 'margin-top:35px')),
                           fileInput("upload", "Upload completed template (.csv)", accept = c(".csv"))
          ),
          
          conditionalPanel("input.sidebar == 'customise'",
                           fluidRow(style='margin-left:0px',
                                    column(6,selectInput('colourscheme',label='Choose colour scheme',choices = names(colourschemes),selected = 'Greyscale')),
                                    column(6, selectInput("theme", label = "Select plot theme", choices = names(themes),selected = 'Minimal'))),
                           fluidRow(style='margin-left:0px',column(6,textInput('xlabtext',label='x-axis label',value = 'Value')),
                                    column(6,textInput('ylabtext',label='y-axis label',value = 'Density'))),
                           fluidRow(style='margin-left:0px',column(6,radioButtons(inputId='legend','Display legend?',choices=names(legend_positions),selected = "Yes",inline=TRUE),style = 'margin-top:10px'),column(6,uiOutput("legend_title"))),
                           fluidRow(style='margin-left:0px',
                                    column(3,selectInput("fformat", "Format",c("png" = "png","tiff" = "tiff","jpeg" = "jpeg"), 'png')),
                                    column(3,selectInput(inputId = "fres",label = "Resolution",c("300 dpi"=300,"600 dpi"=600),selected = "300")),
                                    column(3,numericInput(inputId = "fheight",label = "Height (cm)",min = 8,max = 22,step = 1,value = 10)),
                                    column(3,numericInput(inputId = "fwidth",label = "Width (cm)",min = 8,max = 22,step = 1,value = 15))),
                           fluidRow(style='margin-left:0px',column(10,textInput(inputId = 'fig_fname',label='Figure name',value='figure'))),
                           fluidRow(column(6,downloadButton("downloadFigure", "Download Figure",style = 'margin-left:30px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')))
          ),
          ),
      
      menuItem("References", tabName = "citation", icon = icon("list-alt"))
    )
  )
  ## Body content
body <- dashboardBody(
    
    tabItems(
      # main output window
      tabItem(tabName = "setup",
              column(2,selectInput(inputId='chooseViz',label='Select plot',choices=c("Full dataset","Summary by study","Summary by checklist item"),selected="Full dataset",multiple = F)),
              downloadButton("downloadFigure", "Download",style = 'margin-left:0px;margin-top:25px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold'),
              fluidRow(box(title=NULL,solidHeader=T,width=12,column(12,align="center",plotOutput("plot1",width = "1000px",height = "800px"))))
      ),
      #citation info
      tabItem(tabName = 'citation',h3('References'))
    ),
    
    
    #style elements
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #003D58;
                                color: #EAE23B;
                                font-family: "Arial";
                                font-weight: bold;
                                font-size: 24px;
                                margin-left: 0px;

                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #003D58;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #009FE3;
                                font-family: "Arial";
                                font-size: 16px;
                                font-weight:bold;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #009FE3;
                                color: #EAE23B;
                                
                                }
            
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color:#009FE3;
                                color: #003D58;
                                }


                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                              
                                /*other text*/
                                .shiny-output-error-validation {color: #EAE23B;font-weight: bold;}
                                ')))
    
    
  )
  

  
ui <- dashboardPage(header, sidebar, body)
  
server <- function(input, output,session) {

  set.seed(122)
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ",",col_types="c"),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )

  })

  output$head <- renderTable(data())


  plot_studies <- function(){
    plot_data = data() %>% mutate(author_year = paste(author,year)) %>% select(-author,-year) %>%
      gather(item,item_score,-author_year) %>% mutate_at('item_score',~replace_na(.,'missing'))
    if(input$chooseViz=="Full dataset"){
      final_plot <- plot_data %>% ggplot(aes(x=item,y=author_year,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=12),
              legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal') +
        labs(x = 'Checklist item', y = 'Study')
    }
    if(input$chooseViz=="Summary by study"){
      n_items = length(unique(plot_data$item))
      final_plot <- plot_data %>% ggplot(aes(y=author_year,fill=item_score))+geom_bar()+
        scale_x_continuous('Number of items',breaks=0:n_items)+
        theme(panel.background = element_blank(),
              text = element_text(size=12),
              legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')+
        labs(y='Study')

    }
    if(input$chooseViz=="Summary by checklist item"){
      n_studies = length(unique(plot_data$author_year))
      final_plot <- plot_data %>% ggplot(aes(y=item,fill=item_score))+geom_bar()+
        scale_x_continuous('Number of studies',breaks=0:n_studies)+
        theme(panel.background = element_blank(),
              text = element_text(size=12),
              legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')+
        labs(y='Checklist item')
    }
    final_plot <- final_plot +scale_fill_manual(values=cbPalette)
    return(final_plot)
  }

  output$plot1 <- renderPlot({plot_studies()})

}
shinyApp(ui, server)
