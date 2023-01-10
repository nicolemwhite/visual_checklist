
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
sidebar <- dashboardSidebar(
  width=600,
  sidebarMenu(id = "sidebar",
              menuItem("Application", tabName = "setup", icon = icon("list"),startExpanded = T),
              menuItem("Customisation", tabName = "customise", icon = icon("wrench"),startExpanded=F),
              menuItem("References", tabName = "citation", icon = icon("list-alt")),
              conditionalPanel(condition="input.sidebar == 'setup'",
                               fluidRow(
                                 column(7,selectInput(inputId="template",label='Select template (TODO)',choices=c("CHEERS","STROBE","TRIPOD","PROBAST",selected="CHEERS"))),
                                 column(4,downloadButton("download_template"),style = 'margin-top:35px')),
                               fileInput("upload", "Upload completed template (.csv)", accept = c(".csv"))
                               ),
              
              conditionalPanel(condition="input.sidebar == 'customise'",
                               fluidRow(style='margin-left:0px',
                                        column(6,selectInput('colourscheme',label='Choose colour scheme (todo)',choices = names(colourschemes),selected = 'Greyscale')),
                                        column(6, radioButtons(inputId='legend','Display legend?',choices=names(legend_positions),selected = "Yes",inline=TRUE))),
                               fluidRow(style='margin-left:0px',column(6,textInput('xlabtext',label='x-axis label (todo)',value = 'todo')),
                                        column(6,textInput('ylabtext',label='y-axis label (todo)',value = 'todo'))),
                               fluidRow(uiOutput("sliders"),style='margin-top:10px')
              )
  )
)

## Body content
body <- dashboardBody(
  
  tabItems(
    # main output window
    tabItem(tabName = "setup",
            fluidRow(
              column(3,selectInput(inputId='chooseViz',label='Select plot',choices=c("Full dataset","Summary by study","Summary by checklist item"),selected="Full dataset",multiple = F)),
              downloadButton("downloadFigure", "Download",style = 'margin-left:0px;margin-top:25px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold'),
              column(1,selectInput("fformat", "Format",c("png" = "png","tiff" = "tiff","jpeg" = "jpeg"), 'png')),
              column(2,selectInput(inputId = "fres",label = "Resolution (dpi)",c("300 dpi"=300,"600 dpi"=600),selected = "300")),
              column(1,numericInput(inputId = "fheight",label = "Height (in)",min = 1, value = 10)),
              column(1,numericInput(inputId = "fwidth",label = "Width (in)",min = 1,value = 15))
            ),
            fluidRow(box(title=NULL,solidHeader=T,width=12,column(12,align="center",plotOutput("plot1",width = "auto",height = "800px"))))
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
  


  
  plot_studies <-function(){
    show_legend = legend_positions[[input$legend]]
    #plot_data <- prepare_plot_data()$plot_data
    plot_data = data() %>% 
      gather(study_label,item_score,-section,-item_number,-item_text) %>% mutate_at('item_score',~replace_na(.,'Missing')) %>%
      mutate_at('item_score',~factor(.)) %>%
      mutate_at('item_number',~factor(.,levels=1:length(unique(item_number)))) 
    
   item_lookup = plot_data %>% distinct(item_number,item_text)
    
    if(input$chooseViz=="Full dataset"){
      final_plot <- plot_data %>% ggplot(aes(x=item_number,y=study_label,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        scale_x_discrete("Checklist item",breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = show_legend,legend.direction = 'horizontal') +
        labs(y = 'Study')
    }
   
    if(input$chooseViz=="Summary by study"){
      n_items = length(unique(plot_data$item_number))
      final_plot <- plot_data %>% ggplot(aes(y=study_label,fill=item_score))+geom_bar()+
        scale_x_continuous('Number of items',breaks=0:n_items)+
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')+
        labs(y='Study')
      
    }
    if(input$chooseViz=="Summary by checklist item"){
      n_studies = length(unique(plot_data$study_label))
      final_plot <- plot_data %>% ggplot(aes(y=item_number,fill=item_score))+geom_bar()+
        scale_x_continuous('Number of studies',breaks=0:n_studies)+
        scale_y_discrete("Checklist item",breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40),limits = rev(item_lookup$item_number))+
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')
    }
    final_plot <- final_plot +scale_fill_manual(values=cbPalette)
    return(list(final_plot=final_plot,plot_data=plot_data))
  }

  output$plot1 <- renderPlot({plot_studies()$final_plot})
  
  #figure
  fn_download_fig <- function()
  {
    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    
    # open file dependent on format    
    if(input$fformat=="png") png(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="in")
    if(input$fformat=="tiff") tiff(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="in", compression="lzw")
    if(input$fformat=="jpeg") jpeg(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="in", quality=100)
    
    g = plot_studies()
    print(g)
    dev.off()
  }
  # create filename
  fn_downloadname_fig <- reactive({
    
    fname = "Figure" #isolate(input$fig_fname)
    if(input$fformat=="png") filename <- paste0(fname,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(fname,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(fname,".jpg",sep="")
    return(filename)
  })
  
  # download handler
  output$downloadFigure <- downloadHandler(
    filename = fn_downloadname_fig,
    content = function(file) {
      fn_download_fig()
      file.copy(fn_downloadname_fig(), file, overwrite=T)
    }
  )
  #examle dynamic number of inputs
  n_categories <- reactiveValues(K=NULL,item_label = NULL)
  
  observeEvent(input$upload,{
  n_categories$item_label<<-levels(plot_studies()$plot_data$item_score)
  n_categories$K<<-length(unique(plot_studies()$plot_data$item_score))
  })

  output$sliders <- renderUI({
    #item_labels
    if(!is.null(n_categories$K)){
      item_order <- rep("",n_categories$K);item_order[1]<-'Order'
      item_colour <- rep("",n_categories$K);item_colour[1]<-'Colour'
      tagList(
        h4('Customise item scores',style='margin-left:40px;font-family: "Arial";font-size: 16px;font-weight:bold;'),
        lapply(1:as.integer(n_categories$K), function(i) 
          fluidRow(
            column(4,HTML(paste0('<br>',n_categories$item_label[[i]],'<br>')),style='margin-top:20px;text-align: right'),
            column(2,numericInput(input=paste0('itemLabel',i),value=i,label=item_order[i])),
            column(3,textInput(input=paste0('itemColour',i),value=cbPalette[i],label=item_colour[i]))
          )
        ),
        column(4,actionButton(inputId = "updateItems",label="Update items",icon=icon('arrows'),style='margin-left:30px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')),
      )
    }
    else NULL
  })
  
  # #todo: if item features updates, makes changes in plot_data
  # observeEvent(input$updateItems){
  #   NULL
  # }
  

  
  
  #column(4,  HTML('<b>A lnger title</b>'),style='margin-top:20px'),
  #column(2, numericInput("a1", label = "Order", value = 1))
  
}
shinyApp(ui, server)
