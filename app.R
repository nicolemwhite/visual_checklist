## app.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(colourpicker)
library(RColorBrewer)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

colourschemes <- list('Greyscale' = c("#000000","#737373","#BDBDBD","#D9D9D9","#F0F0F0"),#"Greys",
                      'Accent' = c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#666666"),
                      'Dark2'= brewer.pal(n=8,name="Dark2"),
                      'Paired'=brewer.pal(n=8,name="Paired"),
                      "Set1" = brewer.pal(n=8,name="Set1"),
                      "Set2"=brewer.pal(n=8,name="Set2"),
                      "Set3"=brewer.pal(n=8,name="Set3"),
                      'Colour-blind friendly'=  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                      'Custom'=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
themes <- list("Light" = theme_light(),"Minimal" = theme_minimal(),"Black/White" = theme_bw(),"Classic" = theme_classic(),"Gray"=theme_gray())
legend_positions <- list("No" = 'none',"Yes" = 'top')


header<- dashboardHeader(title = "Visual reporting checklists", disable = FALSE, titleWidth  = 600)
## Sidebar content
sidebar <- dashboardSidebar(
  width=600,
  sidebarMenu(id = "sidebar",
              menuItem("Application", tabName = "setup", icon = icon("list"),startExpanded = T),
              #menuItem("Customisation", tabName = "customise", icon = icon("wrench"),startExpanded=F),
              menuItem("Code", tabName = "code", icon = icon("code"),startExpanded = T),
              menuItem("References", tabName = "citation", icon = icon("list-alt")),
              conditionalPanel(condition="input.sidebar == 'setup'",
                               fluidRow(
                                 column(7,selectInput(inputId="template",label='Select template (TODO)',choices=c("CHEERS","STROBE","TRIPOD","PROBAST",selected="CHEERS"))),
                                 column(4,downloadButton("download_template",style = 'margin-top:35px;background-color:#f9f9f9;font-family: Arial;font-weight: bold'))),
                               fluidRow(fileInput("upload", "Upload completed template (.csv)", accept = c(".csv")),style='margin-left:15px'),
                               fluidRow(style='margin-left:0px',
                                        column(6,selectInput('colourscheme',label='Choose colour scheme',choices = names(colourschemes),selected = 'Custom'))),
                               fluidRow(style='margin-left:0px',column(6,textInput('xlabtext',label='x-axis label',value = NULL)),
                                        column(6,textInput('ylabtext',label='y-axis label',value = NULL))),
                               fluidRow(column(6,radioButtons(inputId='legend','Display legend?',choices=names(legend_positions),selected = "Yes",inline=TRUE),style='margin-left:15px')),
                               fluidRow(uiOutput("customItem"),style='margin-top:10px'),
                               column(6, tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/", "Hexidecimal colour codes",style='color: #EAE23B'),style='margin-top:0px;margin-left:15px')
              )
  )
)


## Body content
body <- dashboardBody(
  
  tabItems(
    # main output window
    tabItem(tabName = "setup",
            fluidPage(

                         fluidRow(
                           column(3,selectInput(inputId='chooseViz',label='Select plot',choices=c("Full dataset","Summary by study","Summary by checklist item"),selected="Full dataset",multiple = F)),
                           downloadButton("downloadFigure", "Download",style = 'margin-left:0px;margin-top:25px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold'),
                           column(1,selectInput("fformat", "Format",c("png" = "png","tiff" = "tiff","jpeg" = "jpeg"), 'png')),
                           column(2,selectInput(inputId = "fres",label = "Resolution (dpi)",c("300 dpi"=300,"600 dpi"=600),selected = "300")),
                           column(1,numericInput(inputId = "fheight",label = "Height (px)",min = 100, value = 600)),
                           column(1,numericInput(inputId = "fwidth",label = "Width (px)",min = 100,value = 1000))
                         ),
            column(12,align="center",plotOutput("plot1",  brush = "plot_brush"))
            #plotOutput("plot1", brush = "plot_brush"),
            #column(12,align="center",tableOutput("data"))
            )
            #tableOutput("data")
    ),

    tabItem(tabName='code',
            box(width=12,title="Full dataset",column(width=12,verbatimTextOutput('ggplotFull')),expanded=T,collapsible = T),
            box(width=12,title="Summary by study",column(width=12,verbatimTextOutput('ggplotByStudy')),expanded=T,collapsible = T),
            box(width=12,title="Summary by checklist item",column(width=12,verbatimTextOutput('ggplotByItem')),expanded=T,collapsible = T)
            ),
    #citation info: 
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
                                height: 90vh; overflow-y: auto;
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
  
  #reactive functions for colourpicker
  choose_palette <- reactive({
    if (input$colourscheme=='Custom'){"square"}
    else {"limited"}
  })
  choose_colours <-reactive({
    if (input$colourscheme=='Custom'){NULL}
    else {colourschemes[[input$colourscheme]]}
  })
  
  define_axis_labels<-reactive({
    switch(input$chooseViz,
           "Full dataset" = c('Checklist item','Study'),
           "Summary by study" = c('Number of checklist items','Study'),
           "Summary by checklist item" = c('Number of studies','Checklist item')
    )
  })

  
  observe({
    updateTextInput(session,"xlabtext",value=define_axis_labels()[1])
    updateTextInput(session,"ylabtext",value=define_axis_labels()[2])
  })
  
  observeEvent(input$upload,{
    
    #run the main function to create custom UI
    plot_output<-plot_studies()
    
    output$customItem <- renderUI({
      lapply(1:plot_output$K,function(i) 
        column(5,colourInput(input=paste0('itemColour',i,sep='_'), 
                             label=plot_output$item_scores[i],value=colourschemes[[input$colourscheme]][i],
                             palette = choose_palette(),allowedCols= choose_colours(),closeOnClick = T),style='margin-left:10px'))      
  
      
      
    })
    

    
    
  })
  
  #plot the result only if output$customItem exists
  plot_output<-reactive({plot_studies()})
  
  

  
  create_custom_plot <- function(){
    plot_colours <- unlist(sapply(1:plot_output()$K, function(i) {input[[paste0("itemColour", i, sep="_")]]}))
    
    if(length(plot_colours)>0){
      return(plot_output()$final_plot+scale_fill_manual(values=plot_colours))
    }
    else{NULL}
    
  }
  
  output$plot1 <- renderPlot(
    width = function() input$fwidth,
    height = function() input$fheight,
    {
      req(plot_output())
      create_custom_plot()
    })
  

  output$data <- renderTable({
    #nearPoints(isolate(plot_output()$plot_data), input$plot_click,xvar="item_number",yvar="study_label")
    brushedPoints(isolate(plot_output()$plot_data), input$plot_brush,xvar="item_number",yvar="study_label")
  })
  
  
  
  
  getplotColours<- function(){
    colour_list <-unlist((sapply(1:plot_output()$K, function(i) {input[[paste0("itemColour", i, sep="_")]]})))
    out <- paste(lapply(colour_list,function(x) paste0("'",x,"'")),collapse=',')
    return(paste0("c(",out,")"))
    
  }
  
  output$ggplotFull <- renderPrint({
    cat(paste0("
    library(tidyverse)
    library(vroom)
    
    data = vroom('",input$upload$name,"', delim = ',',col_types='c')
    
    plot_data = data() %>% 
      gather(study_label,item_score,-section,-item_number,-item_text) %>% mutate_at('item_score',~replace_na(.,'Missing')) %>%
      mutate_at('item_score',~factor(.)) %>%
      mutate_at('item_number',~factor(.,levels=1:length(unique(item_number)))) 
      
      item_lookup = plot_data %>% distinct(item_number,item_text)
      
      final_plot <- plot_data %>% ggplot(aes(x=item_number,y=study_label,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        scale_x_discrete('",input$xlabtext,"',breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = ",input$legend,") +
        labs(y = '",input$ylabtext,"')+scale_fill_manual(values=",getplotColours(),")
        
      final_plot
      "
    ))
    
  })
  
  
  output$ggplotByStudy <- renderPrint({
    cat(paste0("
    library(tidyverse)
    library(vroom)
    
    data = vroom('",input$upload$name,"', delim = ',',col_types='c')
    n_items = length(unique(plot_data$item_number)) 
    plot_data = data() %>% 
      gather(study_label,item_score,-section,-item_number,-item_text) %>% mutate_at('item_score',~replace_na(.,'Missing')) %>%
      mutate_at('item_score',~factor(.)) %>%
      mutate_at('item_number',~factor(.,levels=1:length(unique(item_number)))) 
      
      item_lookup = plot_data %>% distinct(item_number,item_text)
      
      final_plot <- plot_data %>% ggplot(aes(x=study_label,fill=item_score))+
        geom_bar()+
        scale_x_discrete('",input$xlabtext,"',breaks=0:n_items)+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = ",input$legend,") +
        labs(y = '",input$ylabtext,"')+scale_fill_manual(values=",getplotColours(),")
        
        final_plot
      "
    ))
  })
  
  output$ggplotByItem <- renderPrint({
    cat(paste0("
    library(tidyverse)
    library(vroom)
    
    data = vroom('",input$upload$name,"', delim = ',',col_types='c')
    
    n_studies = length(unique(plot_data$study_label)) 
    plot_data = data() %>% 
      gather(study_label,item_score,-section,-item_number,-item_text) %>% mutate_at('item_score',~replace_na(.,'Missing')) %>%
      mutate_at('item_score',~factor(.)) %>%
      mutate_at('item_number',~factor(.,levels=1:length(unique(item_number)))) 
      
      item_lookup = plot_data %>% distinct(item_number,item_text)
      
      final_plot <- plot_data %>% ggplot(aes(x=item_number,fill=item_score))+
        geom_bar()+
        scale_x_discrete('",input$xlabtext,"',breaks=0:n_studies)+
        scale_y_discrete('",input$ylabtext,"',breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40),limits = rev(item_lookup$item_number))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = ",input$legend,") +
        scale_fill_manual(values=",getplotColours(),")
        
        final_plot
      "
    ))

  })
  
  
  plot_studies <-function(){
    show_legend = legend_positions[[input$legend]]
    
    plot_data = data() %>% 
      gather(study_label,item_score,-section,-item_number,-item_text) %>% mutate_at('item_score',~replace_na(.,'Missing')) %>%
      mutate_at('item_score',~factor(.)) %>%
      mutate_at('item_number',~factor(.,levels=1:length(unique(item_number)))) 
    
    item_lookup = plot_data %>% distinct(item_number,item_text)
    
    if(input$chooseViz=="Full dataset"){
      final_plot <- plot_data %>% 
        ggplot(aes(x=item_number,y=study_label,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        scale_x_discrete(input$xlabtext,breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = show_legend) +
        labs(y = input$ylabtext)
    }
    
    if(input$chooseViz=="Summary by study"){
      n_items = length(unique(plot_data$item_number))
      final_plot <- plot_data %>% 
        ggplot(aes(y=study_label,fill=item_score))+geom_bar()+
        scale_x_continuous(input$xlabtext,breaks=0:n_items)+
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = show_legend)+
        labs(y=input$ylabtext)
      
    }
    if(input$chooseViz=="Summary by checklist item"){
      n_studies = length(unique(plot_data$study_label))
      final_plot <- plot_data %>% 
        ggplot(aes(y=item_number,fill=item_score))+geom_bar()+
        scale_x_continuous(input$xlabtext,breaks=0:n_studies)+
        scale_y_discrete(input$ylabtext,breaks=item_lookup$item_number,labels=str_wrap(item_lookup$item_text,40),limits = rev(item_lookup$item_number))+
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = show_legend)
    }
    
    
    K <-length(levels(plot_data$item_score))
    item_scores<-levels(plot_data$item_score)
    return(list(final_plot=final_plot,plot_data=plot_data,K=K,item_scores=item_scores))
  }
  
  
  
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
    
    g = create_custom_plot()
    print(g)
    dev.off()
  }
  # create filename
  fn_downloadname_fig <- reactive({
    
    fname = "checklist_figure" #isolate(input$fig_fname)
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
  
  
}
shinyApp(ui, server)
