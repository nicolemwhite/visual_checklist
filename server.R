shinyServer(function(input, output,session) {
  
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
  
})

