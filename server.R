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
  
  
  output$data_title<-renderText({
    if(is.null(input$upload)){return("Interactive data summary")}
    else if(input$chooseViz=='Full dataset'){return("Interactive data summary (drag mouse over plot area)")}
    else{return("Interactive data summary (click plot cell)")}
  })
  getplotColours<- function(){
    colour_list <-unlist((sapply(1:plot_output()$K, function(i) {input[[paste0("itemColour", i, sep="_")]]})))
    out <- paste(lapply(colour_list,function(x) paste0("'",x,"'")),collapse=',')
    return(paste0("c(",out,")"))
    
  }
  
  plot_studies <-function(){
    validate(check_col_names(data()))
    
    show_legend = legend_positions[[input$legend]]
    
    
    plot_data = data() %>% 
      mutate(item_number = row_number()) %>%
      gather(study_label,item_score,-section,-item_number,-checklist_item,-item_full_text,factor_key = T) %>% 
      mutate_at('item_score',~replace_na(.,'Missing') %>% factor(.)) %>%
      mutate_at('item_number',~factor(.)) 
    
    study_labels = switch(input[['studyorder']],'No' = rev(unique(plot_data$study_label)),'Yes' = sort(unique(as.character(plot_data$study_label)),decreasing=T))
    plot_data = plot_data %>% mutate_at('study_label',~factor(.,levels=study_labels))
    
    item_lookup = plot_data %>% distinct(item_number,checklist_item)
    
    if(input$chooseViz=="Full dataset"){
      final_plot <- plot_data %>% 
        ggplot(aes(x=item_number,y=study_label,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        scale_x_discrete(input$xlabtext,breaks=item_lookup$item_number,labels=str_wrap(item_lookup$checklist_item,50))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=12),
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
        scale_y_discrete(input$ylabtext,breaks=item_lookup$item_number,labels=str_wrap(item_lookup$checklist_item,50),limits = rev(item_lookup$item_number))+
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = show_legend)
    }
    
    
    K <-length(levels(plot_data$item_score))
    item_scores<-levels(plot_data$item_score)
    return(list(final_plot=final_plot,plot_data=plot_data,K=K,item_scores=item_scores))
  }
  
  #plot the result only if output$customItem exists
  plot_output<-reactive({plot_studies()})
  
  
  check_col_names <- function(indat = data()){
    col_vars = names(indat)
    if(!all(c('section','checklist_item') %in% col_vars)){paste("The variables 'section' and 'checklist_item' must appear as variable names in the dataset. Please update your dataset and upload again.")}
    else{NULL}
  }
  check_total_items <- function(K=plot_output()$K,chosen_palette=NULL){
    if(K>chosen_palette){paste("The number of scoring categories is greater than the number of colours available. Choose a different colour scheme or reduce the number of item categories in the dataset")}
    else{NULL}
  }
  
  
  create_custom_plot <- function(){
    validate(check_total_items(plot_output()$K,length(colourschemes[[input$colourscheme]])))
    plot_colours <- unlist(sapply(1:plot_output()$K, function(i) {input[[paste0("itemColour", i, sep="_")]]}))
    
    if(length(plot_colours)>0){
      return(plot_output()$final_plot+scale_fill_manual(values=plot_colours))
    }
    else{NULL}
  }
  
  
  
  
  
  
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
  
  
  
  
  output$plot_brush_click <- renderUI({
    output$plot1 <- renderPlot(
      width = function() input$fwidth,
      height = function() input$fheight,
      {
        req(plot_output())
        create_custom_plot()
      })
    
    if(input$chooseViz=='Full dataset'){return(plotOutput("plot1",  brush = "plot_brush", height="100%",width="100%"))}
    else{return(plotOutput("plot1",  click = "plot_click",height="100%",width="100%"))}
  })
  
  custom_data_table <- function(){
    if(input$chooseViz=='Full dataset'){
      if(is.null(input$plot_brush)){tab_output<-NULL;text_output<-''}
      else{
        
        tab_output = brushedPoints(isolate(plot_output()$plot_data), input$plot_brush,xvar="item_number",yvar="study_label") %>%
          #mutate(checklist_item=paste0(item_number,'. ',item_text)) %>% 
          mutate_at('section',~factor(.,levels=unique(.))) %>%
          mutate_at('study_label',~factor(.,levels=unique(.))) %>%
          select(section,study_label,checklist_item,item_score) %>% spread(study_label,item_score) %>%
          rename_with(function(x) make_clean_names(x,case='title'),.cols=everything()) 
        text_output <-''
      }
    }
    if(input$chooseViz=='Summary by study'){
      if(is.null(input$plot_click$x)){tab_output<-NULL;text_output<-''}
      else{
        index_dat <- plot_output()$plot_data %>% arrange(study_label,desc(item_score)) %>%
          group_by(study_label) %>%  mutate(item_score_index=row_number()) %>% ungroup() %>%
          mutate(study_label_index=as.numeric(factor(study_label,labels=1:length(unique(study_label)))))
        index_intervals = index_dat %>% group_by(study_label_index,item_score) %>% summarise(a=min(item_score_index),b=max(item_score_index),.groups='drop')
        
        # define xvar, yvar based on plot click
        index_study <- round(as.numeric((input$plot_click$y)))
        index_score <- ceiling(as.numeric((input$plot_click$x)))
        
        
        target_study_score = filter(index_dat,study_label_index==index_study,item_score_index==index_score) %>% select(study_label_index,item_score)
        
        summary_dat = semi_join(index_dat,target_study_score, by = c("study_label_index", "item_score"))
        target_study = unique(summary_dat$study_label)
        target_item_score = unique(summary_dat$item_score)
        
        tab_output = summary_dat %>% 
          mutate_at('section',~factor(.,levels=unique(.))) %>%
          group_by(section) %>% summarise('Checklist item(s)' = paste(checklist_item,collapse = '; '),.groups='drop') %>% rename('Section'=section) 
        text_output = paste0(target_study,': ',target_item_score)
        
      }
      
    }
    if(input$chooseViz=='Summary by checklist item'){
      if(is.null(input$plot_click$x)){tab_output<-NULL;text_output<-''}
      else{
        index_dat <- plot_output()$plot_data %>% 
          arrange(item_number,desc(item_score)) %>% 
          group_by(item_number) %>%  mutate(item_score_index=row_number()) %>% ungroup() 
        # define xvar, yvar based on plot click; nb: need to reverse map y
        total_items = max(as.numeric(index_dat$item_number))
        index_item <- (total_items-round(as.numeric((input$plot_click$y))))+1
        index_score <- ceiling(as.numeric((input$plot_click$x)))
        
        #get the corresponding study (yaxis) and item score category (fill)
        target_item_score = filter(index_dat,item_number==index_item,item_score_index==index_score) %>% select(item_number,item_score)
        
        summary_dat = semi_join(index_dat,target_item_score,by = c("item_number", "item_score"))
        target_checklist_item = paste0(unique(summary_dat$checklist_item))
        target_item_score = unique(summary_dat$item_score)
        
        target_section = unique(summary_dat$section)
        target_studies = paste(summary_dat$study_label,collapse='; ')
        
        tab_output = summary_dat %>% group_by(section) %>% summarise('Study label(s)'=paste(study_label,collapse='; ')) %>% rename('Section'=section)
        text_output = paste0(target_checklist_item,': ',target_item_score)
      }
      
    }
    return(list(tab_output = tab_output,text_output=text_output))
  }
  
  output$summary_table <- renderTable({custom_data_table()$tab_output})
  output$summary_text <- renderPrint({cat(custom_data_table()$text_output,'\n')})
  output$input_error <- renderText({print_errors()})
  #output$summary_tag <- renderText() #need additional renderPrint for item score category and study
  
  
  
  
  output$ggplotFull <- renderPrint({
    cat(paste0("
    library(tidyverse)
    library(vroom)
    data = vroom('",input$upload$name,"', delim = ',',col_types='c')
    
    plot_data = data %>% 
      mutate(item_number = row_number()) %>%
      gather(study_label,item_score,-section,-item_number,-checklist_item,-item_full_text,factor_key = T) %>% 
      mutate_at('item_score',~replace_na(.,'Missing') %>% factor(.)) %>%
      mutate_at('item_number',~factor(.)) 

      item_lookup = plot_data %>% distinct(item_number,checklist_item)
      
      final_plot <- plot_data %>% ggplot(aes(x=item_number,y=study_label,fill=item_score))+
        geom_tile(colour = 'white', size = 0.5) +
        scale_x_discrete('",input$xlabtext,"',breaks=item_lookup$item_number,labels=str_wrap(item_lookup$checklist_item,50))+
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
    plot_data = data %>% 
      mutate(item_number = row_number()) %>%
      gather(study_label,item_score,-section,-item_number,-checklist_item,-item_full_text,factor_key = T) %>% 
      mutate_at('item_score',~replace_na(.,'Missing') %>% factor(.)) %>%
      mutate_at('item_number',~factor(.)) 
      
      item_lookup = plot_data %>% distinct(item_number,checklist_item)
      
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
    plot_data = data %>% 
      mutate(item_number = row_number()) %>%
      gather(study_label,item_score,-section,-item_number,-checklist_item,-item_full_text,factor_key = T) %>% 
      mutate_at('item_score',~replace_na(.,'Missing') %>% factor(.)) %>%
      mutate_at('item_number',~factor(.)) 
      
      item_lookup = plot_data %>% distinct(item_number,checklist_item)
      
      final_plot <- plot_data %>% ggplot(aes(x=item_number,fill=item_score))+
        geom_bar()+
        scale_x_discrete('",input$xlabtext,"',breaks=0:n_studies)+
        scale_y_discrete('",input$ylabtext,"',breaks=item_lookup$item_number,labels=str_wrap(item_lookup$checklist_item,40),limits = rev(item_lookup$item_number))+
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank(),
              text = element_text(size=14),
              legend.title = element_blank(),legend.position = ",input$legend,") +
        scale_fill_manual(values=",getplotColours(),")
        
      final_plot
      "
    ))
    
  })
  
  
  
  
  
  #figure
  fn_download_fig <- function()
  {
    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    
    # open file dependent on format    
    if(input$fformat=="png") png(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="px")
    if(input$fformat=="tiff") tiff(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="px", compression="lzw")
    if(input$fformat=="jpeg") jpeg(fn_downloadname_fig(), height=fheight, width=fwidth, res=fres, units="px", quality=100)
    
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
  
  #download outputs
  #table - to do
  fn_download_tab <- function(){
    ftab <- custom_data_table()$tab_output %>% flextable() %>% fontsize(size=9,part=c("all")) %>% width(width=1)
    save_as_docx(ftab,path=fn_downloadname_tab())
  }
  # create filename
  fn_downloadname_tab <- reactive({
    fname = "checklist_table"
    filename <- paste0(fname,".docx",sep="")
    return(filename)
  })
  
  # download handler
  
  output$downloadTable<- downloadHandler(
    filename = fn_downloadname_tab,
    content = function(file) {
      fn_download_tab()
      file.copy(fn_downloadname_tab(), file, overwrite=T)
    }
  ) 
  
  #download template

  fn_download_template <- function(){
    ftab = create_template(choose_template=input$template,indat=template_data)
    write.table(ftab,file=fn_downloadname_template(),na="",sep=',',row.names = FALSE)
  }
  
  
  fn_downloadname_template <- reactive({
    fname = tolower(input$template)
    filename <- paste0('template_',fname,'.csv',sep="")
    
    return(filename)
  })
  
  
  output$download_template<-downloadHandler(
    filename = fn_downloadname_template,
    
    content = function(file) {
      fn_download_template()
      file.copy(fn_downloadname_template(),file, overwrite=T)
    })
  
  
  #clear temp files
  session$onSessionEnded(function() { 
    tempname = grep('.csv',list.files(),value=T)
    #tempname = isolate(fn_downloadname_template())
      unlink(tempname)

    fname = isolate(fn_downloadname_fig())
    if (file.exists(fname)) {
      unlink(fname)
    } else(NULL)
    tname = isolate(fn_downloadname_tab())
    if (file.exists(tname)) {
      unlink(tname)
    } else(NULL)    
  })

    
}

