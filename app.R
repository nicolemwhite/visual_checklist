## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ui <- dashboardPage(

  dashboardHeader(title = "Visual EQUATOR checklists", disable = FALSE, titleWidth  = 400),

  dashboardSidebar(

    width=400,
  
    
    sidebarMenu(
      
      
      id = 'sidebar',
      
      style = "position: relative; overflow: visible;",
      
      
      menuItem("Application", tabName = "application", icon = icon("list")),
      
      div( id = 'sidebar_setup',

           conditionalPanel("input.sidebar === 'application'",

                            fluidRow(

                              column(7,selectInput(inputId="template",label='Select template (TODO)',choices=c("CHEERS","STROBE","TRIPOD","PROBAST",selected="CHEERS"))),

                              column(4,downloadButton("download_template"),style = 'margin-top:35px')),

                            fileInput("upload", "Upload completed template (.csv)", accept = c(".csv")),

                            fluidRow(column(7,h4(strong("Figure customisation (TODO)")),style = 'margin-left:15px'))

                            
           )),

      menuItem("FAQ", tabName = "faq", icon = icon("question")),

      menuItem("References", tabName = "citation", icon = icon("list-alt")))),

  dashboardBody(
tabItems(
tabItem(tabName = "application",
fluidRow(box(title='Figure',solidHeader=T,width=12,column(12,align="center",plotOutput("plot1",width = "1000px",height = "800px")))),
fluidRow(box(title="Study summary (TODO)",collapsible=T,solidHeader=T,width=12,NULL))

      ),
      
      
      #faq
      
      
      tabItem(tabName = 'faq',h3('Frequently asked questions')),
      
      
      #citation info
      
      
      tabItem(tabName = 'citation',h3('References'))
      
      
    ),

    tags$head(tags$style(HTML('

 
                                /* logo */

 
                                .skin-blue .main-header .logo {

 
                                background-color: #003D58;

 
                                color: #EAE23B;

 
                                font-family: "Georgia", Times, "Times New Roman", serif;

 
                                font-weight: bold;

 
                                }

 
                                /* logo when hovered */

 
                                .skin-blue .main-header .logo:hover {

 
                                background-color: #003D58;

 
                                }

                                /* navbar (rest of the header) */

 
                                .skin-blue .main-header .navbar {

 
                                background-color: #003D58;

 
                                }

 
                                /* main sidebar */

 
                                .skin-blue .main-sidebar {

 
                                background-color: #009FE3;

 
                                font-family: "Georgia", Times, "Times New Roman", serif;

 
                                font-size: 14px;

 
                                }

 
                                

 
                                /* active selected tab in the sidebarmenu */

 
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{

 
                                background-color: #009FE3;

 
                                color: #EAE23B;

 
                                }

 
            

 
                                /* other links in the sidebarmenu */

 
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{

 
                                # background-color: #009FE3;

 
                                color: #000000;
 
                                }

                                /* other links in the sidebarmenu when hovered */

 
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{

 
                                background-color: #EAE23B;

 
                                }

 
                                /* toggle button when hovered  */

 
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{

 
                                background-color: #EAE23B;

 
                                }


 
                                /* body */

 
                                .content-wrapper, .right-side {

 
                                background-color: #FFFFFF;

 
                                }

                                ')))
    )
)

server <- function(input, output,session) {

  data <- reactive({

    req(input$upload)

    ext <- tools::file_ext(input$upload$name)

    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ",",col_types="c"),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file") )

  })
 output$head <- renderTable(data())
output$plot1 <- renderPlot({data() %>% mutate(author_year = paste(author,year)) %>% select(-author,-year) %>%
gather(item,item_score,-author_year) %>% mutate_at('item_score',~replace_na(.,'missing')) %>%
ggplot(aes(x=item,y=author_year,fill=item_score))+
 geom_tile(colour = 'white', size = 0.5) +
theme(axis.text.x = element_text(angle = 45, hjust=1),
 panel.background = element_blank(),
text = element_text(size=12)) +
labs(x = 'Checklist item', y = 'Study') +
scale_fill_manual(values=cbPalette)+

    theme(legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')

  })
}
