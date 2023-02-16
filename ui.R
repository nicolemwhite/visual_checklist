header<- dashboardHeader(title = "Visual reporting checklists", disable = FALSE, titleWidth  = 500)
## Sidebar content
sidebar <- dashboardSidebar(
  width=500,
  sidebarMenu(id = "sidebar",
              menuItem("Home",tabName='home',icon=icon("home"),startExpanded = T),
              menuItem("Application", tabName = "setup", icon = icon("list"),startExpanded = T),
              menuItem("Code", tabName = "code", icon = icon("code"),startExpanded = T),
              #menuItem("Useful resources", tabName = "resources", icon = icon("list-alt")),
              conditionalPanel(condition="input.sidebar == 'setup'",
                               fluidRow(fileInput("upload", "Upload completed template (.csv)", accept = c(".csv")),style='margin-left:15px'),
                               fluidRow(style='margin-left:0px',
                                        column(6,selectInput('colourscheme',label='Choose colour scheme',choices = names(colourschemes),selected = 'Custom'))),
                               fluidRow(style='margin-left:0px',column(6,textInput('xlabtext',label='x-axis label',value = NULL))),
                               fluidRow(style='margin-left:0px',column(6,textInput('ylabtext',label='y-axis label',value = NULL))),
                               fluidRow(style ='margin-left:0px', column(12,radioButtons(inputId='studyorder','Arrange studies in alphabetical order',choices=c("No","Yes"),selected = "No",inline=TRUE))),
                               fluidRow(column(6,radioButtons(inputId='legend','Display legend',choices=names(legend_positions),selected = "Yes",inline=TRUE),style='margin-left:15px')),
                               
                               fluidRow(uiOutput("customItem"),style='margin-top:10px'),
                               column(6, tags$a(href="http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/", "Hexadecimal colour codes",style='color: #EAE23B'),style='margin-top:0px;margin-left:15px')
              )
  )
)


## Body content
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName='home',
            p("Reporting checklists provide expert guidance to researchers on how to transparently report details of their study. Summarising adherence to reporting checklists across multiple studies is a useful way to assess current trends in reporting, for example, as part of a literature review."),
            p("This Shiny application summarises assessments of individual studies against reporting checklists, based on data provided by the user. Results are summarised as figures and tables, which can be downloaded for use in reports or publications."),
            br(),
            p("To get started, download a template from the dropdown menu below:"),
            fluidRow(
              column(3,selectInput(inputId="template",label='Select template',choices=c("CHEERS","CONSORT","PRISMA","PRISMA ScR","PROBAST","SPIRIT","SQUIRE","SRQR","STARD","STROBE","TIDIER","TRIPOD","Custom"),selected="CHEERS"),style='margin-left:15px'),
              downloadButton("download_template",style = 'margin-left:0px;margin-top:25px;background-color:	#f9f9f9;font-family: Arial;font-weight: bold')
            ),
            br(),
            br(),
            p("Guidance for template use",style='font-weight: bold'),
            p("1. Data for each study is entered as a separate column. Study names can be changed as needed. For example, Smith et. al (2023) in place of Study 1. Assigned field names will be displayed in all application figures and data summaries."),
            p("2. Field values for each study should be entered as short text labels (e.g, 'Yes', 'No', 'Partial' or 'Not applicable). Study responses are used to determine the number of categories represented in figures and summary tables."),
            p("3. The field names 'section' and 'checklist_list' must not be changed in the template. Selected rows may be removed from the template in cases where reporting is limited to a subset of checklist items (e.g., from the Methods section)."),
            p("4. Save the completed template as a an .xlsx or .csv file"),
            p("5. Upload the completed template in the Application menu"),
            br(),
            p('Full details are provided in the application',a(href='https://osf.io/preprints/uhqzf/','vignette'),'. Questions about ShinyPrior and suggestions for future updates can be sent to',a(href="https://www.aushsi.org.au/about-us/team/nicole-white/", "Nicole White")),
            br(),
            p("Recommended citation",style='font-weight: bold'),
            p('White NM, Borg DN, Barnett AG. A Shiny application to summarise study adherence to
reporting checklists. OSF Preprints [date here]. https://www.doi.org/x.'),
            br(),
            strong("Contributors",style='font-weight: bold'),
            br(),
            p('Nicole White (Conceptualization, Methodology, Software, Writing - Original Draft)'),
            p('David Borg (Conceptualization, Validation, Writing - Original Draft)'),
            p('Adrian Barnett (Conceptualization, Validation, Writing - Review and Editing)'),
            br(),
            strong("Acknowledgement",style='font-weight: bold'),
            br(),
            p('We thank', a(href="https://www.aushsi.org.au/about-us/team/sameera-senanayake/","Sameera Senanayake"),'for their feedback that improved early versions of the application')
            
            
            ),
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
              box(title="Figure",width=12,column(12,align="center",uiOutput("plot_brush_click")),collapsible = T,collapsed =F),
              box(title=textOutput("data_title"),width=12,
                  column(12,textOutput("summary_text"),style='font-size:12pt;font-weight:bold;margin-bottom:20px'),
                  column(12,tableOutput("summary_table")),
                  column(6,downloadButton("downloadTable", "Download table",style='background-color:	#f9f9f9;font-family: Arial;font-weight: bold')),
                  collapsible = T,collapsed =T)
              
            )
    ),
    
    tabItem(tabName='code',
            box(width=12,title="Full dataset",column(width=12,verbatimTextOutput('ggplotFull')),expanded=T,collapsible = T),
            box(width=12,title="Summary by study",column(width=12,verbatimTextOutput('ggplotByStudy')),expanded=T,collapsible = T),
            box(width=12,title="Summary by checklist item",column(width=12,verbatimTextOutput('ggplotByItem')),expanded=T,collapsible = T)
    )
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
                                .shiny-output-error-validation {color: #003D58;font-weight: bold;font-family: "Arial";font-size: 16px;}
                                ')))
  
  
)



ui <- dashboardPage(header, sidebar, body)
