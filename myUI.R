###############################################################################
########## Define UI for application that construct the dashboard #############
###############################################################################

ui <- dashboardPage(
  dashboardHeader(title = span(" NIAID Trainees Portfolio", style = "font-size: 35px"), titleWidth = 300),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(span("General Overview", style = "font-size: 20px"), tabName = "General_Info", icon = icon("th")),
      menuItem(span("Application & Award", style = "font-size: 20px"), tabName = "Applications_Awards", icon = icon("th")),
      menuItem(span("Project Overview", style = "font-size: 20px"), tabName = "All_Projects", icon = icon("th")),
      menuItem(span("Dashboard Info", style = "font-size: 20px"), tabName = "Dash_Info", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    
    ## This is needed to write an equation or mat formula inside of HTML
    withMathJax(),
    
    # need this in the ui to use shinyjs functions including reset() for inputs
    useShinyjs(),
    
    tabItems(
      
      
      #########################
      ################## General Overview (First tab) ------
      ########################
      
      
      
      tabItem(tabName = "General_Info",
  
              fluidRow(
                column(width = 3, shinydashboard::box(background = "blue", width = NULL,
                                      h2(strong("Data Summary of Trainees Portfolio"), align="center"))),
                column(width = 3, valueBoxOutput("current_apps", width = NULL)),
                column(width = 3, valueBoxOutput("current_awd", width = NULL)),
                column(width = 3, valueBoxOutput("current_fund", width = NULL))
              ),

              
              fluidRow(column(width = 4, style='padding-left:20px;', h2("Project Filters:"))),
              
              fluidRow(
                column(width = 8, style='padding-left:40px;',
                       # pickerInput(inputId = "genFY",
                       #             label = "Fiscal Year:",
                       #             choices = sort(unique(tr_data.niaid$FY), decreasing = TRUE),
                       #             multiple = TRUE,
                       #             selected = sort(unique(tr_data.niaid$FY), decreasing = TRUE),
                       #             options = list(title = "e.g. 2021, 2020")
                       sliderTextInput(inputId = "genFY",
                                   label = "Fiscal Year:",
                                   #from_min = min(tr_data.niaid$FY, na.rm = TRUE),
                                   #to_max = max(tr_data.niaid$FY, na.rm = TRUE),
                                   #value = c(min(tr_data.niaid$FY, na.rm = TRUE),max(tr_data.niaid$FY, na.rm = TRUE)),
                                   width = '100%',
                                   grid = TRUE,
                                   choices = unique(tr_data.niaid$FY),
                                   selected = c(min(tr_data.niaid$FY, na.rm = TRUE),max(tr_data.niaid$FY, na.rm = TRUE))
                                   )
                ),
                column(width = 2, 
                       pickerInput(inputId = "genDiv",
                                   label = "Select Division:",
                                   choices = c("DMID", "DAIDS", "DAIT", "DEA"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. DAIDS, DEA"))
                ),
                column(width = 2, 
                       pickerInput(inputId = "majorActv",
                                   label = HTML("Select Major Activity:  ", 
                                                as.character(actionLink(inputId = 'typeLinkGen_MActv', label = "?"))),
                                   choices = c("F", "K", "T"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. F, K, T"))
                )
                
              ),
              
              fluidRow(
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "specialTopic",
                                   label = HTML("Select Special Topic:  ", 
                                                as.character(actionLink(inputId = 'typeLinkGen_SpecialT', label = "?"))),
                                   choices = c("Diversity", "Mosaic", "Physician Scientist", "Non-Human Primate"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. MOSAIC"))
                ),
                column(width = 2, 
                       pickerInput(inputId = "genType",
                                   label = "Application Type:",
                                   choices = c("All", "Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6", "Type 7", "Type 8", "Type 9"),
                                   selected = "All",
                                   options = list(title = "e.g. Type 1, 2", `live-search` = TRUE))
                ),
                column(width = 2, 
                       pickerInput(inputId = "genCompete",
                                   label = "Competing Status:",
                                   choices = unique(tr_data.niaid$Competing),
                                   multiple = TRUE,
                                   options = list(title = "e.g. Competing"))
                ),
                column(width = 2, 
                       pickerInput(inputId = "genAwardStatus",
                                   label = HTML("Award Status:  ", 
                                                as.character(actionLink(inputId = 'genStatusLink', label = "?"))),
                                   choices = c("Awarded", "To Be Paid", "Not Awarded"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. Awarded"))
                ),
                column(width = 2, 
                       pickerInput(inputId = "genActv",
                                   label = "Select Activity Code:",
                                   choices = sort(as.character(unique(tr_data.niaid$ACTIVITY_CODE))),
                                   multiple = TRUE,
                                   options = list(title = "e.g. K08, F32", `live-search` = TRUE))
                )
              ),
              
              fluidRow(
                column(width = 6, shinydashboard::box(title = tags$p(strong("Total Number of Application and Award"), style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = NULL, 
                                      height = NULL, 
                                      plotlyOutput("gen_mean.apps_awd", height = "450px")%>% withSpinner())
                ),
                column(width = 6, shinydashboard::box(title = tags$p(strong("Award and Success Rates"), style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = NULL, 
                                      height = NULL, 
                                      plotlyOutput("gen_rate.apps_awd", height = "450px")%>% withSpinner())
                )
              ),
              
              fluidRow(
                column(width = 8, box(title = tags$p("Rates Summary", style = "font-size: 100%;"),
                                      width = 12, DTOutput("gen_table.rate") %>% withSpinner())
                ),
                column(width = 4,
                       fluidRow(shinydashboard::box(background = NULL, solidHeader = TRUE, width = NULL,
                                    h2(strong("Average Summary (2005-2021)")))),
                       fluidRow(valueBoxOutput("mean_apps", width = NULL)),
                       fluidRow(valueBoxOutput("mean_awd", width = NULL)),
                       fluidRow(valueBoxOutput("mean_fund", width = NULL))
                       )

              )
              
                
              

              
      ),
      
      #####################
      ############# Applications and Awards Summary tab (second tab) ------
      ###############################
      tabItem(tabName = "Applications_Awards",
              fluidRow(column(width = 8, style='padding-left:20px;', h1("Trainees Applications and Awards"))),
              
              fluidRow(
                column(width = 8, style='padding-left:40px;',
                       sliderTextInput(inputId = "fy_apps_awd",
                                       label = "Fiscal Year:",
                                       width = '100%',
                                       grid = TRUE,
                                       choices = unique(tr_data.niaid$FY),
                                       selected = c(min(tr_data.niaid$FY, na.rm = TRUE),max(tr_data.niaid$FY, na.rm = TRUE))
                       )
                )
              ),
              fluidRow(
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "div_apps_awd",
                                   label = HTML("Select Division:  ", 
                                                as.character(actionLink(inputId = 'typeLinkAppsAwd_div', label = "?"))),
                                   choices = c("All", "DMID", "DAIDS", "DAIT", "DEA"),
                                   selected = "All",
                                   multiple = TRUE)
                ),
                column(width = 2, 
                       pickerInput(inputId = "type_apps_awd",
                                   label = "Application Type:",   
                                   choices = c("All","Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6", "Type 7", "Type 8", "Type 9"),
                                   selected = "All",
                                   multiple = TRUE)
                ),
                
                column(width = 2, 
                       pickerInput(inputId = "compete_apps_awd",
                                   label = "Competing Status:  ",
                                   choices = unique(tr_data.niaid$Competing),
                                   multiple = TRUE)
                ),
                
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "specialTopic_apps_awd",
                                   label = HTML("Select Special Topic:  ", 
                                                as.character(actionLink(inputId = 'typeLinkAppsAwd_SpecialT', label = "?"))),
                                   choices = c("Diversity", "Mosaic", "Physician Scientist", "Non-Human Primate"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. MOSAIC"))
                )
                
                
              ),
              
              fluidRow(column(width = 8, style='padding-left:20px;', h2("Applications By Activity Codes"))),
              
              fluidRow(
                column(width = 4, shinydashboard::box(title = tags$p("Fellowship", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allfellowship_apps", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Research Career", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allres.career_apps", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Training", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("alltraining_apps", height = "300px") %>% withSpinner())
                )
              ),
              
              fluidRow(column(width = 4, style='padding-left:20px;', h2("Awards By Activity Codes"))),
              
              fluidRow(
                column(width = 4, shinydashboard::box(title = tags$p("Fellowship", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("allfellowship_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Research Career", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("allres.career_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Training", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("alltraining_awd", height = "300px") %>% withSpinner())
                )
              ),
              
              fluidRow(column(width = 4, style='padding-left:20px;', h2("Applications By Division"))),
              
              fluidRow(
                column(width = 4, shinydashboard::box(title = tags$p("Fellowship", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allfellowship_div_apps", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Research Career", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allres.career_div_apps", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Training", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("alltraining_div_apps", height = "300px") %>% withSpinner())
                )
              ),
              
              fluidRow(column(width = 4, style='padding-left:20px;', h2("Awards By Division"))),
              
              fluidRow(
                column(width = 4, shinydashboard::box(title = tags$p("Fellowship", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allfellowship_div_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Research Career", style = "font-size: 120%;"), 
                                      status = NULL, 
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12, 
                                      height = 370, 
                                      plotlyOutput("allres.career_div_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 4, shinydashboard::box(title = tags$p("Training", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("alltraining_div_awd", height = "300px") %>% withSpinner())
                )
              ),
              
              fluidRow(column(width = 8, style='padding-left:20px;', h2("Award and Success Rates By Division"))),
              
              fluidRow(
                column(width = 6, shinydashboard::box(title = tags$p("DMID", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("rateDMID_div_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 6, shinydashboard::box(title = tags$p("DAIDS", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("rateDAIDS_div_awd", height = "300px") %>% withSpinner())
                )
              ),
              fluidRow(
                column(width = 6, shinydashboard::box(title = tags$p("DAIT", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("rateDAIT_div_awd", height = "300px") %>% withSpinner())
                ),
                column(width = 6, shinydashboard::box(title = tags$p("DEA", style = "font-size: 120%;"),
                                      status = NULL,
                                      solidHeader = FALSE,
                                      collapsible = TRUE,
                                      width = 12,
                                      height = 370,
                                      plotlyOutput("rateDEA_div_awd", height = "300px") %>% withSpinner())
                )
              )
              
      ),
      
      
      ###############################################
      ############ Project Overview tab (third tab) ------
      ##################################################
      
      tabItem(tabName = "All_Projects",
              fluidRow(column(width = 4, style='padding-left:20px;', h2("Project Filters:"))),
              
              fluidRow(
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "projFY",
                                   label = "Fiscal Year:",
                                   choices = unique(tr_data.niaid$FY),
                                   multiple = TRUE,
                                   selected = 2021)
                ),
                column(width = 2, 
                       pickerInput(inputId = "projActv",
                                   label = "Select Activity Code:",
                                   choices = sort(as.character(unique(tr_data.niaid$ACTIVITY_CODE))),
                                   multiple = TRUE,
                                   options = list(
                                     title = "e.g. K08, F32",
                                     `live-search` = TRUE))
                ),
                column(width = 2, 
                       pickerInput(inputId = "proj_rfa",
                                   label = "Select RFA/PA Number:",
                                   choices = sort(unique(tr_data.niaid$RFA_PA_NUMBER)),
                                   multiple = TRUE,
                                   options = list(
                                     title = "e.g. PA21-268",
                                     `live-search` = TRUE))
                ),
                column(width = 2, style='padding-right:40px;',
                       pickerInput(inputId = "projType",
                                   label = "Application Type:", 
                                   choices = c("All", "Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6", "Type 7", "Type 8", "Type 9"),
                                   selected = "All")
                       
                ),
                column(width = 2, 
                       pickerInput(inputId = "projCompete",
                                   label = "Competing Status:",
                                   choices = unique(tr_data.niaid$Competing),
                                   multiple = TRUE)
                ),
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "specialTopicProj",
                                   label = HTML("Select Special Topic:  ", 
                                                as.character(actionLink(inputId = 'typeLinkProj_SpecialT', label = "?"))),
                                   choices = c("Diversity", "Mosaic", "Physician Scientist", "Non-Human Primate"),
                                   multiple = TRUE,
                                   options = list(title = "e.g. MOSAIC"))
                )
                
              ),
              
              fluidRow(
                column(width = 2, style='padding-left:40px;',
                       pickerInput(inputId = "projDiv",
                                   label = "Select Division:",
                                   choices = c("DAIDS", "DAIT", "DMID", "DEA"),
                                   multiple = TRUE)
                ),
                column(width = 2, 
                       pickerInput(inputId = "select_funding",
                                   label = HTML("Award Status:  ", 
                                                as.character(actionLink(inputId = 'projStatusLink', label = "?"))),
                                   choices = c("Awarded", "To Be Paid", "Not Awarded"),
                                   multiple = TRUE)
                ),
                column(width = 2, style='padding-right:40px;',
                       pickerInput(inputId = "projNOSI",
                                   label = "Select NOSI:",
                                   choices = sort(unique(tr_data.niaid$AGENCY_ROUTING_NUM)),
                                   multiple = TRUE,
                                   options = list(
                                     title = "e.g. NOT-AI-20-059",
                                     `live-search` = TRUE))
                ),
                column(width = 3, 
                       sliderInput(
                         inputId = "percent_slide",
                         label = HTML("Percentile Filter:  ", 
                                      as.character(actionLink(inputId = 'percentSlideLink', label = "?"))),
                         min = 0,
                         max = 55,
                         value = 55,
                         ticks = FALSE)
                ),
                column(width = 3, 
                       sliderInput(
                         inputId = "priority_slide",
                         label = HTML("Priority Score Filter:  ", 
                                      as.character(actionLink(inputId = 'scoreSlideLink', label = "?"))),
                         min = 0,
                         max = 500,
                         value = 500,
                         ticks = FALSE)
                )
              ),
              
              
              fluidRow(
                column(width = 3, style='padding-left:40px;',
                       actionBttn(
                         inputId = "reset_filters",
                         label = "Reset Filters", 
                         style = "bordered",
                         color = "primary",
                         size = "sm",
                         block = FALSE,
                         icon = icon("undo")),
                       br(),
                       br()
                )
              ),
              fluidRow(
                box(width = 12, title = tags$p("Trainees Projects", style = "font-size: 170%;"),
                    DTOutput("project_table")%>% withSpinner())
              )
              
      ),
      
      #########################
      ################## Dashboard Information (5th tab) ------
      ########################
        
      tabItem(tabName = "Dash_Info", title = "General Information on the Dashboard",
              
              ### CSS Style for carousel
              ## https://stackoverflow.com/questions/69254476/remove-shaded-background-and-border-of-indicators-in-shinydashboardpluscarouse
              
              tags$head(
                tags$style(HTML("
      .carousel-caption {
        display: none !important;      
      }
      a.carousel-control:focus {
        outline: none;
        /*change background-image to none if want to remove black bars on right*/
        background-image: none;;
        box-shadow: none;
      } 
      a.carousel-control.left:focus {
        /*change background-image to none if want to remove black bars on left*/
          background-image: none;
      }
      .carousel-tablist-highlight.focus {
        outline: none;
        background-color: transparent;
      }
      "))),
              
  
              fluidRow(
                column(width = 1),
                column(width = 10,
              carousel(
                width = NULL,
                id = "mycarousel",
                indicators = TRUE,
                carouselItem(box(
                  title = tags$p("Fiscal Year", style = "font-weight: 550;font-size: 120%;"),
                  solidHeader = TRUE,
                  width = NULL,
                  status = "info",
                  htmlOutput("Info1")
                )),
                carouselItem(box(
                  title = tags$p("Activity Code", style = "font-weight: 550;font-size: 120%;"),
                  solidHeader = TRUE,
                  width = NULL,
                  status = "info",
                  htmlOutput("Info2")
                )),
                carouselItem(box(
                  title = tags$p("Award and Success Rate", style = "font-weight: 550;font-size: 120%;"),
                  solidHeader = TRUE,
                  width = NULL,
                  status = "info",
                  htmlOutput("Info3")
                )),
                carouselItem(box(
                  title = tags$p("Special Topics", style = "font-weight: 550;font-size: 120%;"),
                  solidHeader = TRUE,
                  width = NULL,
                  status = "info",
                  htmlOutput("Info4")
                )),
                carouselItem(box(
                  title = tags$p("Data Refreshed", style = "font-weight: 550;font-size: 120%;"),
                  solidHeader = TRUE,
                  width = NULL,
                  status = "info",
                  htmlOutput("Info5")
                ))
                
              )),
              column(width = 1)
              )
              
              
      )
      
    )  
    
  )
)
