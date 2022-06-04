

###############################################################################
############ Define server logic required to draw plots and tables ###########
###############################################################################


server <- function(input, output) {
  
  
  # reset inputs
  observeEvent(input$reset_filters, {
    reset("projFY")
    reset("projActv")
    reset("projType")
    reset("projDiv")
    reset("percent_slide")
    reset("select_funding")
    reset("priority_slide")
  })
  
  
  ###### Modal (popup info boxes) --------------
  
  ######## Tried using one observeevent for all of these countLinks but it would always trigger on app startup for no reason, separating them worked
  ###### ObserveEvent
  
  ## General tab
  observeEvent(input$typeLinkGen_MActv, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Note:</b> The choices K, F, T will select only the K, F, and T activities codes respectively.")
    ))
  })
  
  observeEvent(input$typeLinkGen_SpecialT, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Diversity:</b> Any project with yes as diversity flag or projects that have the word `diversity` in either the project or the RFA PA title.<br><br>
      <b>Mosaic:</b> Any project that have the word `mosaic` in either the project or the RFA PA title.<br><br>
      <b>Physician Scientist:</b> Any project that have the words `physician scientist or physician-scientist` in either the project or the RFA PA titles.<br><br>
      <b>Non-Human Primate:</b> Any project that have the words `Non-Human Primate or Nonhuman Primate` in either the project or the RFA PA titles.")
    ))
  })
  
  observeEvent(input$genStatusLink, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Awarded:</b> Any project with status 5, 6, or 99.<br><br>
      <b>To Be Paid:</b> Any project with status 19 or 35. <br><br>
      <b>Not Awarded:</b> All other statuses.")
    ))
  })
  
  observeEvent(input$current_appLink1, {
    showModal(modalDialog(
      title = "Type Info",
      # HTML("<b>Grants:</b> Count of distinct IC + Serial Number.")
      HTML("<b>Note:</b> Count of distinct Applications by APPL ID in FY 2021.")
    ))
  })
  
  observeEvent(input$current_awdLink1, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Note:</b> Count of distinct Awards by APPL ID in FY 2021.")
    ))
  })
  
  
  observeEvent(input$current_fundLink1, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Projects:</b> Sum of distinct awards (counted by APPL ID) in dollars amount in FY 2021.")
    ))
  })
  
  
  observeEvent(input$mean_appLink1, {
    showModal(modalDialog(
      title = "Project Count",
      # HTML("<b>Grants:</b> Count of distinct IC + Serial Number.")
      HTML("<b>Projects:</b> Count of distinct applications by APPL ID per FY.<br><br>
            <b>Average:</b> Mean applications received by NIAID from 2005 to 2021.")
    ))
  })
  
  
  observeEvent(input$mean_awdLink1, {
    showModal(modalDialog(
      title = "Project Count",
      # HTML("<b>Grants:</b> Count of distinct IC + Serial Number.")
      HTML("<b>Projects:</b> Count of distinct awards by APPL ID per FY.<br><br>
            <b>Average:</b> Mean number of awards received by NIAID from 2005 to 2021.")
    ))
  })
  
  observeEvent(input$mean_fundLink1, {
    showModal(modalDialog(
      title = "Type Info",
      # HTML("<b>Grants:</b> Count of distinct IC + Serial Number.")
      HTML("<b>Projects:</b> Count of distinct awards by APPL ID per FY.<br><br>
            <b>Average:</b> Mean awards received by NIAID from 2005 to 2021 in dollar amounts.")
    ))
  })
  
  
  ##### Application & Award tab
  
  observeEvent(input$typeLinkAppsAwd_div, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b> Note:</b> Several Non-Awarded APPL IDs do not have PCC Codes and thus are identified as No PCC).")
    ))
  }) 
  
  observeEvent(input$typeLinkAppsAwd_SpecialT, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Diversity:</b> Any project with yes as diversity flag or projects that have the word `diversity` in either the project or the RFA PA title.<br><br>
      <b>Mosaic:</b> Any project that have the word `mosaic` in either the project or the RFA PA title.<br><br>
      <b>Physician Scientist:</b> Any project that have the words `physician scientist or physician-scientist` in either the project or the RFA PA titles.<br><br>
      <b>Non-Human Primate:</b> Any project that have the words `Non-Human Primate or Nonhuman Primate` in either the project or the RFA PA titles.")
    ))
  })
  
  
  ##### Project Overview tab
  
  observeEvent(input$typeLinkProj_SpecialT, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Diversity:</b> Any project with yes as diversity flag or projects that have the word `diversity` in either the project or the RFA PA title.<br><br>
      <b>Mosaic:</b> Any project that have the word `mosaic` in either the project or the RFA PA title.<br><br>
      <b>Physician Scientist:</b> Any project that have the words `physician scientist or physician-scientist` in either the project or the RFA PA titles.<br><br>
      <b>Non-Human Primate:</b> Any project that have the words `Non-Human Primate or Nonhuman Primate` in either the project or the RFA PA titles.")
    ))
  })
  
  observeEvent(input$projStatusLink, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b>Awarded:</b> Any project with status 5, 6, or 99.<br><br>
      <b>To Be Paid:</b> Any project with status 19 or 35. <br><br>
      <b>Not Awarded:</b> All other statuses.")
    ))
  })
  
  
  observeEvent(input$percentSlideLink, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b> Note:</b> All data are reported unless the user makes a change in this slider.")
    ))
  }) 
  
  observeEvent(input$scoreSlideLink, {
    showModal(modalDialog(
      title = "Type Info",
      HTML("<b> Note:</b> All data are reported unless the user makes a change in this slider.")
    ))
  }) 
  
  
  
  ########### Valueboxes for General tab ----
  
  #### Current FY
  output$current_apps <- renderValueBox({
    data <- tr_data.niaid %>%
      dplyr::filter(FY=="2021") %>% 
      summarize(project = n_distinct(project_id)) 
    
    valueBox(subtitle = HTML('<FONT size="4pt">FY2021 Applications</FONT>', 
                             as.character(actionLink(inputId = 'current_appLink1', label = "?"))), 
             value = round(data$project), color = "teal",
             icon = icon("list"), width = NULL) 
  })
  
  
  output$current_awd <- renderValueBox({
    data <- tr_data.niaid %>%
      dplyr::filter(FY=="2021") %>%
      filter(APPL_STATUS_CODE %in% c(5,6)) %>%
      summarize(project = n_distinct(project_id))
    
    valueBox(subtitle = HTML('<FONT size="4pt">FY2021 Awards</FONT>', 
                             as.character(actionLink(inputId = 'current_awdLink1', label = "?"))), 
             value = round(data$project), color = "aqua",
             icon = icon("list"), width = NULL) 
  })
  
  output$current_fund <- renderValueBox({
    data <- tr_data.niaid %>%
      dplyr::filter(FY=="2021") %>%
      filter(APPL_STATUS_CODE %in% c(5,6)) %>%
      distinct(project_id, .keep_all = TRUE) %>%
      summarize(funding = sum(TOTAL_AWARDED_AMT, na.rm = TRUE))
    
    valueBox(subtitle = HTML('<FONT size="4pt">FY2021 Funding</FONT>', as.character(actionLink(inputId = 'current_fundLink1', label = "?"))),
             value = paste0("$ ", prettyNum(round(data$funding/1000000),big.mark=",",scientific=FALSE), "M"), color = "light-blue",
             icon = icon("dollar"), width = NULL)
  })
  
  
  #### Average over 2005-2022
  output$mean_apps <- renderValueBox({
    data <- tr_data.niaid %>%
      group_by(FY) %>% 
      summarize(project = n_distinct(project_id))
    
    valueBox(subtitle = HTML('<FONT size="4pt">Mean Applications</FONT>', 
                             as.character(actionLink(inputId = 'mean_appLink1', label = "?"))), 
             value = round(mean(data$project, na.rm = TRUE)), color = "teal",
             icon = icon("list"), width = NULL) 
  })
  
  
  output$mean_awd <- renderValueBox({
    data <- tr_data.niaid %>%
      group_by(FY) %>%
      filter(APPL_STATUS_CODE %in% c(5,6)) %>%
      summarize(project = n_distinct(project_id))
    
    valueBox(subtitle = HTML('<FONT size="4pt">Mean Awards</FONT>', 
                             as.character(actionLink(inputId = 'mean_awdLink1', label = "?"))), 
             value = round(mean(data$project, na.rm = TRUE)), color = "aqua",
             icon = icon("list"), width = NULL) 
  })
  
  output$mean_fund <- renderValueBox({
    data <- tr_data.niaid %>%
      filter(APPL_STATUS_CODE %in% c(5,6)) %>%
      group_by(FY) %>%
      distinct(project_id, .keep_all = TRUE) %>%
      summarize(funding = sum(TOTAL_AWARDED_AMT, na.rm = TRUE))
    
    valueBox(subtitle = HTML('<FONT size="4pt">Mean Funding</FONT>', as.character(actionLink(inputId = 'mean_fundLink1', label = "?"))),
             value = paste0("$ ", prettyNum(round(mean(data$funding, na.rm= TRUE)/1000000),big.mark=",",scientific=FALSE), "M"), color = "light-blue",
             icon = icon("dollar"), width = NULL)
  })
  
  
  
  ######################### Data Filters
  
  ############## General tab
  GenData <- reactive({
    df1 <- tr_data.niaid %>% dplyr::filter(FY %in% (min(input$genFY):max(input$genFY)))
    
    # if(length(input$fy_apps_awd)){
    #   df1 <- filter(df1, FY %in% input$fy_apps_awd)
    # }
    
    if(length(input$majorActv)){
      df1 <- dplyr::filter(df1, MAJOR_ACTIVITY_CODE %in% input$majorActv)
    }
    if(length(input$genActv)){
      df1 <- dplyr::filter(df1, ACTIVITY_CODE %in% input$genActv)
    }
    if(length(input$genDiv)){
      df1 <- dplyr::filter(df1, division %in% input$genDiv)
    }
    # if(length(input$specialTopic)) {
    # 
    # if((input$specialTopic == "Diversity")) {
    #     df1 <- dplyr::filter(df1, grepl('Diversity',paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '),
    #                                     ignore.case = TRUE))
    # }
    # if ((input$specialTopic == "Mosaic")){
    #     df1 <- dplyr::filter(df1, grepl('MOSAIC',paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '),
    #                                     ignore.case = TRUE))
    # }
    # if ((input$specialTopic == "Physician Scientist")){
    #     df1 <- dplyr::filter(df1, grepl(paste(c('physician-scientist','physician scientist'), collapse = "|"),
    #                                     paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
    # }
    # if ((input$specialTopic == "Non-Human Primate")){
    #     df1 <- dplyr::filter(df1, grepl(paste(c('Non-Human Primate','NonHuman Primate'), collapse = "|"),
    #                                     paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
    # }
    # }
    if(length(input$specialTopic)) {
      a1 <- if("Mosaic" %in% input$specialTopic) {c("Mosaic")} #else {NA}
      b1 <- if("Physician Scientist" %in% input$specialTopic) {c('physician-scientist','physician scientist')} #else {NA}
      c1 <- if("Non-Human Primate" %in% input$specialTopic) {c('Non-Human Primate','NonHuman Primate')} #else {NA}
      if("Diversity" %in% input$specialTopic){
        all_specialTopic1 <- c(a1, b1, c1, "Diversity")
        df1 <- df1 %>% dplyr::filter((DIVERSITY_FLAG== "Y") | grepl(paste0(all_specialTopic1, collapse = "|"), 
                                        paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE)) 
        } else {
      all_specialTopic2 <- c(a1, b1, c1)
      df1 <- dplyr::filter(df1, grepl(paste0(all_specialTopic2, collapse = "|"), 
                                      paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
        }
    }
    if(length(input$genCompete)) {
      df1 <- filter(df1, Competing %in% input$genCompete)
    } 
    if(length(input$genAwardStatus)) {
      a <- if("Awarded" %in% input$genAwardStatus){ c(5,6,99)} else {NA}
      b <- if("To Be Paid" %in% input$genAwardStatus) {c(19,35)} else {NA}
      c <- if("Not Awarded" %in% input$genAwardStatus) {c(1:4,7:18,20:34,36:98)} else {NA}
      all_stats <- c(a, b, c)
      df1 <- dplyr::filter(df1, APPL_STATUS_CODE %in% all_stats)
    }
    #if(length(input$genType)) {
    if(input$genType == "Type 1") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 1)
    }
    if(input$genType == "Type 2") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 2)
    }
    if(input$genType == "Type 3") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 3)
    }
    if(input$genType == "Type 4") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 4)
    }
    if(input$genType == "Type 5") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 5)
    }
    if(input$genType == "Type 6") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 6)
    }
    if(input$genType == "Type 7") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 7)
    }
    if(input$genType == "Type 8") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 8)
    }
    if(input$genType == "Type 9") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 9)
    }
    else{df1}
    #}
  })
  
  
  
  ######### Application & Award Filter tab
   ## with division as filter
  AppAwdData <- reactive({
    
    
    data1 <- tr_data.niaid %>%  dplyr::filter(FY %in% (min(input$fy_apps_awd):max(input$fy_apps_awd)))
    
    # if(length(input$fy_apps_awd)){
    #   data1 <- filter(data1, FY %in% input$fy_apps_awd)
    # }
      
    if(length(input$specialTopic_apps_awd)) {
      a1 <- if("Mosaic" %in% input$specialTopic_apps_awd) {c("Mosaic")} #else {NA}
      b1 <- if("Physician Scientist" %in% input$specialTopic_apps_awd) {c('physician-scientist','physician scientist')} #else {NA}
      c1 <- if("Non-Human Primate" %in% input$specialTopic_apps_awd) {c('Non-Human Primate','NonHuman Primate')} #else {NA}
      if("Diversity" %in% input$specialTopic_apps_awd){
        all_specialTopic_apps_awd1 <- c(a1, b1, c1, "Diversity")
        data1 <- data1 %>% dplyr::filter((DIVERSITY_FLAG == "Y") | grepl(paste0(all_specialTopic_apps_awd1, collapse = "|"), 
                                                                    paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE)) 
      } else {
        all_specialTopic_apps_awd2 <- c(a1, b1, c1)
        data1 <- dplyr::filter(data1, grepl(paste0(all_specialTopic_apps_awd2, collapse = "|"), 
                                        paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
      }
    }
    
    if(length(input$div_apps_awd)){
      if (input$div_apps_awd == "DMID") {
      data1 <- dplyr::filter(data1, division == "DMID")
    } 
      if(input$div_apps_awd == "DAIT") {
      data1 <- dplyr::filter(data1, division == "DAIT")
    } 
      if(input$div_apps_awd == "DAIDS") {
      data1 <- dplyr::filter(data1, division == "DAIDS")
    } 
      if(input$div_apps_awd == "DEA") {
      data1 <- dplyr::filter(data1, division == "DEA")
    } 
    }
    
    
    if(input$type_apps_awd == "Type 1") {
      data1 <- filter(data1, APPL_TYPE_CODE == 1)
    } 
    if(input$type_apps_awd == "Type 2") {
      data1 <- filter(data1, APPL_TYPE_CODE == 2)
    } 
    if(input$type_apps_awd == "Type 3") {
      data1 <- filter(data1, APPL_TYPE_CODE == 3)
    } 
    if(input$type_apps_awd == "Type 4") {
      data1 <- filter(data1, APPL_TYPE_CODE == 4)
    } 
    if(input$type_apps_awd == "Type 5") {
      data1 <- filter(data1, APPL_TYPE_CODE == 5)
    } 
    if(input$type_apps_awd == "Type 6") {
      data1 <- filter(data1, APPL_TYPE_CODE == 6)
    } 
    if(input$type_apps_awd == "Type 7") {
      data1 <- filter(data1, APPL_TYPE_CODE == 7)
    } 
    if(input$type_apps_awd == "Type 8") {
      data1 <- filter(data1, APPL_TYPE_CODE == 8)
    } 
    if(input$type_apps_awd == "Type 9") {
      data1 <- filter(data1, APPL_TYPE_CODE == 9)
    }
    if(length(input$compete_apps_awd)) {
      data1 <- filter(data1, Competing %in% input$compete_apps_awd)
    } 
    else{data1}
    
  })
  
  
  #### With division static and not a filter
  
  AppAwdData_noDiv <- reactive({
    
    
    data1 <- tr_data.niaid %>% 
      dplyr::filter(FY %in% (min(input$fy_apps_awd):max(input$fy_apps_awd)))
    if(length(input$specialTopic_apps_awd)) {
      a1 <- if("Mosaic" %in% input$specialTopic_apps_awd) {c("Mosaic")} #else {NA}
      b1 <- if("Physician Scientist" %in% input$specialTopic_apps_awd) {c('physician-scientist','physician scientist')} #else {NA}
      c1 <- if("Non-Human Primate" %in% input$specialTopic_apps_awd) {c('Non-Human Primate','NonHuman Primate')} #else {NA}
      if("Diversity" %in% input$specialTopic_apps_awd){
        all_specialTopic_apps_awd1 <- c(a1, b1, c1, "Diversity")
        data1 <- data1 %>% dplyr::filter((DIVERSITY_FLAG == "Y") | grepl(paste0(all_specialTopic_apps_awd1, collapse = "|"), 
                                                                     paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE)) 
      } else {
        all_specialTopic_apps_awd2 <- c(a1, b1, c1)
        data1 <- dplyr::filter(data1, grepl(paste0(all_specialTopic_apps_awd2, collapse = "|"), 
                                        paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
      }
      
    }
    
    if(input$type_apps_awd == "Type 1") {
      data1 <- filter(data1, APPL_TYPE_CODE == 1)
    } 
    if(input$type_apps_awd == "Type 2") {
      data1 <- filter(data1, APPL_TYPE_CODE == 2)
    } 
    if(input$type_apps_awd == "Type 3") {
      data1 <- filter(data1, APPL_TYPE_CODE == 3)
    } 
    if(input$type_apps_awd == "Type 4") {
      data1 <- filter(data1, APPL_TYPE_CODE == 4)
    } 
    if(input$type_apps_awd == "Type 5") {
      data1 <- filter(data1, APPL_TYPE_CODE == 5)
    } 
    if(input$type_apps_awd == "Type 6") {
      data1 <- filter(data1, APPL_TYPE_CODE == 6)
    } 
    if(input$type_apps_awd == "Type 7") {
      data1 <- filter(data1, APPL_TYPE_CODE == 7)
    } 
    if(input$type_apps_awd == "Type 8") {
      data1 <- filter(data1, APPL_TYPE_CODE == 8)
    } 
    if(input$type_apps_awd == "Type 9") {
      data1 <- filter(data1, APPL_TYPE_CODE == 9)
    }
    if(length(input$compete_apps_awd)) {
      data1 <- filter(data1, Competing %in% input$compete_apps_awd)
    } 
    else{data1}
    
  })
  
  
  
  ###### Projects view tab-----
  projData <- reactive({
    df1 <- tr_data.niaid %>%
      dplyr::filter(FY %in% (min(input$projFY):max(input$projFY)))
    
    if(length(input$projActv)){
      df1 <- dplyr::filter(df1, ACTIVITY_CODE %in% input$projActv)
    }
    if(length(input$proj_rfa)){
      df1 <- filter(df1, RFA_PA_NUMBER %in% input$proj_rfa)
    }
    if(length(input$specialTopicProj)) {
      a1 <- if("Mosaic" %in% input$specialTopicProj) {c("Mosaic")} #else {NA}
      b1 <- if("Physician Scientist" %in% input$specialTopicProj) {c('physician-scientist','physician scientist')} #else {NA}
      c1 <- if("Non-Human Primate" %in% input$specialTopicProj) {c('Non-Human Primate','NonHuman Primate')} #else {NA}
      if("Diversity" %in% input$specialTopicProj){
        all_specialTopicProj1 <- c(a1, b1, c1, "Diversity")
        df1 <- df1 %>% dplyr::filter((DIVERSITY_FLAG == "Y") | grepl(paste0(all_specialTopicProj1, collapse = "|"), 
                                                                     paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE)) 
      } else {
        all_specialTopicProj2 <- c(a1, b1, c1)
        df1 <- dplyr::filter(df1, grepl(paste0(all_specialTopicProj2, collapse = "|"), 
                                        paste(NOTICE_TITLE, PROJECT_TITLE, sep = ' '), ignore.case = TRUE))
      }
    }
    if(length(input$projCompete)) {
      df1 <- filter(df1, Competing %in% input$projCompete)
    } 
    if(length(input$projNOSI)){
      df1 <- filter(df1, AGENCY_ROUTING_NUM %in% input$projNOSI) ## for NOSI
    }
    if(length(input$select_funding)) {
      a <- if("Awarded" %in% input$select_funding){ c(5,6,99)} else {NA}
      b <- if("To Be Paid" %in% input$select_funding) {c(19,35)} else {NA}
      c <- if("Not Awarded" %in% input$select_funding) {c(1:4,7:18,20:34,36:98)} else {NA}
      all_stats <- c(a, b, c)
      df1 <- dplyr::filter(df1, APPL_STATUS_CODE %in% all_stats)
    }
    if(length(input$projDiv)){
      df1 <- dplyr::filter(df1, division %in% input$projDiv)
    }
    if(input$priority_slide != 500){ ## don't do anything unless user selection different than 100
      df1 <- dplyr::filter(df1, PRIORITY_SCORE_NUM <= input$priority_slide)
    }
    if(input$percent_slide != 55){ ## don't do anything unless user selection different than 55
      df1 <- dplyr::filter(df1, IRG_PERCENTILE_NUM <= input$percent_slide)
    }
    if(input$projType == "Type 1") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 1)
    }
    if(input$projType == "Type 2") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 2)
    }
    if(input$projType == "Type 3") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 3)
    }
    if(input$projType == "Type 4") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 4)
    }
    if(input$projType == "Type 5") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 5)
    }
    if(input$projType == "Type 6") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 6)
    }
    if(input$projType == "Type 7") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 7)
    }
    if(input$projType == "Type 8") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 8)
    }
    if(input$projType == "Type 9") {
      df1 <- dplyr::filter(df1, APPL_TYPE_CODE == 9)
    }
    else{df1}
  })
  
  ######----------------
  
  #### Plot average applications and awards from 2005-2020 in General overview
  output$gen_mean.apps_awd <- renderPlotly({
    
    data = GenData()
    
    validate(
      need(nrow(data) > 0, 'No data exists, please make a different selection')
    )
    
    ###### All projects
    tr_fy.app <- data %>%  
      dplyr::group_by(FY) %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::summarise(count = n()) %>% 
      mutate(Type = "Applications",
        texts = map(paste('<b>Type:</b>', Type, '<br>','<b>Fiscal Year:</b>', FY, '<br>', '<b>Count:</b>', count), HTML))
    
    ##### Awarded projects
    tr_fy.awd <- data %>%  
      dplyr::filter(APPL_STATUS_CODE %in% c(5,6,99)) %>% 
      dplyr::group_by(FY) %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::summarise(count = n()) %>% arrange(desc(count)) %>% 
      mutate(Type = "Awards",
        texts = map(paste('<b>Type:</b>', Type, '<br>','<b>Fiscal Year:</b>', FY, '<br>', '<b>Count:</b>', count), HTML)) 
    
    
    
    ##### Awarded projects + Application
    tr_fy.awd_app <- rbind(tr_fy.app, tr_fy.awd)
    
    validate(
      need(nrow(tr_fy.awd_app) > 0, 'No data exists, please make a different selection')
    )
    
    ##### Plotting (Total project + Awarded Applications)
    p_mean.apps_awd <- ggplot(data = tr_fy.awd_app) +
      geom_point(aes(x=FY, y=count, colour=Type, text=texts), size = 3)+
      geom_line(aes(x=FY, y=count, colour=Type), size = 0.8)+
      scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
      scale_color_manual(name = "", values = c("Applications" = "#56B4E9", "Awards" = "#E69F00")) +
      labs(x = "",y = "")  +
      theme_bw()  + Nice.Label + theme(legend.position = "none") 
    
    #p_mean.apps_awd
    if(nrow(tr_fy.awd) == 0){
      yval <- ifelse(length(subset(tr_fy.app, FY == (max(tr_fy.app$FY, na.rm = T)-1))$count)==0, NA, 
                     subset(tr_fy.app, FY == (max(tr_fy.app$FY, na.rm = T)-1))$count)
      
      ggplotly(p_mean.apps_awd, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = -0.5, y =-1),
               xaxis = list(tickangle = 90), yaxis = list(title = "")) %>% 
        add_annotations(x = max(tr_fy.app$FY, na.rm = T)-1, y = yval , text = "Applications", 
                        showarrow = TRUE, font = list(color = "#56B4E9", size = 18), arrowcolor="#00000000") 
    } else if(nrow(tr_fy.app) == 0){
      yval <- ifelse(length(subset(tr_fy.awd, FY == (max(tr_fy.awd$FY, na.rm = T)-1))$count)==0, NA, 
                     subset(tr_fy.app, FY == (max(tr_fy.awd$FY, na.rm = T)-1))$count)
      
      ggplotly(p_mean.apps_awd, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = -0.5, y =-1),
               xaxis = list(tickangle = 90), yaxis = list(title = "")) %>% 
        add_annotations(x = max(tr_fy.awd$FY, na.rm = T)-1, y = yval , text = "Awards", 
                        showarrow = TRUE, font = list(color = "#E69F00", size = 18), arrowcolor="#00000000") 
    } else {
      yvar1 <- ifelse(length(subset(tr_fy.awd_app, FY == (max(tr_fy.app$FY, na.rm = T)-1) & Type == "Applications")$count) ==0,
                      NA, subset(tr_fy.awd_app, FY == (max(tr_fy.app$FY, na.rm = T)-1) & Type == "Applications")$count)
      yvar2 <- ifelse(length(subset(tr_fy.awd_app, FY == (max(tr_fy.awd$FY, na.rm = T)-1) & Type == "Awards")$count)==0,
                      NA, subset(tr_fy.awd_app, FY == (max(tr_fy.awd$FY, na.rm = T)-1) & Type == "Awards")$count)
      
    ggplotly(p_mean.apps_awd, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "")) %>% 
      add_annotations(x = max(tr_fy.app$FY, na.rm = T)-1, y = yvar1 , text = "Applications", 
                      showarrow = TRUE, font = list(color = "#56B4E9", size = 18), arrowcolor="#00000000") %>% 
      add_annotations(x = max(tr_fy.awd$FY, na.rm = T)-1, y = yvar2 , text = "Awards", 
                      showarrow = TRUE, font = list(color = "#E69F00", size = 18), arrowcolor="#00000000") 
    }
  })
  
  
  #### Plot award and success rates in General overview
  output$gen_rate.apps_awd <- renderPlotly({
    
    data_all <- GenData() %>% dplyr::filter(!is.na(IRG_RECOM_CODE))
    
    validate(
      need(nrow(data_all) > 0, 'No data exists, please make a different selection')
    )
    
    ###### All projects
    data_rate <- rate_func(data_all)
    
    
    validate(
      need(nrow(data_rate) > 0, 'No data exists, please make a different selection')
    )
    
    ## get min and max value
    min.val <- min(data_rate$Value, na.rm = TRUE)
    max.val <- max(data_rate$Value, na.rm = TRUE)

      p_data_rate <- ggplot(data = data_rate,  aes(x = FY, y = Value)) +
        geom_line(aes(x = FY, y = Value, colour = Rates), size = 1.0) +
        geom_point(aes(x = FY, y = Value, colour = Rates, text=texts), size = 2.2) +
        scale_color_manual(values = c("Success" = "#E69F00","Award" = "navy")) +
        scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
        scale_y_continuous(limits = c(min.val,max.val), 
                           breaks = seq(0,100, 10),#c(10, 20,30,40,50),
                           labels = paste0(seq(0,100, 10), "%")) +
        labs(x = "",y = "")  +
        theme_bw()  + Nice.Label + theme(legend.position = "none")
      
      

    #p_niaid.rate_all.tr
    ggplotly(p_data_rate, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "")) %>% 
      add_annotations(x = max(data_rate$FY), y = subset(data_rate, FY == max(data_rate$FY) & Rates == "Success")$Value, 
                      text = "Success", showarrow = TRUE, font = list(color = "#E69F00", size = 18),
                      arrowcolor="#00000000") %>% 
      add_annotations(x = max(data_rate$FY), y = subset(data_rate, FY == max(data_rate$FY) & Rates == "Award")$Value , 
                      text = "Award", showarrow = TRUE, font = list(color = "navy", size = 18),
                      arrowcolor="#00000000") 
    
  })
  
  ###########
  ######################### Applications and Awards----------
  ##################
  
  
  ######### Bar Chart of Fellowship, Research career, and Training
  output$allfellowship_apps <- renderPlotly({
    
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Fellowship") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) 
    
    
    validate(
      need(nrow(graph) > 0, 'No Fellowship data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% arrange(desc(n))
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    p_allfellowship <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -n), y=n)) +
      geom_bar(stat="identity", position=position_dodge(1),show.legend = FALSE, fill = 'mediumturquoise',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Count:</b>', n), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label 
    
    ggplotly(p_allfellowship, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
    
  })
  
  
  output$allres.career_apps <- renderPlotly({
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Research Career") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) 
    
    validate(
      need(nrow(graph) > 0, 'No Research Career data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% arrange(desc(n))
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    p_allres.career <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -n), y=n)) +
      geom_bar(stat="identity", position=position_dodge(1), show.legend = FALSE, fill = 'mediumturquoise',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Count:</b>', n), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label
    
    ggplotly(p_allres.career, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
  })
  
  output$alltraining_apps <- renderPlotly({
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Training") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Training data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% arrange(desc(n))
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    p_alltraining <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -n), y=n)) +
      geom_bar(stat="identity", position=position_dodge(1), show.legend = FALSE, fill = 'mediumturquoise',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Count:</b>', n), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label
    
    ggplotly(p_alltraining, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
  })
  
  
  
  
  ######### Donut Chart by Division of Fellowship, Research career, and Training
  output$allfellowship_div_apps <- renderPlotly({
    
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      #dplyr::filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::filter(ac_category == "Fellowship") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) 
    
    
    validate(
      need(nrow(graph) > 0, 'No Fellowship data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(n / sum(n), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~n, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>% 
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
    
  })
  
  
  output$allres.career_div_apps <- renderPlotly({
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      #filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::filter(ac_category == "Research Career")
    
    validate(
      need(nrow(graph) > 0, 'No Research Career data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(n / sum(n), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~n, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>% 
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
    
  })
  
  output$alltraining_div_apps <- renderPlotly({
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      #filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::filter(ac_category == "Training") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Training data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(n = n_distinct(SERIAL_NUM)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(n / sum(n), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~n, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>% 
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
  })
  
  
  
  
  ######################### Awards ----------
  
  ######### Bar Chart of Fellowship, Research career, and Training
  output$allfellowship_awd <- renderPlotly({
    
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Fellowship") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) 
    
    
    validate(
      need(nrow(graph) > 0, 'No Fellowship data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% arrange(desc(total))
    
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    p_allfellowship <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -total), y=total/1000000)) +
      geom_bar(stat="identity", position=position_dodge(1),show.legend = FALSE, fill = 'deepskyblue',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Amount:</b>', 
                                  paste("$",prettyNum(total,big.mark=",",scientific=FALSE))), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label 
    
    ggplotly(p_allfellowship, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
    
  })
  
  
  output$allres.career_awd <- renderPlotly({
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Research Career") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) 
    
    
    validate(
      need(nrow(graph) > 0, 'No Research Career data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% arrange(desc(total))
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    
    p_allres.career <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -total), y=total/1000000)) +
      geom_bar(stat="identity", position=position_dodge(1), show.legend = FALSE, fill = 'deepskyblue',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Amount:</b>', 
                                  paste("$",prettyNum(total,big.mark=",",scientific=FALSE))), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label
    
    ggplotly(p_allres.career, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
  })
  
  output$alltraining_awd <- renderPlotly({
    
    graph <- AppAwdData()
    
    graph <- graph %>%
      dplyr::filter(ac_category == "Training") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Training data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(ACTIVITY_CODE) %>% 
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% arrange(desc(total))
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    p_alltraining <- ggplot(data=graph, aes(x=reorder(ACTIVITY_CODE, -total), y=total/1000000)) +
      geom_bar(stat="identity", position=position_dodge(1), show.legend = FALSE, fill = 'deepskyblue',
               aes(text=map(paste('<b>Activity Code:</b>', ACTIVITY_CODE, '<br>', '<b>Amount:</b>', 
                                  paste("$",prettyNum(total,big.mark=",",scientific=FALSE))), HTML)))+
      labs(x = "",y = "")  +
      theme_bw() +  theme(legend.position = "none") + Nice.Label
    
    ggplotly(p_alltraining, tooltip = "text") %>%
      layout(yaxis = list(title = "Project Count"))
  })
  
  
  
  
  ######### Donut Chart by Division of Fellowship, Research career, and Training
  output$allfellowship_div_awd <- renderPlotly({
    
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::filter(ac_category == "Fellowship") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Fellowship data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(total / sum(total, na.rm = TRUE), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~total, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>% 
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
    
  })
  
  
  output$allres.career_div_awd <- renderPlotly({
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      dplyr::filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::filter(ac_category == "Research Career") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Research Career data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(total / sum(total, na.rm = TRUE), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~total, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>% 
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
    
  })
  
  output$alltraining_div_awd <- renderPlotly({
    
    graph <- AppAwdData_noDiv()
    
    graph <- graph %>%
      filter(APPL_ID %!in% c("7223873", "9161361", "9992722")) %>% 
      dplyr::filter(ac_category == "Training") %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE)
    
    
    validate(
      need(nrow(graph) > 0, 'No Training data exists, please make a different selection')
    )
    
    graph <- graph %>% 
      dplyr::group_by(division, ac_category) %>%  
      dplyr::summarise(total = sum(TOTAL_AWARDED_AMT, na.rm = TRUE)) %>% ungroup() %>%
      dplyr::group_by(ac_category) %>% 
      dplyr::arrange(desc(division))%>% 
      dplyr::mutate(percentage = round(total / sum(total, na.rm = TRUE), 4)*100, 
                    lab_pos = cumsum(percentage)-0.5*percentage) 
    
    validate(
      need(nrow(graph) > 0, 'No data exists, please make a different selection')
    )
    
    ## add color by division
    graph <- graph %>% 
      group_by(division) %>% 
      mutate(divcolor = cur_group_id()) %>% 
      mutate(divcolor = replace(divcolor , divcolor == 1, "#A6CEE3"),
             divcolor = replace(divcolor , divcolor == 2, "#1F78B4"),
             divcolor = replace(divcolor , divcolor == 3, "#B2DF8A"), 
             divcolor = replace(divcolor , divcolor == 4, "#33A02C"), 
             divcolor = replace(divcolor , divcolor == "No PCC", "#E31A1C"))
    
    colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    m <- list(l = 40, r = 40, b = 40, t = 15, pad = 10)
    
    graph %>% plot_ly(labels = ~division, values = ~total, textinfo = 'label+percent',
                      marker = list(colors = ~divcolor), height = 300, textposition = 'inside') %>%  
      add_pie(hole = 0.6) %>% 
      layout(showlegend = FALSE, margin = m)
  })
  
  
  
  ####################----------------------------------------
  ############ Rates (success, award, and funding) splitted by Division
  
  #### Plot award, success, and funding rates for DMID
  output$rateDMID_div_awd <- renderPlotly({
    
    ###### All projects
    niaid.rate_DMID.tr <- AppAwdData_noDiv() %>% 
      dplyr::filter(!is.na(IRG_RECOM_CODE)) %>%  
      dplyr::filter(division == "DMID") %>% 
      rate_func(.)
    
    
    ##### Plotting Rates (Award, Success, Funding)
    p_niaid.rate_DMID.tr <- ggplot(data = niaid.rate_DMID.tr,  aes(x = FY, y = Value)) +
      geom_line(aes(x = FY, y = Value, colour = Rates), size = 1.0) +
      geom_point(aes(x = FY, y = Value, colour = Rates, text=texts), size = 2.2) +
      scale_color_manual(values = c("Success" = "#56B4E9","Award" = "navy", "Funding" = "#E69F00")) +
      scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
      scale_y_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80),
                         labels = paste0(c(0,10,20,30,40,50,60,70,80), "%")) +
      labs(x = "",y = "")  +
      theme_bw()  + Nice.Label + theme(legend.position = "none")
    
    
    #p_niaid.rate_all.tr
    ggplotly(p_niaid.rate_DMID.tr, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "Rates"))
    
  })
  
  
  #### Plot award, success, and funding rates for DAIDS
  output$rateDAIDS_div_awd <- renderPlotly({
    
    ###### All projects
    niaid.rate_DAIDS.tr <- AppAwdData_noDiv() %>% 
      dplyr::filter(!is.na(IRG_RECOM_CODE)) %>% 
      dplyr::filter(division == "DAIDS") %>% 
      rate_func(.)
    
    
    ##### Plotting Rates (Award, Success, Funding)
    p_niaid.rate_DAIDS.tr <- ggplot(data =  niaid.rate_DAIDS.tr,  aes(x = FY, y = Value)) +
      geom_line(aes(x = FY, y = Value, colour = Rates), size = 1.0) +
      geom_point(aes(x = FY, y = Value, colour = Rates, text=texts), size = 2.2) +
      scale_color_manual(values = c("Success" = "#56B4E9","Award" = "navy", "Funding" = "#E69F00")) +
      scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
      scale_y_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80),
                         labels = paste0(c(0,10,20,30,40,50,60,70,80), "%")) +
      labs(x = "",y = "")  +
      theme_bw()  + Nice.Label + theme(legend.position = "none")
    
    
    #p_niaid.rate_all.tr
    ggplotly(p_niaid.rate_DAIDS.tr, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "Rates"))
    
  })
  
  
  #### Plot award, success, and funding rates for DAIT
  output$rateDAIT_div_awd <- renderPlotly({
    
    ###### All projects
    niaid.rate_DAIT.tr <- AppAwdData_noDiv() %>% 
      dplyr::filter(!is.na(IRG_RECOM_CODE)) %>% 
      dplyr::filter(division == "DAIT") %>% 
      rate_func(.)
    
    
    ##### Plotting Rates (Award, Success, Funding)
    p_niaid.rate_DAIT.tr <- ggplot(data = niaid.rate_DAIT.tr,  aes(x = FY, y = Value)) +
      geom_line(aes(x = FY, y = Value, colour = Rates), size = 1.0) +
      geom_point(aes(x = FY, y = Value, colour = Rates, text=texts), size = 2.2) +
      scale_color_manual(values = c("Success" = "#56B4E9","Award" = "navy", "Funding" = "#E69F00")) +
      scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
      scale_y_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80),
                         labels = paste0(c(0,10,20,30,40,50,60,70,80), "%")) +
      labs(x = "",y = "")  +
      theme_bw()  + Nice.Label + theme(legend.position = "none")
    
    
    #p_niaid.rate_all.tr
    ggplotly(p_niaid.rate_DAIT.tr, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "Rates"))
    
  })
  
  
  #### Plot award, success, and funding rates for DEA
  output$rateDEA_div_awd <- renderPlotly({
    
    ###### All projects
    niaid.rate_DEA.tr <- AppAwdData_noDiv() %>% 
      dplyr::filter(!is.na(IRG_RECOM_CODE)) %>% 
      dplyr::filter(division == "DEA") %>% 
      rate_func(.)
    
    
    ##### Plotting Rates (Award, Success, Funding)
    p_niaid.rate_DEA.tr <- ggplot(data = niaid.rate_DEA.tr,  aes(x = FY, y = Value)) +
      geom_line(aes(x = FY, y = Value, colour = Rates), size = 1.0) +
      geom_point(aes(x = FY, y = Value, colour = Rates, text=texts), size = 2.2) +
      scale_color_manual(values = c("Success" = "#56B4E9","Award" = "navy", "Funding" = "#E69F00")) +
      scale_x_continuous(breaks = seq(2005,2021,1), guide = guide_axis(angle = 30)) +
      scale_y_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80),
                         labels = paste0(c(0,10,20,30,40,50,60,70,80), "%")) +
      labs(x = "",y = "")  +
      theme_bw()  + Nice.Label + theme(legend.position = "none")
    
    
    #p_niaid.rate_all.tr
    ggplotly(p_niaid.rate_DEA.tr, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = -0.5, y =-1),
             xaxis = list(tickangle = 90), yaxis = list(title = "Rates"))
    
  })
  
  ############################## Tables
  

  ########################## Rates Table
  
  output$gen_table.rate <- renderDT({
    
    data <- GenData() %>% dplyr::filter(!is.na(IRG_RECOM_CODE)) 
    
    validate(
      need(nrow(data) > 0, 'No data exists, please make a different selection')
    )
    
    ###### Applications
    tr_fy.app <- data %>%  
      dplyr::group_by(FY) %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::summarise("Total Applications" = n())
      
    
    ##### Awarded projects 
    tr_fy.awd <- data %>%  
      dplyr::filter(APPL_STATUS_CODE %in% c(5,6,99)) %>% 
      dplyr::group_by(FY) %>% 
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::summarise("Total Awards" = n())
    
    rate_table  <- rate_func(data)
    
    validate(
      need(nrow(rate_table) > 0, 'No rate data exists for your selection, please make a different selection')
    )
    
    #### calculate rates
    table1 <- rate_table %>%
      dplyr::mutate(Value = round(Value)) %>% 
      tidyr::spread(Rates, Value) %>% 
      dplyr::select(-c( n.award, n.app.resub,n.app.no_resub,texts)) %>% 
      dplyr::left_join(tr_fy.app, by = "FY") %>% 
      dplyr::left_join(tr_fy.awd, by = "FY") %>% 
      dplyr::rename("Success Rate (%)"= Success, "Award Rate (%)" = Award) %>% 
      dplyr::select(FY , `Total Applications`,`Total Awards`, `Success Rate (%)`, `Award Rate (%)`) %>% 
      # group_by(FY) %>%
      # summarise_all(list(my_no.nafun)) %>% distinct(FY, .keep_all = TRUE) %>% 
      dplyr::arrange(FY)
    
    table_final <- aggregate(table1[-1], list(table1$FY), FUN = mean, na.rm = TRUE)
    colnames(table_final)[1] <- "FY"
    
    validate(
      need(nrow(table_final) > 0, 'No data exists for your selection, please make a different selection')
    )
    
    
    datatable(table_final, selection = "single", extensions = 'Buttons', escape = FALSE,
              options = list(dom = 'lfrtBp',
                             buttons = c('excel','csv','print'),
                             pageLength = 17,
                             scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             headerCallback = DT::JS(
                               "function(thead) {",
                               "  $(thead).css('font-size', '1.2em');",
                               "}")))
  })
  
  ######### project view table -------------------------------
  
  output$project_table <- renderDT(server = FALSE, {
    table3 <- projData() %>%
      dplyr::distinct(APPL_ID, .keep_all = TRUE) %>%
      dplyr::select(PROJECT_TITLE, project_id, APPL_ID, FY, division, APPL_STATUS_CODE,
                    IRG_PERCENTILE_NUM, PRIORITY_SCORE_NUM, ACTIVITY_CODE, APPL_TYPE_CODE, 
                    TOTAL_AWARDED_AMT, RFA_PA_NUMBER, NOTICE_TITLE, AGENCY_ROUTING_NUM, FOA_TITLE) %>%
      dplyr::rename("Project Title" = PROJECT_TITLE, "Project Number" = project_id, "APPL ID"=APPL_ID, 
                    Division = division, Status = APPL_STATUS_CODE, Percentile = IRG_PERCENTILE_NUM, 
                    Score = PRIORITY_SCORE_NUM, "Activity Code"= ACTIVITY_CODE, 
                    Type = APPL_TYPE_CODE, "Total Amount" = TOTAL_AWARDED_AMT,
                    "RFA PA Number" = RFA_PA_NUMBER, "RFA PA Title" = NOTICE_TITLE, 
                    "NOSI Number" = AGENCY_ROUTING_NUM, "NOSI Title" = FOA_TITLE) %>% 
      mutate(`Total Amount` = 
               case_when(!is.na(`Total Amount`)~ paste("$ ",prettyNum(`Total Amount`, big.mark=",",scientific=FALSE))))
    
    validate(
      need(nrow(table3) > 0, 'No data exists, please make a selection'))
    
    datatable(table3,  selection = "single", extensions = 'Buttons', escape = FALSE,
              options = list(dom = 'lfrtBp',
                             # buttons = c('excel', 'csv', 'print'),
                             buttons = list(
                               list(extend = "csv", text = "Download Current Page", filename = "page",
                                    exportOptions = list(
                                      modifier = list(page = "current")
                                    )
                               ),
                               list(extend = "csv", text = "Download Full Results", filename = "data",
                                    exportOptions = list(
                                      modifier = list(page = "all")
                                    )
                               )
                             ),
                             pageLength = 15,
                             # autoWidth = TRUE,
                             scrollX = TRUE, 
                             columnDefs = list(list(targets = c(1, 13, 15), render = JS(
                               "function(data, type, row, meta) {",
                               "return type === 'display' && typeof data === 'string' && data.length > 20 ?",
                               "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                               "}"))),
                             columnDefs = list(list(width = "150px", targets = c(1,13,15)),
                                               list(className = 'dt-left', targets = c(1,13,15)),
                                               list(className = 'dt-center', targets = c(2:13,14))),
                             headerCallback = DT::JS(
                               "function(thead) {",
                               "  $(thead).css('font-size', '1.2em');",
                               "}")))
  })
  
  
  
##################################
########### Dashboard Info
#################
  
  output$Info1 <- renderText(
    paste(
    h3(strong("What Fiscal Year (FY) are covered in the dashboard?"), style = "font-weight: 550"), br(),
    h4("The dashboard includes data from 2005 to 2021."), 
    br(), br(),br(),
    collapse = "<br><br>")
    )
  
  output$Info2 <- renderText(
    paste(
    h3(strong("What activity codes are taken into account in the dashboard?"), style = "font-weight: 550"), br(),
    h4("The dashboard includes all trainees activity codes (K, F, and T mechanisms). All K mechanisms 
    (K01, K02, K08, K18, K22, K23, K24, K25, K99) refer to individualcareer development and transition 
    awards. The F mechanisms (F30, F31, F32, F33) refer to individual fellowship awards. Finally, The 
    T mechanisms (T32, T35) refer to institutional research grants."),
    br(), br(), br(),
    collapse = "<br><br>")
    )
  
  output$Info3 <- renderText(
    paste(
      h3(strong("How are the award and success rates calculated?"), style = "font-weight: 550"), br(),
      # h4("The success rate which describes the likelihood of a project or an idea getting funded is calculated as
      #              the number of award in a fiscal year divided by the number of applications reviewed (excluding resubmissions
      #              in that fiscal year). The award rate describing the chance of an individual application
      #              being funded is calculated as the number of award in a fiscal year divided by the number of applications reviewed
      #              (including resubmissions in that fiscal year). The funding rate reflecting the number of investigators
      #              who seek and obtain funding is equal to the number of unique PIs receiving funding in a fiscal year divided by
      #              the number of unique PIs with applications reviewed in that fiscal year (Note: PIs on multiple PI aplications are included
      #              in the funding rate's calculation)."),
      withMathJax(helpText()),## This is to initiate the first MathJax and then we only use div later
      h4(strong("Success rate"), "which describes the likelihood of a project or an idea getting funded is calculated as:"), br(),
      div('$$\\textsf{Success Rate} = \\frac{\\textsf{Number of awards in a fiscal year}}{\\textsf{Applications reviewed (excluding resubmissions in that fiscal year)}}$$'), br(),
      h4(strong("Award rate"), "describing the chance of an individual application being funded is calculated:"), br(),
      div("$$\\textsf{Award Rate} = \\frac{\\textsf{Number of awards in a fiscal year}}{\\textsf{Applications reviewed (including resubmissions in that fiscal year)}}$$"), br(),
      h4(strong("Funding rate"), "reflecting the number of investigators who seek and obtain funding is as follow:"), br(),
      div("$$\\textsf{Funding Rate} = \\frac{\\textsf{Number of unique PIs receiving funding in a fiscal year}}{\\textsf{Number of unique PIs with applications reviewed in that fiscal year}}$$"), br(),
      h4("Note that PIs on multiple PI aplications are included in the funding rate's calculation."), br(),
      h5(strong("Note: "), "Definition of rates are taken from the Office of Extramural Research (OER) which can be seen on the",
         tagList(a("OER Webpage", href="https://nexus.od.nih.gov/all/2015/06/29/what-are-the-chances-of-getting-funded/")),"."), 
      br(), br(), br(),
      collapse = "<br><br>")
    )
  
  output$Info4 <- renderText(
    paste(
      h3(strong("How are the special topics computed?"), style = "font-weight: 550"), br(),
      h4(strong("Diversity:"), "Any project with yes as diversity flag or projects that have the word `diversity` in either the project or the RFA PA title."),
      h4(strong("Mosaic:"), "Any project that have the word `mosaic` in either the project or the RFA PA title."),
      h4(strong("Physician Scientist:"), "Any project that have the words `physician scientist or physician-scientist` in either the project or the RFA PA titles."),
      h4(strong("Non-Human Primate:"), "Any project that have the words `Non-Human Primate or Nonhuman Primate` in either the project or the RFA PA titles."),
      br(), br(), br(),
      collapse = "<br><br>")
  )
  
  output$Info5 <- renderText(
    paste(
      h3(strong("How often is the data refreshed?"), style = "font-weight: 550"), br(),
      h4("This is an interactive dashboard. It will present the latest data whenever it is access."),
      br(), br(), br(),
      collapse = "<br><br>")
  )
  


  
}

