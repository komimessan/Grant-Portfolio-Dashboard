######## general.R
########################### This file contain all the functions and libraries

### Load Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) ## v. 2.0.3 ## to use slides on a R shiny page with carousel
library(shinyWidgets) ## v. 0.6.2 ## to use pickerinput
library(shinyjs) ##v. 2.0.0 ##. 
library(shinycssloaders) ## v. 1.0.0 ## to use withSpinner
library(DT) ## v. 0.20 ## For Table
library(plotly) ## v. 4.10.0 ## interactive plots 
library(tidyverse) ## v. 1.3.1 ## data wrangling
library(odbc) ## v. 1.3.2 ## access data bases
library(purrr) ## v. 0.3.4 ## data wrangling
library(datapasta) ## v. 3.1.0 ## coping and pasting data to and from R
library(ggplot2) ## v. 3.3.5 ## Visualization
library(ggrepel) ## v. 0.9.1 ## To create labelling for graph
library(stringr) ##v. 1.4.0 ## For string manipulation and extraction
library(gt) ## v. 0.3.1 ## grammar of tables



###################### Loading the trainees data

#### connect to IRDB
## Uncomment this below if you desire to run the app on your computer, replace name_DB with appropriate UID and enter the password when prompt

con <- dbConnect(odbc::odbc(), Driver = "oracle", UID = "name_DB",
                 Port = "1551", PWD = rstudioapi::askForPassword("Database password:"),
                 Host = "flt-scan-irdb-prd-replica.era.nih.gov", SVC = "prdirdb")


## Use this to store the work in RStudio cConnect where pwd was already stored 
# con <- dbConnect(odbc::odbc(), Driver = "oracle", UID = "name_DB",
#                  Port = "1551", PWD = Sys.getenv("irdb_pwd"),
#                  Host = "flt-scan-irdb-prd-replica.era.nih.gov", SVC = "prdirdb")

#### Query the data 
## Select only Reviewed applications using "a.IRG_RECOM_CODE IS NOT NULL AND a.SPECIAL_DISP_CODE IS NULL"
## select PI and MPI only
## select applications from 2009 to 2018
## select no multi-PI
## select period type Budget to take into account only current funding 
## (e.g. COM would take into account planning) so always select BUD for budget


all_data <- dbGetQuery(con, "SELECT a.FY,
a.ACTIVITY_CODE,
a.ADMIN_PHS_ORG_CODE,
a.APPL_ID,
a.APPL_STATUS_CODE,
a.APPL_TYPE_CODE,
a.NEW_INVESTIGATOR_CODE, 
a.ESI_APPL_ELIG_FLAG, 
a.GRANT_NUM,
a.MULTI_PI_INDICATOR_CODE,
a.PROJECT_TITLE,
a.SERIAL_NUM,
a.COMPETING_GRANT_CODE,
a.EXTERNAL_ORG_ID,
a.IRG_RECOM_CODE,
a.SPECIAL_DISP_CODE,
a.PROG_CLASS_CODE,
a.IRG_PERCENTILE_NUM,
a.PRIORITY_SCORE_NUM,
a.RFA_PA_NUMBER,
a.MAJOR_ACTIVITY_CODE,
a.AGENCY_ROUTING_NUM,
rfat.NOTICE_TITLE,
rfat.DIVERSITY_FLAG,
n.FOA_TITLE,
pers.PERSON_ID,
pers.PROFILE_PERSON_ID,
pers.TRANS_FIRST_NAME,
pers.TRANS_LAST_NAME,
pit.ROLE_TYPE_CODE,
awd.TOTAL_OBLGTD_AMT,
awd.TOTAL_AWARDED_AMT,
awd.PERIOD_TYPE_CODE
FROM APPLS_AT a
LEFT JOIN PERSON_INVOLVEMENTS_AT pit ON a.APPL_ID = pit.APPL_ID
LEFT JOIN AWD_FUNDINGS_AT awd ON a.APPL_ID = awd.APPL_ID
LEFT JOIN PERSONS_AT pers ON pit.PERSON_ID = pers.PERSON_ID
LEFT JOIN PUBLISHED_NOTICES_MV n ON a.AGENCY_ROUTING_NUM = n.FOA_NUMBER
LEFT JOIN RFA_PA_NOTICES_MV rfat ON a.RFA_PA_NUMBER = rfat.RFA_PA_NUMBER
WHERE a.FY between 2005 and 2021
AND (awd.PERIOD_TYPE_CODE = 'BUD' OR awd.PERIOD_TYPE_CODE is NULL)
AND pit.ROLE_TYPE_CODE IN ('PI', 'MPI')
AND ADMIN_PHS_ORG_CODE IN ('AI')
AND a.MAJOR_ACTIVITY_CODE IN ('F','K','T')
ORDER BY a.FY") %>% 
  mutate(project_id = str_sub(GRANT_NUM,1,11))
# mutate(project_id = paste(ACTIVITY_CODE, ADMIN_PHS_ORG_CODE, SERIAL_NUM, sep = ""))


## AND (a.IRG_RECOM_CODE IS NOT NULL) (include after awd.period_type_code='bud')
####### Training Activity codes
## F: Fellowship Programs
## K: Research Career Program
## T: Training Programs
# tr_ac <- c("F05","F06","F30","F31","F32","F33","F34","F36","F37","F38",
#            "K01","K02","K04","K05","K07","K08","K10","K11","K12","K14",
#            "K15","K16","K17","K18","K20","K21","K22","K23","K24","K25",
#            "K26","K30","K38","K43","K76","K99","KL1","KL2","KM1",
#            "T01","T14","T15","T22","T23","T32","T34","T35","T36","T37",
#            "T90","TL1","TL4","TU2")

######### Training Data
#### All data
tr_data.niaid <- all_data %>% #dplyr::filter(ACTIVITY_CODE %in% tr_ac) %>% 
  dplyr::rename(pcc = PROG_CLASS_CODE) %>% 
  dplyr::mutate(ac_category = case_when(str_sub(ACTIVITY_CODE,1,1)=="F"~"Fellowship",
                                        str_sub(ACTIVITY_CODE,1,1)=="K"~"Research Career",
                                        str_sub(ACTIVITY_CODE,1,1)=="T"~"Training"),
                division = case_when(str_sub(pcc,1,1)=="A"~"DAIDS",
                                     str_sub(pcc,1,1)=="I"~"DAIT",
                                     str_sub(pcc,1,1)=="M"~"DMID",
                                     str_sub(pcc,1,1)=="X"~"DEA"),
                Competing = case_when(COMPETING_GRANT_CODE=="Y"~"Competing",
                                      COMPETING_GRANT_CODE=="N"~"Non-Competing",
                                      APPL_ID == "7928147"~"Non-Competing"), ## NA APPL_ID corresponding to Non-Competing
                APPL_STATUS_CODE = as.numeric(APPL_STATUS_CODE),
                pcc = gsub(' [A-z0-9 ]*','',pcc)) 



# ## Create data frame of project_id and pcc code since some subproject do not
# ## have pcc code but all parents project do. So we want to populate pcc where needed
# 
# tr_data_pcc_niaid <- tr_data.niaid %>% 
#   select(project_id, pcc) %>% 
#   filter(!is.na(pcc)) %>% 
#   distinct(project_id, .keep_all = TRUE)
# 
# ## setdiff(tr_data.niaid$project_id,tr_data_pcc_niaid$project_id)
# ## The above codes yield 23 project_id and thus we need to find pcc_code for those 23 projects
# 
# tr_data_parent_pcc <- tr_data.niaid %>% 
#   filter(project_id %in% setdiff(tr_data.niaid$project_id,tr_data_pcc_niaid$project_id)) %>% 
#   filter(!is.na(pcc)) %>% select(project_id, pcc) %>% 
#   distinct(project_id, .keep_all = TRUE)


#### Extract data without pcc
data_no_pcc <- subset(tr_data.niaid, is.na(pcc))

#### Extract project_id of those data without pcc and eliminate those without pcc
data_with_pcc <- tr_data.niaid %>% 
  filter(project_id %in% data_no_pcc$project_id) %>% 
  filter(!is.na(pcc)) %>% 
  select(project_id, pcc)

##### Left jojn by pcc
### And Add division based on new added pcc codes
tr_data.niaid <- tr_data.niaid %>% 
  left_join(data_with_pcc, by = "project_id") %>%
  mutate(pcc = coalesce(pcc.x,pcc.y)) %>% 
  select(-pcc.x, - pcc.y) %>% 
  mutate(division = case_when(str_sub(pcc,1,1)=="A"~"DAIDS",
                              str_sub(pcc,1,1)=="I"~"DAIT",
                              str_sub(pcc,1,1)=="M"~"DMID",
                              str_sub(pcc,1,1)=="X"~"DEA"))
#mutate(division = case_when(!is.na(pcc)~division, is.na(pcc)~"No PCC"))

##### Check data again for those with no pcc codes 
## These are parents and do not have pcc codes
data_no_pcc2 <- subset(tr_data.niaid, is.na(pcc))

##### Add "No PCC" for those data that do not have PCC code
tr_data.niaid$division <- ifelse(tr_data.niaid$APPL_ID %in% data_no_pcc2$APPL_ID, "No PCC", 
                                 tr_data.niaid$division)



#### Data for IRG_RECOM_CODE IS NOT NULL
tr_data.niaid2 <- tr_data.niaid %>% dplyr::filter(!is.na(IRG_RECOM_CODE))


#######################################################
#######################################################


#### create template for plot labeling
black.bold.text1 <- element_text(face = "bold", color = "black",size=16) # x and y axis
black.bold.text2 <- element_text(face = "bold", color = "black",size=16) # title
Nice.Label <-theme(axis.text.x = element_text(face="bold", color="black", size=10),
                   axis.text.y = element_text(face="bold", color="black", size=10),
                   title=black.bold.text2,axis.title = black.bold.text1, legend.position = "bottom",
                   legend.box = "vertical",
                   legend.text = element_text(size=20), strip.text.x = element_text(face="bold",size=16), 
                   strip.text.y = element_text(face="bold",size=16)) #18 or 24



###############################################################################
########################################## Success, Award, and Funding Rates
###############################################################################

################# Function to calculate rates
rate_func <- function(data){
  ## data is the main data containing funding information
  
  
  ##################### Preliminary data calculation for success and award rates
  
  ## select the competing (New and Renew) type
  data <- data %>% subset(COMPETING_GRANT_CODE == "Y") #%>%
  #filter(not_with_b4_rev == "yes")
  
  ## PI contact only
  data_pi.c <- data %>% subset(ROLE_TYPE_CODE=="PI") ##distinct(APPL_ID,.keep_all = TRUE) #
  
  ##### Calculate number of awards (Status 5 and 6)
  df_award <- data_pi.c %>% filter(APPL_STATUS_CODE %in% c(5,6)) %>% 
    group_by(FY) %>% 
    distinct(project_id, .keep_all = TRUE) %>% 
    dplyr::summarise(n.award=n())
  
  #### Application review without re-submission 
  df_app.no_resub <- data_pi.c %>%
    group_by(FY) %>% 
    distinct(project_id, .keep_all = TRUE) %>% 
    dplyr::summarise(n.app.no_resub = n())
  
  #### Application review with re-submission
  df_app.resub <- data_pi.c %>%
    group_by(FY) %>%
    dplyr::summarise(n.app.resub = n())
  
  # ##################### Preliminary data calculation for funding rates
  # ## Number of unique PI receiving funding
  # df_pi_fund <- data %>% filter(APPL_STATUS_CODE %in% c(5,6)) %>% # 
  #   group_by(FY) %>% 
  #   distinct(PROFILE_PERSON_ID, .keep_all = TRUE) %>% 
  #   dplyr::summarise(n.pi_fund=n())
  # 
  # ## Number of unique PI with application review that year
  # df_pi.app_rev <- data %>% 
  #   group_by(FY) %>% 
  #   distinct(PROFILE_PERSON_ID, .keep_all = TRUE) %>% 
  #   dplyr::summarise(n_pi.app_rev=n())
  
  ############ Merge all data by FY
  # df_all <- Reduce(function(...) merge(..., all=TRUE), list(df_award, df_app.no_resub, 
  #                                                           df_app.resub, df_pi_fund,
  #                                                           df_pi.app_rev))
  df_all <- Reduce(function(...) merge(..., all=TRUE), list(df_award, df_app.no_resub,df_app.resub))
  
  
  ######################### Rates calculation 
  rates <- df_all %>% 
    group_by(FY) %>% 
    dplyr::mutate(Success = (n.award/n.app.no_resub)*100, 
                  Award = (n.award/n.app.resub)*100)#,
  #Funding = (n.pi_fund/n_pi.app_rev)*100) 
  
  
  ## transform to long table
  rates_long <- gather(rates, Rates, Value, Success:Award, factor_key = TRUE) %>% 
    mutate(texts = map(paste('<b>Rate:</b>', Rates, '<br>','<b>Fiscal Year:</b>', FY, 
                             '<br>', '<b>Percent:</b>', round(Value)), HTML))
  #rates_long$FY <- as.Date(as.character(rates_long$FY), "%Y")
  
  ##return results
  return(rates_long)
}

'%!in%' <- function(x,y){!('%in%'(x,y))} ## not in function


## Function to remove duplicates NA
#my_no.nafun <- function(x) x[!is.na(x)]