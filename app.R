#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


############### Read R files from the current environment
source('general.R', local = TRUE)
source('myUI.R', local = TRUE)
source('myServer.R', local = TRUE)





###############################################################################
############################ Run the application ##############################
###############################################################################

shinyApp(
  ui = ui, 
  server = server
  )
