library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(stringr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Strongman Scoring"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Competitor Input", tabName = "competitor", icon = icon("address-card")),
      menuItem("Event Name", tabName = "event", icon = icon("empire")),
      menuItem("Scoring", tabName = "score", icon = icon("first-order")),
      menuItem("Current Standings", tabName = "standings", icon = icon("rebel"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "competitor",
              h2("Input Competitor Information:"),
               fluidRow(
                 column(4,
                textInput(inputId = "first_name", label = "First Name"),
                textInput(inputId = "last_name", label = "Last Name"),
                radioButtons(inputId = "division", label = "Division", choices = c("WLW", "WOW", "MLW", "MOW"),
                             selected = "WLW"),
              actionButton(inputId = "update_competitor", label = "Add Competitor")
               ),
              fluidRow( 
              column(3, tableOutput("competitor_table"))
              # # )
               ))),
      
      tabItem(tabName = "event",
              h2("Select Events for Competition:")
              ),
      
      tabItem(tabName = "score",
              h2("Input Competitor's Score:")
              ),
      
      tabItem(tabName = "standings",
              h2("Get Current Standings:")
              )
    )
  )
  
)

server <- function(input, output, session) {
  
 comp_info <- reactiveValues()

 # browser()

 # colnames(comp_info) <- c("First Name", "Last Name", "Division")

 reactive({if(input$update_competitor == 0) {

 comp_info$values <<- 
   data.frame("First Name" = NA, "Last Name" = NA,
                                "Division" = NA)



 }else{

   comp_info$values <<-

     data.frame("First Name" = input$first_name, "Last Name" = input$last_name,
                                  "Division" = input$division)

   # colnames(comp_info$values) <- c("First Name", "Last Name", "Division")
  
 }
   
   
 })

 # reactive({
 #   if(input$update_competitor == 1){
 #     
 #     comp_info$values
 #     
 #   }else{
 
 
 
 new_competitor <- observe({
  if(input$update_competitor > 0) {
    
    # browser()
    
     new_entry <- isolate(c(input$first_name, input$last_name, input$division))
     # isolate(comp_info$values[nrow(comp_info$values) + 1] <- c(input$first_name, input$last_name,input$division))

     isolate(comp_info$values <- rbind(comp_info$values, new_entry))

     colnames(comp_info$values) <- c("First Name", "Last Name", "Division")
    
     # browser()
       
     comp_info$values <<- as.data.frame(comp_info$values)
     
     # comp_info$values <<- comp_info$values %>% 
     #   distinct(.keep_all = T)
       
      # comp_info$values <<- comp_info$values[,1]
      
     
     # comp_info$values <- as.matrix(comp_info$values)
    
     # browser()
  
  }
   
   # if(input$update_competitor == 1) {
   #   
   #   # browser()
   #  
   #  colnames(comp_info$values) <- c("First Name", "Last Name", "Division")
   #  
   # }else{
   #   
   #   comp_info$values <- NULL
   # }
   
   
  })
 
 # comp_info$values
 #   }
   
 

   # if(input$update_competitor > 0) {
   # 
   #   # browser()
   # 
   # # comp_info$values <- comp_info$values %>%
   # #   filter(!is.null(comp_info$value))
   # 
   # comp_info$values <- comp_info$values[, -1]

   
   # })
 
 
 
 
 output$competitor_table <- renderTable({
   
   # browser()
   
   # comp_info <- reactiveValues()
   # 
   # if(input$update_competitor == 0) {
   #   
   #   comp_info$values <- data.frame("First Name" = "First Name", "Last Name" = "Last Name",
   #                                  "Division" = "Division")
   #   
   # }else{
   #   
   #   comp_info$values <- data.frame("First Name" = input$first_name, "Last Name" = input$last_name,
   #                                  "Division" = input$division)
   #  
   # }
   # 
   # new_competitor <- observe({
   #   if(input$update_competitor > 1) {
   #     new_entry <- isolate(c(input$first_name, input$last_name, input$division))
   #     # isolate(comp_info$values[nrow(comp_info$values) + 1] <- c(input$first_name, input$last_name,input$division))
   #     
   #     isolate(comp_info$values <- rbind(comp_info$values, new_entry))
   #     
   #   }
   # })
   
   if(is.null(comp_info$values)) {
     
     return()
     
   }else{
   
   comp_info$values <- comp_info$values %>% 
     distinct()
   }
 })
 
}

shinyApp(ui = ui, server = server)