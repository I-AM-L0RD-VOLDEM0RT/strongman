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
  
observeEvent(input$update_competitor, {
  
  if(input$update_competitor[1] == 1) {
    
    comp_info <- isolate(c(input$first_name, input$last_name, input$division))
    comp_info <- t(comp_info)
    colnames(comp_info) <- c("First Name", "Last Name", "Division")
    rownames(comp_info) <- NULL
    comp_info <- as.data.frame(comp_info)
    
    comp_info <- comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      distinct
    
    comp_info <<- comp_info
    
  }else{
    
    new_entry <- isolate(c(input$first_name, input$last_name, input$division))
    new_entry <- t(new_entry)
    new_entry <- as.data.frame(new_entry)
    colnames(new_entry) <- c("First Name", "Last Name", "Division")
    rownames(new_entry) <- NULL
    
    comp_info <- rbind(comp_info, new_entry)
    comp_info <- comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      distinct()
    comp_info <<- comp_info
    
  }
  
  if(input$first_name == "" | input$last_name == "") {
    
    if(input$first_name == "") {
      
      confirmSweetAlert(session = session, 
                        inputId = "no_first_name",
                        title = "Please Input Competitor First Name!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    if(input$last_name == "") {
      
      confirmSweetAlert(session = session, 
                        inputId = "no_last_name",
                        title = "Please Input Competitor Last Name!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    if(input$first_name == "" & input$last_name == "") {
      
      confirmSweetAlert(session = session, 
                        inputId = "no_full_name",
                        title = "Please Input Competitor Name!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
  }else{
    
    confirmSweetAlert(session = session, 
                      inputId = "competitor_success",
                      title = "Competitor Successfully Added!",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }
  
  output$competitor_table <- renderTable({
    
    comp_info
    
  })
  
})
 
}

shinyApp(ui = ui, server = server)