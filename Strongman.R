library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(devtools)
library(fontawesome)

new_entry <- NULL
unfiltered_comp_info <- NULL
comp_info <- NULL

ui <- dashboardPage(
  
  dashboardHeader(title = "Strongman Scoring"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Competition Classes", tabName = "classes", icon = icon("magic")),
      menuItem("Competitor Input", tabName = "competitor", icon = icon("address-card")),
      menuItem("Event Name", tabName = "event", icon = icon("empire")),
      menuItem("Edit  Tables", tabName = "edits", icon = icon("minus-circle"),
               menuSubItem("Edit Division", tabName = "division_edit"),
               menuSubItem("Edit Competitor Input", tabName = "competitor_edit"),
               menuSubItem("Edit Events", tabName = "event_edit")),
      menuItem("Scoring", tabName = "score", icon = icon("first-order")),
      menuItem("Current Standings", tabName = "standings", icon = icon("rebel"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "classes",
              h2("Weight Classes"),
              fluidRow(column(4,
              textInput(inputId = "division_input", label = "Please Input Division Name"),
              actionButton(inputId = "division_add", label = "Add Division")),
              fluidRow(column(3, dataTableOutput("division_table")))
                     )),
      tabItem(tabName = "competitor",
              h2("Input Competitor Information:"),
               fluidRow(
                 column(4,
                textInput(inputId = "first_name", label = "First Name"),
                textInput(inputId = "last_name", label = "Last Name"),
                # radioButtons(inputId = "division", label = "Division", choices = c("WLW", "WOW", "MLW", "MOW"),
                #              selected = "WLW"),
                selectizeInput(inputId = "division", label = "Division", choices = NULL, multiple = T),
              actionButton(inputId = "update_competitor", label = "Add Competitor")
               ),
              fluidRow( 
              column(3, tableOutput("competitor_table"))
              # # )
               ))),
      
      tabItem(tabName = "event",
              h2("Select Events for Competition:"),
              fluidRow(column(4,
                              textInput(inputId = "event_input", label = "Please Input Event Name"),
                              actionButton(inputId = "event_add", label = "Add Event")),
                       fluidRow(column(3, tableOutput("event_table")))
              )
              ),
      
      tabItem(tabName = "score",
              h2("Input Competitor's Score:")
              ),
      
      tabItem(tabName = "standings",
              h2("Get Current Standings:")
              ),
      tabItem(tabName = "division_edit",
              h2("Edit Division Input:")
              ),
      tabItem(tabName = "competitor_edit",
              h2("Edit Competitor Input:")
              ),
      tabItem(tabName = "event_edit",
              h2("Edit Event Input:")
              )
      
    )
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$division_add, {
    
    # browser()
    
    if(input$division_input == ""){
      
      confirmSweetAlert(session = session, 
                        inputId = "no_division_input",
                        title = "You need to input a division!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
    
    if(input$division_add[1] == 1) {
      
      division_info <- isolate(input$division_input)
      division_info <- as.data.frame(division_info)
      # division_info <- t(division_info)
      colnames(division_info)[1] <- ("Division")
      division_info <- division_info 
      rownames(division_info) <- NULL
      # division_info <- as.data.frame(division_info)
      
      unfiltered_division_info <<- division_info
      
      division_info <- division_info %>% 
        filter(Division != "") %>% 
        # filter(`Last Name` != "") %>% 
        distinct()
      
      division_info$Division <- as.character(division_info$Division)
      division_info <<- division_info
      
      confirmSweetAlert(session = session,
                        inputId = "division_success1",
                        title = "Division successfully included!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      new_division <- isolate(input$division_input)
      # new_entry <- t(new_entry)
      new_division <- as.data.frame(new_division)
      new_division <<- new_division
      colnames(new_division)[1] <- ("Division")
      rownames(new_division) <- NULL
      
      # browser()
      
      # if(nrow(union(new_entry, comp_info)) == nrow(comp_info)) {
      #   
      #   duplicate <- T
      #   duplicate <<- duplicate
      #   
      # }else{
      #   
      #   duplicate <- F
      #   duplicate <<- duplicate
      #   
      # }
      
      division_info <- rbind(division_info, new_division)
      unfiltered_division_info <<- division_info %>% 
        filter(Division != "")
      division_info <- division_info %>% 
        filter(Division != "") %>% 
        # filter(`Last Name` != "") %>% 
        distinct()
      division_info$Division <- as.character(division_info$Division)
      division_info <<- division_info
      # browser()
      
      confirmSweetAlert(session = session,
                        inputId = "division_success2",
                        title = "Division successfully included!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
      
      if(input$division_add[1] > 1) {
        
        # browser()
        
        if(nrow(division_info) < nrow(unfiltered_division_info)) {
          
          # browser()
          
          confirmSweetAlert(session = session,
                            inputId = "duplicate_division",
                            title = "Division Already Input!",
                            type = "warning",
                            btn_labels = "OK!",
                            danger_mode = T)
          
          unfiltered_division_info <- unfiltered_division_info %>% 
            filter(Division != "") %>% 
            # filter(`Last Name` != "") %>% 
            distinct()
          
        }
        
      }
    
    output$division_table <- renderDataTable(
      
     division_info, rownames = F, selection = "none", editable = T)
    
   # browser()
    
    updateTextInput(session = session, inputId = "division_input", label = "Please Input Division Name",
                    value = "")
    
    updateSelectizeInput(session = session, inputId = "division",
                         choices = division_info$Division, selected = NULL)
    
    
    }
    
  })
  
observeEvent(input$update_competitor, {
  
  # browser()
  
  if(is.null(input$division)) {
    
    confirmSweetAlert(session = session, 
                      inputId = "no_division",
                      title = "Please input division!",
                      text = "If you haven't created the divisions, please do so.",
                      type = "warning",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }else{
    
    # browser()
    
    if(nrow(as.data.frame(input$division)) > 1) {
      
      confirmSweetAlert(session = session, 
                        inputId = "multiple_division",
                        title = "Please only input one division!",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
  
  if(input$update_competitor[1] == 1) {
    
    comp_info <- isolate(c(input$first_name, input$last_name, input$division))
    comp_info <- t(comp_info)
    colnames(comp_info) <- c("First Name", "Last Name", "Division")
    rownames(comp_info) <- NULL
    comp_info <- as.data.frame(comp_info)
    
    unfiltered_comp_info <<- comp_info%>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "")
    
    comp_info <- comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      distinct()
    
    comp_info <<- comp_info
    
    # browser()
    
    # updateTextInput(session = session, inputId = "first_name", label = "First Name", value = "")
    # updateTextInput(session = session, inputId = "last_name", label = "Last Name", value = "")
    # updateSelectizeInput(session = session, inputId = "division",
    #                      choices = division_info$Division, selected = NULL)
    
    
  }else{
    
    # browser()
    
    new_entry <- isolate(c(input$first_name, input$last_name, input$division))
    new_entry <- t(new_entry)
    new_entry <- as.data.frame(new_entry)
    new_entry <<- new_entry
    colnames(new_entry) <- c("First Name", "Last Name", "Division")
    rownames(new_entry) <- NULL
    
    # browser()
    
    # if(nrow(union(new_entry, comp_info)) == nrow(comp_info)) {
    #   
    #   duplicate <- T
    #   duplicate <<- duplicate
    #   
    # }else{
    #   
    #   duplicate <- F
    #   duplicate <<- duplicate
    #   
    # }
    
    comp_info <- rbind(comp_info, new_entry)
    unfiltered_comp_info <<- comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "")
    comp_info <- comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      distinct()
    comp_info <<- comp_info
    
    # browser()
    
    # updateTextInput(session = session, inputId = "first_name", label = "First Name", value = "")
    # updateTextInput(session = session, inputId = "last_name", label = "Last Name", value = "")
    # updateSelectizeInput(session = session, inputId = "division",
    #                      choices = division_info$Division, selected = NULL)
    
  }
  
  # browser()
  
  # row_in_comp <- new_entry[,1] %in% comp_info
  # truth <- c(TRUE, TRUE, TRUE)
  # true_or_false <- all.equal(row_in_comp, truth)
  
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
  
  # if(input$update_competitor[1] == 3) {
  #   
  #   View(unfiltered_comp_info)
  #   View(comp_info)
  #   
  #   browser()
  #   
  # }
  
  if(input$update_competitor[1] > 1) {
  
  if(nrow(comp_info) < nrow(unfiltered_comp_info)) {
    
    # browser()
    
    confirmSweetAlert(session = session,
                      inputId = "duplicate",
                      title = "Competitor Already Input!",
                      type = "warning",
                      btn_labels = "OK!",
                      danger_mode = T)
    
    unfiltered_comp_info <- unfiltered_comp_info %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      distinct()
    
  }
  }
  
  output$competitor_table <- renderTable({
    
    comp_info
    
  })
    }
  }
  
  updateTextInput(session = session, inputId = "first_name", label = "First Name", value = "")
  updateTextInput(session = session, inputId = "last_name", label = "Last Name", value = "")
  updateSelectizeInput(session = session, inputId = "division",
                       choices = division_info$Division, selected = NULL)
  
})

observeEvent(input$event_add, {
  
  # browser()
  
  if(input$event_input == "") {
    
    # browser()
    
    confirmSweetAlert(session = session, 
                      inputId = "no_event_input",
                      title = "You need to input an event!",
                      type = "warning",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }else{
    
    if(input$event_add[1] == 1) {
      
      event_info <- isolate(input$event_input)
      event_info <- as.data.frame(event_info)
      # division_info <- t(division_info)
      colnames(event_info)[1] <- ("Event")
      # event_info <- division_info 
      rownames(event_info) <- NULL
      # division_info <- as.data.frame(division_info)
      
      unfiltered_event_info <<- event_info
      
      event_info <- event_info %>% 
        filter(Event != "") %>% 
        # filter(`Last Name` != "") %>% 
        distinct()
      
      event_info <<- event_info
      
      confirmSweetAlert(session = session,
                        inputId = "event_success1",
                        title = "Event successfully included!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
      
      new_event <- isolate(input$event_input)
      # new_entry <- t(new_entry)
      new_event <- as.data.frame(new_event)
      new_event <<- new_event
      colnames(new_event)[1] <- ("Event")
      rownames(new_event) <- NULL
      
      # browser()
      
      # if(nrow(union(new_entry, comp_info)) == nrow(comp_info)) {
      #   
      #   duplicate <- T
      #   duplicate <<- duplicate
      #   
      # }else{
      #   
      #   duplicate <- F
      #   duplicate <<- duplicate
      #   
      # }
      
      event_info <- rbind(event_info, new_event)
      unfiltered_event_info <<- event_info
      event_info <- event_info %>% 
        filter(Event != "") %>% 
        # filter(`Last Name` != "") %>% 
        distinct()
      event_info <<- event_info
      # browser()
      
      confirmSweetAlert(session = session,
                        inputId = "event_success2",
                        title = "Event successfully included!",
                        type = "success",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }
    
    if(input$event_add[1] > 1) {
      
      # browser()
      
      if(nrow(event_info) < nrow(unfiltered_event_info)) {
        
        # browser()
        
        confirmSweetAlert(session = session,
                          inputId = "duplicate_event",
                          title = "Event Already Input!",
                          type = "warning",
                          btn_labels = "OK!",
                          danger_mode = T)
        
        unfiltered_event_info <- unfiltered_event_info %>% 
          filter(Event != "") %>% 
          # filter(`Last Name` != "") %>% 
          distinct()
        
      }
      
    }
    
    output$event_table <- renderTable({
      
      event_info
      
    })
    
    # browser()
    
    updateTextInput(session = session, inputId = "event_input", label = "Please Input Event Name",
                    value = "")
    
    # updateSelectizeInput(session = session, inputId = "division",
    #                      choices = division_info$Division, selected = NULL)
    
    
  }
  
})


observeEvent(input$division_table_cell_edit, {
  
  division_proxy <- dataTableProxy("division_table")
  
  # browser()
  
  # Add a conditional if the value is null, but otherwise,
  # you don't need to worry about a character being numeric,
  # so everything should work great. Push it across the board.
  
  # if(input$division_table_cell_edit$value == "") {
  #   
  #   # browser()
  #   
  #   div_edit_info = input$division_table_cell_edit
  #   str(div_edit_info)
  #   i = div_edit_info$row
  #   # j = div_edit_info$col
  #   v = div_edit_info$value
  #   div_as_character <- as.character(division_info[i, 1])
  #   division_info[i] <<- DT::coerceValue(div_as_character, div_as_character)
  #   replaceData(division_proxy, division_info, resetPaging = FALSE)
  #   
  #   output$division_table <- renderDataTable(division_info, rownames =F, editable = T, selection = "none")
  #   
  #   confirmSweetAlert(session = session, 
  #                     inputId = "div_edit_empty",
  #                     title = "There is no value to change the division to!",
  #                     text = "If you would like to delete, switch to the removal tab!",
  #                     type = "warning",
  #                     btn_labels = "OK!", 
  #                     danger_mode = T)
  #   
  # }else{
  
  div_edit_info = input$division_table_cell_edit
  str(div_edit_info)
  i = div_edit_info$row
  # j = div_edit_info$col
  v = div_edit_info$value
  div_as_character <- as.character(division_info[i, 1])
  # browser()
  division_info[i, 1] <<- coerceValue(v, as.character(division_info[i, 1]))
  replaceData(division_proxy, division_info, resetPaging = FALSE)
  
  # browser()
  
  division_info <- division_info %>% 
    distinct() %>% 
    filter(Division != "")
  
  unfiltered_division_info <<- division_info
  
  output$division_table <- renderDataTable(division_info, rownames = F, editable = T, selection = "none")
  
  updateSelectizeInput(session = session, inputId = "division",
                       choices = division_info$Division, selected = NULL)
  
 if(input$division_table_cell_edit$value == "") {
   
   confirmSweetAlert(session = session, 
                     inputId = "remove_division_success", 
                     title = "Successfully removed division!",
                     type = "success",
                     btn_labels = "OK!", 
                     danger_mode = T)
   
 }else{
  
   confirmSweetAlert(session = session, 
                    inputId = "edit_division_success", 
                    title = "Successfully changed division!",
                    type = "success",
                    btn_labels = "OK!", 
                    danger_mode = T)
  
  }
  
})

# observeEvent(input$test, browser())
 
}

shinyApp(ui = ui, server = server)