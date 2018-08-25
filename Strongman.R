library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(devtools)
library(fontawesome)
library(DT)

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
              # textInput(inputId = "division_input", label = "Please Input Division Name"),
              uiOutput("division_action_buttons")),
            
              column(3, dataTableOutput("division_table"))
              # actionButton(inputId = "division_add", label = "Add Division"),
              # actionButton(inputId = "division_save", label = "Finalize Divisions")
              )#,
              
              # fluidRow(column(3, dataTableOutput("division_table")))
              # fluidRow(textOutput("blank")), fluidRow(),
              # fluidRow(column(4, actionButton(inputId = "division_save", label = "Finalize Divisions")))
                     ),
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
              column(3, dataTableOutput("competitor_table"))
              # # )
               ))),
      
      # tabItem(tabName = "event",
      #         h2("Select Events for Competition:"),
      #         fluidRow(column(4,
      #                         textInput(inputId = "event_input", label = "Please Input Event Name"),
      #                         actionButton(inputId = "event_add", label = "Add Event")),
      #                  fluidRow(column(3, dataTableOutput("event_table")))
      #         )
      #         ),
      
      tabItem(tabName = "event",
              h2("Competition Events"),
              fluidRow(column(4,
                       uiOutput("event_action_buttons")),
                       
                       column(3, dataTableOutput("event_table"))
                     
              )),
      
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
      
      # browser()
      
      # division_data_table <- DT::datatable(division_info, rownames = F, selection = "none", editable = T)
    
    output$division_table <- renderDataTable(division_info, rownames = F, selection = "none", editable = T)
    
   # browser()
    
    updateTextInput(session = session, inputId = "division_input", label = "Please Input Division Name",
                    value = "")
    
    if(!is.null(input$verify_division_save)){
    
    updateSelectizeInput(session = session, inputId = "division",
                         choices = division_info$Division, selected = NULL)
    
    }
    }
    
  })
  
observeEvent(input$update_competitor, {
  
  # browser()
  
  if(is.null(input$verify_division_save)){
    
    confirmSweetAlert(session = session,
                      inputId = "require_division_confirm",
                      title = "Please finalize ALL divisions first!",
                      type = "warning",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }else{
  
  if(is.null(input$division)) {
    
    confirmSweetAlert(session = session, 
                      inputId = "no_division",
                      title = "Please input division!",
                      # text = "If you haven't created the divisions, please do so.",
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
    
    comp_info$`First Name` <- as.character(comp_info$`First Name`)
    comp_info$`Last Name` <- as.character(comp_info$`Last Name`)
    comp_info$Division <- as.character(comp_info$Division)
    
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
    
    comp_info$`First Name` <- as.character(comp_info$`First Name`)
    comp_info$`Last Name` <- as.character(comp_info$`Last Name`)
    comp_info$Division <- as.character(comp_info$Division)
    
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
  
  output$competitor_table <- renderDataTable(comp_info, rownames = F, editable = T, selection = "none")
    }
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
      
      event_info$Event <- as.character(event_info$Event)
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
      event_info$Event <- as.character(event_info$Event)
      unfiltered_event_info <<- event_info %>% 
        filter(Event != "")
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
    
    output$event_table <- renderDataTable(event_info, rownames = F, editable = T, selection = "none")
    
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
  
  if(is.null(input$verify_division_save)) {
    
   
  }else{
    
    if(input$verify_division_save == F) {
      
    }else{

      updateSelectizeInput(session = session, inputId = "division",
                           choices = division_info$Division, selected = NULL)
      
      
    }
  }
  
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

observeEvent(input$event_table_cell_edit, {
  
  event_proxy <- dataTableProxy("event_table")
  
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
  
  event_edit_info = input$event_table_cell_edit
  # str(div_edit_info)
  i = event_edit_info$row
  # j = div_edit_info$col
  v = event_edit_info$value
  # browser()
  # div_as_character <- as.character(division_info[i, 1])
  # browser()
  event_info[i, 1] <<- coerceValue(v, as.character(event_info[i, 1]))
  replaceData(event_proxy, event_info, resetPaging = FALSE)
  
  # browser()
  
  event_info <- event_info %>% 
    distinct() %>% 
    filter(Event != "")
  
  unfiltered_event_info <<- event_info
  
  output$event_table <- renderDataTable(event_info, rownames = F, editable = T, selection = "none")
  
  # updateSelectizeInput(session = session, inputId = "division",
  #                      choices = division_info$Division, selected = NULL)
  
  if(input$event_table_cell_edit$value == "") {
    
    confirmSweetAlert(session = session, 
                      inputId = "remove_event_success", 
                      title = "Successfully removed event!",
                      type = "success",
                      btn_labels = "OK!", 
                      danger_mode = T)
    
  }else{
    
    confirmSweetAlert(session = session, 
                      inputId = "edit_event_success", 
                      title = "Successfully changed event!",
                      type = "success",
                      btn_labels = "OK!", 
                      danger_mode = T)
    
  }
  
})

observeEvent(input$competitor_table_cell_edit, {
  
  competitor_proxy <- dataTableProxy("competitor_table")
  
  # browser()
  
  # Add a conditional if the value is null, but otherwise,
  # you don't need to worry about a character being numeric,
  # so everything should work great. Push it across the board.
  
  competitor_edit_info = input$competitor_table_cell_edit
  # # str(div_edit_info)
  # i = competitor_edit_info$row
  # j = competitor_edit_info$col
  # v = competitor_edit_info$value
  # browser()
  # div_as_character <- as.character(division_info[i, 1])
  
  # if(competitor_edit_info$col == 2) {
  #   
  #   confirmSweetAlert(session = session,
  #                     inputId = "no_remove_division",
  #                     text = "Please do not remove division")
  #   
  # }
  
  if(input$competitor_table_cell_edit$value == "") {
    
    confirmSweetAlert(session = session,
                      inputId = "remove_entire_competitor_line",
                      title = "This will remove entire competitor row!",
                      text = "Are you sure you wish to proceed?",
                      btn_labels = c("No", "Yes"),
                      danger_mode = T)

  }else{

    comp_edit_info = input$competitor_table_cell_edit
    # str(comp_edit_info)
    i = comp_edit_info$row
    j = comp_edit_info$col
    v = comp_edit_info$value
    # comp_as_character <- as.character(division_info[i, j + 1])
    # division_info[i] <<- DT::coerceValue(div_as_character, div_as_character)
    # replaceData(division_proxy, division_info, resetPaging = FALSE)
    # 
    # # browser()
    # 
    # output$division_table <- renderDataTable(division_info, rownames =F, editable = T, selection = "none")
    # 
    # confirmSweetAlert(session = session,
    #                   inputId = "div_edit_empty",
    #                   title = "There is no value to change the division to!",
    #                   text = "If you would like to delete, switch to the removal tab!",
    #                   type = "warning",
    #                   btn_labels = "OK!",
    #                   danger_mode = T)

  # }else{
    
    if(j == 2) {
      
      comp_as_character <- as.character(comp_info[i, j + 1])
      comp_info[i, j + 1] <<- DT::coerceValue(comp_as_character, comp_as_character)
      replaceData(competitor_proxy, comp_info, resetPaging = FALSE)
      
      # browser()
      
      output$competitor_table <- renderDataTable(comp_info, rownames =F, editable = T, selection = "none")
      
      
      confirmSweetAlert(session = session, 
                        inputId = "division_edit_refuse",
                        title = "Please do not edit division!",
                        text = "If you want a different division, delete competitor and reinput information.",
                        type = "warning",
                        btn_labels = "OK!",
                        danger_mode = T)
      
    }else{
  
  comp_info[i, j + 1] <<- coerceValue(v, as.character(comp_info[i, j + 1]))
  replaceData(competitor_proxy, comp_info, resetPaging = FALSE)
  
  comp_info <- comp_info %>% 
    distinct() %>% 
    filter(`First Name` != "") %>% 
    filter(`Last Name` != "") %>% 
    filter(Division != "")
  
  unfiltered_comp_info <<- comp_info
  
  output$competitor_table <- renderDataTable(comp_info, rownames = F, editable = T, selection = "none")
  
  # if(input$competitor_table_cell_edit$value == "") {
  #   
  #   confirmSweetAlert(session = session, 
  #                     inputId = "remove_competitor_success", 
  #                     title = "Successfully removed event!",
  #                     type = "success",
  #                     btn_labels = "OK!", 
  #                     danger_mode = T)
  #   
  # }else{
    
    confirmSweetAlert(session = session, 
                      inputId = "edit_competitor_success", 
                      title = "Successfully changed competitor!",
                      type = "success",
                      btn_labels = "OK!", 
                      danger_mode = T)
    
  }
  }
})

observeEvent(input$remove_entire_competitor_line, {
  
  # browser()
  
  competitor_proxy <- dataTableProxy("competitor_table")
  
  comp_edit_info = input$competitor_table_cell_edit
  # str(comp_edit_info)
  i = comp_edit_info$row
  j = comp_edit_info$col
  v = comp_edit_info$value
  
  if(input$remove_entire_competitor_line == T){
    
    # if(j == 2)
    
    comp_info[i, j + 1] <<- coerceValue(v, as.character(comp_info[i, j + 1]))
    replaceData(competitor_proxy, comp_info, resetPaging = FALSE)
    
    comp_info <- comp_info %>% 
      distinct() %>% 
      filter(`First Name` != "") %>% 
      filter(`Last Name` != "") %>% 
      filter(Division != "")
    
    unfiltered_comp_info <<- comp_info
    
    output$competitor_table <- renderDataTable(comp_info, rownames = F, editable = T, selection = "none")
    
    confirmSweetAlert(session = session, 
                      inputId = "comp_remove_success",
                      title = "Competitor data successfully removed!", 
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }else{
    
    # browser()
    
    comp_as_character <- as.character(comp_info[i, j + 1])
    comp_info[i, j + 1] <<- DT::coerceValue(comp_as_character, comp_as_character)
    replaceData(competitor_proxy, comp_info, resetPaging = FALSE)

    # browser()

    output$competitor_table <- renderDataTable(comp_info, rownames =F, editable = T, selection = "none")

    confirmSweetAlert(session = session, 
                      inputId = "comp_same_success",
                      title = "Competitor data remained the same!",
                      text =  "Edits can still be made info is not correct",
                      type = "success",
                      btn_labels = "OK!",
                      danger_mode = T)
    
  }
  
})

observeEvent(input$division_save, {
  
 confirmSweetAlert(session = session, 
                   inputId = "verify_division_save",
                   title = "Do you want to finalize the divisions?",
                   text = "(You will not be able to make any additions or revisions)",
                   type = "info",
                   btn_labels = c("No", "Yes"),
                   danger_mode = T
                   )
  
})

observeEvent(input$verify_division_save, {

  # browser()

  if(input$verify_division_save == T) {

    # hideTab(inputId = "division_input_hide", target = input$division_input)
    # hideTab(inputId = "division_add_hide", target = input$division_add)
    output$division_table <- renderDataTable(division_info, rownames = F)

    # confirmSweetAlert(session = session,
    #                   inputId = "division_finalize_success",
    #                   title = "Divisions successfully finalized!",
    #                   text = "If you need to add / edit a division, please restart App.",
    #                   type = "success",
    #                   btn_labels = "OK!",
    #                   danger_mode = T)

  }

})

# observeEvent(!is.null(input$verify_division_save), {
#   
#   browser()
#   
# })

output$division_action_buttons <- renderUI({
  
  if(is.null(input$verify_division_save)) {
    
    tagList(
    
    column(9,
    textInput(inputId = "division_input", label = "Please Input Division Name"),
    actionButton(inputId = "division_add", label = "Add Division"),
    actionButton(inputId = "division_save", label = "Finalize Divisions")
    ))
    
  }else{
    
    if(input$verify_division_save == F) {
      
      tagList(
        
        column(9,
               textInput(inputId = "division_input", label = "Please Input Division Name"),
               actionButton(inputId = "division_add", label = "Add Division"),
               actionButton(inputId = "division_save", label = "Finalize Divisions")
        ))
      
    }else{
    
    tagList(
    h3("All Competition divisions:")
    )
    
    # confirmSweetAlert(session = session,
    #                   inputId = "notify_division_stop",
    #                   title = "All divisions have been finalized!",
    #                   text = "If you need to edit or add a division, please restart the app.",
    #                   type = "success",
    #                   btn_labels = "OK!",
    #                   danger_mode = T)
    }
  }
  
})

output$event_action_buttons <- renderUI({
  
  if(is.null(input$verify_event_save)) {
    
    tagList(
      
      column(9,
             textInput(inputId = "event_input", label = "Please Input Event Name"),
             actionButton(inputId = "event_add", label = "Add Event"),
             actionButton(inputId = "event_save", label = "Finalize Events")
      ))
    
  }else{
    
    if(input$verify_event_save == F) {
      
      tagList(
        
        column(9,
               textInput(inputId = "event_input", label = "Please Input Event Name"),
               actionButton(inputId = "event_add", label = "Add Event"),
               actionButton(inputId = "event_save", label = "Finalize Events")
        ))
      
    }else{
      
      tagList(
        h3("All Competition events:")
      )
      
  }
  }
  
})

observeEvent(input$event_save, {
  
  confirmSweetAlert(session = session, 
                    inputId = "verify_event_save",
                    title = "Do you want to finalize the events?",
                    text = "(You will not be able to make any additions or revisions)",
                    type = "info",
                    btn_labels = c("No", "Yes"),
                    danger_mode = T
  )
  
})

observeEvent(input$verify_event_save, {
  
  if(input$verify_event_save == T) {
    
    output$event_table <- renderDataTable(event_info, rownames = F)
  
    # browser()
     
  }
  
})

 
}

shinyApp(ui = ui, server = server)