library(shiny)
library(shinyforms)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(htmlTable)
# Define the first form: basic information
# basicInfoForm <- list(
#   id = "basicinfo",
#   questions = list(
#     list(id = "name", type = "text", title = "Name", mandatory = TRUE,
#          hint = "Your name exactly as it is shown on your passport"),
#     list(id = "age", type = "numeric", title = "Age", mandatory = FALSE),
#     list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
#     list(id = "terms", type = "checkbox", title = "I agree to the terms")
#   ),
  # storage = list(
  #   type = STORAGE_TYPES$FLATFILE,
  #   path = "responses"
  # ),
#   name = "Personal info",
#   password = "shinyforms",
#   reset = TRUE,
#   validations = list(
#     list(condition = "nchar(input$name) >= 3",
#          message = "Name must be at least 3 characters"),
#     list(condition = "input$terms == TRUE",
#          message = "You must agree to the terms")
#   )
# )

# Define the second form: soccer
# soccerFormInfo <- list(
#   id = "soccerform",
#   questions = list(
#     list(id = "team", type = "text", title = "Favourite soccer team"),
#     list(id = "player", type = "text", title = "Favourite player")
#   ),
#   storage = list(
#     type = STORAGE_TYPES$FLATFILE,
#     path = "soccer"
#   ),
#   multiple = FALSE
# )

create_html_table <- function(data_row){
  as.character(htmlTable(
    data_row,
    css.cell = rbind(rep("background: lightgrey; padding-left: .5em; padding-right: .2em;", 
                         times = ncol(data_row)),
                     matrix("", 
                            ncol = ncol(data_row), 
                            nrow = 1)))
  )
}


ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("yeti"),
  h1("Anotacje 0.0.1"),
  tabsetPanel(
    id = "tabset_panel",
    tabPanel(
      "Start",
      div(style = "margin: 25px 0px 0px 0px;", 
        sidebarPanel(
          width = 3,
          h5(tags$b("Aplikacja służy do pozyskiwania uzasadnień ceny danego telefonu. 
                    Prosimy o odpowiedzenie na wszystkie pytania, a następnie zatwierdzenie formularza")),
          h5("Aplikacja wykonana w ramach pracy inżynierskiej przez Bartłomieja Granata i Szymona Maksymiuka")#,
        ),
        
        mainPanel(
          width = 2,
          numericInput("sample_size", 
                       "Rozmiar próbki danych", 
                       value = 10, 
                       min = 20, 
                       max = 200),
          numericInput("questions", 
                       "Liczba pytań", 
                       value = 3, 
                       min = 1, 
                       max = 3),
          div(style = "margin: 15px 0px 0px 0px;",
              actionButton("data_browse", 
                           "Podgląd danych", 
                           width = 150)),
          div(style = "margin: 15px 0px 0px 0px;",
              actionButton("next_page", 
                           "Przejdź do pytań",
                           width = 150)),
          div(style = "margin: 15px 0px 0px 0px;",
              actionButton("confirm", 
                           "Confirm",
                           width = 150))
        )
      )
    ),
    tabPanel(
      "Data",
      DT::dataTableOutput("Data"),
      div(style = "margin: 15px 0px 0px 0px;",
          actionButton("next_page_data", 
                       "Przejdź do pytań",
                       width = 150))
      ),
    tabPanel(
      "1st Question",
      uiOutput("st_question")
    ),
    tabPanel(
      "2nd Question",
      uiOutput("nd_question")
    ),
    tabPanel(
      "3rd Question",
      uiOutput("rd_question")
    )
  )
)

server <- function(input, output, session) {
  
  
  data <- read.csv("phones.csv")
  
  
  data_sample <- reactive({
    m <- sample(1:nrow(data), input$sample_size)
    ret <- data[m,]
    rownames(ret) <- NULL
    ret
  })

  
  
  observeEvent(input$data_browse, {
    updateTabsetPanel(session, "tabset_panel", selected = "Data")
  })
  
  observeEvent(input$next_page, {
    updateTabsetPanel(session, "tabset_panel",selected = "1st Question")
  })
  
  observeEvent(input$next_page_data, {
    updateTabsetPanel(session, "tabset_panel",selected = "1st Question")
  })
  

  
  output$Data <- DT::renderDataTable({
    DT::datatable(data_sample())
  })
  
  
  form_data_st <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "st_Question"
    questions <- lapply(1:n, function(x){
      list(id = as.character(x), 
           type = "text" , 
           title = paste(x, "Name the most important feature", sep = " "), 
           hint = HTML(create_html_table(data[x,])))
    })
    storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
    list(id = id, questions = questions, storage = storage)
  })
 
  form_data_nd <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "nd_Question"
    questions <- lapply(1:n, function(x){
      list(id = as.character(x), 
           type = "text" , 
           title = paste(x, "Name the most important feature DRUGIE", sep = " "), 
           hint = HTML(create_html_table(data[x,])))
    })
    storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
    list(id = id, questions = questions, storage = storage)
  })
  
  
  
  output$st_question <- renderUI({
    formUI(form_data_st())
  })

  output$nd_question <- renderUI({
    formUI(form_data_nd())
  })
  
  
  callModule(shinyforms:::formServerHelper, "st_Question", form_data_st)
  callModule(shinyforms:::formServerHelper, "nd_Question", form_data_nd)
  
  
  # formServer({
  #   data <- data_sample %>% mutate_if(is.factor, as.character)
  #   n <- nrow(data)
  #   id <- "st_Question"
  #   questions <- lapply(1:n, function(x){
  #     list(id = as.character(x), 
  #          type = "text" , 
  #          title = paste(x, "Name the most important feature", sep = " "), 
  #          hint = HTML(create_html_table(data[x,])))
  #   })
  #   storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
  #   question_data <- list(id = id, questions = questions, storage = storage)
  #   question_data
  #   
  # })
}



shinyApp(ui = ui, server = server)