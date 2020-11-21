library(shiny)
library(shinyforms)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(htmlTable)
library(shinyjqui)
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
    css.cell = rbind(rep("border-collapse: collapse;
                          margin: 25px 0;
                          font-size: 0.9em;
                          padding-left: .5em; 
                          padding-right: .5em;
                          font-family: sans-serif;
                          box-shadow: 0 0 5px rgba(0, 0, 0, 0.15);
                         ",
                         times = ncol(data_row)),
                     rep("border-collapse: collapse;
                          margin: 25px 0;
                          font-size: 0.9em;
                          padding-left: .5em; 
                          padding-right: .5em;
                          font-family: sans-serif;
                          ",
                         times = ncol(data_row))))
    # css.cell = rbind(rep("background: lightgrey; padding-left: .5em; padding-right: .2em;",
    #                      times = ncol(data_row)),
    #                  matrix("",
    #                         ncol = ncol(data_row),
    #                         nrow = 1)))
  )
}


# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   theme = "bootstrap.css",
#   h1("Annotations 0.0.1"),
#   tabsetPanel(
#     id = "tabset_panel",
#     tabPanel(
#       "Home",
#       div(style = "width: 600px; margin: 25px 0px 0px 0px;", 
#         
#         sidebarPanel(
#           width = 5,
#           h5(tags$b("Apllication's purpose is to acquire human explanations of phone prices by responding to 3 types of questions")),
#           h5("Application made as a part of a diploma by Bartłomiej Granat and Szymon Maksymiuk")#,
#         ),
#         
#         mainPanel(
#           width = 7,
#           h5(tags$b('Both sample size and questions number determine time required to fill the questionnaire.
#              For each additional 5 phones You will need 3-5 more minutes')),
#           splitLayout(
#             numericInput("sample_size", 
#                          "Sample size", 
#                          value = 10, 
#                          min = 5, 
#                          max = 200,
#                          width = 125),
#             numericInput("questions", 
#                          "Questions number", 
#                          value = 3, 
#                          min = 1, 
#                          max = 3,
#                          width = 125)
#           ),
#           div(style = "margin: 15px 0px 0px 0px;",
#               actionButton("data_browse", 
#                            "Browse sample", 
#                            width = 300)),
#           div(style = "margin: 15px 0px 0px 0px;",
#               actionButton("next_page", 
#                            "Answer questions",
#                            width = 300))
#         )
#       )
#     ),
#     tabPanel(
#       "Data",
#       DT::dataTableOutput("Data"),
#       div(style = "margin: 15px 0px 0px 0px;",
#           actionButton("next_page_data", 
#                        "Answer questions",
#                        width = 150))
#       ),
#     tabPanel(
#       "Questions",
#       uiOutput("MainAction"),
#       actionButton("Click.Counter", "Next")  
#     )
#     # tabPanel(
#     #   "2nd Question",
#     #   uiOutput("nd_question")
#     # ),
#     # tabPanel(
#     #   "3rd Question",
#     #   uiOutput("rd_question")
#     # )
#   )
# )


ui <- shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Number of trips
  number_of_trips = textOutput(
    "num_trips",
    inline = T
  ),
  
  sample_size = numericInput("sample_size", 
                            "Sample size",
                            value = 10,
                            min = 5,
                            max = 200,
                            width = 125),
  
  questions_number = numericInput("questions",
                                  "Questions number",
                                  value = 3,
                                  min = 1,
                                  max = 3,
                                  width = 125),
  
  data_browser = DT::dataTableOutput("Data"),
  
  questions = uiOutput("MainAction"),
  
  ccounter = actionButton("Click.Counter", "Next")
  
)


server <- function(input, output) {
  
  # Basic Numbers Page --------------------------------------------------------------
  
  data <- read.csv("phones.csv")


  data_sample <- reactive({
    m <- sample(1:nrow(data), input$sample_size)
    ret <- data[m,]
    rownames(ret) <- NULL
    ret
  })
  
  output$Data <- DT::renderDataTable({
    DT::datatable(data_sample())
  })
  
  output$MainAction <- renderUI( {
    dynamicUi()
  })

  observeEvent(input$close, {
    stopApp()
  })

  dynamicUi <- reactive({
    if (input$Click.Counter==0)
      return(
        list(
          h5("Please fill all of the following questions and remember to SUBMIT your answers")
        )
      )

    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter==1){
      # shinyjs::disable("Click.Counter")
      return(
        list(
          h5("Type 1/3: Looking at the data above please name 3 features affecting price of a telepohone the most"),
          renderUI({
            formUI(form_data_nd())
          })
        )
      )

    }

    if (input$Click.Counter==2){
      #shinyjs::disable("Click.Counter")
      return(
        list(
          h5("Type 2/3: For selected number of telephones please drag and drop the features so they are ordered from most to least important when you look at the telephone price"),
          renderUI({
            formUI(form_data_st())
          })
        )
      )
    }

    if (input$Click.Counter==3){
      #shinyjs::disable("Click.Counter")
      return(
        list(
          h5("Type 3/3: For one random telephone mark the type of impact on price of each single feature"),
          renderUI({
            formUI(form_data_rd())
          })
        )
      )
    }

    if (input$Click.Counter==4){
      shinyjs::disable("Click.Counter")
      return(
        list(
          h5("That is everything we prepared for You. Thank You for participation"),
          actionButton("close", "Close")
        )
      )
    }

  })

  form_data_st <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "st_Question"

    questions <- lapply(1:n, function(x){
      list(id = paste("st", as.character(x), sep = "_"),
           type = "order" ,
           title = paste0("Observation: ", as.character(x)),
           hint = HTML(create_html_table(data[x,])),
           choices = colnames(data))
    })
    storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
    list(id = id, questions = questions, storage = storage)
  })

  form_data_nd <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "nd_Question"
    questions <- lapply(1:1, function(x){
      list(id = paste("nd", as.character(x), sep = "nd"),
           type = "checkbox_multi" ,
           title = "Mark the features",
           hint = "",
           choices = as.list(colnames(data)),
           inline = TRUE)
    })
    storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
    list(id = id, questions = questions, storage = storage)
  })

  form_data_rd <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "rd_Question"
    questions <- lapply(1:n, function(x){
      list(id = paste("rd", as.character(x), sep = "rd"),
           type = "radio" ,
           title = paste(x, "Variable", colnames(data)[sample(1:ncol(data), 1)], "contributes to price: ", sep = " "),
           hint = HTML(create_html_table(data[x,])),
           choices = list("Highly positive", "Slightly positive", "Neutral", "Slightly negative", "Highly negative"),
           inline = TRUE)
    })
    storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
    list(id = id, questions = questions, storage = storage)
  })


  #
  # output$st_question <- renderUI({
  #   formUI(form_data_st())
  # })
  #
  # output$nd_question <- renderUI({
  #   formUI(form_data_nd())
  # })

  output$rd_question <- renderUI({
    formUI(form_data_rd())
  })


  callModule(shinyforms:::formServerHelper, "st_Question", form_data_st)
  callModule(shinyforms:::formServerHelper, "nd_Question", form_data_nd)
  callModule(shinyforms:::formServerHelper, "rd_Question", form_data_rd)
  
}


# server <- function(input, output, session) {
#   
#   
#   data <- read.csv("phones.csv")
#   
#   
#   data_sample <- reactive({
#     m <- sample(1:nrow(data), input$sample_size)
#     ret <- data[m,]
#     rownames(ret) <- NULL
#     ret
#   })
# 
#   
#   
#   observeEvent(input$data_browse, {
#     updateTabsetPanel(session, "tabset_panel", selected = "Data")
#   })
#   
#   observeEvent(input$next_page, {
#     updateTabsetPanel(session, "tabset_panel",selected = "Questions")
#   })
#   
#   observeEvent(input$next_page_data, {
#     updateTabsetPanel(session, "tabset_panel",selected = "Questions")
#   })
#   
#   observeEvent(input$close, {
#     stopApp()
#   })
# 
#   
#   output$Data <- DT::renderDataTable({
#     DT::datatable(data_sample())
#   })
#   # 
#   # observeEvent(input$Click.Counter, { 
#   #   # print(input$Click.Counter[1])
#   #   #shinyjs::disable("Click.Counter")
#   #   if(input$Click.Counter > 1 & input$Click.Counter <= 4){
#   #     
#   #     print(input$Click.Counter[1])
#   #     
#   #   } else if(input$Click.Counter > 5){
#   #     print("Finished with covariates")
#   #   } else {
#   #     print("Initializing...")
#   #   }
#   # })
#   # 
#   output$MainAction <- renderUI( {
#     dynamicUi()
#   })
#   
#   
#   
#   dynamicUi <- reactive({
#     if (input$Click.Counter==0)
#       return(
#         list(
#           h5("Thank you for participating in our project. Please fill all of the following questions and submit your answers")
#         )
#       )
#     
#     # Once the next button has been clicked once we see each question
#     # of the survey.
#     if (input$Click.Counter==1){
#       # shinyjs::disable("Click.Counter")
#       return(
#         list(
#           h5("In this part please mark 3 most important features that determine phone price based on the whole sample"),
#           renderUI({
#             formUI(form_data_st())
#           })
#         )
#       )
#       
#     }
#     
#     if (input$Click.Counter==2){
#       #shinyjs::disable("Click.Counter")
#       return(
#         list(
#           h5("In this part please mark the impact of each single feature on the price"),
#           renderUI({
#             formUI(form_data_nd())
#           })
#         )
#       )
#     }
#     
#     if (input$Click.Counter==3){
#       #shinyjs::disable("Click.Counter")
#       return(
#         list(
#           h5("In this part please mark the impact of each single feature on the price"),
#           renderUI({
#             formUI(form_data_rd())
#           })
#         )
#       )
#     }
#     
#     if (input$Click.Counter==4){
#       shinyjs::disable("Click.Counter")
#       return(
#         list(
#           h5("That is everything we prepared for You. Thank You for participation"),
#           actionButton("close", "Close")
#         )
#       )
#     }
#     
#   })
#   
#   form_data_st <- reactive({
#     data <- data_sample() %>% mutate_if(is.factor, as.character)
#     n <- nrow(data)
#     id <- "st_Question"
#     
#     questions <- lapply(1:n, function(x){
#       list(id = paste("st", as.character(x), sep = "_"), 
#            type = "order" , 
#            title = paste(x, "Order features according to its importance for a given observation", sep = " "), 
#            hint = HTML(create_html_table(data[x,])),
#            choices = colnames(data))
#     })
#     storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
#     list(id = id, questions = questions, storage = storage)
#   })
#  
#   form_data_nd <- reactive({
#     data <- data_sample() %>% mutate_if(is.factor, as.character)
#     n <- nrow(data)
#     id <- "nd_Question"
#     questions <- lapply(1:n, function(x){
#       list(id = paste("nd", as.character(x), sep = "nd"), 
#            type = "checkbox_multi" , 
#            title = paste(x, "Name 3 most important features", sep = " "), 
#            hint = HTML(create_html_table(data[x,])),
#            choices = as.list(colnames(data)),
#            inline = TRUE)
#     })
#     storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
#     list(id = id, questions = questions, storage = storage)
#   })
#   
#   form_data_rd <- reactive({
#     data <- data_sample() %>% mutate_if(is.factor, as.character)
#     n <- nrow(data)
#     id <- "rd_Question"
#     questions <- lapply(1:n, function(x){
#       list(id = paste("rd", as.character(x), sep = "rd"), 
#            type = "radio" , 
#            title = paste(x, "Variable", colnames(data)[sample(1:ncol(data), 1)], "contributes to price: ", sep = " "), 
#            hint = HTML(create_html_table(data[x,])),
#            choices = list("Highly positive", "Slightly positive", "Neutral", "Slightly negative", "Highly negative"),
#            inline = TRUE)
#     })
#     storage <- list(type = STORAGE_TYPES$FLATFILE, path = "responses")
#     list(id = id, questions = questions, storage = storage)
#   })
#   
#   
#   # 
#   # output$st_question <- renderUI({
#   #   formUI(form_data_st())
#   # })
#   # 
#   # output$nd_question <- renderUI({
#   #   formUI(form_data_nd())
#   # })
#   
#   output$rd_question <- renderUI({
#     formUI(form_data_rd())
#   })
#   
#   
#   callModule(shinyforms:::formServerHelper, "st_Question", form_data_st)
#   callModule(shinyforms:::formServerHelper, "nd_Question", form_data_nd)
#   callModule(shinyforms:::formServerHelper, "rd_Question", form_data_rd)
#   
#   
# }



shinyApp(ui = ui, server = server)