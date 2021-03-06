library(shiny)
library(shinyforms)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(htmlTable)
library(shinyjqui)
library(googlesheets4)
library(shinyalert)

gs4_auth(
  cache = ".secrets",
  email = "inzynierkaszymonbartek@gmail.com"
)

# functions used internally to render obesrvations above questions
create_html_table <- function(data_row){
  data_row <- as.matrix(data_row)
  rownames(data_row) <- NULL
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
  )
}

# defining objects to use in html template
ui <- shiny::htmlTemplate(
  # Index Page
  "www/index.html",
 
  # define nick input
  nick = textInput(
    "nick",
    "Your nick",
    width = 500
  ),
  # define questions for personal form
  domain_knowledge = radioButtons("domain",
                                  "Describe your knowledge about phones market",
                                  choices = c("Professional", 
                                              "Advanced", 
                                              "Intermediate", 
                                              "Novice", 
                                              "Fundamental"),
                                  inline = FALSE,
                                  selected = "Intermediate"),
  last_phone = radioButtons("last_phone",
                            "When have you bought your current phone?",
                            choices = c("More than 2 years ago",
                                        "Between 2 years and 1 year ago", 
                                        "Between 1 years and 6 months ago", 
                                        "Less than 6 months ago"),
                            selected = "Between 1 years and 6 months ago"),
  xai_knowledge = radioButtons("xai",
                                  "Describe your knowledge about Machine Learning",
                                  choices = c("Professional", 
                                              "Advanced", 
                                              "Intermediate", 
                                              "Novice", 
                                              "Fundamental"),
                               inline = FALSE,
                               selected = "Intermediate"),
  
  # define interactive data browser
  data_browser = DT::dataTableOutput("Data"),
  # define dynamic questions section
  questions = uiOutput("MainAction"),
  # define next question button for navigation
  ccounter = actionButton("Click.Counter", "Next question")
  
)


server <- function(input, output) {
  
  # INPUT DATA
  data <- read.csv("../data/phones/phones.csv") %>% 
    mutate(resolution_Mpx = round(height_px*width_px/1000000, 2)) %>%
    select(name, brand, back_camera_mpix, front_camera_mpix, battery_mAh, flash_gb, ram_gb, diag, resolution_Mpx, price)
  # load tooltips for interactive data browser
  tooltips <- read.csv('../data/phones/tooltips.csv', header = F)[1,]
  # choose indices for random sample
  m <- reactive({
    ret <- sample(1:nrow(data), 8)
    ret[order(data$price[ret], decreasing = TRUE)]
  })
  # create random sample
  data_sample <- reactive({
    ret <- data[m(),]
    ret <- ret[order(ret$price, decreasing = TRUE),]
    rownames(ret) <- NULL
    ret
  })
  # render dynamic sample browser
  output$Data <- DT::renderDataTable({
    DT::datatable(data_sample(),  
                  rownames = FALSE,
                  options = list(searching = FALSE, paging = FALSE),
                  callback = JS(paste0("
                                        var tips = [", paste0(paste0("'",as.character(t(tooltips)),"'"),  collapse = ","),
                                               "],
                                            header = table.columns().header();
                                        for (var i = 0; i < tips.length; i++) {
                                          $(header[i]).attr('title', tips[i]);
                                        }
                                        ")))
  })
  

  # define dynamic ui for questions
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  # define popup on first click
  observeEvent(input$Click.Counter, {
    if(input$Click.Counter==1){
      shinyalert("Important information!",
                 "SUBMIT each question type SEPARATELY with 'Submit answers' button. Clicking 'Next question' before submitting will skip it without sending your answers.", 
                 type = "info")
    }
  })
  # close app on click
  observeEvent(input$close, {
    stopApp()
  })
  # render initial questions screen
  dynamicUi <- reactive({
    if (input$Click.Counter==0)
      return(
        list(
          h5("Please fill all of the following questions and SUBMIT each type of question SEPARATELY")
        )
      )

    # Once the next button has been clicked we see each question
    # of the survey.
    
    observeEvent(input$Click.Counter, {
      shinyjs::disable("domain")
      shinyjs::disable("last_phone")
      shinyjs::disable("xai")
      shinyjs::disable("nick")
    })
    
    # render first question 
    if (input$Click.Counter==1){
      return(
        list(
          h5(paste("Type 1/", 3 ,": Looking at the data above please name 3 features affecting price of a telephone the most", sep = "")),
          renderUI({
            formUI(form_data_nd())
          })
       )
      )

    }
    # render second question
    if (input$Click.Counter==2){
      return(
        list(
          h5(paste("Type 2/", 3 ,": Looking at each telephone separately please use DRAG & DROP functionality to order features from most to least important for the specific phone", sep = "")),
          renderUI({
            formUI(form_data_st())
          })
        )
      )
    }
    # render third question
    if (input$Click.Counter==3){
      shinyjs::disable("Click.Counter")
      shinyjs::hide("Click.Counter")
      return(
        list(
          h5(paste("Type 3/", 3 ,": Looking at each telephone separately please mark what is the impact of the specific variables' values for each phone", sep = "")),
          renderUI({
            formUI(form_data_rd())
          })
        )
      )
    }
    # render questions end screen
    if (input$Click.Counter==4){
      shinyjs::disable("Click.Counter")
      shinyjs::hide("Click.Counter")
      return(
        list(
          h5("That is everything we prepared for You. Thank You for participation"),
          actionButton("close", "Close")
        )
      )
    }

  })
  # define second question content
  form_data_st <- reactive({
    validate(need(
      input$nick != "",
      "Nick has to be passed"
    ))
    data <- data_sample() %>% 
      mutate_if(is.factor, as.character) 
    n <- nrow(data)
    id <- "st_Question"
    sample_middle <- sort(sample(1:n, 4), decreasing = FALSE)
    questions <- lapply(sample_middle, function(x){
      list(id = paste("st", as.character(x), sep = "_"),
           type = "order" ,
           hint = HTML(create_html_table(data[x,])),
           choices = sample(colnames(data)[2:(ncol(data)-1)]),
           inline = TRUE)
    })
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 2,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m()[sample_middle], collapse = "-"),
                    email_address = input$nick)
    list(id = id, questions = questions, storage = storage)
  })
  # define first question content
  form_data_nd <- reactive({
    validate(need(
      input$nick != "",
      "Nick has to be passed"
    ))
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "nd_Question"
    questions <- lapply(1:1, function(x){
      list(id = paste("nd", as.character(x), sep = "nd"),
           type = "checkbox_multi" ,
           title = "",
           hint = "Mark the features",
           choices = as.list(colnames(data)[2:(ncol(data)-1)]),
           inline = TRUE)
    })
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 1,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m(), collapse = "-"),
                    email_address = input$nick)
    list(id = id, questions = questions, storage = storage)
  })
  # define third question content
  form_data_rd <- reactive({
    validate(need(
      input$nick != "",
      "Nick has to be passed"
    ))
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "rd_Question"
    sample_short <- sort(sample(1:n, 2), decreasing = FALSE)
    questions <- list()
    for (i in sample_short) {
      tmp <- lapply(2:(ncol(data)-1), function(x){
        if (x == 2) {
          list(id = paste("rd", as.character(x-1), i, sep = "_"),
               type = "radio" ,
               title = HTML(create_html_table(data[i,])),
               hint = paste("Variable", colnames(data)[x], "contributes to price: ", sep = " "),
               choices = list("Highly increases", "Slightly increases", "Neutral", "Slightly decreases", "Highly decreases"),
               inline = TRUE,
               selected = "Neutral")
        } else {
          list(id = paste("rd", as.character(x-1), i, sep = "_"),
               type = "radio" ,
               title = "",
               hint = paste("Variable", colnames(data)[x], "contributes to price: ", sep = " "),
               choices = list("Highly increases", "Slightly increases", "Neutral", "Slightly decreases", "Highly decreases"),
               inline = TRUE,
               selected = "Neutral")
        }
     })  
      questions <- c(questions, tmp)
    }
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 3,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m()[sample_short], collapse = "-"),
                    email_address = input$nick)
    list(id = id, questions = questions, storage = storage)
  })

  # render all questions
  output$rd_question <- renderUI({
    formUI(form_data_rd())
  })


  callModule(shinyforms:::formServerHelper, "st_Question", form_data_st)
  callModule(shinyforms:::formServerHelper, "nd_Question", form_data_nd)
  callModule(shinyforms:::formServerHelper, "rd_Question", form_data_rd)
  
}





shinyApp(ui = ui, server = server)