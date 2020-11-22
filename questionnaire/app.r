library(shiny)
library(shinyforms)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(htmlTable)
library(shinyjqui)
library(googlesheets4)

gs4_auth(
  cache = ".secrets",
  email = "inzynierkaszymonbartek@gmail.com"
)

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
  )
}




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
                            value = 5,
                            min = 5,
                            max = 200,
                            width = 125),
  
  questions_number = numericInput("questions",
                                  "Questions number",
                                  value = 3,
                                  min = 1,
                                  max = 3,
                                  width = 125),
  domain_knowledge = radioButtons("domain",
                                  "Describe your knowledge about phones market",
                                  choices = c("Expert", "Experianced", "Moderate", "Newbie", "Hardly any"),
                                  inline = FALSE),
  last_phone = radioButtons("last_phone",
                            "When have you bought your current phone?",
                            choices = c("More than 2 years ago", "Between 2 years and 1 year ago", "Between 1 years and 6 months ago", "Less than 6 months ago")),
  xai_knowledge = radioButtons("xai",
                                  "Describe your knowledge about Machine Learning",
                                  choices = c("Expert", "Experianced", "Moderate", "Newbie", "Hardly any"),
                               inline = FALSE),
  
  
  data_browser = DT::dataTableOutput("Data"),
  
  questions = uiOutput("MainAction"),
  
  ccounter = actionButton("Click.Counter", "Next")
  
)


server <- function(input, output) {
  
  data <- read.csv("phones.csv")

  m <- reactive({
    sample(1:nrow(data), input$sample_size)
  })
  
  data_sample <- reactive({
    ret <- data[m(),]
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
    
    observeEvent(input$Click.Counter, {
      shinyjs::disable("sample_size")
      shinyjs::disable("questions")
      shinyjs::disable("domain")
      shinyjs::disable("last_phone")
      shinyjs::disable("xai")
    })
    
    if (input$Click.Counter==1){
      return(
        list(
          h5(paste("Type 1/", input$questions ,": Looking at the data above please name 3 features affecting price of a telepohone the most", sep = "")),
          renderUI({
            formUI(form_data_nd())
          })
       )
      )

    }

    if (input$Click.Counter==2 & input$questions > 1){
      return(
        list(
          h5(paste("Type 2/", input$questions ,": Looking at the data above please name 3 features affecting price of a telepohone the most", sep = "")),
          renderUI({
            formUI(form_data_st())
          })
        )
      )
    }

    if (input$Click.Counter==3 & input$questions > 2){
      return(
        list(
          h5(paste("Type 3/", input$questions ,": Looking at the data above please name 3 features affecting price of a telepohone the most", sep = "")),
          renderUI({
            formUI(form_data_rd())
          })
        )
      )
    }

    if (input$Click.Counter==4 | input$Click.Counter==3 & input$questions == 2 | input$Click.Counter==2 & input$questions == 1){
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
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 2,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m(), collapse = "-"))
    list(id = id, questions = questions, storage = storage)
  })

  form_data_nd <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "nd_Question"
    questions <- lapply(1:1, function(x){
      list(id = paste("nd", as.character(x), sep = "nd"),
           type = "checkbox_multi" ,
           title = "",
           hint = "Mark the features",
           choices = as.list(colnames(data)),
           inline = TRUE)
    })
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 1,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m(), collapse = "-"))
    list(id = id, questions = questions, storage = storage)
  })

  form_data_rd <- reactive({
    data <- data_sample() %>% mutate_if(is.factor, as.character)
    n <- nrow(data)
    id <- "rd_Question"
    sample_short <- sample(1:n, floor(n*0.5))
    questions <- list()
    for (i in sample_short) {
      tmp <- lapply(2:(ncol(data)-1), function(x){
        list(id = paste("rd", as.character(x-1), i, sep = "_"),
             type = "radio" ,
             title = paste(x, "Variable", colnames(data)[x], "contributes to price: ", sep = " "),
             hint = HTML(create_html_table(data[i,])),
             choices = list("Highly positive", "Slightly positive", "Neutral", "Slightly negative", "Highly negative"),
             inline = TRUE)
      })  
      questions <- c(questions, tmp)
    }
    
    storage <- list(type = STORAGE_TYPES$GOOGLE_SHEETS, key = "1xw1R799ylk8Xua7nGiLEZHr8b6qMLXEPSP_m-GgWJmQ", sheet = 3,
                    domain_knowledge = input$domain, xai_knowledge = input$xai,
                    last_phone = input$last_phone, sample = paste0(m()[sample_short], collapse = "-"))
    list(id = id, questions = questions, storage = storage)
  })


  output$rd_question <- renderUI({
    formUI(form_data_rd())
  })


  callModule(shinyforms:::formServerHelper, "st_Question", form_data_st)
  callModule(shinyforms:::formServerHelper, "nd_Question", form_data_nd)
  callModule(shinyforms:::formServerHelper, "rd_Question", form_data_rd)
  
}





shinyApp(ui = ui, server = server)