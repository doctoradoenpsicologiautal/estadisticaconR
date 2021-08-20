library(shiny)
library(shinyjs)

fieldsMandatory <- c("q1", "q2", "q3", "q4")

appCSS <- "
    .mandatory_star { color: red; }
    #error { color: red; }
"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsAll <- c("q1", "q2", "q3", "q4")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
  titlePanel("Capítulo 1a: ¿Para que nos sirve la estadística?"),
  
  div(id = "form",
      textAreaInput("q1", labelMandatory("¿Que es la estadística descriptiva? ¿Que es la estadística inferencial?"), width = "100%", rows = 6),
      textAreaInput("q2", labelMandatory("¿Qué es una población y una muestra? ¿Qué son los parámetros y los estadísticos?"), width = "100%", rows = 6),
      textAreaInput("q3", labelMandatory("¿Por qué necesitamos obtener una muestra para nuestros experimentos?"), width = "100%", rows = 6),
      textAreaInput("q4", labelMandatory("¿Por qué necesitamos las probabilidades para análisis estadísticos?"), width = "100%", rows = 6),
      
      actionButton("submit", "Submit", class = "btn-primary"),
      
      shinyjs::hidden(
        span(id = "submit_msg", "Submitting..."),
        div(id = "error",
            div(br(), tags$b("Error: "), span(id = "error_msg"))
        )
      )
  ),
  
  shinyjs::hidden(
    div(
      id = "final_msg",
      h3("Thanks, your response was submitted successfully!")
    )
  )  
)

server <- function(input, output) {
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })    
  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::hide("form")
      shinyjs::show("final_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
