#devtools::install_github("arreen/wiki", build_vignettes = TRUE)
#install.packages("shiny")
library(Wiki)
library(shiny)
library(stringr)

ui <- fluidPage(
  fluidRow(column(6,titlePanel("Fewest clicks to Poland"))),
  fluidRow(column(2, actionButton("back", "Previous article"))), #column(2, actionButton("next", "Next article")) ),
  fluidRow(p("")),
  
  
  # Styling
  tags$head(
    tags$style(HTML("
      .scrollable-table {
        max-height: 500px;
        overflow-y: auto;
        border: 1px solid #ccc;
        padding: 10px;
        background-color: #f9f9f9;
      }
      .well-section {
        margin-bottom: 20px;
      }
    "))
  ),
  
  fluidRow(
    column(4,
           div(class = "well-section",
               selectInput(inputId = "lan", label = "Choose a language",
                           choices = c("English", "Polish", "Swedish"))
           ), column(10,
                     uiOutput("dynamic_buttons"),
           )
    ),
    column(6,
           h3(textOutput("current_article")),
           h4("Abstract"),
           div(class = "scrollable-table",
               textOutput("predictions")
           )
    )
  )
  
)


server <- function(input, output) {
  
  #init_article <- sample(Wiki::women_in_statistics, 1)
  
  init_article <- sample(Wiki::women_in_statistics, 1)
  
  current_article <- reactiveVal(init_article)
  
  
  history <- reactiveVal(init_article)
  
  
  observeEvent(input$back, {
    
    if (current_article() == init_article) {
      
    } else{
      
      
      hist <- str_split(history(), "SEParATOR", simplify = TRUE)
      hist <- hist[-length(hist)]
      history(str_c(hist, sep = "SEParATOR", collapse = "SEParATOR"))
      current_article(hist[length(hist)])
    }
    
    
  })
  
  
  output$current_article <- renderText({
    
    if (current_article() == "Poland") {
      paste("You found Poland in", length(str_split(history(), "SEParATOR")), "clicks!")
    } else{current_article()}
    
    
    
    
  })
  
  api_data <- reactive({
    
    if (current_article() == "Poland") {
      "Win"
    } else {
      Wiki::api_wiki_data( current_article = current_article() )
    }
    
    
  })
  
  
  
  abstract <- reactive({
    
    langs <- c(pl = "Polish", en = "English", sv = "Swedish")
    
    if (current_article() == "Poland") {
      " ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
    ⠀⠀⠀⠀⠀⠀⠀⢀⣀⣤⣴⣶⣿⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
    ⠀⢀⣀⣤⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀
    ⠀⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠀⠀
    ⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠀⠀
    ⠀⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀
    ⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣋⠁⠀⠀
    ⠀⠀⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀
    ⠀⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀
    ⠀⠀⠀⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⠀
    ⠀⠀⠀⠀⠀⠉⠛⣿⡿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡧⠀
    ⠀⠀⠀⠀⠀⠀⠀⠈⠃⠀⠙⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⠀⠀
    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠛⣿⣿⣿⣿⣿⣿⣿⣿⣿⠋⠀⠀⠀⠀
    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠁⠀⠀⠀⠀⠈⠙⢿⡄⠀⠀⠀⠀
    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    } else{Wiki::parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$abstract_text}
    
    
    
  })
  
  related <- reactive({
    
    langs <- c(pl = "Polish", en = "English", sv = "Swedish")
    # Only takes the 15 first topics
    
    if (current_article() == "Poland") {
      rep("Poland", 48)
    } else{Wiki::parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$related_topics}
    
    
  })
  
  output$predictions <- renderText({
    abstract()
  })
  
  output$related <- renderText({
    related()
  })
  
  output$dynamic_buttons <- renderUI({
    
    lapply(seq_along(related()), function(i) {
      actionButton(
        inputId = paste0("btn_", i ),
        label = related()[i]
      )
    })
  })
  
  
  button_clicked <- reactiveVal(NULL)
  
  observe(
    lapply(seq_along(related()), function(i) {
      btn_id <- paste0("btn_", i)
      observeEvent(input[[btn_id]], {
        #current_article(related()[[i]])
        button_clicked(i)
        
      })
    })
  )
  
  
  
  reactive_var <- eventReactive(button_clicked(), {
    related()[button_clicked()]
    
  })
  
  
  observeEvent(reactive_var(), {
    current_article(reactive_var())
  })
  
  observeEvent(button_clicked(), {
    
    if (current_article() == init_article) {
      
    } else{
      history(str_c(history(), current_article(), sep = "SEParATOR", collapse = ""))
    }
    
    
  }, ignoreInit = TRUE)
  
  
}



shinyApp(ui, server)
