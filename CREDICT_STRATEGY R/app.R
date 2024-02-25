

library(shiny)
library(shinydashboard)

model <- readRDS("credit_scoring_final_model.rds")

ui <- dashboardPage(
  dashboardHeader(
    title = "LA MIA BANCA",
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Azemfack Pages aime",
        message = "modello giusto",
        href = "https://afouda-datascience.com/cours/machine-learning-pour-la-modelisation-du-risque-de-credit-credit-scoring-dans-r/"
      ),
      # Autre message
      messageItem(
        from = "Azemfack Pages aime",
        message = "la via giusta",
        href = "https://afouda-datascience.com/cours/super-r-shiny-course/"
      )
    )
  ), 
  
  dashboardSidebar( sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Alarms", icon = icon("bell")),
    menuItem("Settings", icon = icon('cog'))
  )), 
  
  dashboardBody(
    
    tabItem(
      tabName = "features",
      fluidRow(box(valueBoxOutput("IL_TUO_RISULTATO")),
               box(numericInput("var1", label = "La tua età", 
                                value = 20, min = 18))),
      
      fluidRow(box(numericInput("var2", label = "stipendio annuale", 
                                value = 10000, min = 0)),
               box(selectInput("var3", 
                               label = "proprietà immobiliare : (MORTGAGE : mutuo, OWN : proprietario, RENT : locatario, OTHER : altri casi)", 
                               choices = c('MORTGAGE', 'OWN', 'RENT', 'OTHER')))),
      
      fluidRow(box(numericInput("var4", 
                                label = "Da quando il richiedente è in attività? (Durata in numero di anni)", 
                                value = 3, min = 0)),
               box(selectInput("var5", 
                               label = "Motivo del prestito : (DEBTCONSOLIDATION :Riscatto di un credito, HOMEIMPROVEMENT : Lavori di ristrutturazione di immobili, VENTURE : Business)", 
                               choices = c('DEBTCONSOLIDATION', 'EDUCATION', 'HOMEIMPROVEMENT', 'MEDICAL', 'PERSONAL', 'VENTURE'))),),
      fluidRow(box(selectInput("var6", 
                               label = "Categoria di credito", 
                               choices = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))),
               box(numericInput("var7", 
                                label = "importo del credito", 
                                value = 2000, min = 0))),
      
      fluidRow(box(numericInput("var8", 
                                label = "Tasso di interesse del credito (in %)", 
                                value = 3.5, min = 0)),
               box(numericInput("var9", 
                                label = "Rapporto debito/reddito del richiedente (valore decimale tra 0 e 1)", 
                                value = 0.1, min = 0, max = 1))),
      
      fluidRow(box(selectInput("var10", 
                               label = "Il richiedente è allo scoperto bancario? (Y: Sì, N: No):", 
                               choices = c('Y', 'N'))),
               box(numericInput("var11", 
                                label = "Scadenza degli stanziamenti in corso (in numero di anni)", 
                                value = 5, min = 0)))
      
    )
    
  )
  
)

server <- function(input, output) {
  
  prediction <- reactive({
    predict(
      model,
      data.frame(
        "person_age" = input$var1,
        "person_income" = input$var2,
        "person_home_ownership" = input$var3,
        "person_emp_length" = input$var4,
        "loan_intent" = input$var5,
        "loan_grade" = input$var6,
        "loan_amnt" = input$var7,
        "loan_int_rate" = input$var8,
        "loan_percent_income" = input$var9,
        "cb_person_default_on_file" = input$var10,
        "cb_person_cred_hist_length" = input$var11
      ),
      type = 'raw'
    )
  })
  
  prediction_label <- reactive({
    ifelse(prediction() == "0", "Ammissibilità al credito", "Non ammissibile al credito")
  })
  
  prediction_prob <- reactive({
    predict(
      model,
      data.frame(
        "person_age" = input$var1,
        "person_income" = input$var2,
        "person_home_ownership" = input$var3,
        "person_emp_length" = input$var4,
        "loan_intent" = input$var5,
        "loan_grade" = input$var6,
        "loan_amnt" = input$var7,
        "loan_int_rate" = input$var8,
        "loan_percent_income" = input$var9,
        "cb_person_default_on_file" = input$var10,
        "cb_person_cred_hist_length" = input$var11
      ),
      type = "prob"
    ) 
  })
  
  prediction_color <- reactive({
    ifelse(prediction() == "0", "green", "red")
  })
  
  output$score_prediction <- renderValueBox({
    
    valueBox(
      value = paste(round(100*prediction_prob()$`1`, 0), "%"),
      subtitle = prediction_label(),
      color = prediction_color(),
      icon = icon("hand-holding-usd")
    )                     
    
  })
  
}

shinyApp(ui, server)

