library(shiny)
library(bslib)
library(jsonlite)
library(RMySQL)
library(ggplot2)
library(DBI)
library(dplyr)
library(htmlwidgets)
library(scales)
library(stringr)

# options(shiny.port = 8080)

# Set the locale to French
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# ----------------------------------------
# Pre-processing
# ----------------------------------------

# Load database configuration from JSON file
config <- fromJSON("www/database_config.json")

# Prepare SQL Query
query <- readLines("www/expense.sql", warn = FALSE) # read sql file
query <- paste(query, collapse = "\n") # convert to single line to read full complex query

query_all_transactions <- readLines("www/transaction.sql", warn = FALSE) # read sql file
query_all_transactions <- paste(query_all_transactions, collapse = "\n") # convert to single line to read full complex query

# Establish database connection
conn <- dbConnect(
  MySQL(),
  host = config$host,
  port = config$port,
  user = config$user,
  password = config$password,
  dbname = config$database,
  unix.socket = config$unix_socket
)

# Query the database
data <- dbGetQuery(conn, query)
all_transactions <- dbGetQuery(conn, query_all_transactions)

# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Get min_date
min_date <- dbGetQuery(conn, "SELECT MIN(start_date_standardized) AS min_date FROM date")$min_date

# Get max_date
max_date <- dbGetQuery(conn, "SELECT MAX(start_date_standardized) AS max_date FROM date")$max_date

# Replace "null" values in Currency column with "Unknown"
# data$Currency[is.na(data$Currency)] <- "Inconnu"

# Define the custom order for rubrics
custom_order_rubrics <- c("Coquina", "Panataria", "Buticularia", "Avena", "Marescalla", "Vestes et Forratura", "Ornamenta", "Cera et quedam extraordinaria", "Scripture et libra", "Bulle et littere Curie", "Vadia familiarium ordinariorum", "Gagia extraordinaria et Armature", "Elemosine", "Puelle et pauperes mulieres maritandis", "Possessiones emptas", "Opere et edificia", "Collegium card.", "Pensiones hospitiorum", "Subsidium Terre Sancte", "Mutua")

# Parse JSON content
datatable_i18n_french <- fromJSON("www/datatable_french_2_0_7.json")



# ----------------------------------------
# Define UI
# ----------------------------------------

ui <- fluidPage( # START: fluidPage
  # theme = bs_theme(preset = "shiny"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_r.css")
  ),
  # titlePanel("Title"),
  sidebarLayout( # START: sidebarLayout
    
    sidebarPanel( # START: sidebarPanel
      id = "left-sidebar",
      width = 3,
      
      selectInput(
        inputId = "currency",
        label = "Monnaie",
        choices = unique(data$Currency),
        selected = unique(data$Currency)
      ),
 
      HTML("<div class='info-box-leftsidebar'>L'ensemble des transactions, exprimées en monnaies différentes, a été converti dans une même monnaie afin de pouvoir comparer les dépenses dans les différentes rubriques et leur évolution dans le temps.</br>Veuillez choisir la monnaie dans laquelle toutes les transactions seront exprimées.</div>"),
      
           
      # Input: Emitter
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset == 'Par émetteur' || input.tabset == 'Par rubrique et émetteur' || input.tabset == 'Par date' " ,
        checkboxGroupInput(
          inputId = "emitter",
          label = "Émetteurs",
          choices = unique(data$Emitter),
          selected = unique(data$Emitter)
        )
      ),  # END: conditionalPanel
   
         
      # Input: Document
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset == 'Par document' || input.tabset == 'Par rubrique et document'" ,
        checkboxGroupInput(
          inputId = "document",
          label = "Documents",
          choices = unique(data$Document),
          selected = unique(data$Document)
        ),
        actionButton("select_all_documents", "Tout sélectionner"),
        actionButton("deselect_all_documents", "Tout désélectionner")
      ),  # END: conditionalPanel
      

      
      # Input: Rubrique
      checkboxGroupInput(
        inputId = "rubric",
        label = "Rubriques",
        # choices = sort(unique(data$Rubric)),
        # selected = unique(data$Rubric)
        choices = custom_order_rubrics,
        selected = custom_order_rubrics
      ),
      actionButton("select_all_rubrics", "Tout sélectionner"),
      actionButton("deselect_all_rubrics", "Tout désélectionner"),
      
      
      
    ), # END: sidebarPanel
    
    mainPanel( # START: mainPanel
      HTML("<div class='title-block'>Dépenses</div>"),
      
      tabsetPanel(  # START: tabsetPanel
        id = "tabset",
        
        tabPanel(
          title = "Par émetteur",
          uiOutput(outputId = "information_by_emitter"),
          downloadButton(outputId = "export_jpg_by_emitter", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_emitter", label = "Export CSV"),
          plotOutput(outputId = "expense_by_emitter"),
          DT::DTOutput(outputId = "table_data_by_emitter"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique montre l'ensemble des dépenses (exprimées dans la même monnaie) pour chaque émetteur. Chaque barre de couleur différente représente un émetteur.<br>
              - L'axe des X répertorie les émetteurs.<br>
              - L'axe des Y indique le montant.</div>"
          )
        ),
        
        tabPanel(
          title = "Par document",
          uiOutput(outputId = "information_by_document"),
          downloadButton(outputId = "export_jpg_by_document", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_document", label = "Export CSV"),
          plotOutput(outputId = "expense_by_document"),
          DT::DTOutput(outputId = "table_data_by_document"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique présente les dépenses (exprimées dans la même monnaie) dans différents documents. Chaque barre de couleur différente représente un document. On peut identifier les documents qui utilisent des monnaies spécifiques le plus fréquemment et comparer l'utilisation de différentes monnaies dans différents documents.<br>
                - L'axe des X répertorie les documents.<br>
                - L'axe des Y montre le montant.</div>"
          )
        ),
        
      
        tabPanel(
          title = "Par rubrique et émetteur",
          uiOutput(outputId = "information_by_rubric_and_emitter"),
          downloadButton(outputId = "export_jpg_by_rubric_and_emitter", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_rubric_and_emitter", label = "Export CSV"),
          plotOutput(outputId = "expense_by_rubric_and_emitter"),
          DT::DTOutput(outputId = "table_data_by_rubric_and_emitter"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique illustre les dépenses (exprimées dans la même monnaie) dans diverses rubriques et émetteur. Chaque groupe de barres représente un émetteur, et chaque barre de couleur différente représente une rubrique. On peut observer quelles rubriques ont engagé plus de dépenses et 
            comparer les dépenses par rubrique pour chaque émetteur.<br>
                - L'axe des X répertorie les émetteur et les rubriques.<br>
                - L'axe des Y montre le montant des dépenses.<br>
                - Les différentes couleurs des barres représentent différentes rubriques.<br>
                - La hauteur de chaque barre colorée indique le montant de dépense dans la rubrique correspondante.</div>"
          )
        ),
        
        tabPanel(
          title = "Par rubrique et document",
          HTML("<div class='info-box simple'><p>Ce type de graphique peut être très riche, mais l'affichage de nombreux éléments en même temps peut le rendre illisible. Pour une meilleure lisibilité, il est recommandé de limiter le nombre d'éléments à afficher (seulement quelques documents, rubriques ou monnaies).</p></div>"),
          uiOutput(outputId = "information_by_rubric_and_document"),
          downloadButton(outputId = "export_jpg_by_rubric_and_document", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_rubric_and_document", label = "Export CSV"),
          plotOutput(outputId = "expense_by_rubric_and_document"),
          DT::DTOutput(outputId = "table_data_by_rubric_and_document"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique illustre les dépenses (exprimées dans la même monnaie) dans diverses rubriques et document. Chaque groupe de barres représente un document, et chaque barre de couleur différente représente une rubrique. On peut observer quelles rubriques ont engagé plus de dépenses et 
            comparer les dépenses par rubrique pour chaque document.<br>
                - L'axe des X répertorie les documents et les rubriques.<br>
                - L'axe des Y montre le montant des dépenses.<br>
                - Les différentes couleurs des barres représentent différentes rubriques.<br>
                - La hauteur de chaque barre colorée indique le montant de dépense dans la rubrique correspondante.</div>"
          )
        ),
        
        tabPanel(
          title = "Par date",
          uiOutput(outputId = "information_by_date"),
          downloadButton(outputId = "export_jpg_by_date", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_date", label = "Export CSV"),
          plotOutput(outputId = "expense_by_date"),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          DT::DTOutput(outputId = "table_data_by_date"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique suit les dépenses (exprimées dans la même monnaie) par rubrique au fil du temps. Chaque ligne représente une rubrique différente, indiquant le montant dépensé à différentes dates. On peut ainsi observer les tendances et les schémas de dépenses au fil du temps, ce qui permet d'identifier les périodes où les dépenses pour certaines rubriques ont été plus ou moins élevées.<br>
                - L'axe des X représente la chronologie (dates).<br>
                - L'axe des Y montre le montant des dépenses.<br>
                - Différentes lignes représentent différentes rubriques.<br>
                - La position des points et les lignes qui les relient indiquent les montants dépensés à des dates précises.</div>"
          )
        )
        
        
      ) # END: tabsetPanel
      
    ) # END: mainPanel
    
  ) # END: sidebarLayout
  
)  # END: fluidPage




# ----------------------------------------
# Define server logic
# ----------------------------------------

server <- function(input, output, session) {

  
  
  # Info: count of transactions and thier conversions
  # ------------------------------------------------------------
  
  # count_converted_original_transactions_by_emitter_for_selected_currency <- reactive({
  #   data %>%
  #     filter(
  #       Emitter == input$emitter,
  #       Currency == input$currency,
  #       !is.na(Amount_original)
  #     ) %>%
  #     nrow()
  # })
  
  
  # By emitter
  count_all_transactions_by_emitter <- reactive({
    all_transactions %>%
    filter(
      Emitter %in% input$emitter,
      Rubric %in% input$rubric
      ) %>%
    nrow()
  })
  
  count_converted_transactions_by_emitter_for_selected_currency <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      ) %>%
      nrow()
  })
  

  # By document
  count_all_transactions_by_document <- reactive({
    all_transactions %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric
      ) %>%
      nrow()
  })
  
  count_converted_transactions_by_document_for_selected_currency <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      ) %>%
      nrow()
  })
  
  
  # By rubric and emitter
  count_all_transactions_by_rubric_and_emitter <- reactive({
    all_transactions %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric
      ) %>%
      nrow()
  })
  
  count_converted_transactions_by_rubric_and_emitter_for_selected_currency <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      ) %>%
      nrow()
  })
  
  
  # By rubric and document
  count_all_transactions_by_rubric_and_document <- reactive({
    all_transactions %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric
      ) %>%
      nrow()
  })
  
  count_converted_transactions_by_rubric_and_document_for_selected_currency <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      ) %>%
      nrow()
  })
  
  
  # By date
  count_all_transactions_by_date <- reactive({
    all_transactions %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      ) %>%
      nrow()
  })
  
  count_converted_transactions_by_date_for_selected_currency <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      ) %>%
      nrow()
  })
  
  

  
  # Info: output the count of transactions and thier conversions
  # ------------------------------------------------------------
  
  output$information_by_emitter <- renderText({
    all_count <- count_all_transactions_by_emitter()
    converted_count <- count_converted_transactions_by_emitter_for_selected_currency()
    # original_count <- count_converted_original_transactions_by_emitter_for_selected_currency()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions pour le(s) émetteur(s) et rubrique(s) sélectionné(s): <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions.</p>",
      "</div>"
    ))
    
  })
  
  
  output$information_by_document <- renderText({
    all_count <- count_all_transactions_by_document()
    converted_count <- count_converted_transactions_by_document_for_selected_currency()
    # original_count <- count_converted_original_transactions_by_emitter_for_selected_currency()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions pour le(s) document(s) et rubrique(s) sélectionné(s): <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions.</p>",
      "</div>"
    ))
    
  })
  
  output$information_by_rubric_and_emitter <- renderText({
    all_count <- count_all_transactions_by_rubric_and_emitter()
    converted_count <- count_converted_transactions_by_rubric_and_emitter_for_selected_currency()
    # original_count <- count_converted_original_transactions_by_emitter_for_selected_currency()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions pour le(s) émetteur(s) et rubrique(s) sélectionné(s): <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions.</p>",
      "</div>"
    ))
    
  })
  
  
  output$information_by_rubric_and_document <- renderText({
    all_count <- count_all_transactions_by_rubric_and_document()
    converted_count <- count_converted_transactions_by_rubric_and_document_for_selected_currency()
    # original_count <- count_converted_original_transactions_by_emitter_for_selected_currency()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions pour le(s) documents(s) et rubrique(s) sélectionné(s): <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions.</p>",
      "</div>"
    ))
    
  })
  
  
  output$information_by_date <- renderText({
    all_count <- count_all_transactions_by_date()
    converted_count <- count_converted_transactions_by_date_for_selected_currency()
    # original_count <- count_converted_original_transactions_by_emitter_for_selected_currency()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions pour le(s) émetteurs(s), rubrique(s) et dates sélectionné(s): <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions.</p>",
      "</div>"
    ))
    
  })
  
  
  # START: Buttons ("select all" etc.)
  # ----------------------------------------
  
  # Select / deselect All button for documents
  observeEvent(input$select_all_documents, {
    updateCheckboxGroupInput(session, "document", selected = unique(data$Document))
  })
  
  observeEvent(input$deselect_all_documents, {
    updateCheckboxGroupInput(session, "document", selected = character(0))
  })
  
  # Select / deselect All button for rubrics
  observeEvent(input$select_all_rubrics, {
    updateCheckboxGroupInput(session, "rubric", selected = unique(data$Rubric))
  })
  
  observeEvent(input$deselect_all_rubrics, {
    updateCheckboxGroupInput(session, "rubric", selected = character(0))
  })
  
  
  
  # Reactive filter data (based on user input selections)
  # ----------------------------------------
  
  filtered_data_for_emitter <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency
        )
  })
  
  
  filtered_data_for_document <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      )
  })
  
  
  filtered_data_for_rubric_and_emitter <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      )
  })
  
  
  filtered_data_for_rubric_and_document <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric,
        Currency %in% input$currency
      )
  })
  
  filtered_data_for_date <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Currency %in% input$currency,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      )
  })
  
  # Prepare reactive filtered datasets
  # ------------------------------------
  
  result_by_emitter <- reactive({
    filtered_data_for_emitter() %>%
      group_by(Emitter, Currency) %>%
      # summarise(Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3)) %>%
      summarise(
        Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      ungroup()
  })
  
  result_by_document <- reactive({
    filtered_data_for_document() %>%
      group_by(Document, Currency) %>%
      # summarise(Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3)) %>%
      summarise(
        Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      ungroup()
  })
  
  result_by_rubric_and_emitter <- reactive({
    filtered_data_for_rubric_and_emitter() %>%
      group_by(Emitter, Rubric, Currency) %>%
      # summarise(Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3)) %>%
      summarise(
        Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      ungroup() %>%
      # arrange(Rubric) %>%
      mutate(Rubric = factor(Rubric, levels = custom_order_rubrics))  # Set the factor levels
      
  })
  
  result_by_rubric_and_document <- reactive({
    filtered_data_for_rubric_and_document() %>%
      group_by(Document, Rubric, Currency) %>%
      # summarise(Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3)) %>%
      summarise(
        Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      ungroup() %>%
      mutate(Rubric = factor(Rubric, levels = custom_order_rubrics))  # Set the factor levels
  })
  
  result_by_date <- reactive({
    filtered_data_for_date() %>%
      group_by(Emitter, Rubric, Date, Currency) %>%
      summarise(Final_sum = round(sum(Amount_converted, na.rm = TRUE), 3)) %>%
      mutate(Date = as.Date(Date))
  })
  
  # Make plots: reactive expressions
  # ------------------------------------
  
    plot_by_emitter <- reactive({
           # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && nrow(result_by_emitter()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
    else {
    ggplot(result_by_emitter(),
           aes(
             # x = Emitter,
             x = reorder(Emitter, FirstDate),
             y = Final_sum,
             fill = Emitter
           )) +
      geom_bar(stat = "identity") +
      # geom_text(position = position_dodge(width = 0.9), aes(label = Currency), size = 3, vjust = -0.5) +                     
      labs(
        title = paste("Dépenses par émetteur (en", input$currency, ")"),
        x = "Émetteur",
        y = paste("Montant (en", input$currency, ")"),
        fill = "Émetteur",
        caption = "Copyright © Ressources comptables"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        # text = element_text(family = "Oswald"),
        plot.title = element_text(
          # face = "bold",
          size = 15,
          hjust = 0.5,
          family = "Arial"
          # family = "serif"
        ),
        axis.title.x = element_text(
          vjust = 0,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.title.y = element_text(
          vjust = 2,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.text.x = element_text(
          vjust = 1,
          size = 12,
          family = "Helvetica"
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12,
          family = "Helvetica"
        ),
        legend.title = element_text(
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        legend.text = element_text(
          size = 10,
          family = "Helvetica"
        ),
        plot.caption = element_text(
          size = 11,
          face = "italic",
          hjust = 1.1,
          family = "Helvetica"
        )
      )
      } # END: else
  })
  
  
  plot_by_document <- reactive({
               # Check if the dataset is empty
    if (is.null(input$document) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les documents et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$document) && !is.null(input$rubric) && nrow(result_by_document()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
    else {
    ggplot(result_by_document(),
           aes(
             # x = str_wrap(Document, width = 15),
             x = reorder(str_wrap(Document, width = 25), FirstDate),
             y = Final_sum,
             fill = Document
           )) +
      geom_bar(stat = "identity") +
      # geom_text(position = position_dodge(width = 0.9), aes(label = Currency), size = 3, vjust = -0.5) +                     
      labs(
        title = paste("Dépenses par document (en", input$currency, ")"),
        x = "Document",
        y = paste("Montant (en", input$currency, ")"),
        fill = "Document",
        caption = "Copyright © Ressources comptables"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        # text = element_text(family = "Oswald"),
        plot.title = element_text(
          # face = "bold",
          size = 15,
          hjust = 0.5,
          family = "Arial"
          # family = "serif"
        ),
        axis.title.x = element_text(
          vjust = 0,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.title.y = element_text(
          vjust = 2,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.text.x = element_text(
          angle = 50,
          vjust = 1,
          hjust = 1,
          size = 9,
          family = "Helvetica"
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12,
          family = "Helvetica"
        ),
        # legend.position="bottom",
        # legend.key.height=unit(0.3,"cm"),
        # legend.title = element_text(size = 13, face = "bold"),
        # legend.text = element_text(size = 10),
        legend.position = "none",
        plot.caption = element_text(
          size = 11,
          face = "italic",
          hjust = 1,
          family = "Helvetica"
        )
      ) 
      # +
      # guides(fill = guide_legend(ncol = 3))
      } # END: else
  })
  
  
  plot_by_rubric_and_emitter <- reactive({
    # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && nrow(result_by_rubric_and_emitter()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
 else { # START: else
      
        ggplot(result_by_rubric_and_emitter(),
               aes(
                 # x = Emitter,
                 # x = reorder(Emitter, FirstDate),
                 x = Rubric,
                 y = Final_sum,
                 # fill = reorder(Rubric, -Final_sum)
                 fill = Rubric
               )) +
          geom_bar(stat = "identity", position = "dodge") +
          # geom_text(position = position_dodge(width = 0.9), aes(label = Currency), size = 3, vjust = -0.5) +                     
          labs(
            title = paste("Dépenses par rubrique et émetteur (en", input$currency, ")"),
            x = "Rubriques par émetteur",
            y = paste("Montant (en", input$currency, ")"),
            fill = "Rubrique",
            caption = "Copyright © Ressources comptables"
          ) +
          scale_y_continuous(labels = scales::comma_format()) +
          theme_minimal() +
          theme(
            # text = element_text(family = "Oswald"),
            plot.title = element_text(
              # face = "bold",
              size = 15,
              hjust = 0.5,
              family = "Arial"
              # family = "serif"
            ),
            axis.title.x = element_text(
              vjust = 0,
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            axis.title.y = element_text(
              vjust = 2,
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            axis.text.x = element_text(
              # vjust = 1,
              # size = 12
              angle = 50,
              vjust = 1,
              hjust = 1,
              size = 11,
              family = "Helvetica"
            ),
            axis.text.y = element_text(
              vjust = 1,
              size = 12,
              family = "Helvetica"
            ),
            legend.margin=margin(t = 4, unit='cm'),
            legend.key.height=unit(0.5,"cm"),
            legend.title = element_text(
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            legend.text = element_text(
              size = 11,
              family = "Helvetica"
            ),
            panel.spacing = unit(1.5, "lines"), # Adjust spacing between facet panels
            plot.caption = element_text(
              size = 11,
              face = "italic",
              hjust = 1.25,
              family = "Helvetica"
            ),
            # Add font size for facet labels
            strip.text = element_text(
              size = 12, # Adjust the size as needed
              family = "Helvetica"
            )
          ) +
          facet_grid(~reorder(Emitter, FirstDate), scales = "free_x", space = "free_x") +
          scale_fill_manual(values = scales::hue_pal()(length(custom_order_rubrics)), 
                            breaks = custom_order_rubrics) # Ensure consistent rubric colors
    } # END: else
  })
  
  
  
  plot_by_rubric_and_document <- reactive({
        # Check if the dataset is empty
    if (is.null(input$document) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les documents et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$document) && !is.null(input$rubric) && nrow(result_by_rubric_and_document()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
    else { # START: else
      
        ggplot(result_by_rubric_and_document(),
               aes(
                 # x = str_wrap(Document, width = 15),
                 # x = reorder(str_wrap(Document, width = 15), FirstDate),
                 x = Rubric,
                 y = Final_sum,
                 # fill = reorder(Rubric, -Final_sum)
                 fill = Rubric
               )) +
          geom_bar(stat = "identity", position = "dodge") +
          # geom_text(position = position_dodge(width = 0.9), aes(label = Currency), size = 3, vjust = -0.5) +                     
          labs(
            title = paste("Dépenses par rubrique et document (en", input$currency, ")"),
            x = "Rubriques par document",
            y = paste("Montant (en", input$currency, ")"),
            fill = "Rubrique",
            caption = "Copyright © Ressources comptables"
          ) +
          scale_y_continuous(labels = scales::comma_format()) +
          theme_minimal() +
          theme(
            # text = element_text(family = "Oswald"),
            plot.title = element_text(
              # face = "bold",
              size = 15,
              hjust = 0.5,
              family = "Arial"
              # family = "serif"
            ),
            axis.title.x = element_text(
              vjust = 0,
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            axis.title.y = element_text(
              vjust = 2,
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            axis.text.x = element_text(
              angle = 50,
              vjust = 1,
              hjust = 1,
              size = 10,
              family = "Helvetica"
            ),
            axis.text.y = element_text(
              vjust = 1,
              size = 12,
              family = "Helvetica"
            ),
            legend.margin=margin(t = 4, unit='cm'),
            legend.key.height=unit(0.5,"cm"),
            legend.title = element_text(
              size = 13,
              face = "bold",
              family = "Arial"
            ),
            legend.text = element_text(
              size = 10,
              family = "Helvetica"
            ),
            panel.spacing = unit(1, "lines"), # Adjust spacing between facet panels
            plot.caption = element_text(
              size = 11,
              face = "italic",
              hjust = 1.25,
              family = "Helvetica"
            )
          ) +
        facet_grid(~reorder(str_wrap(Document, width = 30), FirstDate), scales = "free_x", space = "free_x") +
        scale_fill_manual(values = scales::hue_pal()(length(custom_order_rubrics)), 
                          breaks = custom_order_rubrics) # Ensure consistent rubric colors
      
    } # END: else
  })
  
  
  
  plot_by_date <- reactive({
    # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && nrow(result_by_date()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
  else {
    ggplot(result_by_date(),
           aes(
             x = Date, 
             y = Final_sum, 
             color = Rubric
           )) +
      geom_line() +
      labs(
        title = paste("Dépenses par date (en", input$currency, ")"),
        x = "Date", 
        y = paste("Montant (en", input$currency, ")"), 
        color = "Rubrique",
        caption = "Copyright © Ressources comptables"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          # face = "bold",
          size = 15,
          hjust = 0.5,
          family = "Arial"
          # family = "serif"
        ),
        axis.title.x = element_text(
          vjust = 0,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.title.y = element_text(
          vjust = 2,
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        axis.text.x = element_text(
          # angle = 50,
          vjust = 1,
          hjust = 1,
          size = 12,
          family = "Helvetica"
        ),
        axis.text.y = element_text(
          vjust = 1,
          size = 12,
          family = "Helvetica"
        ),
        legend.key.height=unit(0.5,"cm"),
        legend.title = element_text(
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        legend.text = element_text(
          size = 10,
          family = "Helvetica"
        ),
        plot.caption = element_text(
          size = 11,
          face = "italic",
          hjust = 1.25,
          family = "Helvetica"
        )
        
      )
     } # END: else
  })
  
  
  # Make plots: generate plots
  # ------------------------------------
  
  output$expense_by_emitter <- renderPlot({
    plot_by_emitter()
  })
  
  output$expense_by_document <- renderPlot({
    plot_by_document()
  })
  
  output$expense_by_rubric_and_emitter <- renderPlot({
    plot_by_rubric_and_emitter()
  })
  
  
  output$expense_by_rubric_and_document <- renderPlot({
    plot_by_rubric_and_document()
  })
  
  # Groupped bar chart for "currency by date"
  output$expense_by_date <- renderPlot({
    plot_by_date()
  })
  
  # Make data tables: reactive expressions
  # ------------------------------------
  
  table_by_emitter <- reactive({
    data <- result_by_emitter() %>%
      select(Emitter, Final_sum, Currency)
    colnames(data) <- c("Émetteur", "Montant", "Monnaie")
    data
  })

  table_by_document <- reactive({
    data <- result_by_document() %>%
      select(Document, Final_sum, Currency)
    colnames(data) <- c("Document", "Montant", "Monnaie")
    data
  })
  
  table_by_rubric_and_emitter <- reactive({
    data <- result_by_rubric_and_emitter() %>%
    select(Emitter, Rubric, Final_sum, Currency)
    colnames(data) <- c("Émetteur", "Rubrique", "Montant", "Monnaie")
    data
  })
  
  table_by_rubric_and_document <- reactive({
    data <- result_by_rubric_and_document() %>%
      select(Document, Rubric, Final_sum, Currency)
    colnames(data) <- c("Document", "Rubrique", "Montant", "Monnaie")
    data
  })
  
  # Reactive expression for data table for "currency by date"
  table_by_date <- reactive({
    data <- result_by_date() %>%
      select(Emitter, Rubric, Date, Final_sum, Currency)
    colnames(data) <- c("Émetteur", "Rubrique", "Date", "Montant", "Monnaie")
    data
  })
  
  # Make data tables: generate tables
  # ------------------------------------
  
  output$table_data_by_emitter <- DT::renderDT({
    table_by_emitter()
  }, rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    # searching = FALSE,
    dom = 'ltipr',
    lengthChange = FALSE,
    # lengthMenu = c(5, 10),
    pageLength = 15
  )
  )
  
  
  output$table_data_by_document <- DT::renderDT({
    table_by_document()
  }, rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    # searching = FALSE,
    dom = 'ltipr',
    lengthChange = FALSE,
    # lengthMenu = c(5, 10),
    pageLength = 15
  )
  )
  
  
  output$table_data_by_rubric_and_emitter <- DT::renderDT({
    table_by_rubric_and_emitter()
  }, rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    # searching = FALSE,
    dom = 'ltipr',
    lengthChange = FALSE,
    # lengthMenu = c(5, 10),
    pageLength = 15
  )
  )
  
  
  output$table_data_by_rubric_and_document <- DT::renderDT({
    table_by_rubric_and_document()
  }, rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    # searching = FALSE,
    dom = 'ltipr',
    lengthChange = FALSE,
    # lengthMenu = c(5, 10),
    pageLength = 15
  )
  )
  
  output$table_data_by_date <- DT::renderDT({
    table_by_date()
  }, rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    dom = 'ltipr',
    lengthChange = FALSE,
    pageLength = 15
  )
  )
  
  # Export JPG and CSV
  # ------------------------------------
  
  
  # Function to export both JPG and CSV
  export_data <- function(output_name,
                          plot_function,
                          table_function,
                          title) {
    output[[paste0("export_jpg_", output_name)]] <- downloadHandler(
      filename = function() {
        paste("[Ressources comptables] - ", title, " - Stats.jpg", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_function(), device = "jpg", width = 15)
      }
    )
    
    output[[paste0("export_csv_", output_name)]] <- downloadHandler(
      filename = function() {
        paste("[Ressources comptables] - ", title, " - Stats.csv", sep = "")
      },
      content = function(file) {
        data <- table_function()
        write.csv(data, file, row.names = FALSE)
      }
    )
  }
  
  # Call the function for each export
  export_data("by_emitter",
              plot_by_emitter,
              table_by_emitter,
              "Dépenses par émetteur")
  
  
  export_data("by_document",
              plot_by_document,
              table_by_document,
              "Dépenses par document")
  
  
  export_data("by_rubric_and_emitter",
              plot_by_rubric_and_emitter,
              table_by_rubric_and_emitter,
              "Dépenses par rubrique et émetteur")
  
  export_data("by_rubric_and_document",
              plot_by_rubric_and_document,
              table_by_rubric_and_document,
              "Dépenses par rubrique et document")
  
  
  export_data("by_date", 
              plot_by_date, 
              table_by_date, 
              "Dépenses par date")
  
  
  
  

  # Close the database connection when the app stops
  onStop(function() {
    dbDisconnect(conn)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
