library(shiny)
library(bslib)
library(jsonlite)
library(RMySQL)
library(ggplot2)
library(DBI)
library(dplyr)
library(htmlwidgets)
library(scales)
# library(forcats)
# library(ggthemes)
# library(ggsci)
# library(showtext)
library(stringr)

# Import fonts
# font_add_google('Oswald', 'Oswald')
# showtext_auto()

# options(shiny.port = 8080)

# Set the locale to French
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# ----------------------------------------
# Pre-processing
# ----------------------------------------

# Load functions
# source("functions/ui_functions.R") # not really needed, but I prefer to explicitly include external files

# Load database configuration from JSON file
config <- fromJSON("www/database_config.json")

# Prepare SQL Query
query <- readLines("www/transaction.sql", warn = FALSE) # read sql file
query <- paste(query, collapse = "\n") # convert to single line to read full complex query

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

# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Get min_date
min_date <- dbGetQuery(conn, "SELECT MIN(start_date_standardized) AS min_date FROM date")$min_date

# Get max_date
max_date <- dbGetQuery(conn, "SELECT MAX(start_date_standardized) AS max_date FROM date")$max_date

# Define the custom order for rubrics
custom_order_rubrics <- c("Coquina", "Panataria", "Buticularia", "Avena", "Marescalla", "Vestes et Forratura", "Ornamenta", "Cera et quedam extraordinaria", "Scripture et libra", "Bulle et littere Curie", "Vadia familiarium ordinariorum", "Gagia extraordinaria et Armature", "Elemosine", "Puelle et pauperes mulieres maritandis", "Possessiones emptas", "Opere et edificia", "Collegium card.", "Pensiones hospitiorum", "Subsidium Terre Sancte", "Mutua")

# Parse JSON content
datatable_i18n_french <- fromJSON("www/datatable_french_2_0_7.json")


# ----------------------------------------
# Define UI
# ----------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_r.css")
  ),
  # titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      id = "left-sidebar",
      width = 3,
      
      # Input: Emitter
      conditionalPanel(
        condition = "input.tabset == 'Par émetteur' || input.tabset == 'Par rubrique' || input.tabset == 'Par date'" ,
        checkboxGroupInput(
          inputId = "emitter",
          label = "Émetteurs",
          choices = unique(data$Emitter),
          selected = unique(data$Emitter)
        )
      ),
      
      
      # Input: Document
      conditionalPanel(
        condition = "input.tabset == 'Par document'",
        checkboxGroupInput(
          inputId = "document",
          label = "Documents",
          choices = unique(data$Document),
          selected = unique(data$Document)
        ),
        actionButton("select_all_documents", "Tout sélectionner"),
        actionButton("deselect_all_documents", "Tout désélectionner")
      ),
      
      
      # Input: Rubric
      checkboxGroupInput(
        inputId = "rubric",
        label = "Rubriques",
        # choices = sort(unique(data$Rubric)),
        # selected = unique(data$Rubric)
        choices = custom_order_rubrics,
        selected = custom_order_rubrics
      ),
      actionButton("select_all_rubrics", "Tout sélectionner"),
      actionButton("deselect_all_rubrics", "Tout désélectionner")
      
      
    ),
    mainPanel(
      HTML("<div class='title-block'>Transactions</div>"),
      
      tabsetPanel(
        id = "tabset",
        
        tabPanel(
          title = "Par émetteur",
          # HTML("<div class='subpart-title'>Plot</div>"),
          downloadButton(outputId = "export_jpg_by_emitter", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_emitter", label = "Export CSV"),
          plotOutput(outputId = "transaction_by_emitter"),
          # HTML("<div class='subpart-title'>Data table</div>"),
          DT::DTOutput(outputId = "table_data_by_emitter"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique montre le nombre de transactions pour chaque émetteur. Chaque barre de couleur différente représente un émetteur.<br>
              - L'axe des X répertorie les émetteurs.<br>
              - L'axe des Y indique le nombre total de transactions.<br>
              - La hauteur de chaque barre colorée indique le nombre de transactions par l'émetteur correspondant.</div>"
          )
        ),
        
        tabPanel(
          title = "Par document",
          # HTML("<div class='subpart-title'>Plot</div>"),
          downloadButton(outputId = "export_jpg_by_document", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_document", label = "Export CSV"),
          plotOutput(outputId = "transaction_by_document"),
          # HTML("<div class='subpart-title'>Data table</div>"),
          DT::DTOutput(outputId = "table_data_by_document"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique présente le nombre de transactions dans différents documents. Chaque barre de couleur différente représente un document.<br>
                - L'axe des X répertorie les documents.<br>
                - L'axe des Y indique le nombre total de transactions.<br>
                - Les différentes couleurs des barres représentent différents documents.<br>
                - La hauteur de chaque barre colorée indique le nombre de transactions dans le document correspondant.</div>"
          )
          # textOutput(outputId = "explanation_by_document")
        ),
        
        tabPanel(
          title = "Par rubrique",
          # HTML("<div class='subpart-title'>Plot</div>"),
          downloadButton(outputId = "export_jpg_by_rubric", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_rubric", label = "Export CSV"),
          plotOutput(outputId = "transaction_by_rubric"),
          # HTML("<div class='subpart-title'>Data table</div>"),
          DT::DTOutput(outputId = "table_data_by_rubric"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique illustre le nombre de transactions dans diverses rubriques. Chaque barre de couleur différente représente une rubrique.<br>
                - L'axe des X répertorie les rubriques.<br>
                - L'axe des Y indique le nombre total de transactions.<br>
                - La hauteur de chaque barre colorée indique le nombre de transactions dans la rubrique correspondante.</div>"
          )
        ),
        
        tabPanel(
          title = "Par date",
          # HTML("<div class='subpart-title'>Plot</div>"),
          downloadButton(outputId = "export_jpg_by_date", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_date", label = "Export CSV"),
          plotOutput(outputId = "transaction_by_date"),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          # HTML("<div class='subpart-title'>Data table</div>"),
          DT::DTOutput(outputId = "table_data_by_date"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique suit le nombre de transactions au fil du temps. Chaque ligne représente un émetteur différente, indiquant le nombre de transactions à différentes dates. On peut voir les tendances et les schémas des transactions au fil du temps. Cela permet d'identifier les périodes pendant lesquelles le nombre de transactions a été plus ou moins important.<br>
                - L'axe des X représente la chronologie (dates).<br>
                - L'axe des Y indique le nombre total de transactions.<br>
                - Différentes lignes représentent différents émetteur.<br>
                - La position des points et les lignes qui les relient indiquent le nombre de transactions à des dates précises.</div>"
          )
          # textOutput(outputId = "explanation_by_date")
        )
      )
      
    )
  )
)



# ----------------------------------------
# Define server logic
# ----------------------------------------

server <- function(input, output, session) {
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

  
  # END: Buttons ("select all" etc.)
  # ----------------------------------------
  
  
  
  
  # Reactive filter data (based on user input selections)
  # ----------------------------------------
  
  filtered_data_for_emitter <- reactive({
    # req(input$emitter, input$rubric, input$currency) # Ensure that inputs variables is not NULL
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric
      )
  })
  
  
  
  filtered_data_for_document <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric
      )
  })
  
  
  
  filtered_data_for_date <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      )
  })
  
  
  
  
  # Prepare reactive filtered datasets
  # ------------------------------------
  
  # Dataset for "transaction by emitter"
  result_by_emitter <- reactive({
    filtered_data_for_emitter() %>%
      group_by(Emitter) %>%
      # summarise(TotalOccurrences = n()) %>%
      # arrange(desc(TotalOccurrences))  # Sort by TotalOccurrences in descending order
      summarise(
        TotalOccurrences = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
      )
  })
  
  # Dataset for "transaction by documents"
  result_by_document <- reactive({
    filtered_data_for_document() %>%
      group_by(Document) %>%
      # summarise(TotalOccurrences = n()) %>%
      # arrange(desc(TotalOccurrences))  # Sort by TotalOccurrences in descending order
      summarise(
        TotalOccurrences = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
      )
  })
  
  # Dataset for "transaction by rubrics"
  result_by_rubric <- reactive({
    filtered_data_for_emitter() %>%
      group_by(Rubric) %>%
      summarise(TotalOccurrences = n()) %>%
      arrange(desc(TotalOccurrences)) %>%  # Sort by TotalOccurrences in descending order
      mutate(Rubric = factor(Rubric, levels = custom_order_rubrics)) # Set the factor levels
  })
  
  # Dataset for "transaction by date"
  result_by_date <- reactive({
    filtered_data_for_date() %>%
      group_by(Emitter, Date) %>%
      summarise(TotalOccurrences = n()) %>%
      mutate(Date = as.Date(Date))
  })
  
  
  # Make plots: reactive expressions
  # ------------------------------------
  
  
  # Reactive expression for groupped bar chart for "transaction by emitter"
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
    else { # START: else
    ggplot(result_by_emitter(),
           aes(
             # x = Emitter,
             x = reorder(Emitter, FirstDate),
             y = TotalOccurrences,
             fill = Emitter
           )) +
      geom_bar(stat = "identity") +
      # geom_text(position = position_dodge(width = 0.9), aes(label = Currency), size = 3, vjust = -0.5) +                     
      labs(
        title = "Nombre de transactions par émetteur",
        x = "Émetteur",
        y = "Nombre de transactions",
        fill = "Émetteur",
        caption = "Copyright © Ressources comptables"
      ) +
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
  
  
  # Reactive expression for groupped bar chart for "transaction by document"
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
    else { # START: else
    ggplot(
      result_by_document(),
      aes(
          # x = str_wrap(Document, width = 15), 
          x = reorder(str_wrap(Document, width = 25), FirstDate),
          y = TotalOccurrences,
          fill = Document
      )) +
      geom_bar(stat = "identity") +
      labs(
        title = "Nombre de transactions par document", 
        x = "Document", 
        y = "Nombre de transactions",
        fill = "Document",
        caption = "Copyright © Ressources comptables"
      ) +
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
      } # END: else
  })
  
  
  
  
  # Reactive expression for groupped bar chart for "transaction by rubric"
  plot_by_rubric <- reactive({
    # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs et les rubriques pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && nrow(result_by_rubric()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    }
    else { # START: else
    ggplot(result_by_rubric(),
           aes(
             x = Rubric,
             y = TotalOccurrences,
             fill = Rubric
           )) +
      geom_bar(stat = "identity") +
      labs(
        title = "Nombre de transactions par rubrique", 
        x = "Rubrique", 
        y = "Nombre de transactions",
        fill ="Rubrique",
        caption = "Copyright © Ressources comptables"
      ) +
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
        # legend.key.height=unit(0.4,"cm"),
        # legend.margin=margin(t = 5, unit='cm'),
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
       } # END: else
  })
  
  
  
  # Reactive expression for groupped bar chart for "transaction by date"
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
    else { # START: else
    ggplot(result_by_date(),
           aes(
             x = Date, 
             y = TotalOccurrences, 
             color = Emitter
           )) +
      geom_line() +
      labs(
        title = "Nombre de transactions par date",
        x = "Date", 
        y = "Nombre de transaction", 
        color = "Émetteur",
        caption = "Copyright © Ressources comptables"
      ) +
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
        legend.title = element_text(
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        legend.text = element_text(
          size = 10
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
  
  
  
  # Make plots: generate plots
  # ------------------------------------
  
  # Groupped bar chart for "currency by emitter"
  output$transaction_by_emitter <- renderPlot({
    plot_by_emitter()
  })
  
  
  # Groupped bar chart for "currency by document"
  output$transaction_by_document <- renderPlot({
    plot_by_document()
  })
  
  # Groupped bar chart for "currency by document"
  output$transaction_by_rubric <- renderPlot({
    plot_by_rubric()
  })
  
  # Groupped bar chart for "currency by date"
  output$transaction_by_date <- renderPlot({
    plot_by_date()
  })
  
  
  # Make data tables: reactive expressions
  # ------------------------------------
  
  # Reactive expression for data table for "transaction by emitter"
  table_by_emitter <- reactive({
    data <- result_by_emitter() %>%
      select(Emitter, TotalOccurrences)
    # Set column names
    colnames(data) <- c("Émetteur", "Nombre total de transactions")
    data
  })
  
  # Reactive expression for data table for "transaction by document"
  table_by_document <- reactive({
    data <- result_by_document() %>%
      select(Document, TotalOccurrences)
    colnames(data) <- c("Document", "Nombre total de transactions")
    data
  })
  
  # Reactive expression for data table for "transaction by rubric"
  table_by_rubric <- reactive({
    data <- result_by_rubric() %>%
      select(Rubric, TotalOccurrences)
    colnames(data) <- c("Rubrique", "Nombre total de transactions")
    data
  })
  
  # Reactive expression for data table for "transaction by date"
  table_by_date <- reactive({
    data <- result_by_date() %>%
      select(Date, Emitter, TotalOccurrences)
    colnames(data) <- c("Date", "Émetteur", "Nombre total de transactions")
    data
  })
  
  
  # Make data tables: generate tables
  # ------------------------------------
  
  # Data table for "transaction by emitter"
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
  
  
  # Data table for "transaction by document"
  output$table_data_by_document <- DT::renderDT({
    table_by_document()
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
  
  
  # Data table for "transaction by rubric"
  output$table_data_by_rubric <- DT::renderDT({
    table_by_rubric()
  }, 
  rownames = FALSE, 
  filter = 'top', 
  callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tout" )'), 
  options = list(
    language = datatable_i18n_french,
    dom = 'ltipr',
    lengthChange = FALSE,
    pageLength = 15
  )
  )
  
  
  # Data table for "transaction by date"
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
              "Nombre de transactions par émetteur")
  
  export_data("by_document",
              plot_by_document,
              table_by_document,
              "Nombre de transactions par document")
  
  export_data("by_rubric",
              plot_by_rubric,
              table_by_rubric,
              "Nombre de transactions par rubrique")
  
  export_data("by_date", 
              plot_by_date, 
              table_by_date, 
              "Nombre de transactions par date")
  
  
  
  # Close the database connection when the app stops
  onStop(function() {
    dbDisconnect(conn)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
