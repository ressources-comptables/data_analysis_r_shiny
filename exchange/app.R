# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(htmlwidgets)
library(DT)

library(jsonlite)
library(RMySQL)
library(DBI)
library(scales)
library(stringr)

# options(shiny.port = 8080)

# Set the locale to French
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

# ----------------------------------------
# Pre-processing
# ----------------------------------------

# Source the script to load the dataframe (FOR DIRECT CALCULATION IN THE APP)
# source("www/exchange_rates_full.R")

# Load the pre-constructed dataset
exchange_rates_combined_sorted <- readRDS("www/exchange_rates_combined_sorted.rds")

# Alternatively, load from CSV if used
# exchange_rates_combined_sorted <- read.csv("www/exchange_rates_combined_sorted.csv")

# Load min_date and max_date
date_info <- readRDS("www/date_info.rds")
min_date <- date_info$min_date
max_date <- date_info$max_date


# Translation to french for datatable
datatable_i18n_french <- fromJSON("www/datatable_french_2_0_7.json")

# Convert dates to R date format
exchange_rates_combined_sorted$Date <- as.Date(exchange_rates_combined_sorted$Date, origin = "1970-01-01")

# Create Currency_pair for the rows with Exchange_rate_value and Date
# also create PointType (solid if Anchor_currency is NA, dashed if Anchor_currency exist)
exchange_rates_combined_sorted <- exchange_rates_combined_sorted %>%
  filter(!is.na(Exchange_rate_value) & !is.na(Date)) %>%
  mutate(Currency_pair = paste(Currency_source, Currency_target, sep = "-"),
         PointType = ifelse(is.na(Anchor_currency), "solid", "dashed"))

# Sort the data frame by the Date column (to display chronologicaly the input Emitter and Document)
# exchange_rates_combined_sorted_chronological <- exchange_rates_combined_sorted[order(exchange_rates_combined_sorted$Date), ]


# Replace "null" values with "Unknown"
# exchange_rates_combined_sorted$Emitter[is.na(exchange_rates_combined_sorted$Emitter)] <- "Inconnu"
# exchange_rates_combined_sorted$Document[is.na(exchange_rates_combined_sorted$Document)] <- "Inconnu"


# Debugging step: Check unique values
# unique(exchange_rates_combined_sorted$PointType)

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
      
      # Input: Emitter
      conditionalPanel(
        
        condition = "input.tabset == 'Taux de change' " ,
        
        radioButtons(
          inputId = "exchange_rate_type",
          label = "Type de taux",
          choices = c(
            "Connus d'après les sources" = "known",
            "Reconstitués (expérimental)" = "reconstructed",
            "Combinés (expérimental)" = "combined"
          ),
          selected = "known"
        ),
        
        
        checkboxGroupInput(
          inputId = "currency_source",
          label = "Monnaie source",
          choices = sort(unique(exchange_rates_combined_sorted$Currency_source)),
          selected = NULL
        ),
        
        checkboxGroupInput(
          inputId = "currency_target",
          label = "Monnaie cible",
          choices = sort(unique(exchange_rates_combined_sorted$Currency_target)),
          selected = NULL
        )
        
      ),
      
      
      # Input: Emitter
      conditionalPanel(
        condition = "input.tabset == 'Mentions par émetteur' " ,
        checkboxGroupInput(
          inputId = "emitter",
          label = "Émetteurs",
          choices = unique(na.omit(exchange_rates_combined_sorted$Emitter)),
          selected = unique(na.omit(exchange_rates_combined_sorted$Emitter))
        )
      ),
      
      # Input: Document
      conditionalPanel(
        condition = "input.tabset == 'Mentions par document' ",
        checkboxGroupInput(
          inputId = "document",
          label = "Documents",
          choices = unique(na.omit(exchange_rates_combined_sorted$Document)),
          selected = unique(na.omit(exchange_rates_combined_sorted$Document))
        ),
        actionButton("select_all_documents", "Tout sélectionner"),
        actionButton("deselect_all_documents", "Tout désélectionner")
      )
      
      
    ), # END: sidebarPanel
    
    mainPanel( # START: mainPanel
      HTML("<div class='title-block'>Taux de change</div>"),
      
      tabsetPanel(  # START: tabsetPanel
        id = "tabset",
        
        tabPanel(
          title = "Taux de change",
          downloadButton(outputId = "export_jpg_for_exchange_rates", label = "Export JPG"),
          downloadButton(outputId = "export_csv_for_exchange_rates", label = "Export CSV"),
          plotOutput(outputId = "exchange_rates_by_date"),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          DT::DTOutput(outputId = "table_data_for_exchange_rates"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>
            
            <p>Ce graphique illustre comment les taux de change des différentes paires de monnaies évoluent au fil du temps. Chaque ligne représente une paire de monnaie différente. On peut voir les tendances et les schémas de l'évolution des taux de change au fil du temps.</p>
              - L'axe des X ( <i>Date</i> ) représente le temps, avec les dates affichées dans l'ordre chronologique.<br>
              - L'axe des Y ( <i>Valeur du taux de change</i> )  montre la valeur du taux de change.<br>
              - Les lignes de différentes couleurs représentent différentes paires de devises.<br>
              <br>
              
              <p><span style='font-weight:350;'>Types de taux de change</span></p>
               <p><i>Taux de change connus d'après les sources.</i><br> Ce sont des taux confirmés obtenus directement des documents. Sur le graphique les taux de change de ce type sont marqués avec des points pleins et connectés par des lignes pleines. Les informations sur le document source et sur le montant commme il est présenté dans le texte sont disponibles dans le tableau de données.</p>
               <p><i>Taux de change reconstitués.</i><br> Ce sont des taux reconstitués par triangulation entre devises. Cette méthode estime les taux de change en se basant sur un taux de change connu avec une troisième devise, dite devise d'ancrage. Sur le graphique les taux de change de ce type sont marqués avec des points creux et connectés par des lignes pointillées. Le tableau de données fournit des détails sur la devise d'ancrage et les taux de change connus (leurs identifiants 'ids') utilisés pour la triangulation.</p>
               <p><i>Taux de change combinés.</i><br> Ce sont des taux qui incluent à la fois les taux connus et reconstitués. Sur le graphique les taux de change de ce type sont marqués avec des points pleins pour les taux connus et les points creux pour les taux reconstitués, tous connectés par des lignes pointillées.</p>
              <br>
              
              <p><span style='font-weight:350;'>Tableau de données</span></p>
              <p>Veuillez également noter que le format tabulaire ne permet pas de représenter la structure hiérarchique des données ce qui peut provoquer la répétition des taux de change dans le tableau. Cela signifie que si le même taux apparaît à différentes dates ou dans différents documents, chaque occurrence sera listée séparément dans le tableau de données.</p>
            </div>"
          )
        ),
        
        tabPanel(
          title = "Mentions par émetteur",
          downloadButton(outputId = "export_jpg_by_emitter", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_emitter", label = "Export CSV"),
          plotOutput(outputId = "exchange_rate_by_emitter"),
          DT::DTOutput(outputId = "table_data_by_emitter"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique montre le nombre de mentions des différents taux de change dans les documents pour chaque émetteur. Chaque barres représente un émetteur.<br>
              - L'axe des X répertorie les émetteurs.<br>
              - L'axe des Y indique le nombre total de mentions de taux de change par émitteur.<br>
              - Les différentes couleurs des barres représentent différents émitteurs.<br>
              - La hauteur de chaque barre colorée indique le nombre total de mentions de taux de change par l'émetteur correspondant.</div>"
          )
        ),
        
        tabPanel(
          title = "Mentions par document",
          downloadButton(outputId = "export_jpg_by_document", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_document", label = "Export CSV"),
          plotOutput(outputId = "exchange_rate_by_document"),
          DT::DTOutput(outputId = "table_data_by_document"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique montre le nombre de mentions des différents taux de change pour chaque documents. Chaque barres représente un document.<br>
              - L'axe des X répertorie les documents<br>
              - L'axe des Y indique le nombre total de mentions de taux de change.<br>
              - Les différentes couleurs des barres représentent différents émetteurs.<br>
              - La hauteur de chaque barre colorée indique le nombre total de mentions de taux de change par le document correspondant.</div>"
          )
        )
        
      ) # END: tabsetPanel
      
    ) # END: mainPanel
    
  ) # END: sidebarLayout
  
)  # END: fluidPage



# ----------------------------------------
# Define server logic
# ----------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Buttons ("select all" etc.)
  # ----------------------------------------
  
  # Select / deselect All button for documents
  observeEvent(input$select_all_documents, {
    updateCheckboxGroupInput(session, "document", selected = unique(exchange_rates_combined_sorted$Document))
  })
  
  observeEvent(input$deselect_all_documents, {
    updateCheckboxGroupInput(session, "document", selected = character(0))
  })
  
  
  
  # Debug: Check if the data is correctly initialized
  # observe({
    # print(unique(na.omit(exchange_rates_combined_sorted$Document)))
  # })
  
  # Select all documents
  observeEvent(input$select_all_documents, {
    updateCheckboxGroupInput(
      session, 
      "document", 
      selected = unique(na.omit(exchange_rates_combined_sorted$Document))
    )
  })
  
  # Deselect all documents
  observeEvent(input$deselect_all_documents, {
    updateCheckboxGroupInput(
      session, 
      "document", 
      selected = character(0)
    )
  })
  
  # Reactive filter data (based on user input selections)
  # ----------------------------------------
  
  filtered_data_for_exchange_rates <- reactive({
    data <- exchange_rates_combined_sorted %>%
      filter(
        Currency_source %in% input$currency_source,
        Currency_target %in% input$currency_target,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      )
    
    # Adjust the data based on the chosen plot type
    if ("known" %in% input$exchange_rate_type) {
      data <- data %>%
        filter(is.na(Anchor_currency))
      line_type <- "solid"
    } else if ("reconstructed" %in% input$exchange_rate_type) {
      data <- data %>%
        filter(!is.na(Anchor_currency))
      line_type <- "dashed"
    } else if ("combined" %in% input$exchange_rate_type) {
      line_type <- "dashed"
    }
    list(data = data, line_type = line_type)
  })
  
  
  filtered_data_for_emitter <- reactive({
    exchange_rates_combined_sorted %>%
      filter(
        Emitter %in% input$emitter,
        is.na(Anchor_currency)
      )
  })
  
  
  filtered_data_for_document <- reactive({
    exchange_rates_combined_sorted %>%
      filter(
        Document %in% input$document,
        is.na(Anchor_currency)
      )
  })
  

  
  # Prepare reactive filtered datasets
  # ------------------------------------
  
  result_by_emitter <- reactive({
    filtered_data_for_emitter() %>%
      group_by(Emitter) %>%
      summarise(
        Count = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      arrange(desc(Count))
  })
  
  result_by_document <- reactive({
    filtered_data_for_document() %>%
      group_by(Document) %>%
      summarise(
        Count = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
        ) %>%
      arrange(desc(Count))
  })
  
  
  # Make plots: reactive expressions
  # ------------------------------------

  plot_for_exchange_rates <- reactive({
    filtered_data <- filtered_data_for_exchange_rates()$data
    line_type <- filtered_data_for_exchange_rates()$line_type
    
    # Check if both currency_source and currency_target are NULL
    if (is.null(input$currency_source) || is.null(input$currency_target)) {
      plot.new()
      text(0.5, 0.5, "Sélectionnez les paires de monnaies pour afficher le graphique", cex = 1.5)
    } 
    # Check if both currency_source and currency_target are not NULL but filtered_data is empty
    else if (!is.null(input$currency_source) && !is.null(input$currency_target) && nrow(filtered_data) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les paires de monnaies sélectionnées", cex = 1.5)
    } 
    # Plot the data if both currency_source and currency_target are selected and data is available
    else {
      ggplot(filtered_data, aes(x = Date, y = Exchange_rate_value, color = Currency_pair)) +
        geom_path(aes(linetype = line_type), size = 0.5) +
        geom_point(aes(shape = PointType), size = 2) +
        scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"), guide = FALSE) +
        scale_shape_manual(
          values = c("solid" = 16, "dashed" = 1),
          labels = c("solid" = "taux connu", "dashed" = "taux reconstitué")
        ) +
        labs(
             title = "Taux de change",
             x = "Date",
             y = "Valeur de taux de change (en deniers)",
             color = "Paire de monnaies",
             # linetype = "Line Type",
             shape = "Type de taux",
             caption = "Copyright © Ressources comptables"
             ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            # face = "bold",
            size = 15,
            hjust = 0.5,
            # family = "serif"
            family = "Arial"
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
      
    }
  })
  
  
  plot_by_emitter <- reactive({
     # Check if the dataset is empty
    if (is.null(input$emitter)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$emitter) && nrow(result_by_emitter()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
else {
    ggplot(
      result_by_emitter(), 
      aes(
        # x = Emitter, 
        x = reorder(Emitter, FirstDate),
        y = Count, 
        fill = Emitter
        )) +
      geom_bar(stat = "identity") +
      labs(
        title = "Nombre de taux de change connu d'après les sources par émetteur",
        x = "Émetteur",
        y = "Nombre de taux de change",
        fill = "Émetteur",
        caption = "Copyright © Ressources comptables"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          # face = "bold",
          size = 15,
          hjust = 0.5,
          # family = "serif"
          family = "Arial"
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
          size = 10,
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
    if (is.null(input$document)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les documents pour afficher le graphique", cex = 1.5)
    } 
    else if (!is.null(input$document) && nrow(result_by_document()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
else {
    ggplot(
      result_by_document(), 
      aes(
        # x = str_wrap(Document, width = 15), 
        x = reorder(str_wrap(Document, width = 25), FirstDate),
        y = Count, 
        fill = Document)
      ) +
      geom_bar(stat = "identity") +
      labs(
        title = "Nombre de taux de change connu d'après les sources par document", 
        x = "Document", 
        y = "Nombre de taux de change",
        fill = "Document",
        caption = "Copyright © Ressources comptables"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          # face = "bold",
          size = 15,
          hjust = 0.5,
          # family = "serif"
          family = "Arial"
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
  
  
  
  # Make plots: generate plots
  # ------------------------------------
  
  output$exchange_rates_by_date <- renderPlot({
    plot_for_exchange_rates()
  })
  
  output$exchange_rate_by_emitter <- renderPlot({
    plot_by_emitter()
  })

  output$exchange_rate_by_document <- renderPlot({
    plot_by_document()
  })
  
  
  # Make data tables: reactive expressions
  # ------------------------------------
  
  table_for_exchange_rates <- reactive({
    data <- filtered_data_for_exchange_rates()$data %>%
      select(Id, Currency_source, Currency_target, Exchange_rate_value, Date, Emitter, Amount_original, Amount_target_converted_to_smallest_unit_of_count, Document, Anchor_currency, Triangulation_exchange_rates_ids)
    # Set column names (needed here for CSV export)
    colnames(data) <- c("Id", "Monn. source", "Monn. cible", "Taux (en den.)", "Date", "Émitteur", "Montant dans le texte", "Montant en den.", "Document", "Monn. ancrage", "Ids triang.")
    data
  })
  
  
  
  table_by_emitter <- reactive({
    data <- result_by_emitter() %>%
      select(Emitter, Count)
    # Set column names
    colnames(data) <- c("Émetteur", "Nombre total de taux de change connus dans les documents")
    data
  })
  
  table_by_document <- reactive({
    data <- result_by_document() %>%
      select(Document, Count)
    colnames(data) <- c("Document", "Nombre total de taux de change connus dans le document")
    data
  })
  
  # Make data tables: generate tables
  # ------------------------------------ 
  
  
  # Create the custom header with two rows for exchange rates by date
  sketch <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, "Id"),
        th(rowspan = 2, "Monn. source"),
        th(rowspan = 2, "Monn. cible"),
        th(rowspan = 2, "Taux (en den.)"),
        th(rowspan = 2, "Date"),
        th(rowspan = 2, "Émitteur"),
        th(colspan = 3, "Taux de change connus"),
        th(colspan = 2, "Taux de change reconstitués")
      ),
      tr(
        th("Montant dans le texte"),
        th("Montant en den."),
        th("Document"),
        th("Monn. ancrage"),
        th("Ids triang.")
      )
    )
  ))
  
  
  output$table_data_for_exchange_rates <- DT::renderDT({
    datatable(
      table_for_exchange_rates(),
      rownames = FALSE, 
      filter = 'top', 
      callback = JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "" )'), 
      options = list(
        language = datatable_i18n_french,
        dom = 'ltipr',
        lengthChange = FALSE,
        pageLength = 15
      ),
      container = sketch
    )
  })
      
   
  
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
  export_data("for_exchange_rates",
              plot_for_exchange_rates,
              table_for_exchange_rates,
              "Taux de change par date")
  
  
  export_data("by_emitter",
              plot_by_emitter,
              table_by_emitter,
              "Taux de change par émetteur")
  
  export_data("by_document",
              plot_by_document,
              table_by_document,
              "Taux de change par document") 
  


}

# Run the application
shinyApp(ui = ui, server = server)