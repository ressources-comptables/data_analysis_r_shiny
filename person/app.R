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
library(tidyr)
library(purrr)
library(visNetwork)
library(igraph)

# ----------------------------------------
# Configuration
# ----------------------------------------

# fix port when work on localhost
# options(shiny.port = 8080)

# Set the locale to French
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")


# ----------------------------------------
# Pre-processing
# ----------------------------------------

# Load database configuration from JSON file
config <- fromJSON("www/database_config.json")

# Prepare SQL Query
query <- readLines("www/person.sql", warn = FALSE) # read sql file
query <- paste(query, collapse = "\n") # convert to single line to read full complex query

# query_all_transactions_with_participants <- readLines("www/all_transactions_with_participants.sql", warn = FALSE) # read sql file
# query_all_transactions_with_participants <- paste(query_all_transactions_with_participants, collapse = "\n") # convert to single line to read full complex query

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

# Query the database: Main query for construct network
data <- dbGetQuery(conn, query)
# all_transactions_with_participants <- dbGetQuery(conn, query_all_transactions_with_participants)

# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Set up as "unkown" the empty values for Currency in sql result
data$Currency[is.na(data$Currency)] <- "inconnu"

# Set up as "unkown" the empty values for Person_role in sql result
data$Person_role[is.na(data$Person_role)] <- "inconnu"

# Get min_date (for date range selector)
min_date <- dbGetQuery(conn, "SELECT MIN(start_date_standardized) AS min_date FROM date")$min_date

# Get max_date (for date range selector)
max_date <- dbGetQuery(conn, "SELECT MAX(start_date_standardized) AS max_date FROM date")$max_date

# Define the custom order for emitters
# custom_order_emitters <- c("Jean XXII", "Benoît XII", "Clément VI", "Innocent VI")

# Define the custom order for rubrics
custom_order_rubrics <- c("Coquina", "Panataria", "Buticularia", "Avena", "Marescalla", "Vestes et Forratura", "Ornamenta", "Cera et quedam extraordinaria", "Scripture et libra", "Bulle et littere Curie", "Vadia familiarium ordinariorum", "Gagia extraordinaria et Armature", "Elemosine", "Puelle et pauperes mulieres maritandis", "Possessiones emptas", "Opere et edificia", "Collegium card.", "Pensiones hospitiorum", "Subsidium Terre Sancte", "Mutua")

# Add french language for datatable
datatable_i18n_french <- fromJSON("www/datatable_french_2_0_7.json")

# Source the external script containing the functions
source("www/edges_person.R") # create edges dataset for person
source("www/nodes_person.R") # create nodes dataset for person
source("www/edges_role.R") # create edges dataset for role
source("www/nodes_role.R") # create nodes dataset for role
source("www/visnetwork_data.R") # format data for network plot with visnetwork

# Close the database connection
# dbDisconnect(conn)

# Debugging: Check the structure of the data
# print(str(data))


# ----------------------------------------
# Define UI
# ----------------------------------------

ui <- fluidPage( # START: fluidPage
  
  # theme = bs_theme(preset = "shiny"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_r.css")
  ),
  
  # START: SIDEBAR
  # ----------------------------------------
  
  # titlePanel("Title"),
  sidebarLayout( # START: sidebarLayout
    
    sidebarPanel( # START: sidebarPanel
      
      id = "left-sidebar",
      width = 3,
      
      
      # Emitter input
      # ----------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset != 'Rôle par document' " ,
        
          checkboxGroupInput(
            inputId = "emitter",
            label = "Émetteurs",
            choices = unique(data$Emitter),
            selected = unique(data$Emitter)
            # choices = custom_order_emitters,  # Use factor for custom order
            # choices = unique(factor(data$Emitter, levels = custom_order_emitters)),  # Use factor for custom order
            # selected = custom_order_emitters  # Select all by default, in custom order
          ),
        
      ),  # END: conditionalPanel
      
      
      
      # Document input
      # ----------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset != 'Rôle par émetteur' && input.tabset != 'Rôle par rubrique' && input.tabset != 'Rôle par date' " ,
        
          checkboxGroupInput(
            inputId = "document",
            label = "Documents",
            choices = unique(data$Document),
            selected = unique(data$Document)
          ),
          actionButton("select_all_documents", "Tout sélectionner"),
          actionButton("deselect_all_documents", "Tout désélectionner"),
      
      ),  # END: conditionalPanel
      
      
      
      # Rubric input
      # ----------------------------------------
      
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
      
      
      # Personne (analyse de réseaux)
      # ----------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset == 'Personne (analyse de réseaux)' " ,

        # Edge type input
        checkboxGroupInput(
          inputId = "edge_type_person", 
          label = "Type de lien", 
          choices = NULL
          ),
        HTML("<div class='info-box-leftsidebar'>
             Le lien 'émetteur-bénéficiaire' relie les émetteurs aux bénéficiaires ayant reçu des montants de ces émetteurs.<br>
             Le lien 'bénéficiaire-bénéficiaire' relie les bénéficiaires ayant participé à la même transaction.<br>
             Pour plus de détails, veuillez consulter les explications en bas de la page.
             </div>"),
        
        # Transaction count input
        sliderInput(
          inputId = "transaction_count_person", 
          label = "Nombre de transactions", 
          min = 0, 
          max = 1, 
          value = c(0, 1)
          ),
        HTML("<div class='info-box-leftsidebar'>
              Dans le cas des liens 'émetteur-bénéficiaire', il s'agit du nombre de transactions auxquelles un bénéficiaire a participé avec le même émetteur.<br>
              Dans le cas des liens 'bénéficiaire-bénéficiaire', il s'agit du nombre de transactions auxquelles les mêmes bénéficiaires ont participé ensemble.<br>
              Plus le nombre de transactions est élevé, plus épais sera le lien sur le graphique.<br>
              Pour plus de détails, voir les explications en bas de la page.
             </div>"),
        
        # Amount count input
        sliderInput(
          inputId = "amount_count_person", 
          label = "Montant perçu", 
          min = 0,
          max = 1, 
          value = c(0, 1)
          ),
        HTML("<div class='info-box-leftsidebar'>
              Dans les deux cas de liens ('émetteur-bénéficiaire' ou 'bénéficiaire-bénéficiaire'), il s'agit toujours du montant perçu par chaque bénéficiaire de la part de l'émetteur.<br>
              Le montant est exprimé dans la monnaie sélectionnée dans le menu déroulant 'Monnaie'.<br>
              Pour plus de détails, veuillez consulter les explications en bas de la page.
             </div>"),
        
      ),  # END: conditionalPanel
      
      
      # Rôle (analyse de réseaux)
      # ----------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset == 'Rôle (analyse de réseaux)' " ,
        
      
        # Edge type input
        checkboxGroupInput(
          inputId = "edge_type_role", 
          label = "Type de lien", 
          choices = NULL
        ),
        HTML("<div class='info-box-leftsidebar'>
             Le lien 'émetteur-bénéficiaire(rôle)' relie les émetteurs aux rôles des bénéficiaires ayant reçu des montants de ces émetteurs.<br>
             Le lien 'bénéficiaire(rôle)-bénéficiaire(rôle)' relie les rôles des bénéficiaires ayant participé à la même transaction.<br>
             Pour plus de détails, veuillez consulter les explications en bas de la page.
             </div>"),
        
        # Transaction count input
        sliderInput(
          inputId = "transaction_count_role", 
          label = "Nombre de transactions", 
          min = 0, 
          max = 1, 
          value = c(0, 1)
        ),
        HTML("<div class='info-box-leftsidebar'>
              Dans le cas des liens 'émetteur-bénéficiaire(rôle)', il s'agit du nombre de transactions auxquelles des bénéficiaires avec le même rôle ont participé avec le même émetteur.<br>
              Dans le cas des liens 'bénéficiaire(rôle)-bénéficiaire(rôle)', il s'agit du nombre de transactions auxquelles les bénéficiaires avec le même rôle ont participé ensemble.<br>
              Plus le nombre de transactions est élevé, plus épais sera le lien sur le graphique.<br>
              Pour plus de détails, voir les explications en bas de la page.
             </div>"),
        
        # Amount count input
        sliderInput(
          inputId = "amount_count_role", 
          label = "Montant perçu", 
          min = 0,
          max = 1, 
          value = c(0, 1)
        ),
        HTML("<div class='info-box-leftsidebar'>
              Dans les deux cas de liens ('émetteur-bénéficiaire(rôle)' ou 'bénéficiaire(rôle)-bénéficiaire(rôle)'), il s'agit toujours du montant perçu par les bénéficiaires avec le même rôle de la part de l'émetteur.<br>
              Le montant est exprimé dans la monnaie sélectionnée dans le menu déroulant 'Monnaie'.<br>
              Pour plus de détails, veuillez consulter les explications en bas de la page.
             </div>"),
        
      ),  # END: conditionalPanel
      
      
      # Currency choise input / for Personne (analyse de réseaux) and Rôle (analyse de réseaux) / 
      # -------------------------------------------------------------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        
        condition = "input.tabset == 'Personne (analyse de réseaux)' || input.tabset == 'Rôle (analyse de réseaux)' " ,
        # Use && for logical AND and || for logical OR in Shiny condition expressions
          selectInput(
            inputId = "currency",
            label = "Monnaie",
            choices = sort(unique(data$Currency)),
            selected = sort(unique(data$Currency))[1]
          ),
          
          HTML("<div class='info-box-leftsidebar'>L'ensemble des transactions, exprimées en monnaies différentes, a été converti dans une même monnaie afin de pouvoir comparer les dépenses dans les différentes rubriques et leur évolution dans le temps.</br>Veuillez choisir la monnaie dans laquelle toutes les transactions seront exprimées.</div>"),
      
        ),  # END: conditionalPanel
      
      
      # Roles
      # ----------------------------------------
      
      conditionalPanel( # START: conditionalPanel
        condition = "input.tabset != 'Personne (analyse de réseaux)' && input.tabset != 'Rôle (analyse de réseaux)'" ,
        # Use && for logical AND and || for logical OR in Shiny condition expressions
        checkboxGroupInput(
          inputId = "person_role",
          label = "Rôles",
          choices = sort(unique(data$Person_role)),
          selected = NULL
        ),
        actionButton("select_all_roles", "Tout sélectionner"),
        actionButton("deselect_all_roles", "Tout désélectionner"),
      )  # END: conditionalPanel
      
    ), # END: sidebarPanel
   
    
     # END: SIDEBAR
    # ----------------------------------------   
    
    
    
    # START: MAIN PAGE CONTENT
    # ----------------------------------------
    
    mainPanel( # START: mainPanel
      
      HTML("<div class='title-block'>Personne</div>"),
      
      tabsetPanel(  # START: tabsetPanel
        id = "tabset",
        
        
        # Network "By person" tab
        # ----------------------------------------
        
        tabPanel( 
          title = "Personne (analyse de réseaux)",
          uiOutput(outputId = "information_by_person"),
          downloadButton(outputId = "export_csv_by_person", label = "Export CSV"),
          visNetworkOutput(
            outputId = "network_plot_by_person",
            height = "600px",
            width = "100%"
            ),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          DT::DTOutput(outputId = "table_data_by_person"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>
                Ce réseau représente les transactions entre les différentes personnes identifiées dans les comptes. <span class='bold-italic'>Les nœuds sont soit des bénéficiaires, soit des émetteurs de fonds.</span> Les bénéficiaires sont colorés en bleu, tandis que les émetteurs sont colorés en rouge.<br>
                Le réseau comporte <span class='bold-italic'>deux types de liens : 'émetteur-bénéficiaire' et 'bénéficiaire-bénéficiaire'</span>. Le lien 'émetteur-bénéficiaire' relie les émetteurs aux bénéficiaires ayant reçu des montants de ces émetteurs. Le lien 'bénéficiaire-bénéficiaire' relie les bénéficiaires ayant participé à la même transaction.<br>
                <span class='bold-italic'>L'épaisseur du lien sur le graphique représente le nombre de transactions entre les deux nœuds.</span> Dans le cas des liens 'émetteur-bénéficiaire', il s'agit du nombre de transactions auxquelles un bénéficiaire a participé avec le même émetteur. Dans le cas des liens 'bénéficiaire-bénéficiaire', il s'agit du nombre de transactions auxquelles les mêmes bénéficiaires ont participé ensemble. Plus le nombre de transactions est élevé, plus épais sera le lien sur le graphique.<br>
                <span class='bold-italic'>La taille d'un nœud représente le montant des fonds reçus par le bénéficiaire</span>, les nœuds plus grands indiquant des montants plus élevés. Dans les deux cas de liens ('émetteur-bénéficiaire' ou 'bénéficiaire-bénéficiaire'), il s'agit toujours du montant perçu par chaque bénéficiaire de la part de l'émetteur. Le montant est exprimé dans la monnaie sélectionnée dans le menu déroulant 'Monnaie'.<br>
                La visualisation du réseau permet d'identifier les principaux bénéficiaires et de mieux comprendre les relations entre les entités. Elle aide à apercevoir des modèles, des clusters et des acteurs clés des transactions. La visualisation du réseau simplifie la compréhension des relations complexes dans les données.
            </div>"
          )
        ), # END: tabPanel
        
        
        # Network "By role" tab
        # ----------------------------------------
        
        tabPanel(
          title = "Rôle (analyse de réseaux)",
          uiOutput(outputId = "information_by_role"),
          downloadButton(outputId = "export_csv_by_role", label = "Export CSV"),
          visNetworkOutput(
            outputId = "network_plot_by_role",
            height = "600px",
            width = "100%"
          ),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          DT::DTOutput(outputId = "table_data_by_role"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>
                Ce réseau représente les transactions entre les personnes avec les différents rôles identifiées dans les comptes. <span class='bold-italic'>Les nœuds sont soit des rôles des bénéficiaires, soit des émetteurs de fonds.</span> Les rôles des bénéficiaires sont colorés en bleu, tandis que les émetteurs sont colorés en rouge.<br>
                Le réseau comporte <span class='bold-italic'>deux types de liens : 'émetteur - bénéficiaire (rôle)' et 'bénéficiaire (rôle) - bénéficiaire (rôle)'</span>. Le lien 'émetteur - bénéficiaire (rôle)' relie les émetteurs aux rôles des bénéficiaires ayant reçu des montants de ces émetteurs. Le lien 'bénéficiaire (rôle) - bénéficiaire (rôle)' relie les bénéficiaires avec le même rôle ayant participé à la même transaction.<br>
                <span class='bold-italic'>L'épaisseur du lien sur le graphique représente le nombre de transactions entre les deux nœuds.</span> Dans le cas des liens 'émetteur (rôle) - bénéficiaire (rôle)', il s'agit du nombre de transactions auxquelles des bénéficiaires avec le même rôle ont participé avec le même émetteur. Dans le cas des liens 'bénéficiaire (rôle) - bénéficiaire (rôle)', il s'agit du nombre de transactions auxquelles les bénéficiaires avec le même rôle ont participé ensemble. Plus le nombre de transactions est élevé, plus épais sera le lien sur le graphique.<br>
                <span class='bold-italic'>La taille d'un nœud représente le montant des fonds reçus par les bénéficiaires avec le rôle donné</span>, les nœuds plus grands indiquant des montants plus élevés. Dans les deux cas de liens ('émetteur (rôle) - bénéficiaire (rôle)' ou 'bénéficiaire (rôle) - bénéficiaire (rôle)'), il s'agit toujours du montant perçu par des bénéficiaires avec le même rôle de la part de l'émetteur. Le montant est exprimé dans la monnaie sélectionnée dans le menu déroulant 'Monnaie'.<br>
                La visualisation du réseau permet d'identifier les principaux rôles des bénéficiaires et de mieux comprendre les relations entre les entités. Elle aide à apercevoir des modèles, des clusters et des acteurs clés des transactions. La visualisation du réseau simplifie la compréhension des relations complexes dans les données.
            </div>"
          )
        ), # END: tabPanel

        
        
        # "Rôle par émetteur" tab
        # ----------------------------------------
        
        tabPanel(
          title = "Rôle par émetteur",
          downloadButton(outputId = "export_jpg_by_role_by_emitter", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_role_by_emitter", label = "Export CSV"),
          plotOutput(outputId = "role_by_emitter"),
          DT::DTOutput(outputId = "table_data_by_role_by_emitter"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique montre les occurrences de différents rôles pour chaque émetteur. Chaque groupe de barres représente un émetteur, et chaque barre de couleur différente représente un rôle. On peut voir quels rôles sont les plus fréquemment mentionnés pour chaque émetteur et comparer les mentions des différents rôles entre les émetteurs.<br>
              - L'axe des X répertorie les émetteurs.<br>
              - L'axe des Y indique le nombre total d'occurrences de chaque rôle.<br>
              - Les différentes couleurs des barres représentent différents rôle.<br>
              - La hauteur de chaque barre colorée indique le nombre de fois que le rôle est mentionné pour l'émetteur correspondant.</div>"
          )
        ), # END: tabPanel
        
        
        
        # "Rôle par document" tab
        # ----------------------------------------
        
        tabPanel(
          title = "Rôle par document",
           HTML("<div class='info-box simple'><p>Ce type de graphique peut être très riche, mais l'affichage de nombreux éléments en même temps peut le rendre illisible. Pour une meilleure lisibilité, il est recommandé de limiter le nombre d'éléments à afficher (seulement quelques documents et rôles).</p></div>"),
          downloadButton(outputId = "export_jpg_by_role_by_document", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_role_by_document", label = "Export CSV"),
          plotOutput(outputId = "role_by_document"),
          DT::DTOutput(outputId = "table_data_by_role_by_document"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique présente les occurrences de divers rôles dans différents documents. Chaque groupe de barres représente un document, et chaque barre de couleur différente représente un rôle. On peut identifier les documents qui mentionnent des rôles spécifiques le plus fréquemment et comparer la mention de différents rôles dans différents documents.<br>
                - L'axe des X répertorie les documents.<br>
                - L'axe des Y montre le nombre total d'occurrences de chaque rôle.<br>
                - Les différentes couleurs des barres représentent différents rôle.<br>
                - La hauteur de chaque barre colorée indique le nombre de fois que le rôle apparaît dans le document correspondant.</div>"
          )
        ), # END: tabPanel
        
        
        
        # "Rôle par rubrique" tab
        # ----------------------------------------
        
        tabPanel(
          title = "Rôle par rubrique",
          HTML("<div class='info-box simple'><p>Ce type de graphique peut être très riche, mais l'affichage de nombreux éléments en même temps peut le rendre illisible. Pour une meilleure lisibilité, il est recommandé de limiter le nombre d'éléments à afficher (seulement quelques rubriques et rôles).</p></div>"),
          downloadButton(outputId = "export_jpg_by_role_by_rubric", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_role_by_rubric", label = "Export CSV"),
          plotOutput(outputId = "role_by_rubric"),
          DT::DTOutput(outputId = "table_data_by_role_by_rubric"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique illustre les occurrences de différents rôles dans diverses rubriques. Chaque groupe de barres représente une rubrique, et chaque barre de couleur différente représente un rôle. On peut observer quels rôles sont les plus fréquemment mentionnés dans chaque rubrique et comparer les mentions de différents rôles dans différentes rubriques.<br>
                - L'axe des X répertorie les rubriques.<br>
                - L'axe des Y montre le nombre total d'occurrences de chaque rôle.<br>
                - Les différentes couleurs des barres représentent différents rôle.<br>
                - La hauteur de chaque barre colorée indique le nombre de fois que le rôle apparaît dans la rubrique correspondante.</div>"
          )
        ), # END: tabPanel
        
        
        
        # "Rôle par date" tab
        # ----------------------------------------
        
        tabPanel(
          title = "Rôle par date",
          downloadButton(outputId = "export_jpg_by_role_by_date", label = "Export JPG"),
          downloadButton(outputId = "export_csv_by_role_by_date", label = "Export CSV"),
          plotOutput(outputId = "role_by_date"),
          sliderInput(
            inputId = "dateRange",
            label = NULL,
            min = as.Date(min_date),
            max = as.Date(max_date),
            value = c(as.Date(min_date), as.Date(max_date)),
            step = 1,
            width = '100%'
          ),
          DT::DTOutput(outputId = "table_data_by_role_by_date"),
          HTML("<div class='subpart-title'>Commentaire</div>"),
          HTML(
            "<div class='explanation-block'>Ce graphique suit les occurrences de différents rôles au fil du temps. Chaque ligne représente un rôle différent, indiquant la fréquence à laquelle cet rôle est mentionnée à différentes dates. On peut voir les tendances et les schémas des occurrences de rôles au fil du temps. Cela permet d'identifier les périodes pendant lesquelles certains rôle ont été mentionnées plus ou moins fréquemment.<br>
                - L'axe des X représente la chronologie (dates).<br>
                - L'axe des Y montre le nombre total d'occurrences.<br>
                - Différentes lignes représentent différents rôle.<br>
                - La position des points et les lignes qui les relient indiquent le nombre de fois qu'un rôle est mentionné à des dates précises.</div>"
          )
        ) # END: tabPanel
        

        
      ) # END: tabsetPanel
      
      
    ) # END: mainPanel
    
    # END: MAIN PAGE CONTENT
    # ----------------------------------------  

  )
)

# ----------------------------------------
# Define server logic
# ----------------------------------------

server <- function(input, output, session) {
  
  # Info: count of transactions and thier conversions
  # ------------------------------------------------------------
  
  # By person
  count_all_transactions_by_person <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Document %in% input$document,
        Rubric %in% input$rubric,
        Date >= input$dateRange[1] & Date <= input$dateRange[2],
      ) %>%
      nrow()
  })
  
  count_converted_transactions_by_person_for_selected_currency <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Document %in% input$document,
        Rubric %in% input$rubric,
        Date >= input$dateRange[1] & Date <= input$dateRange[2],
        Currency %in% input$currency
      ) %>%
      nrow()
  })
  
  count_persons_selected <- reactive({
    nodes <- filtered_network_person()$nodes
    nrow(nodes)
  })
  
  count_roles_selected <- reactive({
    nodes <- filtered_network_role()$nodes
    nrow(nodes)
  })
  
  
  # Debugging: Check the structure of the filtered data
  # observe({
  #   print(str(count_all_transactions_by_person()))
  # })  
  
  
  # Info: output the count of transactions and thier conversions
  # ------------------------------------------------------------
  
  generate_information_by_person <- function() {
    all_count <- count_all_transactions_by_person()
    converted_count <- count_converted_transactions_by_person_for_selected_currency()
    persons_selected <- count_persons_selected()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions avec les bénéficiaires pour les émetteurs, documents, rubriques et dates sélectionnés: <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions avec les bénéficiaires.</p>",
      "<p>Nombre total des personnes selectionnées: <span class='data'>", persons_selected, "</span></p>",
      "</div>"
    ))
  }
  
  generate_information_by_role <- function() {
    all_count <- count_all_transactions_by_person()
    converted_count <- count_converted_transactions_by_person_for_selected_currency()
    roles_selected <- count_roles_selected()
    percentage <- if (all_count > 0) (converted_count * 100 / all_count) else 0
    
    HTML(paste(
      "<div class='info-box'>",
      "<p>Nombre total de transactions avec les bénéficiaires pour les émetteurs, documents, rubriques et dates sélectionnés: <span class='data'>", all_count, "</span></p>",
      "<p>Parmi ces transactions exprimées en monnaies différentes, <span class='data'>", converted_count, "</span> ont pu être représentées en <span class='data'>", input$currency, "</span>, ce qui correspond à <span class='data'>", round(percentage, 2), "%</span> de l'ensemble des transactions avec les bénéficiaires.</p>",
      "<p>Nombre total des rôles selectionnés: <span class='data'>", roles_selected, "</span></p>",
      "</div>"
    ))
  }

  
  output$information_by_person <- renderUI({
    generate_information_by_person()
  })
  
  output$information_by_role <- renderUI({
    generate_information_by_role()
  })
  

  
  # Buttons ("select all" etc.)
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
  
  
  # Select / deselect All button for roles
  observeEvent(input$select_all_roles, {
    updateCheckboxGroupInput(session, "person_role", selected = unique(data$Person_role))
  })
  
  observeEvent(input$deselect_all_roles, {
    updateCheckboxGroupInput(session, "person_role", selected = character(0))
  })
  
  
  
  # START: (NETWORK) Filters and network creation
  # -----------------------------------------------
  
  
  # 1) We filter result from sql query according to user inputs in Emitter, Document, Rubric, Date and Currency.
  # ------------------------------------------------------------------------------------
  
  # Filter SQL result by user inputs
  filtered_data <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Document %in% input$document,
        Rubric %in% input$rubric,
        Date >= input$dateRange[1] & Date <= input$dateRange[2],
        Currency %in% input$currency
        )
  })
  
  # Debugging: Check the structure of the filtered data
  # observe({
  #   print(str(filtered_data()))
  # })
  
  
  # 2) From previously filtered data we creat two dataframe: for edges and for nodes.
  # ------------------------------------------------------------------------------------
  
  # For Person
  # -------------
  
  # Create edges and nodes data from filtered SQL result
  edges_person <- reactive({
    req(filtered_data())
    tryCatch({
      process_edges_person(filtered_data())
    }, error = function(e) {
      # print(paste("Error in process_edges_person:", e))
      NULL
    })
  })
  
  nodes_person <- reactive({
    req(filtered_data())
    tryCatch({
      process_nodes_person(filtered_data())
    }, error = function(e) {
      # print(paste("Error in process_nodes_person:", e))
      NULL
    })
  })
  
  
  # For Role
  # -------------
  
  # Create edges and nodes data from filtered SQL result
  edges_role <- reactive({
    req(filtered_data())
    tryCatch({
      process_edges_role(filtered_data())
    }, error = function(e) {
      # print(paste("Error in process_edges_role:", e))
      NULL
    })
  })
  
  nodes_role <- reactive({
    req(filtered_data())
    tryCatch({
      process_nodes_role(filtered_data())
    }, error = function(e) {
      # print(paste("Error in process_nodes_role:", e))
      NULL
    })
  })
  
  
  
  # 3) With two previously created dataframe (for edges and nodes) we construct the whole network data in the format of visNetwork.
  # ------------------------------------------------------------------------------------------------------------------------------
  
  # Create visNetwork data from edges and nodes data
  visnetwork_data_person <- reactive({
    req(edges_person(), nodes_person())
    tryCatch({
      create_visnetwork(edges_person(), nodes_person())
    }, error = function(e) {
      # print(paste("Error in create_visnetwork:", e))
      NULL
    })
  })
  
  # Create visNetwork data from edges and nodes data
  visnetwork_data_role <- reactive({
    req(edges_role(), nodes_role())
    tryCatch({
      create_visnetwork(edges_role(), nodes_role())
    }, error = function(e) {
      # print(paste("Error in create_visnetwork:", e))
      NULL
    })
  })
  
  
  # 4) Now we can filter this network (in visNetwork format) according to user inputs in Edges types, Transaction and Amount_converted.
  # -----------------------------------------------------------------------------------------------------------------------------------
  
  # Filter network (edges & nodes) by user inputs
  filtered_network_person <- reactive({
    req(visnetwork_data_person()$edges, input$edge_type_person, input$transaction_count_person, input$amount_count_person)
    tryCatch({
      filtered_edges <- visnetwork_data_person()$edges %>%
        filter(Type %in% input$edge_type_person,
               Transaction >= input$transaction_count_person[1],
               Transaction <= input$transaction_count_person[2],
               Amount_converted >= input$amount_count_person[1],
               Amount_converted <= input$amount_count_person[2])
      
      # Get the ids of the nodes that are still present in the filtered edges
      node_ids <- unique(c(filtered_edges$from, filtered_edges$to))
      
      # Filter the nodes based on these ids
      filtered_nodes <- visnetwork_data_person()$nodes %>%
        filter(id %in% node_ids)
      
      list(nodes = filtered_nodes, edges = filtered_edges)
    }, error = function(e) {
      # print(paste("Error in filtered_network_person:", e))
      # list(nodes = data.frame(), edges = data.frame())
    })
  })
  
  
  # Filter network (edges & nodes) by user inputs
  filtered_network_role <- reactive({
    req(visnetwork_data_role()$edges, input$edge_type_role, input$transaction_count_role, input$amount_count_role)
    tryCatch({
      filtered_edges <- visnetwork_data_role()$edges %>%
        filter(Type %in% input$edge_type_role,
               Transaction >= input$transaction_count_role[1],
               Transaction <= input$transaction_count_role[2],
               Amount_converted >= input$amount_count_role[1],
               Amount_converted <= input$amount_count_role[2])
      
      # Get the ids of the nodes that are still present in the filtered edges
      node_ids <- unique(c(filtered_edges$from, filtered_edges$to))
      
      # Filter the nodes based on these ids
      filtered_nodes <- visnetwork_data_role()$nodes %>%
        filter(id %in% node_ids)
      
      list(nodes = filtered_nodes, edges = filtered_edges)
    }, error = function(e) {
      # print(paste("Error in filtered_network_role:", e))
      # list(nodes = data.frame(), edges = data.frame())
    })
  })
  
  # END: (NETWORK) Filters and network creation
  # -----------------------------------------------
  
  
  # START: (NETWORK) Observers for inputs update
  # ----------------------------------------------
  
  # The inputs in Edges Types, Transaction and Amount_converted can be shown directly because they token from final whole network.
  # So, to show this inputs, first we need to filter result from sql query according to user input (see above) and then "observe" these inputs 
  # and construct the inputs for Edges Types, Transaction and Amount_converted from constructed network (see above).
  # So, the inputs for Edges Types, Transaction and Amount_converted re-constructed each type the input for Emitter, Documents, etc, changes.
  
  
  
  # Observers for network "By Person"
  # ----------------------------------------
  
  
  # Observer to update edge_type choices
  # ----------------------------------------
  observe({
    req(visnetwork_data_person()$edges)
    
    # Create a named vector for choices
    edge_types <- unique(visnetwork_data_person()$edges$Type)
    display_names <- sapply(edge_types, function(type) {
      if (type == "Directed") {
        "émetteur - bénéficiaire"
      } else if (type == "Undirected") {
        "bénéficiaire - bénéficiaire"
      } else {
        type # Default case, if there are other types
      }
    })
    
    names(edge_types) <- display_names
    
    updateCheckboxGroupInput(session, "edge_type_person",
                             choices = edge_types,
                             selected = edge_types)
  })
  
  
  
  # Observer to update transaction_count range
  # ----------------------------------------
  observe({
    req(visnetwork_data_person()$edges)
    updateSliderInput(session, "transaction_count_person",
                      min = min(visnetwork_data_person()$edges$Transaction, na.rm = TRUE),
                      max = max(visnetwork_data_person()$edges$Transaction, na.rm = TRUE),
                      value = c(min(visnetwork_data_person()$edges$Transaction, na.rm = TRUE),
                                max(visnetwork_data_person()$edges$Transaction, na.rm = TRUE)))
  })
  
  # Observer to update amount_count range
  # ----------------------------------------
  observe({
    req(visnetwork_data_person()$edges)
    updateSliderInput(session, "amount_count_person",
                      min = min(visnetwork_data_person()$edges$Amount_converted, na.rm = TRUE),
                      max = max(visnetwork_data_person()$edges$Amount_converted, na.rm = TRUE),
                      value = c(min(visnetwork_data_person()$edges$Amount_converted, na.rm = TRUE),
                                max(visnetwork_data_person()$edges$Amount_converted, na.rm = TRUE)))
  })
  
  
  
  # Observers for network "By Role"
  # ----------------------------------------
  
  
  # Observer to update edge_type choices
  # ----------------------------------------
  observe({
    req(visnetwork_data_role()$edges)
    
    # Create a named vector for choices
    edge_types <- unique(visnetwork_data_role()$edges$Type)
    display_names <- sapply(edge_types, function(type) {
      if (type == "Directed") {
        "émetteur - bénéficiaires (rôle)"
      } else if (type == "Undirected") {
        "bénéficiaires (rôle) - bénéficiaires (rôle)"
      } else {
        type # Default case, if there are other types
      }
    })
    
    names(edge_types) <- display_names
    
    updateCheckboxGroupInput(session, "edge_type_role",
                             choices = edge_types,
                             selected = edge_types)
  })
  
  
  
  # Observer to update transaction_count range
  # ----------------------------------------
  observe({
    req(visnetwork_data_role()$edges)
    updateSliderInput(session, "transaction_count_role",
                      min = min(visnetwork_data_role()$edges$Transaction, na.rm = TRUE),
                      max = max(visnetwork_data_role()$edges$Transaction, na.rm = TRUE),
                      value = c(min(visnetwork_data_role()$edges$Transaction, na.rm = TRUE),
                                max(visnetwork_data_role()$edges$Transaction, na.rm = TRUE)))
  })
  
  # Observer to update amount_count range
  # ----------------------------------------
  observe({
    req(visnetwork_data_role()$edges)
    updateSliderInput(session, "amount_count_role",
                      min = min(visnetwork_data_role()$edges$Amount_converted, na.rm = TRUE),
                      max = max(visnetwork_data_role()$edges$Amount_converted, na.rm = TRUE),
                      value = c(min(visnetwork_data_role()$edges$Amount_converted, na.rm = TRUE),
                                max(visnetwork_data_role()$edges$Amount_converted, na.rm = TRUE)))
  })
  
  
  # END: (NETWORK) Observers for inputs update
  # ----------------------------------------------
  
  
  
  # START: (ROLE) Reactive filter data (based on user input selections)
  # --------------------------------------------------------------------
  
  
  filtered_data_for_role_by_emitter <- reactive({
    # req(input$emitter, input$rubric, input$currency) # Ensure that inputs variables is not NULL
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Person_role %in% input$person_role
      ) 
      # %>%
      # arrange(Date)  # Sort by Date in ascending order
  })
  
  
  filtered_data_for_role_by_document <- reactive({
    data %>%
      filter(
        Document %in% input$document,
        Rubric %in% input$rubric,
        Person_role %in% input$person_role
      )
  })
  
  
  
  filtered_data_for_role_by_date <- reactive({
    data %>%
      filter(
        Emitter %in% input$emitter,
        Rubric %in% input$rubric,
        Person_role %in% input$person_role,
        Date >= input$dateRange[1] & Date <= input$dateRange[2]
      )
  })
  
  

  # END: (ROLE) Reactive filter data (based on user input selections)
  # --------------------------------------------------------------------
  
  
  
  # START: (ROLE) Prepare reactive filtered datasets
  # --------------------------------------------------------------------
  
  
  # Dataset for "role by emitter"
  result_by_role_by_emitter <- reactive({
    filtered_data_for_role_by_emitter() %>%
      group_by(Emitter, Person_role) %>%
      # summarise(TotalOccurrences = n()) %>%
      # arrange(desc(TotalOccurrences))  # Sort by TotalOccurrences in descending order
      summarise(
        TotalOccurrences = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
      ) 
      # %>%
      # arrange(FirstDate)  # Sort the summary by the first date within each group
  })

  
  # Dataset for "role by documents"
  result_by_role_by_document <- reactive({
    
    summarized_data <- filtered_data_for_role_by_document() %>%
      group_by(Document, Person_role) %>%
      # summarise(TotalOccurrences = n()) %>%
      # arrange(desc(TotalOccurrences))  # Sort by TotalOccurrences in descending order
      summarise(
        TotalOccurrences = n(),
        FirstDate = min(Date),  # First date in each group (oldest date)
        LastDate = max(Date)    # Last date in each group (most recent date)
      ) 
      # %>%
      # arrange(FirstDate) 
    
    # Debug: Print the summarized data to check
    # cat("Summarized Data:\n")
    # print(summarized_data)
    
    # return(summarized_data)
  })
  
  # Dataset for "role by rubric"
  result_by_role_by_rubric <- reactive({
    filtered_data_for_role_by_emitter() %>%
      group_by(Rubric, Person_role) %>%
      summarise(TotalOccurrences = n()) %>%
      arrange(desc(TotalOccurrences)) %>% # Sort by TotalOccurrences in descending order
      mutate(Rubric = factor(Rubric, levels = custom_order_rubrics)) # Set the custom order for rubrics display
  })
  
  # Dataset for "role by date"
  result_by_role_by_date <- reactive({
    filtered_data_for_role_by_date() %>%
      group_by(Date, Person_role) %>%
      summarise(TotalOccurrences = n()) %>%
      mutate(Date = as.Date(Date))
  })
  
  
  # END: (ROLE) Prepare reactive filtered datasets
  # --------------------------------------------------------------------
  
  
  
  # Make plots: reactive expressions
  # ------------------------------------
  
  # Network for person 
  # ------------------------------------
  
  plot_by_person <- reactive({
    data <- filtered_network_person()
    req(data$nodes, data$edges)
    
    visNetwork(
      nodes = data$nodes, 
      edges = data$edges, 
      main = list(
        text = "Personnes mentionnées dans les comptes",
        style = "font-family: Verdana; color:#484848; font-size:16px; text-align:center;"
        # style = "font-family: Arial; color:#484848; font-size:15px; font-weight:bold; text-align:center;"
      ),
      footer = list(
        text = "Copyright © Ressources comptables",
        style = "font-family: Arial, sans-serif; color:#484848; font-size:12px; font-style: italic; text-align:right;"
      ),
      width = "100%",
      height = "600px"
    ) %>%
      visLegend(
        useGroups = FALSE,
        addNodes = list(
          list(label = "Bénéficiaire", color = "#2A7ACB", shape = "dot"),
          list(label = "Émetteur", color = "#F06543", shape = "dot")
        ),
        width = 0.1, position = "right"
      ) %>%
      visNodes(
        font = list(
          size = 14,  # Font size for labels
          face = "Arial",  # Font face for labels
          color = "#484848",  # Font color for labels
          bold = TRUE,  # Bold labels for emphasis
          background = "#FFFFFF",  # Background color for labels
          border = "#CCCCCC",  # Border color for labels
          borderWidth = 1  # Border width for labels
        ),
        borderWidth = 2,  # Border width for nodes
        borderWidthSelected = 3,  # Border width when selected
        color = list(
          border = "#000000",  # Default border color for all nodes
          highlight = list(border = "#FF4500"),  # Border color when highlighted
          hover = list(border = "#FF4500")  # Border color when hovered
        ),
        shadow = list(enabled = TRUE, size = 10)  # Adding shadow
      ) %>%
      visEdges(
        color = list(
          color = "#A0A0A0",  # Default edge color
          highlight = "#FF4500",  # Edge color when highlighted
          hover = "#FF4500"  # Edge color when hovered
        ),
        width = 2,  # Edge width
        smooth = FALSE
        # arrows = list(
        #   to = list(enabled = TRUE, scaleFactor = 0.5)  # Adding arrows to edges
        # ),
        # smooth = list(enabled = TRUE, type = "dynamic")  # Smooth edges
      )  %>%
      visPhysics(
        stabilization = FALSE,
        maxVelocity = 50,
        minVelocity = 0.1,
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -250)
      ) %>%
      visOptions(
        highlightNearest = TRUE, 
        nodesIdSelection = list(enabled = TRUE, main = "Trouver un bénéficiaire")
        ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visExport(
        type = "jpeg",
        name = "[Ressources comptables] - Personnes - Stats",
        label = paste0("Export JPG")
      )
    
  })
  
  
  # Network for role 
  # ------------------------------------
  
  plot_by_role <- reactive({
    data <- filtered_network_role()
    req(data$nodes, data$edges)
    
    visNetwork(
      nodes = data$nodes, 
      edges = data$edges, 
      main = list(
        text = "Rôles des bénéficiaires mentionnés dans les comptes",
        style = "font-family: Verdana; color:#484848; font-size:16px; text-align:center;"
      ),
      footer = list(
        text = "Copyright © Ressources comptables",
        style = "font-family: Arial, sans-serif; color:#484848; font-size:12px; font-style: italic; text-align:right;"
      ),
      width = "100%",
      height = "600px"
    ) %>%
      visLegend(
        useGroups = FALSE,
        addNodes = list(
          list(label = "Bénéficiaires (rôle)", color = "#2A7ACB", shape = "dot"),
          list(label = "Émetteur", color = "#F06543", shape = "dot")
        ),
        width = 0.1, position = "right"
      ) %>%
      visNodes(
        font = list(
          size = 14,  # Font size for labels
          face = "Arial",  # Font face for labels
          color = "#484848",  # Font color for labels
          bold = TRUE,  # Bold labels for emphasis
          background = "#FFFFFF",  # Background color for labels
          border = "#CCCCCC",  # Border color for labels
          borderWidth = 1  # Border width for labels
        ),
        borderWidth = 2,  # Border width for nodes
        borderWidthSelected = 3,  # Border width when selected
        color = list(
          border = "#000000",  # Default border color for all nodes
          highlight = list(border = "#FF4500"),  # Border color when highlighted
          hover = list(border = "#FF4500")  # Border color when hovered
        ),
        shadow = list(enabled = TRUE, size = 10)  # Adding shadow
      ) %>%
      visEdges(
        color = list(
          color = "#A0A0A0",  # Default edge color
          highlight = "#FF4500",  # Edge color when highlighted
          hover = "#FF4500"  # Edge color when hovered
        ),
        width = 2,  # Edge width
        smooth = FALSE
        # arrows = list(
        #   to = list(enabled = TRUE, scaleFactor = 0.5)  # Adding arrows to edges
        # ),
        # smooth = list(enabled = TRUE, type = "dynamic")  # Smooth edges
      )  %>%
      visPhysics(
        stabilization = FALSE,
        maxVelocity = 10,
        minVelocity = 0.1,
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -250)
      ) %>%
      visOptions(
        highlightNearest = TRUE, 
        nodesIdSelection = list(enabled = TRUE, main = "Trouver un rôle")
        ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visExport(
        type = "jpeg",
        name = "[Ressources comptables] - Rôles - Stats",
        label = paste0("Export JPG")
      )
    
  })
  
  
  # Groupped bar chart for "role by emitter"
  # ------------------------------------------------------------------
  
  plot_by_role_by_emitter <- reactive({
    # Get the data
    # data <- result_by_role_by_emitter()
    # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric) || is.null(input$person_role)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs, les rubriques ainsi qu'un ou plusieurs rôles pour afficher le graphique", cex = 1.3)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && !is.null(input$person_role) && nrow(result_by_role_by_emitter()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
 else { # START: else
      # If data is not empty, create the grouped bar chart
      
        ggplot(result_by_role_by_emitter(),
               aes(
                 # x = Emitter,
                 # x = reorder(Emitter, FirstDate),
                 x = str_wrap(Person_role, width = 20),
                 y = TotalOccurrences,
                 # fill = reorder(Person_role, -TotalOccurrences)
                 fill = Person_role
               )) +
          geom_bar(stat = "identity", position = "dodge") +
          # geom_text(position = position_dodge(width = 0.9), aes(label = Person_role), size = 3, vjust = -0.5) +                     
          labs(
            title = "Occurrences des rôles par émetteur",
            x = "Rôles par émetteur",
            y = "Nombre total d'occurrences",
            fill = "Rôle",
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
        # Use a consistent color scale
        scale_fill_manual(values = scales::hue_pal()(length(unique(result_by_role_by_emitter()$Person_role))), 
                          breaks = unique(result_by_role_by_emitter()$Person_role))
    } # END: else
    
  })
  
  

  # Reactive expression for groupped bar chart for "role by document"
  # ------------------------------------------------------------------
  
  plot_by_role_by_document <- reactive({
    # Check if the dataset is empty
    if (is.null(input$document) || is.null(input$rubric) || is.null(input$person_role)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les documents, les rubriques ainsi qu'un ou plusieurs rôles pour afficher le graphique", cex = 1.3)
    } 
    else if (!is.null(input$document) && !is.null(input$rubric) && !is.null(input$person_role) && nrow(result_by_role_by_document()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
  else { # START: else
    ggplot(
      result_by_role_by_document(),
      aes(
          # x = str_wrap(Document, width = 15),
          # x = reorder(Document, FirstDate),
          # x = str_wrap(reorder(Document, FirstDate), width = 15),
          # x = reorder(str_wrap(Document, width = 15), FirstDate),
          x = str_wrap(Person_role, width = 20),
          y = TotalOccurrences, 
          # fill = reorder(Person_role, -TotalOccurrences)
          fill = Person_role
      )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Occurrences des rôles par document", 
        x = "Rôles par document", 
        y = "Nombre total d'occurrences",
        fill = "Rôle",
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
        legend.position="bottom",
        legend.key.height=unit(0.4,"cm"),
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
          hjust = 1,
          family = "Helvetica"
        )
      ) +
        # Set the number of rows in the legend
        guides(fill = guide_legend(nrow = 3)) +
        facet_grid(~reorder(str_wrap(Document, width = 30), FirstDate), scales = "free_x", space = "free_x") +
        # Use a consistent color scale
        scale_fill_manual(values = scales::hue_pal()(length(unique(result_by_role_by_document()$Person_role))), 
                          breaks = unique(result_by_role_by_document()$Person_role))
    } # END: else
  })
  
  
  # Reactive expression for groupped bar chart for "role by rubric"
  # ------------------------------------------------------------------
  
  plot_by_role_by_rubric <- reactive({
    # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric) || is.null(input$person_role)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs, les rubriques ainsi qu'un ou plusieurs rôles pour afficher le graphique", cex = 1.3)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && !is.null(input$person_role) && nrow(result_by_role_by_rubric()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
 else { # START: else
      
    ggplot(result_by_role_by_rubric(),
           aes(
             # x = Rubric,
             x = str_wrap(Person_role, width = 20),
             y = TotalOccurrences,
             # fill = reorder(Person_role, -TotalOccurrences)
             fill = Person_role
           )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Occurrences des rôles par rubrique", 
        x = "Rôles par rubrique", 
        y = "Nombre total d'occurrences",
        fill = "Rôle",
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
        legend.position="bottom",
        legend.key.height=unit(0.4,"cm"),
        legend.title = element_text(
          size = 13,
          face = "bold",
          family = "Arial"
        ),
        legend.text = element_text(
          size = 10,
          family = "Helvetica"
        ),
        panel.spacing = unit(1.5, "lines"), # Adjust spacing between facet panels
        plot.caption = element_text(
          size = 11,
          face = "italic",
          hjust = 1,
          family = "Helvetica"
        )
      ) +
        # Set the number of rows in the legend
        guides(fill = guide_legend(nrow = 3)) +
        facet_grid(~Rubric, scales = "free_x", space = "free_x") +
        # Use a consistent color scale
        scale_fill_manual(values = scales::hue_pal()(length(unique(result_by_role_by_rubric()$Person_role))), 
                          breaks = unique(result_by_role_by_rubric()$Person_role))
    } # END: else
  })
  
  
  
  # Reactive expression for groupped bar chart for "role by date"
  # ------------------------------------------------------------------
  
  plot_by_role_by_date <- reactive({
        # Check if the dataset is empty
    if (is.null(input$emitter) || is.null(input$rubric) || is.null(input$person_role)) {
      # If data is empty, create a plot with a message
        plot.new()
              text(0.5, 0.5, "Sélectionnez les émetteurs, les rubriques ainsi qu'un ou plusieurs rôles pour afficher le graphique", cex = 1.3)
    } 
    else if (!is.null(input$emitter) && !is.null(input$rubric) && !is.null(input$person_role) && nrow(result_by_role_by_rubric()) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les éléments sélectionnés", cex = 1.5)
    } 
    else { # START: else
    ggplot(result_by_role_by_date(),
           aes(
             x = Date, 
             y = TotalOccurrences, 
             color = Person_role
           )) +
      geom_line() +
      labs(
        title = "Occurrences des rôles par date",
        x = "Date", 
        y = "Nombre total d'occurrences", 
        color = "Rôle",
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
        legend.key.height=unit(0.5,"cm"),
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
          hjust = 1.25,
          family = "Helvetica"
        )
      )
    } # END: else
  })
  
  
  
  
  # Make plots: generate plots
  # ------------------------------------
  
  output$network_plot_by_person <- renderVisNetwork({
    plot_by_person()
  })
  
  output$network_plot_by_role <- renderVisNetwork({
    plot_by_role()
  })
  
  output$role_by_emitter <- renderPlot({
    plot_by_role_by_emitter()
  })
  
  output$role_by_document <- renderPlot({
    plot_by_role_by_document()
  })
  
  output$role_by_rubric <- renderPlot({
    plot_by_role_by_rubric()
  })
  
  output$role_by_date <- renderPlot({
    plot_by_role_by_date()
  })
  
  
  # Make data tables: reactive expressions
  # ------------------------------------
  
  # Reactive expression for data table for "network by person"
  table_by_person <- reactive({
    data <- filtered_network_person()$edges %>%
      select(from, to, Type, Transaction, Amount_converted)%>%
    mutate(Type = ifelse(Type == "Directed", "émetteur - bénéficiaire", "bénéficiaire - bénéficiaire"))
    colnames(data) <- c(
      "Émetteur / Bénéficiaire", 
      "Bénéficiaire", 
      "Type de lien", 
      "Nombre de transactions (en commun)", 
      paste0("Montant reçu par bénéficiaire (en ", input$currency, ")")
      )
    data
  })
  
  # Reactive expression for data table for "network by role"
  table_by_role <- reactive({
    data <- filtered_network_role()$edges %>%
      select(from, to, Type, Transaction, Amount_converted)%>%
      mutate(Type = ifelse(Type == "Directed", "émetteur - bénéficiaires (rôle)", "bénéficiaires (rôle) - bénéficiaires (rôle)"))
    colnames(data) <- c(
      "Émetteur / Bénéficiaires (rôle)", 
      "Bénéficiaires (rôle)", 
      "Type de lien", 
      "Nombre de transactions (en commun)", 
      paste0("Montant reçu par bénéficiaires (en ", input$currency, ")")
    )
    data
  })
  

  # Reactive expression for data table for "role by document"
  table_by_role_by_emitter <- reactive({
    data <- result_by_role_by_emitter() %>%
      select(Emitter, Person_role, TotalOccurrences)
    # Set column names
    colnames(data) <- c("Émetteur", "Rôle", "Nombre total d'occurrences")
    data
  })
  
  
  # Reactive expression for data table for "role by document"
  table_by_role_by_document <- reactive({
    data <- result_by_role_by_document() %>%
      select(Document, Person_role, TotalOccurrences)
    colnames(data) <- c("Document", "Rôle", "Nombre total d'occurrences")
    data
  })
  
  # Reactive expression for data table for "role by rubric"
  table_by_role_by_rubric <- reactive({
    data <- result_by_role_by_rubric() %>%
      select(Rubric, Person_role, TotalOccurrences)
    colnames(data) <- c("Rubrique", "Rôle", "Nombre total d'occurrences")
    data
  })
  
  # Reactive expression for data table for "role by date"
  table_by_role_by_date <- reactive({
    data <- result_by_role_by_date() %>%
      select(Date, Person_role, TotalOccurrences)
    colnames(data) <- c("Date", "Rôle", "Nombre total d'occurrences")
    data
  })
  
  
  
  # Make data tables: generate tables
  # ------------------------------------
  
  # Data table for "network by person"
  output$table_data_by_person <- DT::renderDT({
    table_by_person()
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
  
  # Data table for "network by role"
  output$table_data_by_role <- DT::renderDT({
    table_by_role()
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
  
  
  # Data table for "role by emitter"
  output$table_data_by_role_by_emitter <- DT::renderDT({
    table_by_role_by_emitter()
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
  
  
  
  # Data table for "currency by document"
  output$table_data_by_role_by_document <- DT::renderDT({
    table_by_role_by_document()
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
  
  
  # Data table for "currency by rubric"
  output$table_data_by_role_by_rubric <- DT::renderDT({
    table_by_role_by_rubric()
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
  
  
  # Data table for "currency by date"
  output$table_data_by_role_by_date <- DT::renderDT({
    table_by_role_by_date()
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
  
  
  
  
  # (NETWORK) Export JPG and CSV
  # ------------------------------------

  # Difference between export for network and for plot, that in the export for network this no export of jpg. Indeed, export for jpg is made differently for network (throught viznetwork utilities)
  
  # Function to export both JPG and CSV
  export_data <- function(output_name,
                          table_function,
                          title) {
    
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
  export_data("by_person",
              table_by_person,
              "Réseaux des personnes")
  
  # Call the function for each export
  export_data("by_role",
              table_by_role,
              "Réseaux des personnes (par rôles)")
  
  
  
  # (ROLE) Export JPG and CSV
  # ------------------------------------
  
  # Function to export both JPG and CSV
  export_data_role <- function(output_name,
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
  export_data_role("by_role_by_emitter",
              plot_by_role_by_emitter,
              table_by_role_by_emitter,
              "Occurrences des rôles par émetteur")
  
  export_data_role("by_role_by_document",
              plot_by_role_by_document,
              table_by_role_by_document,
              "Occurrences des rôles par document")
  
  export_data_role("by_role_by_rubric",
              plot_by_role_by_rubric,
              table_by_role_by_rubric,
              "Occurrences des rôles par rubrique")
  
  export_data_role("by_role_by_date", 
              plot_by_role_by_date, 
              table_by_role_by_date, 
              "Occurrences des rôles par date")
  

}

shinyApp(ui, server)
