# Define the nodes function
process_nodes_person <- function(data) {
  # 2. Create Nodes Dataframe
  # ----------------------------------------
  
  # 2.1) Nodes for Person_name
  # ----------------------------------------
  
  # Check if 'Additional_participants' exists and handle it
  if (!"Additional_participants" %in% colnames(data)) {
    data <- data %>%
      mutate(Additional_participants = NA)
  }
  
  # Group by Person_name to gather associated information
  nodes_person <- data %>%
    group_by(Person_name) %>%
    summarise(
      Type = first(Person_type),
      Role = first(Person_role),
      Function = first(Person_function),
      Additional_participants = first(Additional_participants)
    ) %>%
    ungroup() %>%
    mutate(ID_node = row_number(),
           Person = Person_name) %>%
    select(Person, Type, Role, Function, Additional_participants, ID_node)
  
  
  # 2.2) Nodes for Emitters
  # ----------------------------------------
  
  
  # Distinct Emitters
  nodes_emitter <- data %>%
    filter(!is.na(Emitter)) %>%
    distinct(Emitter) %>%
    mutate(
      ID_node = row_number() + nrow(nodes_person),  # Continue ID_node numbering
      Person = Emitter,
      Type = "natural",
      Role = "pope",
      Function = "emitter",
      Additional_participants = NA
    ) %>%
    select(Person, Type, Role, Function, Additional_participants, ID_node)
  
  
  # Combine Nodes Dataframes
  
  nodes <- bind_rows(nodes_person, nodes_emitter)
  
  return(nodes)
}
