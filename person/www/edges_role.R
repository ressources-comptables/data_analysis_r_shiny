# Define the edges function
process_edges_role <- function(data) {

  # 1. Create Edges Dataframe
  # ----------------------------------------
  
  
  # 1.1) Emitter to Person_role Edges
  # ----------------------------------------
  
  # Filter out rows where Emitter or Person_role is NA
  valid_data <- data %>%
    filter(!is.na(Emitter) & !is.na(Person_role))
  
  # Group by Emitter and Person_role to calculate the number of distinct Line_id and sum of Amount_converted
  edges_emitter_person <- valid_data %>%
    group_by(Emitter, Person_role) %>%
    summarise(
      Transaction = n_distinct(Line_id),
      Amount_converted = sum(Amount_converted, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Add ID_edge and Type columns
  edges_emitter_person <- edges_emitter_person %>%
    mutate(
      ID_edge = row_number(),
      Source = Emitter,
      Target = Person_role,
      Type = "Directed"
    ) %>%
    select(Source, Target, Type, Transaction, Amount_converted, ID_edge)
  
  
  # 1.2) Person_role to Person_role Edges
  # ----------------------------------------
  
  # Step 1: Filter rows with distinct Line_id having more than 1 distinct Person_role
  filtered_data <- data %>%
    filter(!is.na(Line_id) & !is.na(Person_role)) %>%
    group_by(Line_id) %>%
    filter(n_distinct(Person_role) > 1) %>%
    ungroup()
  
  # Check if filtered_data is empty (if not it will not work when there are no person-person edges)
  if (nrow(filtered_data) > 0) {
    
    # Step 2: Construct Person_role pairs
    person_pairs <- filtered_data %>%
      select(Line_id, Person_role, Amount_converted) %>%
      group_by(Line_id) %>%
      summarise(pairs = list(combn(Person_role, 2, simplify = FALSE)), Amount_converted = first(Amount_converted)) %>%
      unnest(pairs) %>%
      mutate(Source = sapply(pairs, `[`, 1),
             Target = sapply(pairs, `[`, 2),
             Type = "Undirected") %>%
      select(Source, Target, Type, Amount_converted)
    
    # Step 3: Aggregate the pairs
    aggregated_pairs <- person_pairs %>%
      group_by(Source, Target, Type) %>%
      summarise(Transaction = n(),
                Amount_converted = sum(Amount_converted)) %>%
      ungroup()
    
    # Step 4: Add unique identifier
    edges_person_person <- aggregated_pairs %>%
      filter(Source != Target) %>%  # Remove pairs where Source and Target are the same
      mutate(ID_edge = row_number() + nrow(edges_emitter_person)) %>%
      select(Source, Target, Type, Transaction, Amount_converted, ID_edge)
    
    
    # Combine Edges Dataframes
    # ----------------------------------------
    edges <- bind_rows(edges_emitter_person, edges_person_person)
    
    
  } else {
    
    # If not Person-Person edges, return only Emitter-Person edges
    # ----------------------------------------
    edges <- edges_emitter_person
    
  }
  
   return(edges)
}
