# Define the edges function
create_visnetwork <- function(edges, nodes) {
  
  
  # Convert dataframe to network graph
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  # Convert netwokr graph to visnetwork data
  visnetwork_data <- toVisNetworkData(g)
  
  
  # Process nodes
  # --------------------------------------------
  
  # Add color column based on the Function value
  visnetwork_data$nodes$color <- ifelse(visnetwork_data$nodes$Function == "beneficiary", "#2A7ACB", 
                                        ifelse(visnetwork_data$nodes$Function == "emitter", "#F06543", "gray"))
  
  
  # Calculate the size of each target node based on Amount_converted
  
  # Normalize the sizes to a range (e.g., 10 to 50)
  min_size <- 10
  max_size <- 50
  
  node_sizes <- visnetwork_data$edges %>%
    group_by(to) %>%
    summarise(size = sum(Amount_converted)) %>%
    rename(id = to) %>%
    mutate(size = min_size + (size - min(size)) * (max_size - min_size) / (max(size) - min(size)))
  
  # Merge the sizes with the nodes data frame
  visnetwork_data$nodes <- visnetwork_data$nodes %>%
    left_join(node_sizes, by = "id") %>%
    mutate(size = ifelse(is.na(size), min_size, size)) # Set a default size if no incoming edges
  
  
  
  # Process edges
  # --------------------------------------------
  
  # Normalize the Transaction values to a range (e.g., 1 to 10)
  min_width <- 1
  max_width <- 20
  
  visnetwork_data$edges <- visnetwork_data$edges %>%
    mutate(width = min_width + (Transaction - min(Transaction)) * (max_width - min_width) / (max(Transaction) - min(Transaction)))
  
  
  return(visnetwork_data)
}