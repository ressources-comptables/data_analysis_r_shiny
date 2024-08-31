# Load necessary libraries
library(dplyr)
library(lubridate)
library(RMySQL)
library(jsonlite)

# ----------------------------------------
# Pre-processing
# ----------------------------------------

# Load database configuration from JSON file
config <- fromJSON("www/database_config.json")

# Prepare SQL Query
query <- readLines("www/exchange.sql", warn = FALSE) # read sql file
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
exchange_rates_original <- dbGetQuery(conn, query)

# Get min_date
min_date <- dbGetQuery(conn, "SELECT MIN(start_date_standardized) AS min_date FROM date")$min_date

# Get max_date
max_date <- dbGetQuery(conn, "SELECT MAX(start_date_standardized) AS max_date FROM date")$max_date


# ----------------------------------------
# Process data
# ----------------------------------------


# Exchange rates original
# ----------------------------------------


# Convert Date column to Date format
exchange_rates_original$Date <- as.Date(exchange_rates_original$Date)

# Convert Exchange_rate_value and Amount_target_converted... to numeric format
# exchange_rates_original$Exchange_rate_value <- as.numeric(exchange_rates_original$Exchange_rate_value)
# exchange_rates_original$Amount_target_converted_to_smallest_unit_of_count <- as.numeric(exchange_rates_original$Amount_target_converted_to_smallest_unit_of_count)


# Add missing columns used in triangulation dataframe to exchange_rates_original
exchange_rates_original$Anchor_currency <- NA
exchange_rates_original$Triangulation_exchange_rates_ids <- NA

# Make a list of all unique currencies present in original exchange rates (to use when make triangulation)
all_unique_currencies <- unique(c(exchange_rates_original$Currency_source, exchange_rates_original$Currency_target))


# Exchange rates reconstructed
# ----------------------------------------


# Defin the dataframe for exchange rates reconstructed
exchange_rate_reconstructed <- data.frame(
  Id = character(),
  Currency_source = character(),
  Currency_target = character(),
  Amount_original = character(),  # Add other columns as necessary
  Exchange_rate_value = numeric(),
  Amount_target_converted_to_smallest_unit_of_count = numeric(),
  Emitter = character(),
  Document = character(),
  Rubric = character(),
  Subrubric = character(),
  Date = as.Date(character()),
  Occurrences = numeric(),
  Anchor_currency = character(),
  Triangulation_exchange_rates_ids = character(),
  stringsAsFactors = FALSE
)

# Explanation of exchange rate reconstruction

# 1) Extract values from Currency_source and Currency_target columns and create a list of unique currencies (all_unique_currencies).

# 2) First loop:
#    For each currency (denoted as Currency_source_A) in all_unique_currencies:
#    2.1) In dataframe exchange_rates_original, find all rows where Currency_source_A is present in either the Currency_source or Currency_target column.
#         Call this set set_of_pairs_currency_A.
#         Example data for illustration:
#         Id / Currency_source / Currency_target / Exchange_rate_value / Date
#         1 / dollar / Currency_source_A / 124 / 2000-01-12
#         5 / Currency_source_A / dollar / 23 / 2020-03-03
#         8 / lira / Currency_source_A / 56 / 2018-04-08
#         12 / Currency_source_A / yen / 53 / 2021-07-08

#    2.2) Second (nested) loop:
#         For each remaining currency (denoted as Currency_target_B), excluding Currency_source_A itself, from all_unique_currencies:
#         2.2.1) In dataframe exchange_rates_original, find all rows where Currency_target_B is present in either the Currency_source or Currency_target column.
#                Call this set set_of_pairs_currency_B.
#                Example data for illustration:
#                Id / Currency_source / Currency_target / Exchange_rate_value / Date
#                2 / Currency_target_B / euro / 43 / 2001-08-11
#                3 / Currency_target_B / dollar / 49 / 2003-05-03
#                17 / dollar / Currency_target_B / 92 / 2004-07-02
#                21 / yen / Currency_target_B / 112 / 2021-10-11

#         2.2.2) Identify common currencies present in both set_of_pairs_currency_A and set_of_pairs_currency_B.
#                Example sets:
#                set_of_pairs_currency_A:
#                Id / Currency_source / Currency_target / Exchange_rate_value / Date
#                1 / dollar / Currency_source_A / 124 / 2000-01-12
#                5 / Currency_source_A / dollar / 23 / 2020-03-03
#                8 / lira / Currency_source_A / 56 / 2018-04-08
#                12 / Currency_source_A / yen / 53 / 2021-07-08
#                
#                set_of_pairs_currency_B:
#                Id / Currency_source / Currency_target / Exchange_rate_value / Date
#                2 / Currency_target_B / euro / 43 / 2001-08-11
#                3 / Currency_target_B / dollar / 49 / 2003-05-03
#                17 / dollar / Currency_target_B / 92 / 2004-07-02
#                21 / yen / Currency_target_B / 112 / 2021-10-11
#
#                Common currency "dollar":
#                Id / Currency_source / Currency_target / Exchange_rate_value / Date
#                1 / dollar / Currency_source_A / 124 / 2000-01-12
#                5 / Currency_source_A / dollar / 23 / 2020-03-03
#                3 / Currency_target_B / dollar / 49 / 2003-05-03
#                17 / dollar / Currency_target_B / 92 / 2004-07-02
#
#                Common currency "yen":
#                Id / Currency_source / Currency_target / Exchange_rate_value / Date
#                12 / Currency_source_A / yen / 53 / 2021-07-08
#                21 / yen / Currency_target_B / 112 / 2021-10-11

#         2.2.3) For each set with a different common currency:
#                2.2.3.1) Take all rows where Currency_source_A is present in either the Currency_source or Currency_target column.
#                       2.2.3.1.1) For each row:
#                                 - If Currency_source_A is in the Currency_source column, use the Exchange_rate_value as is (named exchange_value_A_to_C_...(Id)) and retain Date and Id
#                                 - If Currency_source_A is in the Currency_target column, use 1 / Exchange_rate_value (named exchange_value_A_to_C_...(Id)) and retain Date and Id
#                                 Resulting values will be stored as exchange_value_A_to_C_...(Id).

#                2.2.3.2) Take all rows where Currency_target_B is present in either the Currency_source or Currency_target column.
#                       2.2.3.2.1) For each row:
#                                 - If Currency_target_B is in the Currency_source column, use the Exchange_rate_value as is (named exchange_value_B_to_C_...(Id)) and retain Date and Id
#                                 - If Currency_target_B is in the Currency_target column, use 1 / Exchange_rate_value (named exchange_value_B_to_C_...(Id)) and retain Date and Id
#                                 Resulting values will be stored as exchange_value_B_to_C_...(Id).

#                2.2.3.3) For each exchange_value_A_to_C_...(Id), find the exchange_value_B_to_C_...(Id) with the closest date, and calculate exchange_value_A_to_B as:
#                        exchange_value_A_to_B = exchange_value_A_to_C / exchange_value_B_to_C
#                        Calculate the date for exchange_value_A_to_B as the midpoint between the dates of exchange_value_A_to_C_...(Id) and exchange_value_B_to_C_...(Id).
#                        Store results in dataframe "exchange_rate_reconstructed" with columns:
#                        Id (auto-increment, format: "r." followed by number),
#                        Currency_source (Currency_source_A),
#                        Currency_target (Currency_target_B),
#                        Exchange_rate_value (exchange_value_A_to_B),
#                        Date (midpoint date),
#                        Anchor_currency (common currency),
#                        Triangulation_exchange_rates_ids (Id of exchange_value_A_to_C and exchange_value_B_to_C separated by comma).

# After processing all data, print the resulting dataframe "exchange_rate_reconstructed".



# Initialize auto-increment ID counter for the reconstructed dataframe
reconstructed_id_counter <- 1

# Process exchange rate reconstruction with triangulation
for (Currency_source_A in all_unique_currencies) {
  set_of_pairs_currency_A <- exchange_rates_original %>%
    filter(Currency_source == Currency_source_A | Currency_target == Currency_source_A)
  
  for (Currency_target_B in setdiff(all_unique_currencies, Currency_source_A)) {
    set_of_pairs_currency_B <- exchange_rates_original %>%
      filter(Currency_source == Currency_target_B | Currency_target == Currency_target_B)
    
    common_currencies <- intersect(
      unique(c(set_of_pairs_currency_A$Currency_source, set_of_pairs_currency_A$Currency_target)),
      unique(c(set_of_pairs_currency_B$Currency_source, set_of_pairs_currency_B$Currency_target))
    )
    common_currencies <- setdiff(common_currencies, c(Currency_source_A, Currency_target_B))
    
    for (common_currency in common_currencies) {
      set_A_to_C <- set_of_pairs_currency_A %>%
        filter(Currency_source == common_currency | Currency_target == common_currency)
      
      set_B_to_C <- set_of_pairs_currency_B %>%
        filter(Currency_source == common_currency | Currency_target == common_currency)
      
      for (i in 1:nrow(set_A_to_C)) {
        row_A_to_C <- set_A_to_C[i, ]
       # if (nrow(row_A_to_C) == 0) next
        
        for (j in 1:nrow(set_B_to_C)) {
          row_B_to_C <- set_B_to_C[j, ]
         # if (nrow(row_B_to_C) == 0) next
          
          if (row_A_to_C$Currency_source == Currency_source_A) {
            exchange_value_A_to_C <- row_A_to_C$Exchange_rate_value
          } else {
            exchange_value_A_to_C <- 1 / row_A_to_C$Exchange_rate_value
          }
          
          if (row_B_to_C$Currency_source == Currency_target_B) {
            exchange_value_B_to_C <- row_B_to_C$Exchange_rate_value
          } else {
            exchange_value_B_to_C <- 1 / row_B_to_C$Exchange_rate_value
          }
          
          exchange_value_A_to_B <- exchange_value_A_to_C / exchange_value_B_to_C
          date_A <- row_A_to_C$Date
          date_B <- row_B_to_C$Date
          
          # if (!is.na(date_A) && !is.na(date_B)) {
          #   date_A_to_B <- as.Date((as.numeric(date_A) + as.numeric(date_B)) / 2, origin = "1970-01-01")
          # } else {
          #   date_A_to_B <- NA
          # }
          
          date_A_to_B <- as.Date((as.numeric(date_A) + as.numeric(date_B)) / 2, origin = "1970-01-01")
          
          # Ensure all columns have the same length and no NA values
          # if (is.na(exchange_value_A_to_B) || is.na(date_A_to_B)) next
          
          # Construct row for exchange_rate_reconstructed dataframe 
          new_row <- data.frame(
            Id = paste0("r.", reconstructed_id_counter),
            Currency_source = Currency_source_A,
            Currency_target = Currency_target_B,
            Amount_original = NA,
            Exchange_rate_value = exchange_value_A_to_B,
            Amount_target_converted_to_smallest_unit_of_count = NA,
            Emitter = ifelse(date_A_to_B >= as.Date("1300-01-01") & date_A_to_B <= as.Date("1315-05-05"), "Jean XXII",
                             ifelse(date_A_to_B >= as.Date("1315-07-03") & date_A_to_B <= as.Date("1330-02-10"), "Benoît XII", NA)),
            Document = NA,
            Rubric = NA,
            Subrubric = NA,
            Date = date_A_to_B,
            Occurrences = NA,
            Anchor_currency = common_currency,
            Triangulation_exchange_rates_ids = paste(row_A_to_C$Id, row_B_to_C$Id, sep = ","),
            stringsAsFactors = FALSE
          )
          
          exchange_rate_reconstructed <- rbind(exchange_rate_reconstructed, new_row)
          reconstructed_id_counter <- reconstructed_id_counter + 1
          
          # Start: limit exchange_rate_reconstructed rows
          # Check if the number of rows in exchange_rate_reconstructed is 1000 or more
          if (nrow(exchange_rate_reconstructed) >= 10) {
            break
          }
          # End: limit exchange_rate_reconstructed rows
          
          
          
        }
      }
    }
  }
}

# Append exchange_rate_reconstructed to exchange_rates_original
exchange_rates_combined <- rbind(exchange_rates_original, exchange_rate_reconstructed)

# Sort the combined dataframe (sort by Currency_source, Currency_target, Date)
exchange_rates_combined_sorted <- exchange_rates_combined %>%
  arrange(Currency_source, Currency_target, Date)


# Print the reconstructed dataframe

# print(exchange_rates_combined_sorted)
# print(str(exchange_rates_combined_sorted))
