# server.R

#### Server ####
server <- function(input, output, session) {
  
  # Call the server portion of our new module
  # We assign it to a variable so we can track when the sync is complete later on
  is_sync_complete <- home_server("home_init")
  
  # Optional: You can add an observer here later to switch tabs automatically
  # once `is_sync_complete()` returns TRUE
  
}