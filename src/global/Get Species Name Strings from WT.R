# Pull official species common_name strings from WildTrax database

# Attach required packages:
library(RPostgreSQL) # WildTrax is a PostgreSQL database
library(DBI)
library(glue) # Compose SQL query

library(pipeR)
library(readr)
library(dplyr)

# Path to Shared Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Establish connection to WildTrax database

wt_conn <- function(username, password) {

  conn <- DBI::dbConnect(drv = DBI::dbDriver("PostgreSQL"),
                         dbname = "wildtrax",
                         host = "prod.wildtrax.ca",
                         port = "5432",
                         user = username,
                         password = password)

  return(conn)

}

get_cam_species_name_strings <- function() {

  # Generate SQL query
  query <- glue::glue_sql(
    "SELECT * FROM common.lu_species;",
    .con = wt, # Connection has to be named 'wt'
  )

  # Send query to WildTrax
  send_query <- DBI::dbSendQuery(conn = wt, statement = query)

  # Fetch results
  x <- DBI::dbFetch(send_query) |>
    dplyr::filter(species_use_in_cam == TRUE,
                  !(species_class == "None" | species_class == "Unknown"))

  # Clear query
  DBI::dbClearResult(send_query)

  return(x)

}

# Connect to the WildTrax database
wt <- wt_conn(username = key_get("db_username", keyring = "wildtrax"),
              password = key_get("db_password", keyring = "wildtrax"))

# Retrieve data
native_sp <- get_cam_species_name_strings() %>>%
  write_csv(paste0(g_drive, "data/lookup/WildTrax_Camera_Species_Strings.csv")) %>>%
  select(species_common_name) |>
  pull()

save(native_sp, file = paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

dbDisconnect(wt)





