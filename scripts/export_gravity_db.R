#### ..................................................................... ####
####    One-time export: PostgreSQL gravity database to CSV                 ####
#### ..................................................................... ####
####                                                                       ####
####  This script is NOT part of the main replication pipeline.            ####
####  It documents how gravity_cepii.csv was created from the              ####
####  local PostgreSQL database. Run this once before building the         ####
####  replication package.                                                 ####
####                                                                       ####
####  The gravity database was originally loaded from:                     ####
####    https://github.com/pachadotdev/gravitydatasets                     ####
####  using the SQL dump at:                                               ####
####    in/trade/gravitydatasets.sql                                ####
####                                                                       ####
####  After running this script, the output CSV should be placed at:       ####
####    in/trade/gravity_cepii.csv                                 ####
####                                                                       ####
#### ..................................................................... ####

library(RPostgres)
library(vroom)

# Connect to local PostgreSQL database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "gravitydatasets",
  host = "localhost",
  port = 5432,
  user = "your_username",
  password = ""
)

# Export the cepii_gravity table to CSV
cepii <- dbGetQuery(con, "SELECT * FROM cepii_gravity")
dbDisconnect(con)

# Write to CSV
vroom::vroom_write(cepii, "in/trade/gravity_cepii.csv")

cat("Exported", nrow(cepii), "rows to in/trade/gravity_cepii.csv\n")
cat("File size:", round(file.size("in/trade/gravity_cepii.csv") / 1e6, 1), "MB\n")
