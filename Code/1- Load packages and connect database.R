# Load common libraries ---------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)     # ecosystem of packages for working with "tidy" data
  
  # Set default graphics theme: minimalist, with transparent background
  theme_set(theme_minimal() + theme(rect = element_rect(fill = "transparent")))
  # use ggsave(..., bg = "transparent") to save .png or .pdf plots with transparency
  
  # Database interface packages
  library(dbplyr)        # SQL database backend for dplyr
  library(DBI)           # database interface (middleware)
  library(odbc)          # ODBC syntax database interface
})

# Access Burning Glass Oracle database @ JRC using odbc and tidyverse ----

# Connect via ODBC to JRC `scipro` Oracle database server storing Burning Glass data
# Requires `DBI` and `odbc` packages in the library 

# Create connection to SCIPRO Oracle database,
# with UID and PWD kept as secret environment settings in ~/.Renviron
con_bglass <- DBI::dbConnect(
  drv = odbc::odbc(),    # ODBC translation driver 
  dsn = "scipro",        # server name
  UID = "READ_BGLASS", # Username, also stored in ~/.Renviron
  PWD = Sys.getenv("SCIPRO_DB_PWD")  # Password, stored in ~/.Renviron
)

# Connect to MAIN table, contains individual advertisements
# The table has one record (row) for each individual ad, with unique index `JOB_ID`
bgt_main <- tbl(con_bglass, in_schema("OWN_BGLASS", "MAIN"))

# Connect to SKILLS table: multiple skills for each JOB_ID, invariant attributes assigned by BGT
bgt_skills <- tbl(con_bglass, in_schema("OWN_BGLASS", "SKILLS"))

# DEGREE table: multiple educational requirement for each JOB_ID 
bgt_degree <- tbl(con_bglass, in_schema("OWN_BGLASS", "DEGREE"))

# MAJOR table: multiple educational specialisation for each JOB_ID 
bgt_major <- tbl(con_bglass, in_schema("OWN_BGLASS", "STANDARD_MAJOR"))

# MAJOR table: multiple professional certification for each JOB_ID 
bgt_certs <- tbl(con_bglass, in_schema("OWN_BGLASS", "CERTS"))
