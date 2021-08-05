# Access Burning Glass Oracle database @ JRC Scipro server using ODBC and tidyverse ----

library(tidyverse)
library(dbplyr) # interface between dplyr and (remote) RDMBS databases

# Connect to remote database ----

# Connect via ODBC to JRC `scipro` Oracle database server storing Burning Glass data
# Requires `DBI` and `odbc` packages in the library 
# user/schema READ_BGLASS has read-only permission
# UID = READ_BGLASS = Sys.getenv("SCIPRO_DB_UID")
# PWD = Sys.getenv("SCIPRO_DB_PWD")

library(DBI) # database interface
library(odbc) # database interface, using odbc syntax

con_bglass <- DBI::dbConnect(
  drv = odbc::odbc(),    # ODBC translation driver 
  dsn = "scipro",        # server name
  UID = "READ_BGLASS",   # username
  PWD = Sys.getenv("SCIPRO_DB_PWD") # password
)

# Show connection info
dbGetInfo(con_bglass) 

# Native ROracle driver: may be faster, but not actively maintained
# Cannot write to database! https://github.com/tidyverse/dbplyr/issues/635 
library(ROracle)
con_bglass_ro <- DBI::dbConnect(
  drv = dbDriver("Oracle"),   # native ROracle driver
  dbname = "scipro",          # server name
  username = "READ_BGLASS",   # username
  password = Sys.getenv("SCIPRO_DB_PWD") # password 
)

dbGetInfo(con_bglass_ro)


## Connecting from shell terminal:
#$ isql -v scipro [user] [password]

# List tables in database schema OWN_BGLASS
dbListTables(con_bglass, schema = "OWN_BGLASS") 

# ignore those with `$`, which are internal Oracle tables
dbListTables(con_bglass, schema = "OWN_BGLASS") %>% purrr::discard(~str_detect(., "\\$"))


# Connect to the tables on remote database ----

# Connect to MAIN table, contains individual advertisements
# The table has one record (row) for each individual ad, with unique index `JOB_ID`
bgt_main <- tbl(con_bglass, in_schema("OWN_BGLASS", "MAIN"))

# Connect to SKILLS table: all skills for each JOB_ID
bgt_skills <- tbl(con_bglass, in_schema("OWN_BGLASS", "SKILLS"))

# Preview the table from remote connection
# NB: only a preview of records are actually loaded in local memory ("lazy loading")
bgt_skills

# Copy table with unique skills in local memory
skills_count <- tbl(con_bglass, in_schema("OWN_BGLASS", "SKILLS_UNIQUE")) %>%
  collect() %>% # forces download the table from remote database into local memory 
  # encode literal "na" as proper <NA> missing value
  mutate_if(is.character, ~ na_if(., "na")) 


# Count total number of records ----
# Example: find total number of ads (number of records of main table).

# the local table preview has unknown number of records, until we force the remote database to compute!
nrow(bgt_main) 

# Force computation with collect(). Slow remote database operation!
n_ads <- bgt_main %>% tally() %>% collect() %>% as.numeric()
n_ads <- 60725519 # shortcut to value, to avoid slow computation in future


# Advanced remote database query with quoted SQL ----
bgt_main %>% filter(sql("JOB_DATE == TO_DATE('26/JAN/2016','dd/mon/yyyy')")) %>% show_query()

# for dates, supplying the date as string in this format (DD/MM/YYYY) works as well (in Oracle, at least)
bgt_main %>% filter(JOB_DATE == "01/MAR/2019") %>% show_query()

bgt_main %>% filter(JOB_DATE == "01/MAR/2019") %>% select(JOB_ID, JOB_DATE, CLEAN_JOB_TITLE) %>% collect()


# Writing idiomatic SQL queries and getting results as tibbles ----

# write SQL query, notice use of ' as string delimiter, and " in query itself (though redundant in this case)
query_simple <- 'SELECT * FROM "OWN_BGLASS"."SKILLS_UNIQUE"'

# for complex queries using both ' and " quotes, use raw stings, delimited by r"{ … }" 
# (actually there the double quotes here are redundant, but the single quotes around dates are necessary)
query_quotes <- r"{ SELECT * FROM ("OWN_BGLASS"."MAIN") WHERE (JOB_DATE == TO_DATE('26/JAN/2016','dd/mon/yyyy')) }"

# see how the query is parsed
SQL(query_quotes)

# get query results (as tibble)
dbGetQuery(con_bglass, query_simple) %>% as_tibble()

# Missing values: replace literal "na" from Oracle (found in character fields) as missing values <NA>:
dbGetQuery(con_bglass, 'SELECT * FROM OWN_BGLASS.SKILLS_UNIQUE') %>%
  as_tibble() %>%
  mutate(across(where(is.character), ~na_if(., "na")))
  
# Get (pseudo) 20 records from a 10% sample (not really random)
dbGetQuery(con_bglass, "SELECT * FROM OWN_BGLASS.SKILLS SAMPLE(10) WHERE ROWNUM <= 20") %>%
  as_tibble() %>% 
  mutate(across(where(is.character), ~na_if(., "na")))


# Query remote database by interpreting records from a local table ----

# Suppose we are interested in first five elements of variable `SKILL` in table `skills_count`
some_skills <- skills_count$SKILL[1:5] 

some_skills

# the syntax `!!some_skills` interprets the values of the object into the query sent to the remote database
bgt_skills %>% filter(SKILL == !!some_skills) %>% show_query()


# Writing tables on server ----

# Requires a connection with writing privileges,

# Copy table (example data mtcars) into server, stores in own schema
copy_to(con_bglass, mtcars, "MTCARS")

# read temporary table from server
tbl(con_bglass, in_schema(Sys.getenv("SCIPRO_DB_UID"), "MTCARS"))

# Remove table from schema (after terminating connection?)
dbRemoveTable(con_bglass, "MTCARS")








# Alternative: connection settings with ROracle and connection string ----
library(tidyverse)
library(DBI)
library(ROracle) # Native Oracle R driver, not updated for a while
library(dbplyr)

# Ensure RJDBC support for Oracle (not sure what it does)
# https://db.rstudio.com/databases/oracle/

# sql_translate_env.OraConnection <- dbplyr:::sql_translate_env.Oracle
# sql_select.OraConnection <- dbplyr:::sql_select.Oracle
# sql_subquery.OraConnection <- dbplyr:::sql_subquery.Oracle

con_oracle <- dbConnect(
  dbDriver("Oracle"),
  username = "READ_BGLASS",
  password = Sys.getenv("SCIPRO_DB_PWD"),
  dbname = "(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=s-jrcsvqdb121p.net1.cec.eu.int)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=scipro)))"
) 

# List tables in schema OWN_BGLASS
dbListTables(con_oracle, schema = "OWN_BGLASS")

# Create connection to OWN_BGLASS.MAIN
con_oracle_main <- tbl(con_oracle, in_schema("OWN_BGLASS", "MAIN"))



# Alternative: Connect with ROracle and data.table ----

# Alternative connection approach, using ROracle directly and data.table instead of tidyverse
library(ROracle)
library(DBI)
library(data.table)

# Define connection with relevant parameters
# instructed on how to handle connection errors
con <- tryCatch( {
  dbConnect(
    dbDriver("Oracle"),
    username = "READ_BGLASS",
    password = Sys.getenv("SCIPRO_DB_PWD"),
    dbname = "(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=s-jrcsvqdb121p.net1.cec.eu.int)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=scipro)))"
  ) },
  error = function(e){print(e); return(FALSE)})


# write SQL query, notice use of ' as string delimiter, and " in query itself
query_simple <- 'SELECT * FROM "OWN_BGLASS"."SKILLS_UNIQUE"'

# for complex queries using both ' and " quotes, use raw stings, delimited by r"{ … }" 
# (actually there the double quotes here are redundant, but the single quotes around dates are necessary)
query_quotes <- r"{ SELECT * FROM ("OWN_BGLASS"."MAIN") WHERE (JOB_DATE == TO_DATE('26/JAN/2016','dd/mon/yyyy')) }"

# see how the query is parsed
SQL(query_quotes)

# get query results (as data.frame)
dbGetQuery(con_bglass, query_simple) 

# Disconnect from database (unnecessary when using dbGetQuery)
dbDisconnect(con)


# Experimental: Database model representation ----
# database model management: represent and manage data in RDBMS tables (like Scipro)
library(dm) 

# Limited support for Oracle databases
con_bglass_dm <- dm_from_src(con_bglass, schema = "OWN_BGLASS")

con_bglass_dm %>% 
  dm_draw()

