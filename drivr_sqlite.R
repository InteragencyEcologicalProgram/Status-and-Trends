# Query a SQLite database created by python
packages = c('DBI', 'ggplot2', 'lubridate', 'dplyr', 'dbplyr')

for (package in packages) {
    library(package, character.only=T)
}
  
db_path='C:/Users/rhartman/California Department of Water Resources/DWR - Status & Trends Reports - Strends SQLite Database/strends_11202019.sqlite.database'
# Connect to the  SQLite database file created by python
con <- dbConnect(RSQLite::SQLite(), db_path)

#generate a list of tames for each table included in the database
tablenames= dbListTables(con)


# emp field water quality data
wqf_datatbl = collect(tbl(con, tablenames[5]) )

wqf_datatblsub = wqf_datatbl[,c(2,9,11,13,15, 17)]

# emp lab water quality data
wql_datatbl = collect(tbl(con, tablenames[6]) )
wql_datatblsub = wql_datatbl[,c(1, 3,5,7,9,10)]
names(wql_datatblsub) = c("Project", "StationCode", "SampleDate", "AnalyteName", "Result", "UnitName")



WQ_all = rbind(wqf_datatblsub, wql_datatblsub)

#flow data
flow = collect(tbl(con, tablenames[7]) )
dbDisconnect(con)
