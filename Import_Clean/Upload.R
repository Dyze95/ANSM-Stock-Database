upload_dataframe_query <- function(data) {
  mydb <-  dbConnect(MySQL(), user = "uvrlzgjvtqvr12zf", password = "6MHaEm02gOCHGKhFpgm3",
                     dbname = "bkginxtulib6wna8ffnc", 
                     host = "bkginxtulib6wna8ffnc-mysql.services.clever-cloud.com",
                     port = 3306)
  for(i in 1:100){
    dbSendQuery(mydb, paste0("INSERT INTO `bkginxtulib6wna8ffnc`.`data` (`Laboratoire`,`CIP7`) VALUES ('Hello",i,"','",i,"')"))
    print(i)
  }
  dbDisconnect(mydb)
}

upload_dataframe <- function(data) {
  password <- readline(prompt="Password :")
  mydb <-  dbConnect(MySQL(), user = "root", password,
                     dbname = "ansm", 
                     host = "34.76.22.251",
                     port = 3306)
  dbWriteTable(mydb, name='data', value=data, overwrite=TRUE)
  dbDisconnect(mydb)
}


