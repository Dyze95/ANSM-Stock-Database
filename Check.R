check_noNA <- function(data, col_name) {
  if(sum(is.na(data[,col_name])) == 0){
    print(paste0(col_name," : OK"))
  } else {
    print(paste0(col_name, " : ", sum(is.na(data[,col_name])), " NA"))
  }
}

check_allNum <- function(data, col_name) {
  if(sum(is.na(as.numeric(data[,col_name])))==0){
    print(paste0(col_name," : OK"))
  } else {
    print(paste0(col_name, " : ", sum(is.na(as.numeric(data[,col_name]))), " non-numbers"))
  }
}


checkColumnNames <- function(data) {
  col_names <- c("Date", "CIP7", "DCI", "Specialite", "Presentation", "Forme", "Dosage",
                 "Unites", "Stock")
  if(all(col_names %in% names(data))){
    print("Nom de colonnes : OK")
  } else {
    print(paste0("Colonnes manquantes", col_names[col_names %in% names(data)]))
  }
}

check_noDuplicateCIP7_sameDate <- function(data) {
  no_duplicate <- TRUE
  for(date in unique(data$Date)) {
    data_date <- data[data$Date == date,]
    if(any(duplicated(data_date$CIP7))) {
      no_duplicate <- FALSE
      print(paste0(date, " : doublons pour le CIP7 ",
                   unique(data_date$CIP7[duplicated(data_date$CIP7)])))
    }
  }
  if(no_duplicate) print("Doublons : OK")
}
