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
  for(date in as.list(unique(data$Date))) {
    data_date <- data[data$Date == date,]
    if(any(duplicated(data_date$CIP7))) {
      no_duplicate <- FALSE
      cat(paste0(date, " : doublons pour le CIP7 ",
                   unique(data_date$CIP7[duplicated(data_date$CIP7)]), "\n"))
    }
  }
  if(no_duplicate) print("Doublons : OK")
}

check_same_DCI_Forme_Dosage_per_CIP7 <- function(data) {
  if(all(sapply(unique(data$CIP7), function(x) {
    if(nrow(distinct(data %>% filter(CIP7 == x) %>% select(DCI, Forme, Dosage, Unites))) == 1) T 
    else {
      print(x)
      F
    } 
  }))) {
    print("Cohérence des DCI / Forme / Dosage par CIP7 : OK")
  }
}

check_no_missing_dates <- function(data) {
  if(all(sapply(unique(data$CIP7), function(x) {
    sub_data <- data %>% filter(CIP7 == x)
    initial_dates <- sort(sub_data$Date)
    target_dates  <- sort(unique(data %>% filter(DCI == sub_data$DCI[1],
                                                 Forme == sub_data$Forme[1],
                                                 Dosage == sub_data$Dosage[1]) %>% .$Date))
    if(!(identical(initial_dates, target_dates))) {
      print(paste0("CIP7 ",x," : manque ",length(target_dates) - length(initial_dates), " jour(s) sur ", length(target_dates)))
      FALSE
    } else TRUE
  })))
    print("Le jeu de données est complet")
}
