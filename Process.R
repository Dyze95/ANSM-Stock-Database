# Raccourcit le DCI initialement dans la colonne data[col_name]
# Ajoute au dataframe data une nouvelle colonne "DCI" à l'aide du dataframe shortDCI
# La premiere colonne du shortDCI contient le long DCI et la deuxieme
# colonne contient le court DCI correspondant
shortenDCI <- function(data, shortDCI) {
  names(data)[names(data) == "DCI"] <- "DCI_long"
  
  names(shortDCI)[1] <- "DCI_long"
  names(shortDCI)[2] <- "DCI"
  
  data <- merge(x=data, y=shortDCI, by="DCI_long", all.x=TRUE)
  return(data)
}

# Raccourcit la forme initialement dans la colonne data[col_name]
# Ajoute au dataframe data une nouvelle colonne "Forme" à l'aide du dataframe shortForme
# La premiere colonne du shortForme contient la longue Forme et la deuxieme
# colonne contient la courte Forme correspondante
shortenForme <- function(data, shortForme) {
  names(data)[names(data) == "Forme"] <- "Forme_long"
  
  names(shortForme)[1] <- "Forme_long"
  names(shortForme)[2] <- "Forme"
  
  data <- merge(x=data, y=shortForme, by="Forme_long", all.x=TRUE)
  return(data)
}

correctDosage <- function(data, corrections) {
  for(row_id in 1:nrow(corrections)) {
    data$Dosage[data$CIP7 %in% corrections$CIP7[row_id]] <- as.character(corrections$Dosage[row_id])
  }
  return(data)
}

# Supprimer les lignes ayant des NA dans la colonne Laboratoire et Specialite
cleanEmptyLines <- function(data) {
  return(data[complete.cases(data[,c("Laboratoire", "Specialite")]),])
}

correctDosage <- function(data, CIP_Dosage) {
  names(data)[names(data) == "Dosage"] <- "Dosage_wrong"
  names(data)[names(data) == "Unites"] <- "Unites_wrong"
  data <- merge(x=data, y=CIP_Dosage, by="CIP7", all.x=TRUE)
  return(data)
}

fillMissing_CIP7 <- function(data, missing_CIP7) {
  missing_CIP7_indexes <- which(is.na(as.numeric(data$CIP7)))
  missing_CIP7_data <- data[is.na(as.numeric(data$CIP7)),]
  
  missing_CIP7_data <- join(x=missing_CIP7_data, y=missing_CIP7, by=c("Laboratoire", "DCI", "Presentation", "Specialite"), type="left")
  missing_CIP7_data$CIP7 <- missing_CIP7_data$CIP7_adhoc
  missing_CIP7_data$CIP7_adhoc <- NULL
  
  data[missing_CIP7_indexes,] <- missing_CIP7_data
  return(data)
}

remove_duplicatesCIP7_sameDate <- function(data) {
  for(date in unique(data$Date)) {
    data_date <- data[data$Date == date,]
    if(any(duplicated(data_date$CIP7))) {
      for(CIP in unique(data_date$CIP7[duplicated(data_date$CIP7)])) {
        print(paste0("Voici les doublons du ",date," pour le CIP ",CIP))
        print(data[data$Date == date & data$CIP7 == CIP,])
        indexes <- row.names(data[data$Date == date & data$CIP7 == CIP,])
        print(class(indexes))
        index_toKeep <- NA
        while(!(index_toKeep %in% indexes)) {
        index_toKeep <- readline(prompt = paste0("Quelle ligne voulez-vous conserver ? (",
                                          paste0(indexes, collapse="/"), ") "))
        }
        indexes_toDrop <- indexes[!indexes==index_toKeep]
        data <- data[!row.names(data) %in% indexes_toDrop,]
      }
    }
  }
  return(data)
}
