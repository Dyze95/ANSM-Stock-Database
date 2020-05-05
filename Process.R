library(dplyr)

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

# correctDosage <- function(data, corrections) {
#   for(row_id in 1:nrow(corrections)) {
#     data$Dosage[data$CIP7 %in% corrections$CIP7[row_id]] <- as.character(corrections$Dosage[row_id])
#   }
#   return(data)
# }

# Supprimer les lignes ayant des NA dans la colonne Laboratoire et Specialite
cleanEmptyLines <- function(data) {
  return(data[complete.cases(data[,c("Laboratoire", "Specialite")]),])
}

correctDosage <- function(data, CIP_Dosage) {
  # Verifier si la correction a déjà été faite
  if("Dosage_wrong" %in% names(data)) {
    data$Dosage <- NULL
    data$Unites <- NULL
  } else {
    names(data)[names(data) == "Dosage"] <- "Dosage_wrong"
    names(data)[names(data) == "Unites"] <- "Unites_wrong"
  }
  
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

compute_dose_mg <- function(data) {
  sapply(data$Dosage, function(dosage) {
    if(grepl("^[0-9,.]+mg$", dosage)) {
      as.numeric(gsub(",", ".", gsub("^([0-9,.]+)mg$", "\\1", dosage)))
    } else if(grepl("^[0-9,.]+mg;[0-9,.]+mg$", dosage)) {
      as.numeric(gsub(",", ".", gsub("^([0-9,.]+)mg;[0-9,.]+mg$", "\\1", dosage)))
    } else if(grepl("^[0-9,.]+mg/m[Ll];[0-9,.]+m[Ll]$", dosage)){
      c <- as.numeric(gsub(",", ".", gsub("^([0-9,.]+)mg/m[Ll];[0-9,.]+m[Ll]$", "\\1", dosage)))
      v <- as.numeric(gsub(",", ".", gsub("^[0-9,.]+mg/m[Ll];([0-9,.]+)m[Ll]$", "\\1", dosage)))
      c * v
    } else if(grepl("^[0-9,.]+mg/m[Ll];[0-9,.]+mg/m[Ll];[0-9,.]+m[Ll]$", dosage)) {
      c <- as.numeric(gsub(",", ".", gsub("^([0-9,.]+)mg/m[Ll];[0-9,.]+mg/m[Ll];[0-9,.]+m[Ll]$", "\\1", dosage)))
      v <- as.numeric(gsub(",", ".", gsub("^[0-9,.]+mg/m[Ll];[0-9,.]+mg/m[Ll];([0-9,.]+)m[Ll]$", "\\1", dosage)))
      c * v
    } else if(grepl("^[0-9]+MUI$", dosage)) {
      as.numeric(gsub("^([0-9]+)MUI$", "\\1", dosage))
    } else if(grepl("^[0-9]+UI$", dosage)) {
      as.numeric(gsub("^([0-9]+)UI$", "\\1", dosage)) / 1e6
    } else {
      NA
    }
  })
}

compute_equiv_factor <- function(data) {
  data %>%
  group_by(DCI, Forme) %>%
  mutate(Equiv_Factor = Dose_mg / min(Dose_mg))
}

# Attention à bien vérifier que pas de doublons de dates pour un même CIP7
fill_missing_dates <- function(data) {
  unique(data$CIP7) %>%
    lapply(FUN = function(x) {
      sub_data <- data %>%
        filter(CIP7 == x)
      sub_data <- sub_data[order(sub_data$Date),]
      initial_dates <- sub_data$Date
      target_dates <- unique(data %>% filter(DCI == sub_data$DCI[1],
                                             Forme == sub_data$Forme[1],
                                             Dosage == sub_data$Dosage[1]) %>% .$Date)
      target_dates %>%
        lapply(FUN = function(date) {
          if(date %in% initial_dates) {
            filter(sub_data, Date == date)
          } else if(date < min(initial_dates)) {
            filter(sub_data, Date == min(initial_dates)) %>% mutate(Date = date)
          } else if(date > max(initial_dates)) {
            filter(sub_data, Date == max(initial_dates)) %>% mutate(Date = date)
          } else {
            index_closest <- max(which(date > initial_dates))
            interval <- as.numeric(initial_dates[index_closest + 1] - initial_dates[index_closest])
            stock_before <- sub_data$Stock[index_closest]
            stock_after  <- sub_data$Stock[index_closest + 1]
            estimated_stock <- stock_before + as.numeric(date - initial_dates[index_closest]) * (stock_after - stock_before) / interval
            filter(sub_data, Date == initial_dates[index_closest]) %>%
              mutate(Date = date, Stock = estimated_stock)
            
          }
        }) %>%
        bind_rows
    }) %>%
  bind_rows
}
