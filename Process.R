# Raccourcit le DCI initialement dans la colonne data[col_name]
# Ajoute au dataframe data une nouvelle colonne "DCI" à l'aide du dataframe shortDCI
# La premiere colonne du shortDCI contient le long DCI et la deuxieme
# colonne contient le court DCI correspondant
shortenDCI <- function(data, shortDCI, col_name) {
  if(col_name == "DCI") {
    names(data)[names(data) == "DCI"] <- "DCI_long"
    col_name <- "DCI_long"
  }
  if(names(shortDCI)[1] == "DCI") {
    names(shortDCI)[1] <- "DCI_long"
  }
  names(shortDCI)[2] <- "DCI"
  
  data <- merge(x=data, y=shortDCI, by.x=col_name, by.y=names(shortDCI)[1], all.x=TRUE)
  return(data)
}

# Raccourcit la forme initialement dans la colonne data[col_name]
# Ajoute au dataframe data une nouvelle colonne "Forme" à l'aide du dataframe shortForme
# La premiere colonne du shortForme contient la longue Forme et la deuxieme
# colonne contient la courte Forme correspondante
shortenForme <- function(data, shortForme, col_name) {
  if(col_name == "Forme") {
    names(data)[names(data) == "Forme"] <- "Forme_long"
    col_name <- "Forme_long"
  }
  if(names(shortForme)[1] == "Forme") {
    names(shortForme)[1] <- "Forme_long"
  }
  names(shortForme)[2] <- "Forme"
  
  data <- merge(x=data, y=shortForme, by.x=col_name, by.y=names(shortForme)[1], all.x=TRUE)
  return(data)
}

# Supprimer les lignes ayant des NA dans la colonne Laboratoire et Specialite
cleanEmptyLines <- function(data) {
  return(data[complete.cases(data[,c("Laboratoire", "Specialite")]),])
}

#shortDCI <- read_excel("shortDCI.xlsx")

#data_2 <- shortenDCI(data, shortDCI, "DCI")
