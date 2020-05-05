library(readxl)
library(plyr)

import_singledir <- function(import_path, data, column_names) {
  date_dir <- NA
  while(is.na(date_dir)) {
    date_dir <- as.Date(readline(prompt = paste0("Quelle date voulez-vous donner au dossier ",import_path," ? (DD/MM/YYYY) ")),
                        format = "%d/%m/%Y")
  }
  liste_files = list.files(import_path, pattern = "^[^~]")
  
  for (filename in liste_files) {
    file = read_excel(paste0(import_path, "/", filename))
    names(file) <- gsub("\r\n", "", names(file))
    file_columnnames = vector()
    
    for(col in names(file)) {
      if(col %in% names(column_names)) {
        if(is.na(column_names[col])) {
          file[col] = NULL
        } else {
          file_columnnames = c(file_columnnames, column_names[col])
        }
      } else {
        print(col)
        keep_col = readline(prompt=paste0("La colonne ", col, " est inconnue, faut-il la conserver ? (Y/N) "))
        if(keep_col == "Y") {
          newname = readline(prompt = "Entrez un nom pour cette nouvelle colonne : ")
          newname <- gsub("[-/ ]", ".", newname)
          column_names[col] = newname
          file_columnnames = c(file_columnnames, newname)
        } else {
          file[col] = NULL
          column_names[col] = NA
        }
      }
    }
    names(file) = file_columnnames
    file["Date"] <- date_dir
    data = rbind.fill(data, file)
  }
  
  return(list(data, column_names))
}

import_multidir <- function(import_path, data, column_names) {
  liste_dirs = list.dirs(import_path, recursive=FALSE)
  
  for(dirname in liste_dirs) {
    tmp <- import_singledir(dirname, data, column_names)
    data <- tmp[[1]]
    column_names <- tmp[[2]]
  }
  
  return(list(data, column_names))
}
