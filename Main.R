source("Import.R")
source("Process.R")

import_dir_path <- "Reponses"
liste_specialites_path <- "shortDCI.xlsx"
liste_formes_path <- "shortFormes.xlsx"
column_names_path <- "column_names.csv"

# Lecture du fichier de noms de colonnes
tmp <- read.csv(column_names_path)
column_names <- as.character(tmp[,2])
names(column_names) <- as.character(tmp[,1])

# Lecture du fichier de specialites
shortDCI <- read_excel(liste_specialites_path)

# Lecture du fichier de formes
shortForme <- read_excel(liste_formes_path)

# Import des donnees
data <- data.frame()
tmp <- import_multidir(import_dir_path, data, column_names)
data <- tmp[[1]]
column_names <- tmp[[2]]

# Sauvegarde des nouveaux noms de colonnes dans un CSV
write.csv(column_names, "column_names.csv")

# Processing des données
data_2 <- shortenDCI(data, shortDCI, "DCI")
data_2 <- shortenForme(data_2, shortForme, "Forme")
data_2 <- cleanEmptyLines(data_2)
  
# Check des données 