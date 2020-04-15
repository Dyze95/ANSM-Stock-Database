source("Import.R")
source("Process.R")
source("Check.R")
source("Helpers.R")

import_dir_path <- "Reponses"
liste_specialites_path <- "shortDCI.csv"
liste_formes_path <- "shortFormes.csv"
liste_dosage_path <- "CIP_Dosage.csv"
column_names_path <- "column_names.csv"

# Lecture du fichier de noms de colonnes
tmp <- read.csv(column_names_path)
column_names <- as.character(tmp[,2])
names(column_names) <- as.character(tmp[,1])

# Lecture du fichier de specialites
shortDCI <- unique(read.csv(liste_specialites_path, sep=";"))

# Lecture du fichier de formes
shortForme <- unique(read.csv(liste_formes_path, sep=";"))

# Lecture du fichier de correction des dosages
CIP_Dosage <- read.csv(liste_dosage_path, sep=";")

# Import des donnees
data <- data.frame()
tmp <- import_multidir(import_dir_path, data, column_names)
data <- tmp[[1]]
column_names <- tmp[[2]]

# Sauvegarde des nouveaux noms de colonnes dans un CSV
write.table(column_names, "column_names.csv", sep=";")

# Verification des noms de colonnes
checkColumnNames(data)

# Processing des données
data_2 <- cleanEmptyLines(data)
data_2 <- shortenDCI(data_2, shortDCI)
data_2 <- shortenForme(data_2, shortForme)
data_2 <- fillMissing_CIP7(data_2, missing_CIP7)
data_2 <- correctDosage(data_2, CIP_Dosage)
#data_2 <- remove_duplicatesCIP7_sameDate(data_2)
  
# Check des données 
check_allNum(data_2, "CIP7")
check_noNA(data_2, "DCI")
check_noNA(data_2, "Forme")
check_noNA(data_2, "Dosage")
check_allNum(data_2, "Unites")
check_noDuplicateCIP7_sameDate(data_2)

# Proposer d'ajouter un CIP7 au missing_CIP7 s'il y a encore des NA
# Proposer d'ajouter un nouveau Dosage dans le CIP_Dosage s'il y a encore des NA



