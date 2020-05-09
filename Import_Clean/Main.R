source("Import.R")
source("Process.R")
source("Check.R")
source("Upload.R")
source("Helpers.R")

import_dir_path <- "Reponses Hebdo"
liste_specialites_path <- "shortDCI.csv"
liste_formes_path <- "shortFormes.csv"
liste_dosage_path <- "CIP_Dosage.csv"
liste_missing_path <- "missing_CIP7.csv"
column_names_path <- "column_names.csv"

# Lecture du fichier de noms de colonnes
tmp <- read.csv(column_names_path, sep=";")
column_names <- as.character(tmp$x)
names(column_names) <- as.character(rownames(tmp))

# Lecture du fichier de specialites
shortDCI <- unique(read.csv(liste_specialites_path, sep=";", stringsAsFactors = F))

# Lecture du fichier de formes
shortForme <- unique(read.csv(liste_formes_path, sep=";", stringsAsFactors = F))

# Lecture du fichier de correction des dosages
CIP_Dosage <- read.csv(liste_dosage_path, sep=";", stringsAsFactors = F)

# Lecture du fichier de missing CIP7
missing_CIP7 <- read.csv(liste_missing_path, sep=";", stringsAsFactors = F)

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
data_2 <- distinct(data)
data_2 <- cleanEmptyLines(data_2)
data_2 <- shortenDCI(data_2, shortDCI)
data_2 <- shortenForme(data_2, shortForme)
data_2 <- fillMissing_CIP7(data_2, missing_CIP7)
data_2 <- correctDosage(data_2, CIP_Dosage)
data_2 <- remove_duplicatesCIP7_sameDate(data_2)

# Formatage des données
data_2$Date <- as.Date(data_2$Date)
data_2$Stock <- sapply(data_2$Stock, FUN = smart_convert_to_numeric)
check_allNum(data_2, "Stock")

check_no_missing_dates(data_2)
data_2 <- fill_missing_dates(data_2)
check_no_missing_dates(data_2)

data_2 <- compute_dose_mg(data_2)
data_2 <- compute_equiv_factor(data_2)

# Analyse des données
data_2$Stock_U <- data_2$Stock * data_2$Unites
data_2$Ventes.J.1_U <- data_2$Ventes.J.1 * data_2$Unites

data_2$Stock_U_equiv <- data_2$Stock_U * data_2$Equiv_Factor
  
# Check des données 
check_allNum(data_2, "CIP7")
check_noNA(data_2, "DCI")
check_noNA(data_2, "Forme")
check_noNA(data_2, "Dosage")
check_allNum(data_2, "Unites")
check_allNum(data_2, "Stock")
check_noDuplicateCIP7_sameDate(data_2)
check_same_DCI_Forme_Dosage_per_CIP7(data_2)
check_no_missing_dates(data_2)

# Proposer d'ajouter un CIP7 au missing_CIP7 s'il y a encore des NA
# Proposer d'ajouter un nouveau Dosage dans le CIP_Dosage s'il y a encore des NA

# Sauvegarde des données
write.table(data_2, "../database-stock_hebdo.csv", sep=";", row.names = FALSE)

# Chargement des données
data_2 <- read.csv("../database-stock.csv", sep=";", stringsAsFactors = F)
data_2$Date <- as.Date(data_2$Date)

# Upload des données
#upload_dataframe(data_2)

#data_2$dose_mg <- compute_dose_mg(data_2)

#data_3 <- data_2[,c("DCI", "Dosage", "Unites", "Stock", "Ventes J-1")]
#data_3$Stock_U <- data_3$Stock * data_3$Unites
#data_3$Ventes_U <- data_3$Ventes * data_3$Unites
#data_3 <- data_3[,c("DCI", "Dosage", "Stock_U", "Ventes_U")]
#data_3 <- NULL
  
#upload_dataframe(data_2)

#data_3 <- unique(data_2[,c("CIP7", "Laboratoire")])

#CIS <- read.delim(file = "Generate_Files/CIS_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="ISO-8859-1")
#CIP_data <- read.csv("Generate_Files/CIP_Data.csv", sep=";")

#CIP_data[grepl("CISATRACURIUM",CIP_data$DCI),]
