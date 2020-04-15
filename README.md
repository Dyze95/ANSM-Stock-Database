# Fichiers CSV

column_names.csv : Traduction des noms de colonnes vus lors de l'import depuis XLSX

shortDCI.csv : Tableau de correspondance entre DCI long et DCI court

shortFormes.csv : Tableau de correspondance entre Forme longue et Forme courte

CIP_Dosage.csv : Tableau de correspondance entre CIP7 et Dosage/Unites. Ce ne sont pas juste des corrections mais contient tous les CIP7 possibles. [TODO : lorsque la fonction d'import rencontre un CIP inconnu, elle propose de l'ajouter à la suite du fichier avec corrections par l'utilisateur si besoin]

missing_CIP7.csv : Tableau permettant d'attribuer un CIP7 adhoc à une présentation pour laquelle le CIP7 est manquant (cela arrive parfois que les labos ne remplissent pas le CIP7 mais uniquement la spécialité et la présentation). Ce CIP7 adhoc permet de faciliter les traitements futurs (retrouver le dosage, notamment)

# Scripts

Import.R : fonctions d'import des données à partir d'un ou plusieurs dossiers

Process.R : fonctions de retraitement basique des données (supprimer les doublons, corriger les DCI, formes et dosages, ...)

Check.R : fonctions de vérification de la qualité des données

Helpers.R : fonctions utilitaires

Main.R : exemples d'utilisation

## Fonctions de Import.R

### import_singledir
Importe les données à partir de tous les fichiers Excel d'un dossier. Les données sont ajoutées en bas du dataframe passé en argument. Les colonnes des fichiers Excel sont renommées selon le vecteur column_names passé en entrée. Si une colonne est inconnue, on demande à l'utilisateur s'il veut garder la colonne et si oui, quel nouveau nom lui donner.

Arguments\
import_path : chemin vers le dossier\
data : dataframe auquel ajouter les données\
column_names : vecteur initial de renommage des colonnes (peut-être vide)

Retour\
liste de deux élements\
[[1]] : dataframe final contenant les données\
[[2]] : vecteur final de renommage des colonnes

### import_multidir
Importe les données de plusieurs dossiers (voir import_singledir). Le chemin donné en argument doit être celui d'un dossier contenant des sous-dossiers. Les sous-dossiers contiennent les fichiers Excel à importer et seront traités un par un.\
Exemple d'arborescence :\
dossier/\
-->sous-dossier-1/\
---->fichier_A.xlsx\
---->fichier_B.xlsx\
-->sous-dossier-2/\
---->fichier_C.xlsx\
---->fichier_D.xlsx\

Arguments\
import_path : chemin vers le dossier\
data : dataframe auquel ajouter les données\
column_names : vecteur initial de renommage des colonnes (peut-être vide)

Retour\
liste de deux élements\
[[1]] : dataframe final contenant les données\
[[2]] : vecteur final de renommage des colonnes

## Fonction de Process.R

### shortenDCI
Raccourcit le DCI initialement de la colonne "DCI" dataframe data à l'aide du dataframe shortDCI. La premiere colonne du shortDCI contient le long DCI et la deuxieme colonne contient le court DCI correspondant. Le long DCI est conservé dans la colonne "DCI_long" du dataframe data.

Arguments\
data : dataframe de données\
shortDCI : dataframe de correspondance des DCI\

Retour\
dataframe augmenté

### shortenForme
Idem que shortenDCI pour les formes

### cleanEmptyLines
Vide les lignes inutiles du dataframe data (c'est-à-dire celles pour lesquelles la colonne Laboratoire et Specialite sont toutes les deux NA)

### correctDosage
Remplace les colonnes "Dosage" et "Unites" du dataframe data par celles du dataframe CIP_Dosage (contenant les bons dosages). Le matching est fait sur le CIP7. Les anciennes colonnes "Dosage" et "Unites" sont conservées et renommées "Dosage_wrong" et "Unites_wrong"

Arguments\
data : dataframe de données\
CIP_Dosage : dataframe de correspondance entre CIP7 et Dosage/Unites

Retour\
dataframe augmenté

### fillMissing_CIP7
Lorsque le CIP7 est manquant dans le dataframe data, la fonction tente de lui assigner un CIP7 adhoc à l'aide du dataframe missing_CIP7. Le matching est fait sur le Laboratoire, la Specialite et la Presentation.

Arguments\
data : dataframe de données\
missing_CIP7 : dataframe de correspondance entre Laboratoire/Specialite/Presentation et CIP7 adhoc

Retour\
dataframe augmenté

### remove_duplicatesCIP7_sameDate
Lorsqu'il y a des doublons de CIP7 à la même date dans la dataframe data, la fonction affiche les lignes de doublons et fait choisir à l'utilisateur quelle ligne il souhaite conserver

Arguments\
data : dataframe de données\

Retour\
dataframe nettoyé

## Fonctions de Check.R

### check_noNA
Verifie que la colonne col_name du dataframe data ne contient pas de NA

### check_allNum
Verifie que la colonne col_name du dataframe data ne contient que des nombres

### checkColumnNames
Verifie que le dataframe data contient au moins les colonnes suivantes : "Date", "CIP7", "DCI", "Specialite", "Presentation", Forme", "Dosage", "Unites", "Stock"

### check_noDuplicateCIP7_sameDate
Verifie que le dataframe data ne contient pas de doublons de CIP7 pour une même date

## Fonctions de Helpers.R

### get_mg_from_dosage
Prend en entrée une chaine de caractères répresentant un dosage et renvoie un nombre représentant la dose en mg. (Non utilisé pour l'instant, mais peut-etre utile...)

Exemples :\
"10mg" -> 10\
"10mg/mL;2mL" -> 20

