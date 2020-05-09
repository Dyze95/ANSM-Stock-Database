# Arborescence du Git

**Dossier Generate_Files** : Application Shiny permettant de générer des tableaux Excel de points stocks à partir d'une liste de DCI choisie par l'utilisateur. Des fonctionnalités de correction des dosages, du nombre d'unités par boîtes et des exploitants sont prévues.

**Dossier Import_Clean** : Ensemble de scripts R permettant d'importer de la donnée Excel retournée par les exploitants et de la retraiter.

**Dossier Dataviz_Stock** : Application Shiny permettant de la visaliser la donnée retraitée par les scripts du dossier Import_Clean

# Fichiers CSV du dossier Import_Clean

column_names.csv : Traduction des noms de colonnes vus lors de l'import depuis XLSX

shortDCI.csv : Tableau de correspondance entre DCI long et DCI court

shortFormes.csv : Tableau de correspondance entre Forme longue et Forme courte

CIP_Dosage.csv : Tableau de correspondance entre CIP7 et Dosage/Unites. Ce ne sont pas juste des corrections mais contient tous les CIP7 possibles. [TODO : lorsque la fonction d'import rencontre un CIP inconnu, elle propose de l'ajouter à la suite du fichier avec corrections par l'utilisateur si besoin]

missing_CIP7.csv : Tableau permettant d'attribuer un CIP7 adhoc à une présentation pour laquelle le CIP7 est manquant (cela arrive parfois que les labos ne remplissent pas le CIP7 mais uniquement la spécialité et la présentation). Ce CIP7 adhoc permet de faciliter les traitements futurs (retrouver le dosage, notamment)

# Scripts du dossier Import_Clean

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
---->fichier_D.xlsx

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
shortDCI : dataframe de correspondance des DCI

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
Lorsque le CIP7 est manquant dans le dataframe data, la fonction tente de lui assigner un CIP7 adhoc à l'aide du dataframe missing_CIP7. Le matching est fait sur le Laboratoire, le DCI, la Specialite et la Presentation.\
Attention : le matching est fait sur les DCI courts, donc attention à bien appeler la fonction fillMissing_CIP7 sur un dataframe déjà retraité par la fonction shortenDCI.

Arguments\
data : dataframe de données\
missing_CIP7 : dataframe de correspondance entre Laboratoire/DCI/Specialite/Presentation et CIP7 adhoc

Retour\
dataframe augmenté

### remove_duplicatesCIP7_sameDate
Lorsqu'il y a des doublons de CIP7 à la même date dans la dataframe data, la fonction affiche les lignes de doublons et fait choisir à l'utilisateur quelle ligne il souhaite conserver

Arguments\
data : dataframe de données

Retour\
dataframe nettoyé

### compute_dose_mg
Ajoute une nouvelle colonne Dose_mg au dataframe data correspondant à la dose en milligrammes du dosage de la spécalité. Le calcul se fait par une analyse regex de la chaine de caractère de la colonne Dosage du dataframe data. Par exemple, pour un dosage de 5mg/mL;2mL la Dose_mg sera de 10.

Arguments\
data : dataframe de données

Retour\
dataframe augmenté de la colonne Dose_mg

### compute_equiv_factor
Ajoute une nouvelle colonne Equiv_Factor au dataframe data correspondant au rapport entre la dose en milligrammes de la spécialité et la dose minimale en milligrames de toutes les spécialités de même DCI et Forme. Cette nouvelle colonne permettra de calculer des bilans de stocks agrégés par (DCI, Forme). Le calcul est fait à partir de la colonne Dose_mg du dataframe data.

Arguments\
data : dataframe de données

Retour\
dataframe augmenté de la colonne Equiv_Factor

### fill_missing_dates
Pour que les agrégations par (DCI, Forme, Dosage) soient correctes, il faut que les données de tous les CIP7 de même DCI, forme et dosage soient renseignées aux mêmes dates exactement. Cette fonction remplit les "trous", c'est-à-dire que si les données d'un CIP7 venaient à manquer pour une date, elle rajoute une ligne au dataframe pour ce CIP7 à cette date en interpôlant son stock à partir des données qu'elle possède sur ce CIP7 à d'autres dates.\
Plus précisément :\
- si la date manquante est antérieure à toutes les dates connues de ce CIP7, le stock est le même qu'à la date minimale connue.\
- si la date manquante est postérieure à toutes les dates connues de ce CIP7, le stock est le même qu'à la date maximale connue.\
- si la date manquante se situe entre deux dates connues de ce CIP7, le stock est la moyenne des stocks de ces deux dates (pondérée par l'écart en jours entre la date manquante et les dates connues).

Arguments\
data : dataframe de données

Retour\
dataframe augmenté des lignes correspondant aux dates manquantes

### smart_convert_to_numeric
Fonction permettant de convertir intelligemment une chaine de caractères (sensée représenter des stocks, des ventes, des approvisionnements) en nombre. Les espaces entre les milliers sont supprimés, les "NA", "N/A" et autres "-" sont convertis en 0. Le reste est marqué comme NA pour indiquer à l'utilisateur qu'il faut vérifier dans l'Excel source.

Arguments\
x : chaine de caractères à convertir en nombre

Retour\
un nombre ou NA

## Fonctions de Check.R

### check_noNA
Verifie que la colonne col_name du dataframe data ne contient pas de NA

### check_allNum
Verifie que la colonne col_name du dataframe data ne contient que des nombres

### checkColumnNames
Verifie que le dataframe data contient au moins les colonnes suivantes : "Date", "CIP7", "DCI", "Specialite", "Presentation", Forme", "Dosage", "Unites", "Stock"

### check_noDuplicateCIP7_sameDate
Verifie que le dataframe data ne contient pas de doublons de CIP7 pour une même date

### check_same_DCI_Forme_Dosage_per_CIP7
Verifie que pour chaque CIP7 du dataframe data, les lignes correspondantes ont le même DCI, la même forme et le même dosage.

### check_no_missing_dates
Verifie que le dataframe data ne comporte pas de dates manquantes pour un CIP7 (voir la fonction fill_missing_dates de Process.R, les dates des CIP7 de même DCI, forme et dosage doivent être strictement les mêmes).

## Fonctions de Helpers.R

### get_mg_from_dosage
[OBSOLETE depuis l'ajout de la fonction compute_dose_mg de Process.R]\
Prend en entrée une chaine de caractères répresentant un dosage et renvoie un nombre représentant la dose en mg. (Non utilisé pour l'instant, mais peut-etre utile...)

Exemples :\
"10mg" -> 10\
"10mg/mL;2mL" -> 20

