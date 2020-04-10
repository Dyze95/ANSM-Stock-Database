# Fichiers

Import.R : fonctions d'import des données à partir d'un ou plusieurs dossiers\
Process.R : fonctions de retraitement basique des données (supprimer les doublons, corriger les DCI, formes et dosages, ...)\
Check.R : fonctions de vérification de la qualité des données\
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
Raccourcit le DCI initialement dans la colonne data[col_name] et ajoute au dataframe data une nouvelle colonne "DCI" à l'aide du dataframe shortDCI. La premiere colonne du shortDCI contient le long DCI et la deuxieme colonne contient le court DCI correspondant.

Arguments\
data : dataframe de données\
shortDCI : dataframe de correspondance des DCI\
col_name : nom de colonne du dataframe data sur laquelle réaliser le merge

Retour\
dataframe augmenté

### shortenForme
Idem que shortenDCI pour les formes

### cleanEmptyLines
Vide les lignes inutiles du dataframe data (c'est-à-dire celles pour lesquelles la colonne Laboratoire et Specialite sont toutes les deux NA)

## Fichier Check.R

TBC
