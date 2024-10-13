# Installation des packages nécessaires
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(readr)) install.packages("readr")
if (!require(jsonlite)) install.packages("jsonlite")

# Importation des bibliothèques
library(openxlsx)
library(readr)
library(jsonlite)

tap <- function(df) {
  resultats <- list()  # Stocker les résultats du tri à plat
  
  # Demander le chemin d'accès pour enregistrer le fichier Excel
  message("Veuillez entrer le chemin complet où vous souhaitez sauvegarder les tris à plat (ex. 'C:/mon_dossier/mon_fichier.xlsx') :")
  chemin_excel <- readline()
  
  # Créer un workbook pour stocker les résultats dans un fichier Excel
  wb <- createWorkbook()
  
  # Demander à l'utilisateur s'il veut formater la colonne Taux_remplissage en pourcentage
  message("Voulez-vous que la colonne 'Taux_remplissage' soit formatée en pourcentage dans Excel ? (o/n)")
  formater_pourcentage <- readline()
  formater_pourcentage <- tolower(formater_pourcentage) == "o"
  
  # Style pour le format pourcentage
  style_pourcentage <- createStyle(numFmt = "0.00%")
  
  # Collecte des réponses pour chaque colonne
  choix_variables <- c()  # Liste pour stocker les colonnes sélectionnées
  for (colonne in colnames(df)) {
    message(paste("Voulez-vous effectuer un tri à plat pour la variable:", colonne, "? (o/n)"))
    reponse <- readline()
    
    if (tolower(reponse) == "o") {
      choix_variables <- c(choix_variables, colonne)
    }
  }
  
  total_tris <- length(choix_variables)  # Nombre total de tris à effectuer
  
  message("\n--------------------\n--------------------")
  
  # Fonction de calcul des fréquences par colonne
  calcul_freq_col <- function(colonne) {
    freqs <- as.data.frame(table(colonne, useNA = "ifany"))
    colnames(freqs) <- c("Variable", "Fréquence")
    
    # Calcul du taux de remplissage
    freqs$Taux_Apparition_Variable <- round((freqs$Fréquence / sum(freqs$Fréquence)), 5)  # Diviser pour obtenir un format décimal
    
    # Supprimer les lignes avec des valeurs nulles ou NA
    freqs <- freqs[!is.na(freqs$Variable) & freqs$Variable != "", ]
    
    return(freqs)
  }
  
  # Traitement des variables sélectionnées
  for (i in seq_along(choix_variables)) {
    colonne <- choix_variables[i]
    
    # Demander le titre à l'utilisateur pour chaque tri à plat
    message(paste("Veuillez entrer le titre pour le tri à plat de la variable:", colonne))
    titre <- readline()
    
    # Extraire la colonne
    variable <- df[[colonne]]
    
    # Calculer les fréquences
    table_freq <- calcul_freq_col(variable)
    
    # Ajouter une ligne pour "case_vide" (valeurs manquantes) si nécessaire
    nb_vide <- sum(is.na(variable) | variable == "")
    if (nb_vide > 0) {
      table_freq <- rbind(table_freq, data.frame(Variable = "case_vide", Fréquence = nb_vide, Taux_Apparition_Variable = round(nb_vide / length(variable), 5)))
    }
    
    # Ajouter une ligne pour l'effectif total
    total_effectif <- sum(table_freq$Fréquence)
    table_freq <- rbind(table_freq, data.frame(Variable = "Total", Fréquence = total_effectif, Taux_Apparition_Variable = 1))
    
    # Ajouter le résultat dans la liste
    resultats[[colonne]] <- table_freq
    
    # Ajouter une feuille au fichier Excel pour cette variable
    addWorksheet(wb, colonne)
    
    # Insérer le titre dans A1
    writeData(wb, sheet = colonne, titre, startCol = 1, startRow = 1)
    
    # Insérer les en-têtes de colonnes sans décalage
    writeData(wb, sheet = colonne, "Variable", startCol = 1, startRow = 2)
    writeData(wb, sheet = colonne, "Fréquence", startCol = 2, startRow = 2)
    writeData(wb, sheet = colonne, "Taux_Apparition_Variable", startCol = 3, startRow = 2)
    
    # Écrire les données des fréquences et taux de remplissage à partir de A3
    writeData(wb, sheet = colonne, table_freq[, "Variable"], startCol = 1, startRow = 3, colNames = FALSE)
    writeData(wb, sheet = colonne, table_freq[, "Fréquence"], startCol = 2, startRow = 3, colNames = FALSE)
    writeData(wb, sheet = colonne, table_freq[, "Taux_Apparition_Variable"], startCol = 3, startRow = 3, colNames = FALSE)
    
    # Appliquer le style pourcentage si l'utilisateur a répondu "oui"
    if (formater_pourcentage) {
      addStyle(wb, sheet = colonne, style = style_pourcentage, cols = 3, rows = 3:(nrow(table_freq) + 2), gridExpand = TRUE)
    }
    
    # Afficher le nombre de tris à plat restants
    print(paste("Nombre de tris à plat restants:", total_tris - i))
  }
  
  # Sauvegarder le fichier Excel
  saveWorkbook(wb, chemin_excel, overwrite = TRUE)
  message(paste("Les tris à plat ont été sauvegardés dans le fichier:", chemin_excel))
  
  # Retourner la liste des résultats
  return(resultats)
}


# ----------------------------------------------
# Tutoriel d'utilisation de la fonction tap
# ----------------------------------------------
# 1. Charger des données à partir d'un fichier CSV :
#
# Exemple de chargement d'un fichier CSV :
# df <- read_csv(file = "chemin/vers/votre_fichier.csv")
#
# 2. Charger des données à partir d'un fichier Excel :
#
# Exemple de chargement d'un fichier Excel :
# df <- read.xlsx(xlsxFile = "chemin/vers/votre_fichier.xlsx", sheet = 1)  # Modifier "sheet" si nécessaire
#
# 3. Charger des données à partir d'un fichier JSON :
#
# Exemple de chargement d'un fichier JSON :
# df <- fromJSON(txt = "chemin/vers/votre_fichier.json")
#
# 4. Utilisation de la fonction tap :
# tap(df)
#
# 5. La fonction vous demandera où sauvegarder les résultats du tri à plat (un fichier Excel).
# 6. La fonction vous demandera aussi si vous souhaitez formater la colonne Taux_D'apparition_Variable en pourcentage.
# 7. Vous pourrez choisir les colonnes à inclure dans le tri à plat en répondant par 'o' ou 'n'.
# 8. Deux lignes de séparation ("-") s'afficheront avant la saisie des titres pour éviter les erreurs de saisie.
# 9. Un fichier Excel sera généré avec les résultats dans les feuilles nommées selon vos colonnes.