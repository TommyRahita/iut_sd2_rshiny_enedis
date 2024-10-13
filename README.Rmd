---
title: "GreenTech Solutions - Analyse de la Consommation Énergétique (DPE)"
author: "Tommy RAHITA, Yannick HOESCHLE, Hicham BELGHACHEM "
date: "`r Sys.Date()`"
output: html_document
---

## Introduction

Cette application Shiny permet de visualiser, analyser et mettre à jour des données de consommation énergétique. Elle propose des graphiques interactifs, des cartes géographiques, et calcule des indicateurs de performance clé (KPI). Conçue pour des gestionnaires ou experts en énergie, elle offre une interface intuitive pour explorer les données de manière approfondie.

## Installation des packages nécessaires

Pour commencer, assurez-vous que les packages suivants ("shiny", "httr", "jsonlite", "reactable", "ggplot2", "dplyr", "leaflet", "mapview", "plotly", "shinyjs", "shinydashboard") sont installés. Vous pouvez les installer en utilisant la commande suivante :


```{r Installation des packages, warning=TRUE ,eval=FALSE}
install.packages(c("shiny", "httr", "jsonlite", "reactable", "ggplot2", "dplyr", "leaflet", "mapview", "plotly", "shinyjs", "shinydashboard"))
```


## Fonctionnalités
- **Authentification** : Sécurisée via nom d'utilisateur et mot de passe. (login : user1 password : pass1)

- **Visualisations interactives** : Graphiques (camembert, nuage de points) pour analyser la répartition de la consommation et autres indicateurs.
- **Cartes interactives** : Géolocalisation des sites sur une carte (Leaflet).
- **KPI** : Calcul et affichage des indicateurs de performance énergétique (consommation totale, coût par kWh, etc.).
- **Mise à jour des données** : Interface utilisateur pour mettre à jour les fichiers de données avec suivi en temps réel.
- **Téléchargement des graphiques** : Possibilité de télécharger les visualisations sous format image.
