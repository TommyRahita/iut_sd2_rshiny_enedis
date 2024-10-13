library(shiny)
library(httr)
library(jsonlite)
library(reactable)
library(ggplot2)
library(jpeg)
library(dplyr)
library(bslib)
library(leaflet)
library(mapview)
library(plotly)
library(DT)
library(ggrepel)
library(shinyjs)
library(shinydashboard)

df_69 <- read.csv("data/df_69.csv", header = TRUE, dec = ".", sep = ",")
df_var_quanti <- read.csv("data/df_var_quanti.csv", header = TRUE, dec = ".", sep = ",")
df_adresses <- read.csv("data/df_adresses.csv", header = TRUE, dec = ".", sep = ",")
df_labels <- read.csv("data/df_labels.csv", header = TRUE, dec = ".", sep = ",")
df_conso_cout <- read.csv("data/df_conso_cout.csv", header = TRUE, dec = ".", sep = ",")
df_filtered <- read.csv("data/df_filtered2.csv")


# Rename the columns for consistency
colnames(df_filtered)[colnames(df_filtered) == "Code_postal_.BAN."] <- "Code_postal"
colnames(df_69)[colnames(df_69) == "Code_postal_.BAN."] <- "Code_postal"

users <- data.frame(
  username = c("user1", "user2"),
  password = c("pass1", "pass2")
)


fluidPage(
  # Theme selector
  shinythemes::themeSelector(),
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # Interface utilisateur de connexion
  div(id = "login", navbarPage(
    tagList(tags$img(src = "logo.png", height = "30px")),
    tabPanel("Connexion",
             textInput("username", "Nom d'utilisateur"),
             passwordInput("password", "Mot de passe"),
             actionButton("login_button", "Connexion"),
             textOutput("login_message")
    )
  )),
  
  # Interface après connexion
  div(id = "app_content", style = "display: none;", 
      actionButton("logout_button", "Se déconnecter"),
      navbarPage(
        tagList(tags$img(src = "logo.png", height = "30px")),
        
        # Onglet 1 : Accueil
        tabPanel("Accueil",
                 h1(id = "main-title", "Impact de la classe (DPE) sur les consommations électriques des logements du Rhône"),
                 fluidRow(
                   column(12, 
                          h2(id = "h2","Contexte"),
                          p("Avec l’accélération du changement climatique et la hausse des prix de l’énergie, la sobriété énergétique est au cœur des préoccupations des Français."),
                          br(),
                          fluidRow(
                            column(4, wellPanel(
                              h3("Données"),
                              p("Les données brutes sur la consommation énergétique des habitants du Rhône sont disponibles avec l'option de filtrer vos recherches et de les exprter sous le format csv.")
                            )),
                            column(4, wellPanel(
                              h3("Carte"),
                              p("Une cartographie interactive est disponible. Elle cartographie les habitations du rhône et affiche des indicateurs de consommations énergétique (filtre par étiquette DPE) .")
                            )),
                            column(4, wellPanel(
                              h3("KPIs"),
                              p("Des indicateurs clés de performances sont affichés et calculés en temps réel.")
                            ))
                           )
                 ),
                 column(12, 
                        br(),
                        fluidRow(
                          column(4, wellPanel(
                            h3("Statistiques"),
                            p("Des graphiques permettant l'étude statiqtique des données disponnibles.(exportaion .png)")
                          )),
                          column(4, wellPanel(
                            h3("Régression linéaire"),
                            p("Permet de réaliser des régressions linéaires sur nos données.(exportaion .png)")
                          )),
                          column(4, wellPanel(
                            h3("Corrélation"),
                            p("Permet de vérifier la corrélation entre deux varibles quantitatives.")
                          ))
                        )
                 ),
                 column(12, 
                        br(),
                        fluidRow(
                          column(4, wellPanel(
                            h3("Mise à jour"),
                            p("Permet la mise à jour des données.")
                          ))
                        )
                 )
        ), tags$div(style = "display: flex; justify-content: center; align-items: center; height: 90vh; margin: 0;",
                   img(src = "dpe_image.jpeg", height = "100%", width = "100%", style = "border: none;"))
        ),
        
        # Onglet 2 : Données
        tabPanel("Données",
                 h1("Tableau de données"),
                 fluidRow(
                   column(4, selectInput("date_reception_DPE", "Date de réception DPE:",
                                         c("All", unique(as.character(df_69$Date_réception_DPE))))),
                   column(4, selectInput("cp", "Code postal:",
                                         c("All", unique(as.character(df_69$Code_postal))))),
                   column(4, selectInput("Etiquette_DPE", "Etiquette DPE:",
                                         c("All", unique(as.character(df_69$Etiquette_DPE))))),
                   column(4, textInput("filename", "Nom de fichier :", value = "data"),
                          downloadButton("downloadData", "Télécharger CSV"))
                 ),
                 DT::dataTableOutput("table")
        ),
        
        # Onglet 3 : Map
        tabPanel("Carte Interactive", 
                 h1("Carte intéractive"),
                 sidebarPanel(selectInput("df_69", "Filter by DPE Etiquette:",
                                          choices = unique(df_69$Etiquette_DPE),
                                          selected = NULL,
                                          multiple = TRUE)),
                 mainPanel(
                   leafletOutput("map", height = 600)
                 )
        ),
        
        # Onglet 4 - KPIs
        tabPanel("KPIs",
                 h1("Indicateurs Clés de Performance"),
                 fluidRow(
                   valueBoxOutput("kpi1"),
                   valueBoxOutput("kpi2"),
                   valueBoxOutput("kpi3"),
                   valueBoxOutput("kpi4"),
                   valueBoxOutput("kpi5"),
                   valueBoxOutput("kpi6")
                 ),
        ),
        
        # Onglet 5 : Statistiques
        tabPanel("Statistiques",
                 # Affichage camambert
                 h1("Graphiques"),
                 h2(id = "h2","Proportion par étiquette DPE des habitations du rhône"),
                 plotOutput("donutPlot"),
                 downloadButton("downloadPlot1", "Télécharger le Graphique"),
                 
                 # Affichage nuage de points
                 h2(id = "h2","Cout des 5 usages énergétique par consommation : Par catégorie"),
                 plotOutput("scatterplot"),
                 downloadButton("downloadPlot2", "Télécharger le Graphique"),
                 
                 # Saut de ligne
                 br(),
                 br(),
                 
                 # Selection des données
                 selectInput("y_hist", "Selection la variable :",
                             choices = names(df_filtered)[sapply(df_filtered, is.numeric)], selected = "Consommation"),
                 # Histogram
                 h2(id = "h2","Histogramme"),
                 plotOutput("histogramPlot"),
                 downloadButton("downloadPlot3", "Télécharger le Graphique"),
                 
                 # Saut de ligne
                 br(),
                 br(),
                 
                 # Selection des données
                 selectInput("y_box", "Selection la variable :",
                             choices = names(df_filtered)[sapply(df_filtered, is.numeric)], selected = "Consommation"),
                 # Boxplot
                 h2(id = "h2","Boxplot"),
                 plotOutput("boxplotPlot"),
                 downloadButton("downloadPlot4", "Télécharger le Graphique")
        ),
        
        # Onglet 6 : Régression Plot
        tabPanel("Régression linéaire",
                 h1("Régression linéaire"),
                 sidebarPanel(
                   selectInput("x_axis", "Sélectionnez l'axe X :", choices = names(df_var_quanti)),
                   selectInput("y_axis", "Sélectionnez l'axe Y :", choices = names(df_var_quanti)),
                   actionButton("plot_regression", "Tracer la régression")
                 ),
                 mainPanel(plotOutput("regressionPlot")),
                 downloadButton("downloadPlot5", "Télécharger le Graphique")
        ),
        
        # Onglet 7 : Corrélation
        tabPanel("Corrélation",
                 h1("Corrélation"),
                 sidebarPanel(
                   selectInput("var1", "Variable 1:", choices = names(df_var_quanti)),
                   selectInput("var2", "Variable 2:", choices = names(df_var_quanti)),
                   actionButton("lancer_corr", "Lancer la Corrélation")
                 ),
                 mainPanel(plotOutput("corr_plot"))
        ),
        
        # Onglet 8 : Mise à jour
        tabPanel("Mise à jour",
                 h1("Mettre à jour les données"),
                 dateRangeInput("update_date_range", "Période:", 
                                start = max(as.Date(df_69$Date_réception_DPE)), 
                                end = Sys.Date(), 
                                min = min(as.Date(df_69$Date_réception_DPE)),
                                max = Sys.Date(),
                                format = "yyyy-mm-dd"),
                 actionButton("update_button", "Mettre à jour les données"),
                 tags$div(id = "update_status", p("Mise à jour en attente...")),
                 tags$div(id = "progress_output", p("Progression: 0%"))
        )
      )
  )
)

