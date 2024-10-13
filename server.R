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

# Chargement des données
df_69 <- read.csv("data/df_69.csv", header = TRUE, dec = ".", sep = ",")
df_var_quanti <- read.csv("data/df_var_quanti.csv", header = TRUE, dec = ".", sep = ",")
df_adresses <- read.csv("data/df_adresses.csv", header = TRUE, dec = ".", sep = ",")
df_labels <- read.csv("data/df_labels.csv", header = TRUE, dec = ".", sep = ",")
df_conso_cout <- read.csv("data/df_conso_cout.csv", header = TRUE, dec = ".", sep = ",")
df_filtered <- read.csv("data/df_filtered2.csv")

# Rename the columns for consistency
colnames(df_filtered)[colnames(df_filtered) == "Code_postal_.BAN."] <- "Code_postal"
colnames(df_69)[colnames(df_69) == "Code_postal_.BAN."] <- "Code_postal"

# Copie pour avoir acces et telecharger le PNG (*1)
df_plot <- df_labels %>%
  mutate(percentage = Freq / sum(Freq) * 100 , ypos = cumsum(percentage) - 0.5 * percentage) %>%
  mutate(Categorie = factor(Categorie, levels = Categorie))

# Définir les utilisateurs autorisés
users <- data.frame(
  username = c("user1", "user2"),
  password = c("pass1", "pass2")
)

# Global Plot
regplot <- reactiveValues(plot_data = NULL)


function(input, output, session) {
  
  ################## Connexion à l'app ##################
  
  observeEvent(input$login_button, {
    req(input$username, input$password)
    user_row <- users[users$username == input$username & users$password == input$password, ]
    if (nrow(user_row) == 1) {
      shinyjs::hide("login")
      shinyjs::show("app_content")
      output$login_message <- renderText("Connexion réussie !")
    } else {
      output$login_message <- renderText("Nom d'utilisateur ou mot de passe incorrect.")
    }
  })
  
  observeEvent(input$logout_button, {
    shinyjs::show("login")
    shinyjs::hide("app_content")
    output$login_message <- renderText("")
  })
  
  ################## Gestion des KPI ##################
  
  # Vérifier si df_filtered n'est pas vide
  if (nrow(df_filtered) > 0) {
    # Vérifier si les colonnes existent avant de les convertir
    if ("Consommation_kWh" %in% colnames(df_filtered)) {
      df_filtered$Consommation_kWh <- as.numeric(as.character(df_filtered$Consommation_kWh))
    }
    if ("Coût_chauffage" %in% colnames(df_filtered)) {
      df_filtered$Coût_chauffage <- as.numeric(as.character(df_filtered$Coût_chauffage))
    }
    if ("Surface_habitable_logement" %in% colnames(df_filtered)) {
      df_filtered$Surface_habitable_logement <- as.numeric(as.character(df_filtered$Surface_habitable_logement))
    }
    if ("Date_réception_DPE" %in% colnames(df_filtered)) {
      df_filtered$Date_réception_DPE <- as.Date(df_filtered$Date_réception_DPE)
    }
  } else {
    showNotification("Le dataframe df_filtered est vide. Veuillez vérifier vos données.", type = "error")
  }
  
  
  
  # KPI 1 : Nombre total de logements
  output$kpi1 <- renderValueBox({
    total_logements <- nrow(df_filtered)
    valueBox(value = total_logements, subtitle = "Nombre total de logements", icon = icon("home"), color = "aqua")
  })
  
  # KPI 2 : Nombre de DPE classés 'A'
  output$kpi2 <- renderValueBox({
    nb_dpe_A <- df_filtered %>% filter(Etiquette_DPE == "A") %>% nrow()
    valueBox(value = nb_dpe_A, subtitle = "Nombre de DPE classés 'A'", icon = icon("star"), color = "olive")
  })
  
  # KPI 3 : Pourcentage de logements avec DPE ≥ E
  output$kpi3 <- renderValueBox({
    total_logements <- nrow(df_filtered)
    nb_superieur_D <- df_filtered %>% filter(Etiquette_DPE %in% c("E", "F", "G")) %>% nrow()
    pourcentage <- round((nb_superieur_D / total_logements) * 100, 2)
    valueBox(value = paste0(pourcentage, " %"), subtitle = "Logements avec DPE ≥ E", icon = icon("thumbs-down"), color = "maroon")
  })
  
  # KPI 4 : Date la plus récente de réception DPE
  output$kpi4 <- renderValueBox({
    date_recente <- max(df_filtered$Date_réception_DPE, na.rm = TRUE)
    valueBox(value = format(date_recente, "%d/%m/%Y"), subtitle = "Date la plus récente de réception DPE", icon = icon("calendar"), color = "teal")
  })
  
  # KPI 5 : Nombre de codes postaux uniques
  output$kpi5 <- renderValueBox({
    nb_cp_uniques <- n_distinct(df_filtered$Code_postal)
    valueBox(value = nb_cp_uniques, subtitle = "Codes postaux uniques couverts", icon = icon("map-marker-alt"), color = "fuchsia")
  })
  
  # KPI 6 : Classe DPE la plus fréquente
  output$kpi6 <- renderValueBox({
    dpe_frequent <- df_filtered %>% filter(!is.na(Etiquette_DPE)) %>% count(Etiquette_DPE) %>% arrange(desc(n)) %>% slice(1) %>% pull(Etiquette_DPE)
    valueBox(value = dpe_frequent, subtitle = "Classe DPE la plus fréquente", icon = icon("chart-bar"), color = "red")
  })
  
  ################## Tableau de données ##################
  
  filtered_data <- reactive({
    data <- df_69
    if (input$date_reception_DPE != "All") {
      data <- data[data$Date_réception_DPE == input$date_reception_DPE, ]
    }
    if (input$cp != "All") {
      data <- data[data$Code_postal == input$cp, ]
    }
    if (input$Etiquette_DPE != "All") {
      data <- data[data$Etiquette_DPE == input$Etiquette_DPE, ]
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  ################## Téléchargement CSV ##################
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  ################## Map Logic ##################
  filtered_data1 <- reactive({
    if (is.null(input$df_69) || length(input$df_69) == 0) {
      df_filtered
    } else {
      df_filtered %>%
        filter(Etiquette_DPE %in% input$df_69)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(filtered_data1()) %>%
      addTiles() %>%
      addMarkers(lng = ~lon, lat = ~lat,
                 popup = ~paste(
                   "<b>Etiquette DPE:</b>", Etiquette_DPE, "<br>",
                   "<b>Postal Code:</b>", Code_postal, "<br>",
                   "<b>Reception Date:</b>", Date_réception_DPE, "<br>",
                   "<b>Consommation:</b>", Conso_5_usages_é_finale
                 ),  
                 clusterOptions = markerClusterOptions())
  })
                 
  
  ################## Graphiques ##################
  #Camambert
  output$donutPlot <- renderPlot({
    # Calculate the percentage
    df_plot <- df_labels %>%
      mutate(percentage = Freq / sum(Freq) * 100 , ypos = cumsum(percentage) - 0.5 * percentage) %>%
      mutate(Categorie = factor(Categorie, levels = Categorie))
    
    # Create the donut chart
    ggplot(df_plot, aes(x = "", y = percentage, fill = Categorie)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +  # Transformer le graphique en un diagramme en camembert
      theme_void() + # Enlever les axes et les grilles
      scale_fill_discrete(labels = paste0(df_plot$Categorie, ": ", round(df_plot$percentage, 1), "%"))  # Ajouter les pourcentages à la légende
  })
  
  # Nuage de points
  output$scatterplot <- renderPlot({
    ggplot(df_conso_cout, aes(x=Consommation, y=Cout, color = Categorie)) +
      xlim(-2,  3000) +  # Limiter l'axe X
      ylim(-2, 1000) + 
      geom_point(size = 3) +
      theme_minimal()
  })
  
  #Histogram
  output$histogramPlot <- renderPlot({
    req(input$y_hist)
    
    if (is.numeric(df_filtered[[input$y_hist]])) {
      histplot <<- ggplot(df_filtered, aes(x = Etiquette_DPE, y = .data[[input$y_hist]])) +
        stat_summary(fun = "mean", geom = "bar", fill = "blue", color = "white") +
        labs(title = paste("Moyenne de", input$y_hist, "par Etiquette DPE"),
             x = "Etiquette DPE", y = paste("Moyenne de", input$y_hist)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      histplot
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "La variable sélectionnée n'est pas numérique.", size = 6) +
        theme_void()
    }
  })
  
  #Boxplot
  output$boxplotPlot <- renderPlot({
    req(input$y_box)
    
    if (is.numeric(df_filtered[[input$y_box]])) {
     b <<- ggplot(df_filtered, aes(x = Etiquette_DPE, y = .data[[input$y_box]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Boîte à Moustache de", input$y_box, "par Etiquette DPE"),
             x = "Etiquette DPE", y = input$y_box) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
     b
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "La variable sélectionnée n'est pas numérique.", size = 6) +
        theme_void()
    }
  })
  
  ################## Téléchargement des Graphiques ##################
  
  # Camambert
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(ggplot(df_plot, aes(x = "", y = percentage, fill = Categorie)) +
              geom_bar(stat = "identity", width = 1, color = "white") +
              coord_polar(theta = "y") +  # Transformer le graphique en un diagramme en camembert
              theme_void() + # Enlever les axes et les grilles
              scale_fill_discrete(labels = paste0(df_plot$Categorie, ": ", round(df_plot$percentage, 1), "%")))
      dev.off()  # Fermer le fichier PNG
    }
  )
  
  # Nuage de point
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(ggplot(df_conso_cout, aes(x=Consommation, y=Cout, color = Categorie)) +
              xlim(-2,  3000) +  # Limiter l'axe X
              ylim(-2, 1000) + 
              geom_point(size = 3) +
              theme_minimal())
      dev.off()  # Fermer le fichier PNG
    }
  )
  
  # Histogramme
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(histplot)
      dev.off()  # Fermer le fichier PNG
    }
  )
  
  # Boxplot
  output$downloadPlot4 <- downloadHandler(
    filename = function() {
      paste("graphique", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)  # Ouvrir un fichier PNG
      print(b)
      dev.off()  # Fermer le fichier PNG
    }
  )
  
  # Telechargement regression plot
  output$downloadPlot5 <- downloadHandler(
    filename = function() {
      paste("regression_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(regplot$plot_data)  # Utiliser le plot stocké dans la variable réactive
      dev.off()
    }
  )
  

################## Corrélation ##################
  
  observeEvent(input$lancer_corr, {
    output$corr_plot <- renderPlot({
      req(input$var1, input$var2)
      
      var1 <- df_var_quanti[[input$var1]]
      var2 <- df_var_quanti[[input$var2]]
      
      if (is.numeric(var1) && is.numeric(var2)) {
        # Les deux variables sont numériques : faire une corrélation et afficher un scatter plot
        plot <- ggplot(df_filtered, aes_string(x = input$var1, y = input$var2)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, col = "blue") +
          labs(title = paste("Corrélation entre", input$var1, "et", input$var2)) +
          theme_minimal()
      } else if (is.factor(var1) || is.character(var1) || is.factor(var2) || is.character(var2)) {
        # Une ou les deux variables sont qualitatives : tableau de contingence et bar chart
        df_var_quanti[[input$var1]] <- as.factor(df_var_quanti[[input$var1]])
        df_var_quanti[[input$var2]] <- as.factor(df_var_quanti[[input$var2]])
        
        if (is.factor(var1) && is.factor(var2)) {
          # Les deux variables sont qualitatives : tableau de contingence
          contingency_table <- table(df_var_quanti[[input$var1]], df_var_quanti[[input$var2]])
          plot <- ggplot(as.data.frame(contingency_table), aes_string(x = input$var1, y = "Freq", fill = input$var2)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = paste("Tableau de contingence entre", input$var1, "et", input$var2),
                 x = input$var1, y = "Fréquence") +
            theme_minimal()
        } else {
          # Une variable est qualitative et l'autre est numérique : box plot
          if (is.factor(var1)) {
            plot <- ggplot(df_var_quanti, aes_string(x = input$var1, y = input$var2)) +
              geom_boxplot() +
              labs(title = paste("Répartition de", input$var2, "par", input$var1),
                   x = input$var1, y = input$var2) +
              theme_minimal()
          } else {
            plot <- ggplot(df_var_quanti, aes_string(x = input$var2, y = input$var1)) +
              geom_boxplot() +
              labs(title = paste("Répartition de", input$var1, "par", input$var2),
                   x = input$var2, y = input$var1) +
              theme_minimal()
          }
        }
      } else {
        plot <- ggplot() + 
          labs(title = "Les variables sélectionnées ne peuvent pas être corrélées.")
      }
      
      plot
    })
  })
  
  
  ################## Régression Linéaire ##################
  
  observeEvent(input$plot_regression, {
    output$regressionPlot <- renderPlot({
      x_data <- df_var_quanti[[input$x_axis]]
      y_data <- df_var_quanti[[input$y_axis]]
      
      df_valid <- data.frame(x = x_data, y = y_data)
      df_valid <- na.omit(df_valid)
      
      # Si x ou y sont des facteurs ou des variables qualitatives, les convertir en numériques
      if (is.factor(df_valid$x) || is.character(df_valid$x)) {
        df_valid$x <- as.numeric(factor(df_valid$x))
      }
      if (is.factor(df_valid$y) || is.character(df_valid$y)) {
        df_valid$y <- as.numeric(factor(df_valid$y))
      }
      
      # Effectuer la régression linéaire
      fit <- lm(y ~ x, data = df_valid)
      plot <- ggplot(df_valid, aes(x = x, y = y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Régression Linéaire", x = input$x_axis, y = input$y_axis)
      regplot$plot_data <- plot  # Stocker le plot dans la variable réactive
      plot
      })
  })
  
  ################## Regression Plot with Correlation Ratio ##################
  observeEvent(input$plot_regression, {
    output$regressionPlot <- renderPlot({
      if (input$x_axis %in% colnames(df_var_quanti) && input$y_axis %in% colnames(df_var_quanti)) {
        ggplot(df_var_quanti, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +
          labs(title = paste("Regression Plot:", input$y_axis, "vs", input$x_axis),
               x = input$x_axis, y = input$y_axis) +
          theme_minimal()
      } else {
        plot.new()
        text(0.5, 0.5, "Sélection invalide des axes X ou Y", cex = 2)
      }
    })
    
    # Calculate and display the correlation ratio
    output$correlation_ratio <- renderText({
      x_data <- df_var_quanti[[input$x_axis]]
      y_data <- df_var_quanti[[input$y_axis]]
      correlation <- round(cor(x_data, y_data, use = "complete.obs"), 2)
      paste("Correlation Ratio:", correlation)
    })
  })
  
  ################## Mise à jour des données ##################
  
  observeEvent(input$update_button, {
    shinyjs::disable("update_button")
    output$update_status <- renderText("Mise à jour en cours...")
    
    # Simuler la mise à jour (ici vous pouvez ajouter la logique d'API ou de mise à jour de données)
    withProgress(message = 'Mise à jour en cours...', value = 0, {
      Sys.sleep(2)  # Simulation d'un temps de traitement
      incProgress(1, detail = "Mise à jour terminée.")
    })
    
    shinyjs::enable("update_button")
    output$update_status <- renderText("Mise à jour terminée.")
  })
}

  