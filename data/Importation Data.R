# Install packages if not already installed
required_packages <- c("httr", "jsonlite", "dplyr", "leaflet", "ggmap", "ggplot2", "corrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(httr)
library(jsonlite)
library(leaflet)
library(ggmap)
library(ggplot2)
library(corrplot)
library(htmlwidgets)


# API Call Logic

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"

df_adresses <- read.csv("adresses-69.csv", sep = ";", dec = ".")
df_69 <- data.frame()
code <- unique(df_adresses$code_postal)

for (code_postal in code) {
  # Initialize variables
  has_more <- TRUE
  start <- 0
  size <- 100
  
  while (has_more) {
    # Prepare the request parameters
    params <- list(
      size = size,
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Identifiant__BAN,Version_DPE,Période_construction,Coût_total_5_usages,Conso_5_usages_é_finale,Emission_GES_5_usages_par_m²",
      q = code_postal,
      q_fields = "Code_postal_(BAN)",
      qs = "",
      start = start  # Add pagination using the start index
    )
    
    url_encoded <- modify_url(base_url, query = params)
    print(url_encoded)
    
    response <- GET(url_encoded)
    
    # Print response status
    print(status_code(response))
    
    # Convert content from the response to a readable format
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    
    # Check the total number of records available
    total <- content$total
    print(total)
    
    # Append new data to df_69
    df_69 <- rbind(df_69, content$result)
    
    # Check if there are more records to fetch
    start <- start + size
    if (start >= total) {
      has_more <- FALSE
    }
  }
}

nrow(df_69)
nrow(df_69) / length(code)
View(df_69)

# Select only the columns "id", "lat", and "lon" from df_adresses
df_adresses_subset <- df_adresses[, c("id", "lat", "lon")]

# Rename the "id" column in df_adresses_subset to "Identifiant__BAN" to match df_69
df_adresses_subset <- rename(df_adresses_subset, Identifiant__BAN = id)

# Merge the two dataframes based on "Identifiant__BAN"
df_merged <- merge(df_69, df_adresses_subset, by = "Identifiant__BAN", all.x = TRUE)

# Filter out rows with missing lat/lon values
df_filtered <- df_merged[complete.cases(df_merged$lat, df_merged$lon), ]
write.csv(df_filtered, file = "df_filtered.csv", row.names = FALSE)

# Create the leaflet map
map <- leaflet(df_filtered) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addMarkers(
    ~lon, ~lat,  # Define longitude and latitude columns
    popup = ~paste("Etiquette DPE:", Etiquette_DPE, "<br>",
                   "Postal Code:", `Code_postal_(BAN)`, "<br>",
                   "Reception Date:", Date_réception_DPE),  # Customize popup info
    clusterOptions = markerClusterOptions()  # Enable marker clustering
  )

#Table pour graphiques
  #camambert
df_labels <- data.frame(table(df_69$Etiquette_DPE))
#Rname colnames
df_labels <- df_labels %>%
  rename(
    Categorie = Var1
  )
  #Nuage de points
df_conso_cout <- data.frame(cbind(df_69$Conso_5_usages_é_finale,df_69$Coût_total_5_usages,df_69$Etiquette_DPE))
df_conso_cout <- df_conso_cout %>%
  rename(
    Consommation = X1,
    Cout = X2,
    Categorie = X3
  )

# Show the map
map

write.csv(df_69, file = "df_69.csv")
write.csv(df_adresses, file = "df_adresses.csv")
write.csv(df_adresses_subset, file = "df_adresses_subset.csv")
write.csv(df_filtered, file = "df_filtered.csv")
write.csv(df_merged, file = "df_merged.csv")
write.csv(df_labels, file = "df_labels.csv")
write.csv(df_conso_cout, file = "df_conso_cout.csv")
saveWidget(map, "map.html")
