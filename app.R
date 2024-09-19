# Carga las bibliotecas necesarias para la aplicación Shiny
library(shiny)        # Para construir aplicaciones web interactivas
library(leaflet)      # Para crear mapas interactivos
library(ggplot2)      # Para crear gráficos estáticos y complejos
library(dplyr)        # Para la manipulación y transformación de datos
library(plotly)       # Para crear gráficos interactivos a partir de ggplot2
library(sf)           # Para manejar datos espaciales y geoespaciales en formato 'simple features'
library(jsonlite)     # Para leer y escribir archivos en formato JSON, útil para datos GeoJSON
library(DBI)          # Para conectar con bases de datos
library(RMySQL)       # Para funciones específicas de bases de datos MySQL
library(RColorBrewer) # Para paletas de colores en gráficos
library(yaml)         # Para leer y escribir archivos en formato YAML
library(httr)         # Para interactuar con APIs web, como la de iNaturalist
library(shinyjs)      # Para agregar funcionalidades JavaScript a las aplicaciones Shiny
library(shinybusy)    # Para mostrar indicadores de carga durante operaciones largas
library(shinycssloaders) #Spinner de carga

##### UI #######
ui <- fluidPage(
  tags$head(
    tags$title("PreserVamos - Ciencia ciudadana en ambientes acuáticos")
  ),
  titlePanel(
    div(
      # Estilos para el encabezado: mostrar logo y mensajes en una estructura flexible
      style = "display: flex; justify-content: space-between; align-items: center; text-align: center;",
      div(
        # Texto a la izquierda del encabezado
        p(
          style = "flex: 1; text-align: right; font-size: 12px;",
          "Todos los datos de este mapa fueron generados con participación ciudadana",
        ),

        p("Conocé mas del proyecto PreserVamos en nuestro ",
          style = "flex: 1; text-align: right; font-size: 12px;",
          a(href = "https://preservamos.ar", "sitio web", target = "_blank")
        )
      ),
      div(
        # Logo en el centro del encabezado
        style = "flex: 1; text-align: center;",
        img(src = "https://preservamos.ar/wp-content/uploads/2022/02/Logo_preservamos.png", height = "100px", width = "auto", style = "display: block; margin: auto;")
      ),
      div(
        # Texto con enlace a la derecha del encabezado
        style = "flex: 1; text-align: right; font-size: 12px;",
        p("¿Querés saber cómo está hecho este mapa interactivo? "),
        p("¡El código es abierto y lo podés ver ",
          a(href = "https://limnolab.shinyapps.io/preservamos_mapa/", "acá!", target = "_blank")
        ),
        p("También puedes descargar el set de datos completos al momento, son abiertos, desde ",
          a(href = "https://preservamos.ar", "acá!", target = "_blank")
        ),
        downloadButton("download_data", "Descargar datos")
      )
    )
  ),
  tags$head(
    tags$style(HTML("
      .ecoregion-logo img {
        max-height: 100px;  
        width: 100%;
      }
    "))
  ),
  fluidRow(
    tags$style(HTML("
    #correlation_explanation {
      margin-bottom: 30px; 
      font-weight: bold;
    }
     ")),
    # Fila superior: Filtros, gráficos y mapa
    column(3,
           wellPanel(
             # Selector de ecorregión y tipo de río
             uiOutput("ecorregion_selector"),
             # Logo for the ecoregion
             tags$div(
                   imageOutput("ecoregion_logo", height = "auto", width = "100%"),
                   class = "ecoregion-logo"
                 ),
             selectInput("river_type", "Selecciona el tipo de cuerpo de agua:", choices = NULL),
             uiOutput("date_range_ui")  # Selector de rango de fechas
           ),
           wellPanel(
             # Selector de campo para correlación y gráfico de correlación
             selectInput("corr_field", "¿Con qué quieres relacionar al índice?", choices = NULL),
             htmlOutput("correlation_explanation"),  # Explicación de la correlación
             plotlyOutput("correlation", height = 300)
          )
    ),
    
    column(6,
           wellPanel(
             # Mapa interactivo con Leaflet
             withSpinner(leafletOutput("map", height = 800)),
           )
    ),
    
    column(3,
           wellPanel(
             # Histograma para visualizar los datos
             plotlyOutput("histogram", height = 300)
           ),
           wellPanel(
             # Descripción de la ecorregión seleccionada
             textOutput("ecoregion_description"),
             htmlOutput("histogram_explanation")  # Interpretación dinámica del histograma
           ),
           wellPanel(
             # Etiqueta y logo sobre los botones de taxones
             tags$div(
               style = "text-align: center; margin-bottom: 10px;",
               tags$p(
                 style = "font-weight: bold;",
                 "Conoce la flora y fauna en este área del mapa gracias a ",
                 tags$a(
                   href = "https://www.argentinat.org",  # Enlace al sitio de ArgentiNat
                   tags$img(
                     src = "https://static.inaturalist.org/sites/16-logo.svg?1582155393",
                     alt = "ArgentiNat Logo",
                     style = "vertical-align: middle; width: 100px;"  # Ajustar tamaño del logo
                   )
                 )
               )
             ),
             
             # Botones para seleccionar taxones
             div(
               style = "display: grid; grid-template-columns: repeat(3, 1fr); grid-gap: 10px; padding: 10px; box-sizing: border-box;",
               
               # Botones con iconos de cada grupo de organismos
               actionButton("show_birds", label = "Aves", icon = icon("crow"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;"),
               actionButton("show_mammals", label = "Mamíferos", icon = icon("paw"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;"),
               actionButton("show_plants", label = "Plantas", icon = icon("leaf"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;"),
               actionButton("show_fish", label = "Peces, reptiles y anfíbios", icon = icon("fish"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;"),
               actionButton("show_inver", label = "Invertebrados", icon = icon("bug"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;"),
               actionButton("show_fungi", label = "Microorganismos y hongos", icon = icon("bacteria"), style = "width: 100%; height: 80px; white-space: normal; line-height: 1.2;")
             ),
             
             # Modal para mostrar imágenes de los taxones
             uiOutput("image_modal"),
             
             # Estilos para las etiquetas superpuestas en las imágenes
             tags$head(
               tags$style(HTML("
                .image-container {
                  position: relative;
                  display: inline-block;
                  margin: 5px;
                }
                .image-label {
                  position: absolute;
                  bottom: 0;
                  left: 0;
                  width: 100%;
                  background-color: rgba(0, 0, 0, 0.5);
                  color: white;
                  text-align: center;
                  padding: 5px;
                  box-sizing: border-box;
                }
              "))
             )
           )
    )
  ),
  
  fluidRow(
    # Fila inferior: Licencia Creative Commons
    div(
      style = "display: flex; justify-content: space-between; align-items: center; text-align: center;",
      div(
        style = "flex: 1; text-align: center; font-size: 12px; margin-left:30px;",
        HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">
        <span property="dct:title">Mapa PreserVamos</span> por 
        <span property="cc:attributionName">PreserVamos</span> tiene una licencia 
        <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">
          CC BY-NC-SA 4.0
          <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt="">
          <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt="">
          <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt="">
          <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" alt="">
        </a>
      </p>')
      )
    ), 
    div(
      # Texto con enlace a la derecha del encabezado
      style = "flex: 1; text-align: right; font-size: 12px;",
      p("Las fotografías fueron tomadas de las Fichas de Ecorregiones generadas por ",
        a(href = "https://www.sib.gob.ar/portal/wp-content/uploads/2021/10/fichas_ecorregiones.pdf", "Parques Nacionales de Argentina", target = "_blank")
      )
    )
  )
)


##### SERVER #######
# Logica del servidor
server <- function(input, output, session) {
  
  ##### MODALES DE INICIO Y DE ARGENTINAT #####
  # Muestra un modal de bienvenida cuando el usuario carga la aplicación
  showModal(modalDialog(
    title = "¡Bienvenido/a al mapa interactivo de PreserVamos!",
    tags$div(
      # Imagen del logo de PreserVamos
      tags$img(src = "https://preservamos.ar/wp-content/uploads/2022/02/Logo_preservamos.png", height = "300px", width = "600px"),
      br(),  # Salto de línea
      # Explicación sobre el mapa y su uso
      p("Este mapa te permitirá visualizar la calidad del ambiente de ribera de distintos cursos de agua del país (ríos, lagos, estuarios)."), 
      p("Podés ver los datos de tu ecorregión, o de todo el país."),
      p("Todos los datos de este mapa son generados con participación ciudadana y se actualizan en tiempo real.")
    ),
    
    # Permite cerrar el modal fácilmente
    easyClose = TRUE,
    footer = modalButton("¡A explorar!")
  ))
  
  # Función para crear un modal que muestra imágenes del taxón seleccionado
  show_modal <- function(taxon) {
    show_modal_spinner()  # Muestra un spinner mientras se cargan las imágenes
    
    # Obtiene las imágenes según el taxón seleccionado
    images <- get_images(taxon)
    
    # Genera etiquetas HTML para mostrar las imágenes con sus nombres
    img_tags <- sapply(images, function(data) {
      paste0('<div class="image-container">
            <img src="', data$url, '" style="width: 100px; height: 100px;">
            <div class="image-label">', data$name, '</div>
          </div>')
    })
    
    # Junta todo el contenido de las imágenes en una cadena HTML
    modal_content <- paste(img_tags, collapse = "")
    remove_modal_spinner()  # Quita el spinner cuando las imágenes ya están cargadas
    
    # Muestra el modal con las imágenes del taxón seleccionado
    showModal(modalDialog(
      title = paste("Observaciones de ", taxon),
      HTML(modal_content),
      easyClose = TRUE,
      footer = modalButton("Cerrar")  # Botón para cerrar el modal
    ))
  }
  
  # Función para obtener imágenes de iNaturalist según el taxón seleccionado
  get_images <- function(taxon) {
    
    # Obtiene los límites actuales del mapa
    bounds <- input$map_bounds
    nelat <- bounds$north
    nelng <- bounds$east
    swlat <- bounds$south
    swlng <- bounds$west
    
    # Convierte los nombres de los taxones a los valores reconocidos por iNaturalist
    if(taxon == "Plantas"){
      taxon <- "Plantae"
    } else if (taxon == "Mamiferos") {
      taxon <- "Mammalia"
    } else if (taxon == "Peces, reptiles y anfíbios") {
      taxon <- "Actinopterigii,Reptilia,Amphibia"
    } else if (taxon == "Invertebrados") {
      taxon <- "Insecta,Arachnida,Mollusca"
    } else if (taxon == "Microorganismos y hongos") {
      taxon <- "Protozoa,Fungi"
    }
    
    # Construye la URL para la API de iNaturalist
    url <- paste0("https://api.inaturalist.org/v1/observations/species_counts?preferred_place_id=7190&locale=es&quality_grade=research&reviewed=true&iconic_taxa=",taxon, "&nelat=", nelat, "&nelng=", nelng, "&swlat=", swlat, "&swlng=", swlng)
    
    # Hace una petición GET a la API y obtiene los datos
    response <- GET(url)
    data_inat <- content(response, "parsed")
    images <- data_inat$results$image_url
    
    observations <- data_inat$results
    
    # Extrae las URLs de las imágenes y los nombres científicos de las observaciones
    image_data <- lapply(observations, function(obs) {
      # Verifica si hay fotos disponibles
      if (!is.null(obs$taxon$default_photo$url) && length(obs$taxon$default_photo$url) > 0) {
        image_url <- obs$taxon$default_photo$url  # Obtiene la URL de la foto
        scientific_name <- if (!is.null(obs$taxon) && !is.null(obs$taxon$name)) {
          obs$taxon$name
        } else {
          "Unknown"
        }
        return(list(url = image_url, name = scientific_name))
      } else {
        return(NULL)
      }
    })
    
    # Filtra las observaciones que no tienen imágenes
    image_data <- Filter(Negate(is.null), image_data)
    
    return(image_data)  # Devuelve los datos de las imágenes
  }
  
  # Observadores que activan los modales para cada taxón cuando se hace clic en los botones
  
  # Muestra el modal para aves
  observeEvent(input$show_birds, {
    show_modal("Aves")
  })
  
  # Muestra el modal para mamíferos
  observeEvent(input$show_mammals, {
    show_modal("Mamiferos")
  })
  
  # Muestra el modal para plantas
  observeEvent(input$show_plants, {
    show_modal("Plantas")
  })
  
  # Muestra el modal para microorganismos y hongos
  observeEvent(input$show_fungi, {
    show_modal("Microorganismos y hongos")
  })
  
  # Muestra el modal para peces, reptiles y anfibios
  observeEvent(input$show_fish, {
    show_modal("Peces, reptiles y anfíbios")
  })
  
  # Muestra el modal para invertebrados
  observeEvent(input$show_inver, {
    show_modal("Invertebrados")
  })
  
  ##### CONEXIÓN A LA BASE DE DATOS DE PRESERVAMOS VIA MYSQL Y AL GEOJSON DE LAS ECORREGIONS#####
  
  # Carga la configuración desde el archivo config.yml
  config <- yaml.load_file("config.yml")
  
  # Establece la conexión con la base de datos MySQL
  db_conn <- dbConnect(
    RMySQL::MySQL(),  # Usamos el paquete RMySQL para la conexión
    dbname = config$db_name,  # Nombre de la base de datos
    host = config$db_host,  # Host donde está alojada la base de datos
    user = config$db_user,  # Usuario de la base de datos
    password = config$db_password  # Contraseña del usuario
  )
  
  # Carga los datos desde la base de datos MySQL de manera reactiva
  data <- reactive({
    # Define la consulta SQL para seleccionar toda la tabla
    query <- paste0("SELECT * FROM ", config$db_table)
    
    # Ejecuta la consulta y guarda los resultados en un dataframe
    df <- dbGetQuery(db_conn, query)
    
    # Convierte las columnas necesarias a tipo numérico para poder usarlas correctamente
    df$Lat <- as.numeric(df$lat)
    df$Lng <- as.numeric(df$lng)
    df$Indice <- as.numeric(df$indice)
    
    # Convierte a numéricos los valores de los diferentes indicadores
    
    df$valorind1 <- suppressWarnings(as.numeric(df$valorind1))
    df$valorind2 <- suppressWarnings(as.numeric(df$valorind2))
    df$valorind3 <- suppressWarnings(as.numeric(df$valorind3))
    df$valorind4 <- suppressWarnings(as.numeric(df$valorind4))
    df$valorind5 <- suppressWarnings(as.numeric(df$valorind5))
    df$valorind6 <- suppressWarnings(as.numeric(df$valorind6))
    df$valorind7 <- suppressWarnings(as.numeric(df$valorind7))
    df$valorind8 <- suppressWarnings(as.numeric(df$valorind8))
    df$valorind9 <- suppressWarnings(as.numeric(df$valorind9))
    df$valorind10 <- suppressWarnings(as.numeric(df$valorind10))
    
    # Convierte también los valores de los indicadores PVM a numéricos
    df$ind_pvm_1 <- suppressWarnings(as.numeric(df$ind_pvm_1))
    df$ind_pvm_2 <- suppressWarnings(as.numeric(df$ind_pvm_2))
    df$ind_pvm_3 <- suppressWarnings(as.numeric(df$ind_pvm_3))
    df$ind_pvm_4 <- suppressWarnings(as.numeric(df$ind_pvm_4))
    df$ind_pvm_5 <- suppressWarnings(as.numeric(df$ind_pvm_5))
    df$ind_pvm_6 <- suppressWarnings(as.numeric(df$ind_pvm_6))
    df$ind_pvm_7 <- suppressWarnings(as.numeric(df$ind_pvm_7))
    df$ind_pvm_8 <- suppressWarnings(as.numeric(df$ind_pvm_8))
    df$ind_pvm_9 <- suppressWarnings(as.numeric(df$ind_pvm_9))
    
    # Convierte la columna de fecha formateada a un objeto de tipo fecha
    df$formated_date <- as.Date(df$formated_date, format = "%Y-%m-%d")
    
    # Devuelve el dataframe procesado
    return(df)
  })
  
  # Cargar datos desde un archivo GeoJSON
  geojson_file <- "ecorregiones_simples.geojson"  # Ruta al archivo GeoJSON
  geojson <- st_read(geojson_file)
  # Convertir los datos y el GeoJSON a objetos espaciales (sf)
  geojson_sf <- st_as_sf(geojson)
  
  geojson_file_arg <- "argentina.geojson"
  geojson_arg <- st_read(geojson_file_arg)
  
  #### ETIQUETAS PARA COMBO BOXES Y COLORES #####
  
  # Definir las etiquetas para los diferentes indicadores de "valorind" y "ind_pvm"
  valorind_labels <- c(
    "Uso del suelo" =  "valorind1",  # Indicador de uso del suelo
    "Ganadería" = "valorind2",  # Indicador de ganadería
    "Vegetación de Ribera - fisionomia" = "valorind3",  # Indicador de la vegetación de ribera y su fisionomía
    "Vegetación acuática" = "valorind4",  # Indicador de la vegetación acuática
    "Agua - Transparencia" = "valorind5",  # Indicador de la transparencia del agua
    "Agua - Olor" = "valorind6",  # Indicador del olor del agua
    "Basura" = "valorind7",  # Indicador de la presencia de basura
    "Desbordado" = "valorind9",  # Indicador de desbordamiento del curso de agua
    "Entubado" = "valorind10",  # Indicador de si el curso de agua está entubado
    
    # Indicadores adicionales del PVM (Parámetros de Vegetación y Manejo)
    "Agua - Aceites" = "ind_pvm_1",  # Indicador de presencia de aceites en el agua
    "Vegetación en márgenes" = "ind_pvm_2",  # Indicador de la vegetación en los márgenes del curso de agua
    "Exóticas - Acacia" = "ind_pvm_3",  # Indicador de presencia de especies exóticas como la Acacia
    "Exóticas - Ligustro" = "ind_pvm_4",  # Indicador de presencia de Ligustro (especie exótica)
    
    # Indicadores de uso del cuerpo de agua
    "Uso - Baño" = "ind_pvm_5",  # Indicador de uso recreativo para baño
    "Uso - Pesca" = "ind_pvm_6",  # Indicador de uso para pesca
    "Uso - Deporte" = "ind_pvm_7",  # Indicador de uso para deportes acuáticos
    "Uso - Navegación" = "ind_pvm_8"  # Indicador de uso para navegación
  )
  
  # Crear una paleta de colores para los valores del Índice
  pal_new <- colorNumeric(
    palette = colorRampPalette(c("red", "orange", "yellow", "green", "blue"))(100),  # Definir un gradiente de colores desde rojo (bajo) hasta azul (alto)
    domain = NULL  # El dominio será establecido más tarde, basado en los datos
  )
  
  # Definir una paleta de colores personalizada para cada ecorregión
  ecoregion_colors <- c(
    "Altos Andes" = "#5fbabf",  # Color asociado a la ecorregión de Altos Andes
    "Puna" = "#0885b1",  # Color para la ecorregión de la Puna
    "Monte de Sierras y Bolsones" = "#e3ac6c",  # Color para el Monte de Sierras y Bolsones
    "Selva de Yungas" = "#ed008c",  # Color para la Selva de Yungas
    "Chaco seco" = "#f6821f",  # Color para la región del Chaco seco
    "Chaco humedo" = "#ca6c38",  # Color para la región del Chaco húmedo
    "Selva Paranaense" = "#ed1b24",  # Color para la Selva Paranaense
    "Esteros del Ibera" = "#ffca08",  # Color para los Esteros del Iberá
    "Campos y Malezales" = "#f7aa8e",  # Color para la región de Campos y Malezales
    "Delta e islas del Parana" = "#475e88",  # Color para el Delta e islas del Paraná
    "Espinal" = "#75bc5c",  # Color para la región del Espinal
    "Pampa" = "#00984f",  # Color para la Pampa
    "Monte de llanuras y mesetas" = "#fef200",  # Color para el Monte de llanuras y mesetas
    "Estepa patagonica" = "#9b5124",  # Color para la Estepa patagónica
    "Bosques patagonicos" = "#3a8477"  # Color para los Bosques patagónicos
  )
  
  ##### FILTRO PARA LOS DATOS DE CORRELACION ####
  # Definir una expresión reactiva para manejar la correlación
  correlation_data <- reactive({
    df <- filtered_data()  # Obtener los datos filtrados según los criterios del usuario
    selected_var <- input$corr_field  # Variable seleccionada por el usuario para la correlación
    req(selected_var)  # Asegurar que una variable ha sido seleccionada antes de continuar
    
    # Crear un data frame con las variables necesarias para la correlación
    correlation_df <- df %>%
      select(Indice, !!sym(selected_var))  # Seleccionar las columnas 'Indice' y la variable seleccionada dinámicamente
    
    return(correlation_df)  # Devolver el data frame preparado para el cálculo de correlación
  })
  
  zoom_reset <- reactiveVal(TRUE)
  
  #### Actualizar el mapa y los marcadores según la ecorregión y el tipo de río seleccionados ####
  observe({
    selected_ecorregion <- input$selected_ecorregion  # Obtener ecorregión seleccionada
    selected_tiporio <- input$selected_tiporio  # Obtener tipo de río seleccionado
    df <- filtered_data()  # Filtrar datos reactivos
    
    # Definir los límites aproximados de Argentina
    argentina_bounds <- list(
      lng1 = -73.4154, lat1 = -55.25,   # Punto suroeste
      lng2 = -53.6283, lat2 = -21.8323  # Punto noreste
    )
    
    # Verificar si `selected_ecorregion` y `selected_tiporio` son NULL o vacíos
    if (is.null(selected_ecorregion)) selected_ecorregion <- "Todas las ecorregiones"
    if (is.null(selected_tiporio)) selected_tiporio <- "All"
    
    # # Convertir los datos y el GeoJSON a objetos espaciales (sf)
    # geojson_sf <- st_as_sf(geojson)
    df_sf <- st_as_sf(df, coords = c("Lng", "Lat"), crs = st_crs(geojson_sf))
    
    # Filtrar por tipo de río si no es "All"
    if (selected_tiporio != "All") {
      df_sf <- df_sf %>% filter(tiporio == selected_tiporio)
    }
    
    if (selected_ecorregion != "Todas las ecorregiones") {
      # Filtrar el GeoJSON según la ecorregión seleccionada
      ecorregion_sf <- geojson_sf %>%
        filter(ECOREGION == selected_ecorregion)
      ecorregion_sf <- st_make_valid(ecorregion_sf)  # Asegurarse de que la geometría sea válida
      
      if (nrow(ecorregion_sf) > 0) {
        # Realizar una intersección espacial para obtener los puntos dentro de la ecorregión seleccionada
        points_in_ecorregion <- st_intersection(df_sf, ecorregion_sf)
        filtered_df <- as.data.frame(points_in_ecorregion)
        
        # Obtener el cuadro delimitador de la ecorregión seleccionada
        ecorregion_bounds <- st_bbox(ecorregion_sf)
        
        # Actualizar el mapa
        suppressWarnings({
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearShapes() %>%
                addPolygons(
                  data = ecorregion_sf,
                  color = ~unname(ecoregion_colors[as.character(ecorregion_sf$ECOREGION)]), # Color según ecorregión
                  weight = 2,
                  fillOpacity = 0.4,
                  #fillColor = "transparent",
                  layerId = "geojson_layer"
                ) %>%
                addCircleMarkers(
                  data = filtered_df,
                  lng = ~lng,
                  lat = ~lat,
                  color = ~ifelse(Indice == -1, "gray", pal_new(Indice)), # Color según el índice
                  radius = 5,
                  fillOpacity = 0.8,
                  layerId = ~as.character(Indice),
                  popup = ~paste(
                    "<b>Indice:</b> ", Indice, "<br>",
                    "<b>Date:</b> ", formated_date, "<br>",
                    "<b>User:</b> ", ifelse(useremail == "" | is.na(useremail), "Anónimo", sub("@.*", "", useremail))  # Handle empty email
                  )
                ) %>%
                fitBounds(
                  lng1 = ecorregion_bounds["xmin"], lat1 = ecorregion_bounds["ymin"],
                  lng2 = ecorregion_bounds["xmax"], lat2 = ecorregion_bounds["ymax"]
                ) # Ajustar el zoom al área seleccionada
        })
      } else {
        # Si no hay datos en la ecorregión seleccionada, limpiar marcadores y formas
        suppressWarnings({
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearShapes()
            })
      }        
    } else {
      #### Caso donde se seleccionan todas las ecorregiones ####
      df$Indice <- ifelse(is.na(df$Indice), -1, df$Indice) # Asignar -1 si el Índice es NA
      if (zoom_reset()) {
        
        suppressWarnings({
          # Actualizar el mapa con todas las ecorregiones
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(
              data = geojson_arg,
              color = "darkgreen",
              weight = 2,
              fillColor = "transparent",
              fillOpacity = 0.4,
              layerId = "geojson_layer"
            ) %>%
            addCircleMarkers(
              data = df_sf,
              lng = ~lng,
              lat = ~lat,
              color = ~ifelse(Indice == -1, "gray", pal_new(Indice)),  # Color según el índice
              radius = 5,
              fillOpacity = 0.8,
              layerId = ~as.character(Indice),
              popup = ~paste(
                "<b>Indice:</b> ", Indice, "<br>",
                "<b>Date:</b> ", formated_date, "<br>",
                "<b>User:</b> ", ifelse(useremail == "" | is.na(useremail), "Anónimo", sub("@.*", "", useremail))  # Handle empty email
              )
            ) %>%
            fitBounds(
              argentina_bounds$lng1, argentina_bounds$lat1,
              argentina_bounds$lng2, argentina_bounds$lat2
            )  # Ajustar el zoom para toda Argentina
          
        })
      }

    }
  })
  
  observeEvent(input$map_zoom, {
    zoom_reset(FALSE)  # Disable resetting bounds when user zooms manually
  })
  
  
  
 ##### IMAGENES DE ECORREGIONES #####
  ecoregion_logos <- list(
    "Altos Andes" = "altos_andes.png",
    "Puna" = "puna.png",
    "Monte de Sierras y Bolsones" = "monte_sierras_bolsones.png",
    "Selva de Yungas" = "selva_yungas.png",
    "Chaco seco" = "chaco_seco.png",
    "Chaco humedo" = "chaco_humedo.png",
    "Selva Paranaense" = "selva_paranaense.png",
    "Esteros del Ibera" = "esteros_ibera.png",
    "Campos y Malezales" = "campos_y_malezales.png",
    "Delta e islas del Parana" = "delta_islas_parana.png",
    "Espinal" = "espinal.png",
    "Pampa" = "pampa.png",
    "Monte de llanuras y mesetas" = "monte_llanuras_mesetas.png",
    "Estepa patagonica" = "estepa_patagonica.png",
    "Bosques patagonicos" = "bosques_patagonicos.png"
  )
  
  observeEvent(input$selected_ecorregion, {
    #Cambia imagen de arriba basado en la ecorregión
    clicked_ecoregion <- input$selected_ecorregion
    
    if (clicked_ecoregion != "Todas las ecorregiones"){
        logo_file <- ecoregion_logos[[clicked_ecoregion]]

        if (file.exists(file.path("www", logo_file))) {
          #print("file exists")
          output$ecoregion_logo <- renderImage({
            list(src = file.path("www", logo_file), height = "auto", width = "100%")
          }, deleteFile = FALSE)
        } else {
          output$ecoregion_logo <- renderImage({
            list(src = "www/default_logo.png", height = "auto", width = "100%")
          }, deleteFile = FALSE)
        }
    }

        zoom_reset(TRUE)  # Reset bounds when user selects a new ecoregion
  })
  
  
  #### Actualizar opciones del menú desplegable según los datos ####
  observe({
    df <- data()  # Obtener los datos reactivos
    
    # Imprimir los tipos de ríos únicos para depuración
    unique_river_types <- unique(df$tipo)
    
    # Definir las etiquetas para los tipos de ríos
    river_type_labels <- c(
      "Lagunas" = "laguna",
      "Ríos de llanura" = "llanura",
      "Ríos de montaña" = "montana",
      "Estuarios" = "estuario",
      "Ríos de sierras" = "sierras"
    )
    
    # Normalizar el campo 'tipo' a minúsculas y mapear
    valid_river_types <- intersect(unique_river_types, river_type_labels)
    
    # Crear un mapeo de valores a etiquetas
    display_river_types <- names(river_type_labels)[river_type_labels %in% valid_river_types]
    
    # Actualizar el input de selección para el tipo de río con las nuevas etiquetas
    updateSelectInput(session, "river_type", 
                      choices = c("Todos" = "", river_type_labels))
    
    # Actualizar el input de selección para el campo de correlación con las etiquetas
    updateSelectInput(session, "corr_field",
                      choices = valorind_labels,
                      selected = names(valorind_labels)[1])  # Establecer el valor predeterminado en la primera opción
  })
  
  ##### Menu ecorregion #### 
  # Crear valor reactivo para la ecorregión seleccionada #
  selected_ecorregion <- reactiveVal("Todas las ecorregiones")
  
  # Crear menú desplegable para seleccionar la ecorregión #
  output$ecorregion_selector <- renderUI({
    req(geojson)  # Asegurarse de que GeoJSON esté cargado
    ecorregion_names <- unique(geojson$ECOREGION)  # Obtener los nombres únicos de ecorregiones
    ecorregion_choices <- c("Todas las ecorregiones", ecorregion_names)  # Crear las opciones para el menú desplegable
    selectInput("selected_ecorregion", "Selecciona una ecorregión:", choices = ecorregion_choices, selected = "Todas las ecorregiones")
  })
  
  # Actualizar la ecorregión seleccionada cuando el usuario elige del menú desplegable #
  observe({
    selected_ecorregion(input$selected_ecorregion)  # Establecer el valor reactivo con la selección del usuario
  })
  
  ##### Menu de fecha #### 
  output$date_range_ui <- renderUI({
    df <- data()  # Obtener los datos
    req(df$formated_date)  # Asegurarse de que 'formated_date' esté disponible
    
    # Crear el input de slider utilizando las fechas mínima y máxima de los datos
    sliderInput("date_range", "Rango de tiempo:",
                min = min(df$formated_date, na.rm = TRUE),  # Fecha mínima
                max = max(df$formated_date, na.rm = TRUE),  # Fecha máxima
                value = c(min(df$formated_date, na.rm = TRUE), max(df$formated_date, na.rm = TRUE)),  # Valor inicial del slider
                timeFormat = "%Y-%m-%d",  # Mostrar la fecha en formato yyyy-mm-dd
                dragRange = TRUE)  # Permitir arrastrar el rango
  })
  
  #### Limites del mapa ####
  # Valor reactivo para almacenar los límites del mapa
  map_bounds <- reactiveVal(NULL)
  
  # Actualizar los límites del mapa cuando el mapa se mueve o se hace zoom
  observeEvent(input$map_bounds, {
    map_bounds(input$map_bounds)  # Guardar los nuevos límites del mapa
  }, ignoreInit = TRUE)  # Ignorar la inicialización para evitar actualizaciones innecesarias
  
  
  ##### Filtrar datos según el tipo de río seleccionado y la ecorregión #####
  filtered_data <- reactive({
    df <- data()  # Obtener los datos completos
    bounds <- map_bounds()  # Obtener los límites actuales del mapa
    selected_ecorregion <- selected_ecorregion()  # Obtener la ecorregión seleccionada
    selected_river_type <- input$river_type  # Obtener el tipo de río seleccionado

    # Normalizar el campo 'tipo' a formato de título
    df$tipo <- tools::toTitleCase(tolower(df$tipo))
    
    # Filtrar los datos según los límites del mapa
    if (!is.null(bounds)) {
      df <- df %>%
        filter(Lng >= bounds$west & Lng <= bounds$east & Lat >= bounds$south & Lat <= bounds$north)
    }
    
    # Filtrar los datos por la ecorregión seleccionada
    if (!is.null(selected_ecorregion) && selected_ecorregion != "Todas las ecorregiones") {
      ecorregion_sf <- geojson %>% filter(ECOREGION == selected_ecorregion)
      if (nrow(ecorregion_sf) > 0) {
        ecorregion_bbox <- st_bbox(st_union(ecorregion_sf))  # Obtener el bbox de la ecorregión seleccionada
        df <- df %>%
          filter(Lng >= ecorregion_bbox["xmin"] & Lng <= ecorregion_bbox["xmax"] & Lat >= ecorregion_bbox["ymin"] & Lat <= ecorregion_bbox["ymax"])
      }
    }
    
    # Filtrar los datos según el tipo de río seleccionado
    if (selected_river_type != "") {
      df <- df %>%
        filter(tiporio == selected_river_type)
    }
    
    # Filtrar los datos según el rango de fechas seleccionado
    if (!is.null(input$date_range)) {
      df <- df %>%
        filter(formated_date >= input$date_range[1] & formated_date <= input$date_range[2])
    }
    
    return(df)  # Devolver los datos filtrados
  })
  
  #### Renderizar mapa con Leaflet ####
  output$map <- renderLeaflet({
    df <- data()  # Obtener los datos completos
    req(nrow(df) > 0) # Asegurarse de que haya datos disponibles
    
    # Definir los límites aproximados de Argentina
    argentina_bounds <- list(
      lng1 = -73.4154, lat1 = -55.25,   # Punto suroeste
      lng2 = -53.6283, lat2 = -21.8323  # Punto noreste
    )
    
    # Manejar valores NA en la columna Indice asignando un valor predeterminado
    df$Indice <- ifelse(is.na(df$Indice), -1, df$Indice) # Usar -1 como valor predeterminado
    
    # Crear el mapa de Leaflet
    suppressWarnings({
      leaflet() %>%
        addTiles() %>%  # Añadir capa base
        addPolygons(
          data = geojson,  # Añadir polígonos del GeoJSON
          color = ~unname(ecoregion_colors[as.character(ECOREGION)]),
          weight = 2,
          fillOpacity = 0.4,
          layerId = "geojson_layer"
        ) %>%
        addCircleMarkers(
          data = df,  # Añadir marcadores de círculos
          lng = ~Lng,
          lat = ~Lat,
          color = ~ifelse(Indice == -1, "gray", pal_new(Indice)), # Color gris para valores predeterminados
          radius = 5,
          fillOpacity = 0.8,
          layerId = ~as.character(Indice),
          popup = ~paste(
            "<b>Indice:</b> ", Indice, "<br>",
            "<b>Date:</b> ", formated_date, "<br>",
            "<b>User:</b> ", ifelse(useremail == "" | is.na(useremail), "Anónimo", sub("@.*", "", useremail))  # Manejar correos electrónicos vacíos
          )
        ) %>%
        addLegend(
          "bottomright",  # Posición de la leyenda
          pal = pal_new,  # Paleta de colores
          values = df$Indice,  # Valores para la leyenda
          title = "Indice",  # Título de la leyenda
          opacity = 1  # Opacidad de la leyenda
        )
    })
  })
  
  
  ##### Renderizar histograma con barras coloreadas, siempre usando "Indice" #####
  output$histogram <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)  # Asegurarse de que haya datos disponibles
    
    # Asegurarse de que el campo "Indice" sea numérico
    df$Indice <- as.numeric(df$Indice)
    
    # Crear intervalos para el histograma
    binwidth <- diff(range(df$Indice, na.rm = TRUE)) / 30
    breaks <- seq(min(df$Indice, na.rm = TRUE), max(df$Indice, na.rm = TRUE), by = binwidth)
    
    # Define color bins
    color_bins <- cut(df$Indice, 
                      breaks = c(-Inf, 20, 40, 60, 80, Inf), 
                      labels = c("red", "orange", "yellow", "green", "blue"))
    
    # Título del histograma
    num_filtered_points <- nrow(df)
    histogram_title <- paste("Índice de Calidad de Ribera")
    
    p <- ggplot(df, aes(x = Indice)) +
      geom_histogram(
        binwidth = binwidth,
        aes(fill = color_bins),  # Colorear las barras según el rango de Indice
        color = "black",
        show.legend = FALSE
      ) +
      scale_fill_manual(
        values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
        name = "Value"
      ) +
      scale_x_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(title = histogram_title,
           x = "Índice",
           y = "Cantidad de datos") +
      theme(legend.position = "none")  # Esconder la leyenda
    
    ggplotly(p)
  })

  ##### Renderizar gráfico de correlación con línea de regresión angosta, punteada, y con R² y ecuación #####
  # Renderiza el gráfico de correlación
  output$correlation <- renderPlotly({
    corr_df <- correlation_data()  # Obtiene los datos para la correlación
    req(nrow(corr_df) > 0)  # Asegura que haya datos disponibles
    
    # Define las variables para la correlación
    x_var <- "Indice"
    y_var <- input$corr_field
    
    # Crea una fórmula para la línea de regresión
    formula <- as.formula(paste(y_var, "~", x_var))
    
    # Ajusta el modelo de regresión lineal
    model <- lm(formula, data = corr_df)
    
    # Extrae los coeficientes y el R² del modelo
    intercept <- coef(model)[1]  # Intercepto de la regresión
    slope <- coef(model)[2]  # Pendiente de la regresión
    r_squared <- summary(model)$r.squared  # Coeficiente de determinación R²
    
    # Crea el gráfico
    p <- ggplot(corr_df, aes_string(x = x_var, y = y_var)) +
      geom_point(color = "blue", alpha = 0.6) +  # Blue points
      geom_smooth(method = "lm", color = "black", linetype = "dashed", size = 0.8, se = FALSE) +  # Regression line
      theme_minimal() +  # Minimal theme
      labs(
        x = x_var,  # X-axis label
        y = names(valorind_labels)[valorind_labels == input$corr_field]  # Y-axis label
      ) +
      theme(
        plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Increase margins
        axis.title = element_text(size = 10),  # Smaller axis labels
        plot.title = element_text(size = 16, hjust = 0.5),  # Centered title
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
        plot.background = element_rect(fill = "transparent", color = NA)  # Transparent plot background
      )
    
    # Personaliza el título con la ecuación y el R²
    p_plotly <- ggplotly(p) %>%
      layout(
        title = list(
          text = paste0(
            "Índice de ribera vs. ", names(valorind_labels)[valorind_labels == input$corr_field], 
            '<br>',
            '<sup>',
            "y = ", round(intercept, 2), " + ", round(slope, 2), "x | R² = ", round(r_squared, 2),
            '</sup>'
          ),
          x = 0.1,  # Horizontal title position
          y = 0.95,  # Vertical title position
          font = list(size = 16)  # Title font size
        ),
        plot_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent plot background
        paper_bgcolor = "rgba(0, 0, 0, 0)",  # Transparent overall background
        xaxis = list(titlefont = list(size = 10)),  # Smaller X-axis labels
        yaxis = list(titlefont = list(size = 10))  # Smaller Y-axis labels
      )
    
    p_plotly  # Devuelve el gráfico interactivo
  })
  
  
  
  
  #### EXPLICACIONES PARA EL GRÁFICO DE CORRELACIÓN ####
  output$correlation_explanation <- renderText({
    corr_df <- correlation_data()  # Obtiene los datos para la correlación
    req(nrow(corr_df) > 0)  # Asegura que hay datos disponibles
    
    # Define las variables para la correlación
    x_var <- "Indice"  # Variable en el eje x
    y_var <- input$corr_field  # Variable seleccionada en el eje y
    
    # Ajusta el modelo de regresión lineal
    model <- lm(as.formula(paste(y_var, "~", x_var)), data = corr_df)
    
    # Extrae los coeficientes y el R² del modelo
    intercept <- coef(model)[1]  # Intercepto de la regresión
    slope <- coef(model)[2]  # Pendiente de la regresión
    r_squared <- summary(model)$r.squared  # Coeficiente de determinación R²
    
    # Crea el texto de explicación basado en el valor de R²
    explanation <- if (r_squared < 0.25) {
      # Si R² es menor a 0.25, la relación es débil
      paste("Hay una relación débil entre las variables. Esto quiere decir que el Índice de Ribera y el", 
            names(valorind_labels)[valorind_labels == input$corr_field], 
            "no están bien relacionados en este área.")
    } else if (r_squared < 0.7) {
      # Si R² está entre 0.25 y 0.7, la relación es moderada
      paste("Hay una relación moderada entre las variables. Esto quiere decir que el Índice de Ribera y el", 
            names(valorind_labels)[valorind_labels == input$corr_field], 
            "están moderadamente relacionados en este área.")
    } else {
      # Si R² es mayor a 0.7, la relación es fuerte
      paste("Hay una relación fuerte entre las variables. Esto quiere decir que el Índice de Ribera y el", 
            names(valorind_labels)[valorind_labels == input$corr_field], 
            "están bien relacionados en este área.")
    }
    
    # Wrap the explanation text in HTML and add some space below
    HTML(paste0("<p style='margin-bottom: 30px;'>", explanation, "</p>"))
    
    return(explanation)  # Devuelve el texto de explicación
  })
  
  #### EXPLICACIONES PARA EL HISTOGRAMA ####
  output$histogram_explanation <- renderText({
    df <- filtered_data()  # Obtiene los datos filtrados para el histograma
    
    if (nrow(df) == 0) {
      # Si no hay datos, muestra un mensaje indicando que no hay datos disponibles
      return("No hay datos para esta búsqueda... ¿Querés ser la primera persona en esta ecorregión en participar del mapa colaborativo? ¡Unite a nuestro proyecto!")
    }
    
    # Calcula estadísticas básicas
    avg_indice <- mean(df$Indice, na.rm = TRUE)  # Promedio del Índice
    median_indice <- median(df$Indice, na.rm = TRUE)  # Mediana del Índice
    data_count <- nrow(df)  # Cantidad de datos
    
    explanation <- ""
    
    if (data_count < 10) {
      # Si hay menos de 10 datos, muestra un mensaje sobre la limitación de los datos
      explanation <- "Los datos disponibles son limitados, por lo que los resultados pueden no ser completamente representativos."
    } else {
      # Basado en el valor promedio del Índice, proporciona una explicación sobre la calidad del agua
      if (avg_indice > 80) {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>MUY BUENA.</strong>"
      } else if (avg_indice > 60) {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>BUENA.</strong>"
      } else if (avg_indice > 40) {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>INTERMEDIA.</strong>"
      } else if (avg_indice > 20) {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>MALA.</strong>"
      } else if (avg_indice < 20) {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>MUY MALA.</strong>"
      } else {
        explanation <- "Los cuerpos de agua en esta región, en general, tienen una calidad de ribera <strong>MUY MALA.</strong>"
      }
    }
    
    # Muestra la cantidad de datos, el valor promedio del Índice y la explicación en formato HTML
    HTML(paste("<strong>Cantidad de datos en el mapa:</strong>", data_count, "<br>",
               "<strong>Promedio del Índice de Calidad de Ribera:</strong>", round(avg_indice, 2), "<br><br>",
               explanation))
  })
  
  ####### BOTON PARA BAJAR LOS DATOS
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("preservamos-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)  # Saves the dataset as a CSV
    }
  )
  
  ####### HAY QUE MATAR LA CONEXION A MYSQL
    killDbConnections <- function () {
    all_cons <- dbListConnections(MySQL())
    #print(all_cons)
    for(con in all_cons)
      +  dbDisconnect(con)
  }
  
  session$onSessionEnded(function() {
    killDbConnections()
  })
  
}

# Correr la app con las dos partes, UI y Server
shinyApp(ui = ui, server = server)



