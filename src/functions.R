#interacciones con aimplas a través de las menciones desde el día 7 de julio de 2020
Mention_Aimplas_Interactions <- function() {
  #cargamos las interacciones
  mentions.aimplas.interactions.all <- read.csv("data_output/aimplas_interactuers.csv")
  #buscamos la network data en nuestras interacciones
  network.data.aimplas <- network_data(lookup_tweets(get_mentions()$status_id) %>% dplyr::filter(as.Date(created_at) > unique(mentions.aimplas.interactions.all$DateUpdate) & as.Date(created_at) < Sys.Date()))
  
  if (nrow(network.data.aimplas) > 1){
    ##arreglo de datos
    
    #convertimos a caracter las columnas de from y to
    network.data.aimplas$from <- as.character(network.data.aimplas$from)
    network.data.aimplas$to <- as.character(network.data.aimplas$to)
    
    #buscamos los nombres de las personas a partir del id de usuario
    people <- c(unique(network.data.aimplas$from), unique(network.data.aimplas$to))
    people <- lookup_users(people)
    people <- people[c("user_id", "screen_name")]
    
    #left join para identificar los usuarios con el id
    network.data.aimplas <- left_join(network.data.aimplas, people, by = c("from" = "user_id"))
    network.data.aimplas <- left_join(network.data.aimplas, people, by = c("to" = "user_id"))
    
    #eliminamos la variable people que ya no la vamos a usar
    remove(people)
    
    #seleccionamos los nombres en lugar del id 
    network.data.aimplas <- network.data.aimplas[c(4, 5, 3)]
    
    #cambiamos el nombre de las columnas
    names(network.data.aimplas) <- c("from", "to", "type")
    
    #creamos un dataframe auxiliar en el cual ponemos los tipos de interaccion que existe. Esto se hace para evitar que, en actualizaciones en un tiempo corto, no aparezcan todos los tipos de interacción y por lo cual no se puedan unir dataframes más adelante
    network.types <- data.frame(from = rep("placeholder_eliminar", 3), to = rep("x", 3), type = c("mention", "quote", "reply"))
    
    #unimos dataframes
    network.data.aimplas <- rbind(network.data.aimplas, network.types)
    
    #creamos la tabla (aunque este proceso tiene una alternativa, estudiar ponerla)
    mentions.aimplas.interactions <- as.data.frame(table(network.data.aimplas)) 
    
    #spread para separar los tipos de interacciones
    mentions.aimplas.interactions <- mentions.aimplas.interactions %>% spread(key = type, value = Freq)
    
    #los NA que puedan salir los convertimos a 0
    mentions.aimplas.interactions[is.na(mentions.aimplas.interactions)] = 0
    
    #creamos la columna de peso para saber el total de interacciones
    mentions.aimplas.interactions <- mentions.aimplas.interactions %>%
      mutate(weigth = select(., 3:length(mentions.aimplas.interactions)) %>% rowSums(na.rm = TRUE)) %>% dplyr::filter(weigth > 0)
    
    #eliminamos el placeholder
    mentions.aimplas.interactions <- mentions.aimplas.interactions[-which(mentions.aimplas.interactions$from == "placeholder_eliminar"), ]
    rownames(mentions.aimplas.interactions) <- NULL
    
    #eliminamos la columna dateUpdate para poder hacer la suma 
    mentions.aimplas.interactions.all$DateUpdate <- NULL
    
    
    #ponemos nombres iguales
    names(mentions.aimplas.interactions) <- names(mentions.aimplas.interactions.all)
    
    #y unimos ambos dataframes
    mentions.aimplas.interactions.all <- rbind(mentions.aimplas.interactions.all, mentions.aimplas.interactions) %>% group_by(source, destinations) %>% summarise_each(funs(sum)) %>% arrange(desc(weight))
    
    #eliminamos variables
    remove(network.types, mentions.aimplas.interactions)
  }
  
  mentions.aimplas.interactions.all$DateUpdate <- Sys.Date() - 1
  #guardamos
  write.csv(mentions.aimplas.interactions.all, "data_output/aimplas_interactuers.csv", row.names=FALSE)
  
  #eliminamos variables
  remove(network.data.aimplas)
}


#datos para el analisis de timeline aimplas
Timeline_Stats <- function() {
  
  #se analizaran los datos de la timeline de aimplas recogiendo, con la función get_timeline, la timeline de aimplas para poder analizar correctamente horarios de actividad, etc
  
  #la primera parte se ha realizado debido a que no se sabe con certeza si los primeros tweets seguiran trayéndose pasado un tiempo. Por esto, se almacena la timeline de aimplas en un dataframe (RData) y se carga, de modos que se actualice y en el caso de que no se traigan todos los tuits por determinado motivo, nosotros siguamos teniendo acceso a dichos tweets
  
  #cargamos la timeline de aimplas
  load(file.path("data_output/timeline_aimplas.RData"))
  
  #obtenemos la timeline de aimplas completa
  aimplas.timeline <- get_timeline("aimplas", n = 10000)
  
  #la unimos a la que teniamos cargada
  aimplas.timeline.full <- rbind(aimplas.timeline.full, aimplas.timeline)
  
  #eliminamos las que tengan id repetido
  aimplas.timeline.full <- aimplas.timeline.full %>% dplyr::distinct(status_id, .keep_all = TRUE)
  
  #eliminamos la variable
  remove(aimplas.timeline)
  
  #guardamos todos los tweets
  #no se puede guardar directamnte como un csv porque muchas columnas son listas y no permiten guardarse como csv
  save(aimplas.timeline.full, file = "data_output/timeline_aimplas.RData")
  
  #separamos entre fecha y horas
  aimplas.timeline.full$Date <- as.Date(aimplas.timeline.full$created_at)
  aimplas.timeline.full$Time <- format(as.POSIXct(aimplas.timeline.full$created_at), format = "%H:%M:%S") 
  
  #pero solo nos interesa la hora, para ver la distribución de los tweets por hora
  aimplas.timeline.full$Hora <- format(strptime(aimplas.timeline.full$Time,"%H:%M:%S"),'%H')
  
  #queremos ver cuando se obtienen más favoritos y retweets
  stats.timeline.aimplas <- aimplas.timeline.full[c("Hora", "Date", "favorite_count", "retweet_count")]
  
  stats.timeline.aimplas$Hora <- sapply(stats.timeline.aimplas$Hora, as.numeric)
  stats.timeline.aimplas$Hora <- stats.timeline.aimplas$Hora + 1
  
  #guardamos el csv
  write.csv(stats.timeline.aimplas, "data_output/timeline_stats.csv", row.names=FALSE)
}

#actualización información seguidores

Followers_Aimplas <- function() {
  #cargamos el fichero de la última semana de los seguidores de aimplas
  followers.aimplas.past <- read_csv("data_output/aimplas_followers_info_now.csv")
  
  #asignamos a "past" el de la ultima actualizacion y a "now" la nueva actualizacion
  followers.aimplas.now <- users_data(lookup_users(get_followers("aimplas", n = 100000)$user_id))
  
  #añadimos la columna de fecha de actualización
  followers.aimplas.now$DateUpdate <- Sys.Date()
  
  #comprobar los nuevos seguidores y la gente que deja de seguirnos
  new.followers <- followers.aimplas.now[which(followers.aimplas.now$screen_name %in% followers.aimplas.past$screen_name == FALSE), ]
  stop.followers <- followers.aimplas.past[which(followers.aimplas.past$screen_name %in% followers.aimplas.now$screen_name == FALSE), ]
  
  #guardamos datos
  write_csv(new.followers, file = "data_output/new_followers.csv")
  write_csv(stop.followers, file = "data_output/stop_followers.csv")
  write_csv(followers.aimplas.now, file = "data_output/aimplas_followers_info_now.csv")
  write_csv(followers.aimplas.past, file = "data_output/aimplas_followers_info_past.csv")
}

Aimplas_Data <- function(){
  aimplas.data <- users_data(lookup_users("aimplas"))
  write_csv(aimplas.data, "data_output/aimplas_data.csv")
}

Hashtags_Plasticos <- function() {
  #cargamos el archivo original
  load(file.path("data_tweets/plasticosfull.RData"))
  
  #limpiamos los hashtags
  hashtags.plasticos <- Quita_Palabras(plasticos.all.tweets)
  
  #omitimos na y valores vacios
  hashtags.plasticos <- na.omit(unlist(hashtags.plasticos$hashtags))
  
  #solucionar problema de plstico y plastico
  
  #creamos la tabla de frecuencias
  hashtags.plasticos <- as.data.frame(table(hashtags.plasticos)) %>% arrange(desc(Freq))
  
  #renombramos las columnas
  names(hashtags.plasticos) <- c("label", "value")
  
  #nos quedamos solo los que aparezcan un umbral de veces (un valor aceptable de momento son 175), y el resto se añaden a "otros"
  hashtags.plasticos <- rbind(hashtags.plasticos %>% filter(value > 130), data.frame(label = "otros", value = sum(hashtags.plasticos[which(hashtags.plasticos$value < 130), ]$value))) %>% arrange(desc(value))
  
  
  #añadimos fecha de actualizaacion
  hashtags.plasticos$DateUpdate <- Sys.Date()
  
  #lo guardamos
  #save(hashtags.plasticos, file = "data_output/hashtags_plasticos.RData")
  write_csv(hashtags.plasticos, file = "data_output/hashtags_plasticos.csv")
}


Graph_Interactions <- function() {
  mentions.aimplas.interactions.all <- read.csv("data_output/aimplas_interactuers.csv") 
  
  mentions.aimplas.interactions.graph <- mentions.aimplas.interactions.all %>% filter(weight > 3)
  
  mentions.aimplas.interactions.graph$source <- as.character(mentions.aimplas.interactions.graph$source)
  mentions.aimplas.interactions.graph$destinations <- as.character(mentions.aimplas.interactions.graph$destinations)
  
  #escogemos los nombres únicos de las cuentas
  source <- mentions.aimplas.interactions.graph %>% distinct(source)
  destination <- mentions.aimplas.interactions.graph %>% distinct(destinations)
  source <- source[c("source")]
  destination <- destination[c("destinations")]
  
  #cambiamos el nombre para hacer el full join
  names(source) = "label"
  names(destination) = "label"
  
  #hacemos el full join y asignamos un id a cada cuenta 
  node <- full_join(source, destination)
  node <- data.frame(label  = unique(node$label))
  node$label <- as.character(node$label)
  node <- node %>% rowid_to_column("id")
  node$group <- rep(1, nrow(node))
  
  #vamos a buscar la cuentas que más interacuan con aimplas
  aimplas.interaccion <- mentions.aimplas.interactions.graph %>% filter(destinations == "aimplas")
  
  #seleccionamos los que mas interactuan con aimplas 
  list.interact <- aimplas.interaccion[which(aimplas.interaccion$weight > 15), ]$source
  
  #los ponemos al grupo 2
  node[which(node$label == list.interact), ]$group = 2
  
  #seleccionamos a aimplas el grupo 3 debido a que queremos distinguirlo del resto
  node[which(node$label == "aimplas"), ]$group <- 3
  
  #arreglamos el edge
  aimplas.mentions <- mentions.aimplas.interactions.graph %>% select(source, destinations, weight)
  aimplas.mentions <- aimplas.mentions %>% ungroup()
  
  #transformamos los nombres a id
  edge <- aimplas.mentions %>% 
    left_join(node, by = c("source" = "label")) %>% 
    rename(from = id) 
  
  edge <- edge %>% 
    left_join(node, by = c("destinations" = "label")) %>% 
    rename(to = id) 
  
  #seleccionamos las columnas que nos interesan
  edge <- edge %>% select(from, to, weight)
  edge$label <- edge$weight
  
  edge <- edge %>% mutate(width = weight)
  
  #dividimos width por 20 para que sea visible
  edge$width <- edge$width/20
  
  #le ponemos una fuente de 20 a los nodos
  node$font.size <- rep(20, nrow(node))
  
  #eliminamos las variables que no sirven
  remove(aimplas.interaccion, list.interact, aimplas.mentions, source, destination, mentions.aimplas.interactions.graph)
  
  #hay que filtrar las interacciones de aimplas porque si no va muy lento y hay muchas que tienen solo 1 y no es relevante
  
  #creación del grafo
  graph.interactions.aimplas <- visNetwork(node, edge, height = "100vh", width = "100%") %>% 
    
    #para destacar las cuentas con las que una cuenta interacciona, hacemos highlightNearest = TRUE
    visOptions(highlightNearest = TRUE) %>%
    
    #Seleccionamos la posición de las flechas así como los colores. Hover indica el color de las relaciones de la cuenta en la que    dejamos el raton encima
    visEdges(arrows = "to",
             color = list(color = "#0085AF", highlight = "#C62F4B", hover = "#A72CD6")) %>%
    
    visNodes(color = list(background = "red", border = "blue", highlight = "yellow")) %>% 
    
    #activamos el hover para que funcione
    visInteraction(hover = TRUE) %>%
    
    #cambiamos el color de los grupos
    visGroups(groupname = "1", color = "green") %>%
    visGroups(groupname = "2", color = "orange") %>%
    visGroups(groupname = "3", color = "red")
  
  #guardamos el grafo
  save(graph.interactions.aimplas, file = "data_output/graph_aimplas.RData")
  
}

#traida de los tweets de aimplas
Tweets_Aimplas <- function() {
  #cargamos el archivo original
  load(file.path("data_output/aimplasfull.RData"))
  
  if(as.Date(max(aimplas.all.tweets$created_at)) != Sys.Date() - 1){
    #recogemos los tweets que contienen aimplas
    aimplas.tweets <- Traer_Tweets("aimplas")
    
    #fechas que faltan
    dates.aimplas <- seq(as.Date(max(aimplas.all.tweets$created_at)) + 1, Sys.Date() - 1, by = "days")
    
    #filtramos por los días
    aimplas.tweets.day <- aimplas.tweets[which(as.Date(aimplas.tweets$created_at) %in% dates.aimplas == TRUE), ]
    
    #unimos el archivo original a los filtrados por los dias deseados
    aimplas.all.tweets <- rbind(aimplas.all.tweets, aimplas.tweets.day)
    
    #si hay alguno repetido lo eliminamos
    aimplas.all.tweets <- aimplas.all.tweets %>% distinct(status_id, .keep_all = TRUE)
    
    #lo guardamos en un RData en la carpeta data
    save(aimplas.all.tweets, file = "data_output/aimplasfull.RData")
    
    #clear variables
    remove(dates.aimplas, aimplas.tweets, aimplas.tweets.day)
  }
  
}

#traida de los tweets de plasticos
Tweets_Plasticos <- function() {
  #cargamos el archivo original
  load(file.path("data_tweets/plasticosfull.RData"))
  #rdata_files <- list.files(path = "data_output/data_tweets_plastico/" ,pattern = "\\.RData")
  
  #plasticos.all.tweets <- NULL
  #for (i in rdata_files) {
  #  load(file.path(paste0("data_output/data_tweets_plastico/", i)))
  #  plasticos.all.tweets <- rbind(plasticos.all.tweets, i)
  #}
  
  if(as.Date(max(plasticos.all.tweets$created_at)) != Sys.Date() - 1){
    #recoger todos los tweets con plastico
    plastico.tweets <- Traer_Tweets("plastico")
    
    #seleccionamos las fechas de los tweets que no están en el dataframe completo
    dates.plasticos <- seq(as.Date(max(plasticos.all.tweets$created_at)) + 1, Sys.Date() - 1, by = "days")
    
    #seleccionamos los tweets de las fecas 
    plastico.tweets.day <- plasticos.all.tweets[which(as.Date(plasticos.all.tweets$created_at) %in% dates.plasticos == TRUE), ]
    
    #unimos el archivo original a los filtrados por los dias deseados
    plasticos.all.tweets <- rbind(plasticos.all.tweets, plastico.tweets)
    
    #si hay alguno repetido lo eliminamos
    plasticos.all.tweets <- plasticos.all.tweets %>% distinct(status_id, .keep_all = TRUE)
    
    #lo guardamos en un RData en la carpeta data
    save(plasticos.all.tweets, file = "data_output/plasticosfull.RData")
    
    #clear variables
    remove(dates.plasticos, plastico.tweets, plastico.tweets.day)
  }
  
}


#estadisticas diarias sacadas de twitter analytics
Stats_Tweets_Daily <- function() {
  #se cargan aqui las estadísticas de los tweets, procedentes de Twitter Analytics, que se deberán descargar e introducir manualmente en la carpeta data/stats_tweets. Despúes, se hace la pequeña manipulación para tener los datos listos para su ejecución en shiny
  
  #obtenemos la lista de archivos de nuestra carpeta donde hemos almacenado la información que tengan el formato csv
  filenames <- list.files(path = "data/stats_tweets", pattern = "\\.csv")
  
  #cargamos todos los dataframes en la variable all
  all <- lapply(filenames, function(i){
    read_csv(paste0("data/stats_tweets/", i))
    #file.remove(paste0("data/stats_tweets/", i))
  })
  
  lapply(filenames, function(i){
    file.remove(paste0("data/stats_tweets/", i))
  })
  
  colnames <- colnames(all[[1]])
  all <- lapply(all, setNames, colnames)
  
  #los unimos en un único dataframe
  daily.stats <- do.call(rbind, all)
  
  #eliminamos las fechas iguales
  daily.stats <- daily.stats %>% dplyr::distinct(Fecha, .keep_all = TRUE)
  
  write_csv(daily.stats, "data/stats_tweets/daily_stats.csv")
  
  #manipulacion de los dataframes
  
  #eliminamos las columnas que no nos interesan
  daily.stats <- daily.stats[-c(21:38)]
  
  #modificamos los nombres de las columnas
  colnames(daily.stats) <- c("Fecha", "Tweets_publicados", "Impresiones", "Interacciones", "Tasa_interaccion", "Retweets", "Respuestas", "Likes", "Clics_de_Perfil_de_Usuario", "Clics_en_URL", "Clics_de_Etiquetas", "Ampliaciones_De_Detalles", "Clics_en_Enlaces_Permanentes", "Se_Abre_la_Aplicacion", "Descargas_de_App", "Seguimientos", "Enviar_Tweet_por_Email", "Marcar_Telefono", "Visualizaciones_Multimedia", "Interacciones_con_el_Contenido_Multimedia")
  
  #variamos la tasa de interaccion a porcentaje
  daily.stats$Tasa_interaccion <- 100 * daily.stats$Tasa_interaccion
  
  #convertimos a date
  daily.stats$Fecha <- as.Date(daily.stats$Fecha)
  
  #creamos la columna date con la fecha
  daily.stats$Date <- as.POSIXct(daily.stats$Fecha)
  
  #eliminamos las columnas que tengan 0 en todo ya que no es relevante analizarlas
  daily.stats <- daily.stats[-(which(colSums(daily.stats[-c(1, ncol(daily.stats))]) == 0)+1)]
  
  #guardamos el dataframe
  write_csv(daily.stats, file = "data_output/daily_stats.csv")
  
  #eliminamos las variables que no nos sirvan
  remove(filenames, all)
}

#tweets diarios de twitter analytics
Tweets_Daily <- function() {
  filenames <- list.files(path = "data/tweets", pattern = "\\.csv")
  
  #cargamos todos los dataframes en la variable all
  all <- lapply(filenames, function(i){
    read_csv(paste0("data/tweets/", i))
  })
  
  lapply(filenames, function(i){
    file.remove(paste0("data/tweets/", i))
  })
  
  colnames <- colnames(all[[1]])
  all <- lapply(all, setNames, colnames)
  
  #los unimos
  all.tweets <- do.call(rbind, all)
  
  all.tweets <- all.tweets %>% dplyr::distinct(`ID del Tweet`, .keep_all = TRUE)
  
  write_csv(all.tweets, file = "data/tweets/all_tweets.csv")
  
  #seleccionamos las columnas que nos interesan y eliminamos las que no tienen valor
  all.tweets <- all.tweets[c(1:21)]
  all.tweets <- all.tweets[-(as.data.frame(which(colSums(all.tweets[c(5:21)]) == 0))[[1]] + 4)] 
  
  #creamos una columna con solamente la fecha del tweet ya que es un análisis diario
  all.tweets$Date <- as.Date(all.tweets$hora)
  
  #eliminamos las variables que no vayamos a usar
  remove(filenames, all)
  
  #guardamos el dataframe
  #save(all.tweets, file = "data_output/all_tweets.RData")
  write_csv(all.tweets, file = "data_output/all_tweets.csv")
}

#analisis de sentimientos de los plasticos
Plasticos_Sentimientos_nrc <- function() {
  load(file.path("data_tweets/plasticosfull.RData"))
  
  my.list.swords <- data.frame(text = c("a", "jaja", "d", "q", "qe", "k", "ke", "", "pq", "pk", "jajajjaja", "jajajajajajajajajjajajajaha", "jajajaj","jajajajajajajajajjajajajaha", "jajajajajajajajajjajajajaha", "jajaja", "aajjajajajajaj", "jajajajj","jaja", "jeje", "jsjsjsjjalabqbajqj", "jsjjajsjasjsba", "jjaja", "jjajajajajajaja", "xd", "xdnt", "xdd","xdddddd", "xdddddddddddd", "xddd", "xddddddd", "xdddd", "t", "jsjsjdjs", "jajajjaaja", "jajajaajaj", "jajajajajaja", "jajajajaja", "jajajjaaj", "jjajaja", "jajajjaajajaj", "jdsjsjjs", "jajajajajajajajjajajajja", "ajajajajja", "jajajjajajaj", "jjdjsk", "jajajsjsjjajajajsjajajajs", "kkjjj", "jjjjjj", "ajjajajajajja", "jajajjajajajaj", "jajjajajja", "ajajajajaja", "jajajajjaaj", "ajajaja", "ajjajaja", "ajajajaja", "ajajaja", "jaajjaja", "jajajajajjajajaja", "jajjaa", "jajajajja", "jajajajajj", "jajajjajaja", "jajaajjaa", "jajajajajjajajajaja", "jjajjsaj", "jsjsjjajajajja", "jajajajja", "jaajjajajajajaja", "jajajajjaja", "ajjaajajaja", "jajajajajja", "jajajajsjaa", "jajjajaja", "jajjajaa", "sjsjjs", "jajja", "jjaajaj", "jajajajajajjajajajaja", "jajja", "jjajajajajaja", "jajjajsjajjaja", "jajajajajjajaaj", "jajajajajajajajjaja", "jajajaajajajja", "jajajja", "jajajajjajajajajajajajajajajjajajajajajaja", "ajjdjnsmddlwkwkendkswkqksjdnsjsjajajshsh", "jajajjjaa", "jajajajajajja", "jajajajjsjkssb", "jajajaajja", "jjsjaja", "jajajsjjs", "jajajjaa", "jajjajjajaja", "anjaajajjsj", "jajajajajajajajajjajajaja", "jejje", "jajajjaa", "ajjajajaj", "jajajajjajajaaj", "jajjajajajajajajajajaja", "ajsjjajasa", "jajajajsj", "jsjsjajajajj", "ajajajajajja", "jajajajajjajajajajajjajaja", "jajajajjjaja", "ajsjjaj", "jajajajajajajajajja", "jejeje", "ajajjaaja", "jajajajajjaa", "jajajjaj", "akdjjdjd", "jjajajajjaja", "jajajjajajajajajajjajajajajaja", "ajjaajajajja", "jajajajjaa", "jajajajajajjaaj", "sjjsjss", "jajajajjaaja", "jajjaja", "jajajaja", "jajjaja", "kjjjj", "jajajajaj", "jajajs", "jajajajajajajaja", "jajajajaaja", "jajajajajajaj", "guaaaajajajaja", "jasjajsaj", "jajajajajaj", "jsjssjsjaja", "jajajssjsjajaajajajajaajajaja", "jajsjsjaa", "jajajajajajaja", "jajajaaajajajajaja", "puajajaja", "jajaj", "jijijaja", "jajajajajajajajajajaja", "jajajaajajajaaja", "kajajajaj", "jajajaa", "jajajajajajajaj", "jajsjajsjsjs", "jajsjsjaja", "jajsjsjaj", "ajajajaj", "jajajajajajajajajaj", "ajaja", "jajajajajajajajajaja", "jajaajaj", "jajajakkja", "jajajajajajajaa", "jajajaajsj", "jajskska", "jajajusjdkdjak", "jakajakajak", "jsjsjs", "jsjajs", "jajaajaj", "jajajajajajajajaj", "jajajshhsh", "jajaaja", "jdhajaj", "jajaajaja", "kajsjajs", "jajajun", "jajususjuji", "jajajajs", "jaj", "sjajajajaj", "jajaaj", "jajsjsj", "jajajajajajajajajajajajajajajaj", "jajsjajs", "ajajajsj", "jajsksajsjs", "jajajsj", "jdjsjajajajajajajajs", "ajdjajs", "jaajajajajaja", "mamojajaja", "jajahshja", "jajsja", "jajajakakak", "ajhsjajaj", "ajsjajaj", "ajajajajajajakakaa", "jajajajajaajajajaj", "janajaj", "jajajajaajajajaja", "jajajx", "jajaajajaj", "ajajajajajaj", "jajskjasjkajkjkas", "jajajajajaajajaj", "kajsjaj", "wajajajajajajajajaja", "akaksjaj", "jajajajaowboys", "ajajajajajajaja", "jejejejeje", "jejejeje", "jejejej", "jiji", "jijiji", "jojojojo", "hahah", "haha", "hahaha", "hahahah", "hahahha", "hahahhaha", "ahahah", "hahahaha", "hahahahah", "hahahahahahhahahahahaaha", "hahahahalloween", "gt", "mas", "asi", "ahi", "ir", "dia", "tambien", "despues", "do", "jajsjajsdj", "jajsjs", "ajjajajajaja", "jajsj", "jajajjajwjwjdf", "jajsjs", "jajd", "jajjaj", "jajajj", "jajajajana", "jajajajjajajaj", "bajajajaj", "jajdjajdjs", "jajjajajaja", "ajajajjajajajajaj", "jajajajaajjaajaa", "jajajajajjajajajakaa", "hahakajaj", "jajajsjajajaaja", "jajajakakjajajajajajajajajaja", "jsjajaja", "jajdkqkskkqkdkq", "wknxkwkf", "jwjdjwjxjjwjfj", "jajdjjaja", "jajajajajajaajaja", "jajajajaaj", "jajajajaa", "jaksjajak", "jajs", "jajajajajaaa", "sjrnjajfjjf", "jajsjsjs", "jajajajajajajajajajajajajajajajajajajjajajajaja", "jaajjajajajajajajjaja", "jajajajajajajajaajajajajajaajajajaajaja", "jajajaaj", "jajaajajaja", "jajajajaaaa", "jajaja", "jajajjajajajajjaja"))
  
  my.list.swords2 <- data.frame(text=c("jajaaaaa", "jajajjajajjajaja", "jajssjsjdsjadjasdjasd", "kdas", "jajjjaja", "jajajjsjs", "jajjjaaja", "hdp", "jjajajajajajajajaja", "jajajajjajajajajjajajajajja", "ajakakajajjakakakaja", "ajjajaaj", "hajajj", "jajajaajajaja", "jajjajja", "hajajakajajaha", "jajajajajajajajaja", "ajajajajajjaajjaja", "etc","etcetera", "jajj", "jsjsjaja", "jajaa", "jajsjajaa", "jajajajajajaajjajakak", "jjajajja", "jajajajajajj", "jajjdjdb", "jajsjajsjaka", "jajajjajja", "jajaajjaajaj", "jaajajajjaja", "jajajajajaajajajajajajajajaaja", "jajjaajajajajajajajaaj", "ajjaj", "janajajajja", "jajssjsjaj", "jajajajjja", "jajkaajjjjqjqjjqjqja", "jajajajajajajajqjqja", "jakakajaj", "ajajajsjsjssj", "jajajajajajaa", "jajsjsja", "jsjsjaja", "jajsjjs", "jajajaaja", "jajajajajala", "wajajaja", "jajajajajaka", "ajajajajajajjaa", "jajjajjajjaja", "jakajajajajaj", "ajakakajajjakakakajaj", "jaajajaj", "jajajajaajaj", "ajjajajajajaja", "jajaaa", "jajajajjajaja", "jajajak", "ajajajajs", "ajajajajaj", "jajajaajj", "jajaksjaja", "nahsjajajaja", "jajajajajajajajajaa", "jajaajajajajajaja", "jsjdjajdjajdjajdhajdjaj", "jjajaajajajajajajaj", "jakajajajsja", "jakajaaaajajaaaa", "jajsjajaja", "jajsjajaja", "jajajajjajaja", "jajajajajajajja", "jajajajajajajajajajajajjaja", "jajajajajaa", "jajajaajaja", "jahshajajsjajajaj", "hhajajjayay", "jajaksjaja", "jajajajajajajja", "aajjajajaajjaja", "jajajaajaja", "jajajajajaa", "jajajajajajjajaja", "jajsjajaja", "jajjajaj", "aj", "jajjajjajajja", "jajjajaj","ja", "weon", "jsjajsja", "jsjajsj", "jajjajjja", "jajajajjajaja", "jsjajsj", "jajajajajajjajaja", "jajajak", "nahsjajajaja", "jajajajajaa","jajaksjaja", "jajajajajaa", "jakajajajsja", "ajajajajs", "jajajajajjaaa", "jajajaajjaaja", "jaajajja", "jja", "jjaaaaaaaaa", "jajajajahahahahaha", "jajajaajjaaja", "jajajajajjaaa", "jajaja", "jajshsyaj", "jajajajajajaja", "jaajajja", "ahahhahaha", "jajajajahahahahaha", "hahahhhh", "hahahahahhaa", "hahahahaha", "hahahahahaha", "hahahahahahahaha", "hahahahha", "jakakajakahah", "ajahahaj", "jasgahahah", "hahahahaha", "jejej", "jej", "hehrheheh", "hehehe", "xq", "mama", "papa", "pene", "pedo", "it", "ah","eh", "oh", "uh", "hola", "cirujano", "labio", "hajahahaha", "by", "in"))
  
  my.list.swords3 <- data.frame(text=c("x", "bienvenido", "the", "hechizo", "teniar", "vio", "trave", "mayday", "llegar", "algun", "adema", "sero", "in", "rubir", "blad", "cuyo", "now", "habio", "halloweer", "pedazo", "g", "par", "alguien", "profesor", "disculpe", "h", "congratular", "segun", "extr", "v", "aco", "applir", "of", "w", "comar", "gustario", "pro", "dr", "coraz", "by", "cuchar", "willie", "tonto", "adaptar", "adeptar", "cacho", "roso", "puto", "estario", "x", "g", "plar", "italia", "turquía", "egipto", "hd", "ramen", "jack", "aco", "mm", "cm", "segun", "rojo", "disculpe", "ipad", "perforador", "compuesta", "h", "ejecutar", "mostrar", "tubular", "squarar", "teatro", "chupar", "echo", "so", "verduro", "times", "amor", "gracia"))
  
  #unimos todos los stopwords (buscar la manera de automatizar este proceso)
  stopwords.all <- rbind(data.frame(text = (stopwordslangs %>% filter(lang == "es" & p > 0.9999))$word), my.list.swords, my.list.swords2, my.list.swords3)
  
  #limpiamos los tweets
  plastico.sents <- Limpia_Tweets(plasticos.all.tweets)
  plastico.sents <- Quita_Palabras(plastico.sents)
  plastico.sents$text <- removeWords(plastico.sents$text, stopwords.all$text)
  plastico.sents$text <- removeWords(plastico.sents$text, stopwords('es'))
  plastico.sents$text <- removeWords(plastico.sents$text, stopwords('en'))
  plastico.sents$text <- gsub("\\s+", " ", plastico.sents$text)
  
  #quitamos los espacios
  plastico.sents$text <- trimws(plastico.sents$text, which = "left")
  
  #analisis de sentimiento
  plastico.sents$sentiment <- get_nrc_sentiment(plastico.sents$text, language = "spanish") #esto se toma su tiempo (aproximadamente 1:15h)
  
  #hacemos la diferencia para evaluar el impacto
  plastico.sents$sentiment$difsent <- plastico.sents$sentiment$positive - plastico.sents$sentiment$negative
  
  #guardamos el dataframe con sentimientos
  save(plastico.sents, file = "data_output/plasticos_all_sentiments.RData")
  
  plastico.sents.table <- data.frame(label = c("Positivos", "Negativos", "Neutros"), value = c(length(which((plastico.sents$sentiment$difsent > 0) == TRUE)), length(which((plastico.sents$sentiment$difsent < 0) == TRUE)), length(which((plastico.sents$sentiment$difsent == 0) == TRUE))))
  
  #date update
  plastico.sents.table$DateUpdate <- Sys.Date()
  
  write_csv(plastico.sents.table, file = "data_output/plastico_sents.csv")
  
}

Plasticos_Sentimientos_syuzhet <- function() {
  #cargamos datos
  rdata_files <- list.files(path = "data_output/data_tweets_plastico/" ,pattern = "\\.RData")
  
  plasticos.all.tweets <- NULL
  for (i in rdata_files) {
    load(file.path(paste0("data_output/data_tweets_plastico/", i)))
    plasticos.all.tweets <- rbind(plasticos.all.tweets, i)
  }
  
  #valor de sentimiento con el metodo syuzhet
  sent.value <- get_sentiment(plasticos.all.tweets$text, method = "syuzhet", language = "spanish")
  
  #separamos los positivos, negativos y neutros
  positivos <- plasticos.all.tweets[sent.value > 0, ]
  negativos <- plasticos.all.tweets[sent.value < 0, ]
  neutros <- plasticos.all.tweets[sent.value == 0, ]
  
  #creamos el dataframe
  analisis.tweets <- data.frame(label = c("positivos", "negativos", "neutros"), value = c(nrow(positivos), nrow(negativos), nrow(neutros)))
  
  #date update
  analisis.tweets$DateUpdate <- Sys.Date()
  
  write_csv(analisis.tweets, file = "data_output/plastico_sents_syuzhet.csv")
  
}

