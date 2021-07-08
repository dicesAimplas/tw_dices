#funciones auxiliares

#funcion para traer tweets de una palabra clave
Traer_Tweets <- function(clave) {
  
  #clave: (string), palabra la cual se desean buscar los últimos tweets
  
  variable <- search_tweets(
    clave, n = 18000, include_rts = FALSE, retryonratelimit = TRUE, lang="es"
  )
  
  return(variable)
}


#funcion para limpiar tweets
Limpia_Tweets <- function(df){
  
  #df debe ser un dataframe creado por la función search_tweets, el cual incluye una columna con el texto del tweet
  
  df$text <- tolower(df$text)
  df$text <- gsub('á', 'a', df$text)
  df$text <- gsub('é', 'e', df$text)
  df$text <- gsub('í', 'i', df$text)
  df$text <- gsub('ó', 'o', df$text)
  df$text <- gsub('ú', 'u', df$text)
  df$text <- gsub('ü', 'u', df$text)
  df$text <- gsub('ñ', 'ny', df$text)
  df$text <- gsub("[^\x01-\x7F]", "", df$text)
  df$text <- gsub("http\\S*", "", df$text)
  df$text <- gsub("@\\w+", " ", df$text)
  df$text <- gsub("#\\w+", " ", df$text)
  df$text <- gsub("[[:punct:]]", " ", df$text)
  df$text <- gsub("[[:digit:]]", " ", df$text)
  df$text <- gsub("\\s+", " ", df$text)
  df$text <- gsub('href', '', df$text)
  return(df)
}

#función para quitar tweets que hablan de temas diferentes a los que queremos

Quita_Palabras <- function(df){
  
  #df debe ser un dataframe creado por la función search_tweets, el cual incluye una columna con el texto del tweet
  
  #eliminamos tweets que carecen de informacion relevante
  df <- dplyr::filter(df, !grepl("justchatting|karchezmeme|trump|laburdeos|lachicadeltren|esnape|snaper", text))
  df <- dplyr::filter(df, !grepl("artista|artistas|cirujano|cirujanos|adesca|80s|a3seriesint|tetas|policia|brianadevolvelastetas", text))
  return(df)
}