source("src/fun_auxiliar.R")
source("src/functions.R")

packages <- c("rtweet", "tidyverse", "visNetwork", "tm", "RJSONIO", "RCurl", "syuzhet", "lubridate", "xts")
invisible(lapply(packages, library, character.only = TRUE))

twitter_token <- create_token(
  app = "AIMPLAS",
  consumer_key = '1XGKl9YCewt9bb9gSPB2XCllu',
  consumer_secret = '4SheB2UP5Iwj1ZgFETmULdEUvkHtTYha9M6CtG2TFl78sPxFfd',
  access_token = '525164307-GbJrPL3uvrVvPEJ8zvcBa49SKkj1zfzxOJnNppAR',
  access_secret = 'OWjar31m93kHcYAzHFvHfJQo3p0l2y1jQVbPcyOybtt13'
)


main <- function() {
  #datos de las interacciones con aimplas
  Mention_Aimplas_Interactions()
  
  #timeline de aimplas
  Timeline_Stats()
  
  #actualizacion info cuenta aimplas
  Aimplas_Data()
  
  #actualizacion ddel grafo
  Graph_Interactions()
  
}

if(getOption("run.main", default=TRUE)) {
  main()
}
