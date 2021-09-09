source("src/fun_auxiliar.R")
source("src/functions.R")

packages <- c("rtweet", "tidyverse", "visNetwork", "tm", "RJSONIO", "RCurl", "syuzhet", "lubridate", "xts", "readr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#tokens de twitter necesarios para la libreria rtweet
twitter_token <- create_token(
  app = "AIMPLAS",
  consumer_key = '1XGKl9YCewt9bb9gSPB2XCllu',
  consumer_secret = '4SheB2UP5Iwj1ZgFETmULdEUvkHtTYha9M6CtG2TFl78sPxFfd',
  access_token = '525164307-GbJrPL3uvrVvPEJ8zvcBa49SKkj1zfzxOJnNppAR',
  access_secret = 'OWjar31m93kHcYAzHFvHfJQo3p0l2y1jQVbPcyOybtt13'
)


main <- function() {
  #recogida de tweets en los que aparece "aimplas"
  Tweets_Aimplas()
  
  #recogida de tweets en los que aparece "plastico"
  #Tweets_Plasticos()
  
  #analisis de sentimientos
  #Plasticos_Sentimientos_syuzhet()
  
}

if(getOption("run.main", default=TRUE)) {
  main()
}
