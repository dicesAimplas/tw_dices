source("src/fun_auxiliar.R")
source("src/functions.R")

packages <- c("rtweet", "tidyverse", "visNetwork", "tm", "RJSONIO", "RCurl", "syuzhet", "lubridate", "xts")
invisible(lapply(packages, library, character.only = TRUE))

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
