library(rtweet)

twitter_token <- create_token(
  app = "AIMPLAS",
  consumer_key = '1XGKl9YCewt9bb9gSPB2XCllu',
  consumer_secret = '4SheB2UP5Iwj1ZgFETmULdEUvkHtTYha9M6CtG2TFl78sPxFfd',
  access_token = '525164307-GbJrPL3uvrVvPEJ8zvcBa49SKkj1zfzxOJnNppAR',
  access_secret = 'OWjar31m93kHcYAzHFvHfJQo3p0l2y1jQVbPcyOybtt13'
)

mentions.aimplas.interactions.all <- read.csv("data_output/aimplas_interactuers.csv")

data <- get_timeline("aimplas", n = 10000)
