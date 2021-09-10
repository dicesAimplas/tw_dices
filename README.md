# Escucha en redes: Repositorio de almacenamiento de los datos

# Descripción
Este repositorio se ha creado de manera pública para poder almacenar los datos descargados con la librería _rtweet_ en R de manera automática con la herramienta de GitHub actions.

## Ficheros yml
Los ficheros yml están localizados en la carpeta ".github/workflows". Estos ficheros son ejecutados con la herramienta de GitHub Actions, de modo que cada uno de ellos ejecutará una orden diferente (escrita en el código). En dicha carpeta, hay localizados 3 ficheros diferentes:

  * r.yml: ejecuta el script tw_update_timeline_interactions.R a las 12:00 cada día.
  * r_tweets.yml: ejecuta el script tw_update_tweets_sents.R a las 02:15 cada día.
  * r_weekly.yml: ejecuta el script tw_update_followers_semanal.R a las 02:00 cada lunes.

El contenido de cada script y su funcionalidad está explicado en el repositorio principal.

Los archivos yml situados en la carpeta workflows funcionan de manera que se ejecutan de acuerdo a un cron (que es un administrador regular de procesos en segundo plano que ejecuta procesos en intervalos regulares). Nos fijaremos en, por ejemplo, el código del fichero r_weekly.yml. En la parte superior, encontramos

```
on:
  schedule:
    - cron: '0 0 * * 1'
```

lo cual indica que se ejecute el archivo en el minuto 0 a la hora 0 a en cualquier dia del mes para cualquier mes y que sea el primer dia de la semana. En la página https://crontab.guru/ se encuentra un generador de expresiones cron en la que se explica detalladamente. Si quisieramos que fuera en todos los días de la semana, indicaríamos un * en lugar de un 1.
