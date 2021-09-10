# Escucha en redes: Repositorio de almacenamiento de los datos

# Descripción
Este repositorio se ha creado de manera pública para poder almacenar los datos descargados con la librería _rtweet_ en R de manera automática con la herramienta de GitHub actions.

## Ficheros yml
Los ficheros yml están localizados en la carpeta ".github/workflows". Estos ficheros son ejecutados con la herramienta de GitHub Actions, de modo que cada uno de ellos ejecutará una orden diferente (escrita en el código). En dicha carpeta, hay localizados 3 ficheros diferentes:

  * r.yml: ejecuta el script tw_update_timeline_interactions.R a las 12:00 cada día.
  * r_tweets.yml: ejecuta el script tw_update_tweets_sents.R a las 02:15 cada día.
  * r_weekly.yml: ejecuta el script tw_update_followers_semanal.R a las 02:00 cada lunes.

El contenido de cada script y su funcionalidad está explicado en el repositorio principal.

Los archivos yml situados en la carpeta workflows funcionan de manera que se ejecutan de acuerdo a un cron (que es un administrador regular de procesos en segundo plano que ejecuta procesos en intervalos regulares). Nos fijaremos en, por ejemplo, el código del fichero r_weekly.yml. En la parte superior, encontramos el nombre que le damos al workflow (que son los diferentes entornos de trabajo que se emplean. Está bien usar un nombre para cada uno de los archivos, de modo que así se comprueba, si hubiese algún error, en qué workflow se ha producido) Debajo del nombre, encontramos el código que indica que se ejecute el archivo en el minuto 0 a la hora 0 a en cualquier dia del mes para cualquier mes y que sea el primer dia de la semana. En la página https://crontab.guru/ se encuentra un generador de expresiones cron en la que se explica detalladamente. Si quisieramos que fuera en todos los días de la semana, indicaríamos un * en lugar de un 1:

```
on:
  schedule:
    - cron: '0 0 * * 1'
```

Después, se indican el trabajo que realizará (con jobs: autoscrape:). Primero se indica el sistema operativo y la versión en la cual se deberá realizar todo el proceso:

```
runs-on: macos-latest
```

Carga el repositorio e instala R: 
```
 steps:
    - uses: actions/checkout@master
    - uses: r-lib/actions/setup-r@master
```

Instala todas las librerías necesarias para la ejecución del script de R:
```
    - name: Install packages
      run: |
        R -e 'install.packages("rtweet", dependencies=TRUE)'
        R -e 'install.packages("tidyverse", dependencies=TRUE)'
        R -e 'install.packages("visNetwork", dependencies=TRUE)'
        R -e 'install.packages("tm", dependencies=TRUE)'
        R -e 'install.packages("RJSONIO", dependencies=TRUE)'
        R -e 'install.packages("RCurl", dependencies=TRUE)'
        R -e 'install.packages("syuzhet", dependencies=TRUE)'
        R -e 'install.packages("lubridate", dependencies=TRUE)'
        R -e 'install.packages("xts", dependencies=TRUE)'
        R -e 'install.packages("visNetwork", dependencies=TRUE)'
```

Indicar el script que tiene que ejecutar:
```
    - name: Scrape
      run: Rscript tw_update_followers_semanal.R
```

Comandos en consola para actualizar los ficheros de datos:
```
    - name: Commit files
      run: |
        git config --local user.name actions-user
        git config --local user.email "actions@github.com"
        git add data_output
        git commit -m "GH ACTION Headlines $(date)"
        git push origin main
      env:
        REPO_KEY: ${{secrets.GITHUB_TOKEN}}
        username: github-actions
```

