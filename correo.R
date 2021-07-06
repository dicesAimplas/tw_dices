library(mailR)
sender <- "sjaunesaimplas@gmail.com"
recipients <- c("sjuanes@aimplas.es")
send.mail(from = sender,
          to = recipients,
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "sjaunesaimplas@gmail.com",            
                      passwd = "Noviembre2020", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
