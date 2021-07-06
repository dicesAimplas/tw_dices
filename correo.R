library("RDCOMClient")
OutApp <- COMCreate("Outlook.Application")

## create an email
outMail = OutApp$CreateItem(0)

## configure email parameter
outMail[["To"]] = "sjuanesaimplas@gmail.com"
outMail[["subject"]] = "Test Email"
outMail[["body"]] = "Hi, How are you?"

## send it
outMail$Send()
