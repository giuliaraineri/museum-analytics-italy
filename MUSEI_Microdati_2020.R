###PRACTICAL ESSAY###
#Survey on Italian museum and similar institutions in 2020#

setwd("C:\\Users\\giuli\\OneDrive - unibs.it\\Desktop\\Data anlysis and big data lab\\PRACTICAL ESSAY\\MUSEI_2020_IT_TXT\\MICRODATI")
getwd()

library(readxl)
dati <- read_excel("MUSEI_Microdati_2020.xlsx")
View(dati)                                                                    
str(dati)

#DEFINITION OF VARIABLES
dati$CATEGO <- factor(dati$CATEGO, labels=c("Museum, gallery and/or collection","Archaeological area or park","Monument or monumental complex"))                
dati$PUBBPRIV <- factor(dati$PUBBPRIV, labels=c("Public","Private"))
dati$OPENGENN <- factor(dati$OPENGENN, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENFEBB <- factor(dati$OPENFEBB, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENMARZ <- factor(dati$OPENMARZ, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENAPRI <- factor(dati$OPENAPRI, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENMAGG <- factor(dati$OPENMAGG, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENGIUG <- factor(dati$OPENGIUG, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENLUGL <- factor(dati$OPENLUGL, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENAGOS <- factor(dati$OPENAGOS, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENSETT <- factor(dati$OPENSETT, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENOTTO <- factor(dati$OPENOTTO, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENNOVE <- factor(dati$OPENNOVE, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$OPENDICE <- factor(dati$OPENDICE, labels=c("Yes","Not to the public","No, for anyone","Data not available"))
dati$CHIUSO <- factor(dati$CHIUSO, labels=c("No answer","Lack of financial resources","Staff shortages","For impossibility to adopt the measures foreseen for the containment of the sanitary emergency","Due to shortage of visitors","Other"))
dati$RIAPERTO <- factor(dati$RIAPERTO, labels=c("No answer","Yes, it has reopened","No, but plans to reopen","No and don’t know if it will reopen"))
dati$CATALOGO <- factor(dati$CATALOGO, labels=c("Yes","No"))
dati$LABORATORI <- factor(dati$LABORATORI, labels=c("Yes","No"))
dati$INCONTRI <- factor(dati$INCONTRI, labels=c("Yes","No"))
dati$SOCIAL <- factor(dati$SOCIAL, labels=c("Yes","No"))
dati$SITOWEB <- factor(dati$SITOWEB, labels=c("Yes","No"))
dati$`VIRTUAL TOUR`<- factor(dati$`VIRTUAL TOUR`, labels=c("Yes","No"))
dati$ALMENO1 <- factor(dati$ALMENO1, labels=c("Yes","No"))
dati$INVEST_SERVIZONLINE <- factor(dati$INVEST_SERVIZONLINE, labels=c("No answer","Online booking of the visit","Virtual tour","Online activities",	"Presence on social channels","Presentation of goods and/or collections on streaming platforms","Other"))

#1- DISTRIBUTIONS
freqCATEGO <- table(dati$CATEGO)
sum(freqCATEGO)
freqCATEGO
freqRELCATEGO <- freqCATEGO/sum(freqCATEGO)
freqRELCATEGO
colors1 <- terrain.colors(3)
pie(freqCATEGO, main = "Distribution of cultural sites", col=colors1, radius=1)

freqREGIONE <- table(dati$REGIONE)
freqREGIONE
df <- as.data.frame(freqREGIONE)
df <- df[order(freqREGIONE, decreasing = T),]
df
barplot(freqREGIONE, main="Distribution of cultural sites per regions", xlab="Region", ylab="Distribution", col=colors1)

freqPROVINCIA <- table(dati$PROVINCIA)
freqPROVINCIA
df <- as.data.frame(freqPROVINCIA)
df <- df[order(freqPROVINCIA, decreasing = T),]
df

Citt_IT <- 59450000
Musei_x_IT <- (Citt_IT/sum(freqCATEGO))/1000
Musei_x_IT

Citt_To <- 3692865
Musei_To <- 541
Musei_x_To <- (Citt_To/Musei_To)/1000
Musei_x_To

Citt_Em <- 4438937
Musei_Em <- 402
Musei_x_Em <- (Citt_Em/Musei_Em)/1000
Musei_x_Em

Citt_Lo <- 9981554
Musei_Lo <- 387
Musei_x_Lo <- (Citt_Lo/Musei_Lo)/1000
Musei_x_Lo

freqPubbPriv <- table(dati$PUBBPRIV)
freqRELPubbPriv <- freqPubbPriv/sum(freqPubbPriv)
freqRELPubbPriv
pie(freqRELPubbPriv, main = "Distributions in the public and private sector", col=colors1) 

#2- OPENINGS
freqGenn <- table(dati$OPENGENN)
freqRELGenn <- freqGenn/sum(freqGenn)
freqRELGenn

freqFebb <- table(dati$OPENFEBB)
freqRELFebb <- freqFebb/sum(freqFebb)
freqRELFebb

freqMarz <- table(dati$OPENMARZ)
freqRELMarz <- freqMarz/sum(freqMarz)
freqRELMarz

freqApri <- table(dati$OPENAPRI)
freqRELApri <- freqApri/sum(freqApri)
freqRELApri

freqMagg <- table(dati$OPENMAGG)
freqRELMagg <- freqMagg/sum(freqMagg)
freqRELMagg

freqGiug <- table(dati$OPENGIUG)
freqRELGiug <- freqGiug/sum(freqGiug)
freqRELGiug

freqLugl <- table(dati$OPENLUGL)
freqRELLugl <- freqLugl/sum(freqLugl)
freqRELLugl

freqAgos <- table(dati$OPENAGOS)
freqRELAgos <- freqAgos/sum(freqAgos)
freqRELAgos

freqSett <- table(dati$OPENSETT)
freqRELSett <- freqSett/sum(freqSett)
freqRELSett

freqOtto <- table(dati$OPENOTTO)
freqRELOtto <- freqOtto/sum(freqOtto)
freqRELOtto

freqNove <- table(dati$OPENNOVE)
freqRELNove <- freqNove/sum(freqNove)
freqRELNove

freqDice <- table(dati$OPENDICE)
freqRELDice <- freqDice/sum(freqDice)
freqRELDice

OpenGenn <- 0.72
OpenFebb <- 0.69
OpenMarz <- 0.15
OpenApri <- 0.20
OpenMagg <- 0.12
OpenGiug <- 0.50
OpenLugl <- 0.67
OpenAgos <- 0.68
OpenSett <- 0.70
OpenOtto <- 0.57
OpenNove <- 0.13
OpenDice <- 0.05
Openings <- c(OpenGenn, OpenFebb, OpenMarz, OpenApri, OpenMagg, OpenGiug, OpenLugl, OpenAgos, OpenSett, OpenOtto, OpenNove, OpenDice)
Openings1 <- data.frame(OpenGenn, OpenFebb, OpenMarz, OpenApri, OpenMagg, OpenGiug, OpenLugl, OpenAgos, OpenSett, OpenOtto, OpenNove, OpenDice)
colors2 <- terrain.colors(3)
barplot(Openings, main = "Openings", ylab = "Percentage of openings", xlab = "Months", names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sept", "Oct", "Nov", "Dec"), col=colors2, ylim=c(0,1))

tapply(dati$TOTVISIT,dati$PROVINCIA,mean)
mean(dati$OPENGG)
sum(dati$TOTVISIT)

Year2011 <- 217948000
Year2015 <- 237510000
Year2017 <- 258398000
Year2018 <- 275372000
Year2020 <- 36067125
SerieStorica <- c(Year2011, Year2015, Year2017, Year2018, Year2020)
plot(SerieStorica, main ="Historical series of total visits", ylab= "Total visits", xlab = "Year", col ="Blue")

freqTOTVISITPROVINCIA <- tapply(dati$TOTVISIT,dati$PROVINCIA,sum)
freqTOTVISITPROVINCIA
df <- as.data.frame(freqTOTVISITPROVINCIA)
df <- df[order(freqTOTVISITPROVINCIA, decreasing = T),]
df

#3- CLOSURES AND REOPENINGS
freqChiuso <- table(dati$CHIUSO)
freqRELChiuso <- freqChiuso/sum(freqChiuso)
freqRELChiuso
pie(freqRELChiuso, main="Reasons for closure")


freqRiaperto <- table(dati$RIAPERTO)
freqRELRiaperto <- freqChiuso/sum(freqChiuso)
freqRELRiaperto

#4- SERVICES
freqAlmeno1 <- table(dati$ALMENO1)
freqRELAlmeno1 <- freqAlmeno1/sum(freqAlmeno1)
freqRELAlmeno1
pie(freqRELAlmeno1, main = "At least 1 service activated", col=terrain.colors(3))

freqCatalogo <- table(dati$CATALOGO)
freqRELCatalogo <- freqCatalogo/sum(freqCatalogo)
freqRELCatalogo

freqLab <- table(dati$LABORATORI)
freqRELLab <- freqLab/sum(freqLab)
freqRELLab

freqIncontri <- table(dati$INCONTRI)
freqRELIncontri <- freqIncontri/sum(freqIncontri)
freqRELIncontri

freqSocial <- table(dati$SOCIAL)
freqRELSocial <- freqSocial/sum(freqSocial)
freqRELSocial

freqSitoweb<- table(dati$SITOWEB)
freqRELSitoweb <- freqSitoweb/sum(freqSitoweb)
freqRELSitoweb

freqVirtualTour<- table(dati$`VIRTUAL TOUR`)
freqRELVirtualTour <- freqVirtualTour/sum(freqVirtualTour)
freqRELVirtualTour

#INVESTIMENTS 
freqInvestimenti <- table(dati$INVEST_SERVIZONLINE)
freqRELInvestimenti <- freqInvestimenti/sum(freqInvestimenti)
freqRELInvestimenti
pie(freqRELInvestimenti, main ="Invest in")

