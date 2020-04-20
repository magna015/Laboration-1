# Användbara paket
{library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(RColorBrewer)
library(survey)}

# Sätter arbetsmapp för laborationen
setwd("D:/Skola/VT/Surveymetodik/Laborationer")

###############

# Läser in data från angivna filen och gör ett OSU
# för att använda i laborationen
agpop <- read.table("agpop.dat", header = TRUE, sep = ",")
agpop <- dplyr::filter(agpop, ACRES92 > 0 & ACRES87 > 0)

OSUindex <- sample(size = 300, 1:nrow(agpop), replace = FALSE)
agOSUdata <- agpop[OSUindex,]

# fpc står för "finite population correction"
# srs står för "simple random sampling" (OSU)
fpc.srs <- rep(nrow(agpop), 300)

# Skapar surveyobjekt
agOSU <- svydesign(ids = ~1, data = agOSUdata, fpc = fpc.srs)

# Summerar informationen av surveyobjektet
summary(agOSU)

# FRÅGA 1A)
# Funktionen ger information om objektets populationsstorlek: 3042
# Den ger information om hur många och vilka variabler som objektet
# innehar.
# Den ger även information om dess sannolikheter.

# Skattar populationsmedelvärdet och populationstotalen
svymean(~ACRES92, design = agOSU)
svytotal(~ACRES92, design = agOSU)


# FRÅGA 1B)
# Populationsmedelvärdet är 330454, med ett standardfel av 23408
# Populationstotalen är 1005240074, med ett standardfel av 71206481

# Skapar variabel för vår skattning
medel <- svymean(~ACRES92, design = agOSU)
confint(medel)

mean(agpop$ACRES92)

# FRÅGA 1C)
# Med 95% sannolikhet så ligger medelvärdet mellan 284575
# och 376332. Detta intervall täcker det sanna värdet i
# populationen som urvalet dragits från.

###############

str(agOSU)

# Skapar en ny variabel som anger icke-stora gårdar
agOSU$variables$nonLargeFarms <- agOSU$variables$FARMS92 - agOSU$variables$LARGEF92
svymean(~nonLargeFarms, design = agOSU)

# Skattar medelvärde och skapar tillhörande konfidensintervall
medel_nonLargeFarms <- svymean(~nonLargeFarms, design = agOSU)
confint(medel_nonLargeFarms)
medel_nonLargeFarms

# FRÅGA 2A)
# Med 95% sannolikhet så ligger medelvärdet mellan 565 och 665.
# Detta intervall täcker det sanna medelvärdet, 615.

# Skattar medelvärdet för variabeln ACRES92 i respektive region
svyby(~ACRES92, by = ~REGION, design = agOSU, FUN = svymean)

# FRÅGA 2B)
# Det är stor skillnad mellan regionerna. Den största regionen är W.

# Alternativt sätt att skatta medelvärdet för variablen ACRES92
# i respektive region
agOSUne <- subset(agOSU, REGION == "NE")
svymean(~ACRES92, design = agOSUne)

# FRÅGA 2C)
# Skillnaden i detta fallet är att vi enbart tog en specifik region, NE,
# när vi använde subset. Medan vi tog alla regioner när vi använde oss utav svyby.
# Resultatet för den specifika regionen blir densamma (om vi ignorerar avrundning)
# vilket vi kan förvänta oss, då medelvärdet och standardfelet ska vara detsammma


varians <- var(agpop$ACRES92)
medel_varde <- mean(agpop$ACRES92)

# FOR-LOOP
urval <- 400
antal_urval <- 10000
and_korr <- rep(nrow(agpop), urval)

spara <- matrix(data = NA, nrow = antal_urval, ncol = 2)

for (nr in 1:antal_urval){
  taget_urval <- sample(x = 1:nrow(agpop),
                        size = urval,
                        replace = FALSE)
  surveyObjekt <- svydesign(ids = ~1,
                            data = agpop[taget_urval, c("ACRES92", "ACRES87")],
                            fpc = and_korr)
  medel <-svymean(~ACRES92, surveyObjekt)
  KI <- confint(medel, level = 0.9)
  
  intervallSant <- findInterval(x = medel_varde,
                                vec = KI) == 1
  spara[nr,] <- c(medel[1], intervallSant)
}


# Räknar antalet 1:or
antal <- 0
for (i in 1:antal_urval){
  if (spara[i,2] == 1)
    antal <- antal+1
}

tackning <- antal / antal_urval
tackning

spara[,1]

# FRÅGA 3A)
# Den förväntade täckningsgraden förväntas ligga runt 90%, då detta är
# vad vi angett som konfidensgrad.

# Täckningsgraden för intervallskattningen är 0.8912, eller 89.12%, vilket
# är vad vi förväntar oss att den ska vara. Min tolkning av detta är att
# modellen är ganska bra.


# FRÅGA 3B)
# Skapar histogram av intervallskattningen.
spara <- as.data.frame(spara)

plot1 <- ggplot(data = spara) +
  aes(spara[,1]) +
  geom_histogram(fill = "orange",
                 color = "black",
                 bins = 30) +
  geom_vline(aes(xintercept = medel_varde, col = "true mean"), size = 1.2) +
  theme_bw() +
  theme(text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  labs(y = "count",
       x = "estimated mean",
       color = "")
plot1

# Diagrammet visar på en hyfsat fin bell curve, med centrum ganska nära
# det sanna medelvärdet och ungefär lika fördelat på båda sidorna.


# FRÅGA 3C)
# Förväntar mig ett värde som ligger nära det sanna värdet.

# Beräknar det genomsnittliga värdet och jämför
mean(spara[,1])
medel_varde

# Det skattade medelvärdet ligger ca 32 från varandra. Detta innebär att
# skattningarna från simuleringen väl reflekterar det sanna medelvärdet.


# FRÅGA 3D)
# Beräknar det teoretiska medelfelet
SE <- sqrt((1 - (urval/antal_urval)) * (varians/urval))
SE

# FRÅGA 3E)
# Beräknar standardavvikelsen för de 10000 medelvärdesskattningarna
S <- sd(spara[,1])
S

# Standardavvikelsen visar den genomsnittliga avvikelsen från medelvärdet
# Medelfelet visar hur precis vår skattning är. Det är standardavvikelsen
# för våra estimatorer från alla urval.

# Ju större vårt urval, desto mindre förväntar vi oss att vårt medelfel
# blir, medan vi inte förväntar oss att den genomsnittsliga avvikelsen
# (standardavvikelsen) ska ändras nämnvärt.