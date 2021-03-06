# Anv�ndbara paket
{library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(RColorBrewer)
library(survey)}

# S�tter arbetsmapp f�r laborationen
setwd("D:/Skola/VT/Surveymetodik/Laborationer")

###############

# L�ser in data fr�n angivna filen och g�r ett OSU
# f�r att anv�nda i laborationen
agpop <- read.table("agpop.dat", header = TRUE, sep = ",")
agpop <- dplyr::filter(agpop, ACRES92 > 0 & ACRES87 > 0)

OSUindex <- sample(size = 300, 1:nrow(agpop), replace = FALSE)
agOSUdata <- agpop[OSUindex,]

# fpc st�r f�r "finite population correction"
# srs st�r f�r "simple random sampling" (OSU)
fpc.srs <- rep(nrow(agpop), 300)

# Skapar surveyobjekt
agOSU <- svydesign(ids = ~1, data = agOSUdata, fpc = fpc.srs)

# Summerar informationen av surveyobjektet
summary(agOSU)

# FR�GA 1A)
# Funktionen ger information om objektets populationsstorlek: 3042
# Den ger information om hur m�nga och vilka variabler som objektet
# innehar.
# Den ger �ven information om dess sannolikheter.

# Skattar populationsmedelv�rdet och populationstotalen
svymean(~ACRES92, design = agOSU)
svytotal(~ACRES92, design = agOSU)


# FR�GA 1B)
# Populationsmedelv�rdet �r 330454, med ett standardfel av 23408
# Populationstotalen �r 1005240074, med ett standardfel av 71206481

# Skapar variabel f�r v�r skattning
medel <- svymean(~ACRES92, design = agOSU)
confint(medel)

mean(agpop$ACRES92)

# FR�GA 1C)
# Med 95% sannolikhet s� ligger medelv�rdet mellan 284575
# och 376332. Detta intervall t�cker det sanna v�rdet i
# populationen som urvalet dragits fr�n.

###############

str(agOSU)

# Skapar en ny variabel som anger icke-stora g�rdar
agOSU$variables$nonLargeFarms <- agOSU$variables$FARMS92 - agOSU$variables$LARGEF92
svymean(~nonLargeFarms, design = agOSU)

# Skattar medelv�rde och skapar tillh�rande konfidensintervall
medel_nonLargeFarms <- svymean(~nonLargeFarms, design = agOSU)
confint(medel_nonLargeFarms)
medel_nonLargeFarms

# FR�GA 2A)
# Med 95% sannolikhet s� ligger medelv�rdet mellan 565 och 665.
# Detta intervall t�cker det sanna medelv�rdet, 615.

# Skattar medelv�rdet f�r variabeln ACRES92 i respektive region
svyby(~ACRES92, by = ~REGION, design = agOSU, FUN = svymean)

# FR�GA 2B)
# Det �r stor skillnad mellan regionerna. Den st�rsta regionen �r W.

# Alternativt s�tt att skatta medelv�rdet f�r variablen ACRES92
# i respektive region
agOSUne <- subset(agOSU, REGION == "NE")
svymean(~ACRES92, design = agOSUne)

# FR�GA 2C)
# Skillnaden i detta fallet �r att vi enbart tog en specifik region, NE,
# n�r vi anv�nde subset. Medan vi tog alla regioner n�r vi anv�nde oss utav svyby.
# Resultatet f�r den specifika regionen blir densamma (om vi ignorerar avrundning)
# vilket vi kan f�rv�nta oss, d� medelv�rdet och standardfelet ska vara detsammma


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


# R�knar antalet 1:or
antal <- 0
for (i in 1:antal_urval){
  if (spara[i,2] == 1)
    antal <- antal+1
}

tackning <- antal / antal_urval
tackning

spara[,1]

# FR�GA 3A)
# Den f�rv�ntade t�ckningsgraden f�rv�ntas ligga runt 90%, d� detta �r
# vad vi angett som konfidensgrad.

# T�ckningsgraden f�r intervallskattningen �r 0.8912, eller 89.12%, vilket
# �r vad vi f�rv�ntar oss att den ska vara. Min tolkning av detta �r att
# modellen �r ganska bra.


# FR�GA 3B)
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

# Diagrammet visar p� en hyfsat fin bell curve, med centrum ganska n�ra
# det sanna medelv�rdet och ungef�r lika f�rdelat p� b�da sidorna.


# FR�GA 3C)
# F�rv�ntar mig ett v�rde som ligger n�ra det sanna v�rdet.

# Ber�knar det genomsnittliga v�rdet och j�mf�r
mean(spara[,1])
medel_varde

# Det skattade medelv�rdet ligger ca 32 fr�n varandra. Detta inneb�r att
# skattningarna fr�n simuleringen v�l reflekterar det sanna medelv�rdet.


# FR�GA 3D)
# Ber�knar det teoretiska medelfelet
SE <- sqrt((1 - (urval/antal_urval)) * (varians/urval))
SE

# FR�GA 3E)
# Ber�knar standardavvikelsen f�r de 10000 medelv�rdesskattningarna
S <- sd(spara[,1])
S

# Standardavvikelsen visar den genomsnittliga avvikelsen fr�n medelv�rdet
# Medelfelet visar hur precis v�r skattning �r. Det �r standardavvikelsen
# f�r v�ra estimatorer fr�n alla urval.

# Ju st�rre v�rt urval, desto mindre f�rv�ntar vi oss att v�rt medelfel
# blir, medan vi inte f�rv�ntar oss att den genomsnittsliga avvikelsen
# (standardavvikelsen) ska �ndras n�mnv�rt.