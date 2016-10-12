library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

Sys.setlocale('LC_ALL','C')  
## engagments database made public by CIDE
enf <- read_tsv("data/enfrentamientos/BD_A-E.txt")
colnames(enf)[1] <- "ID" #correct column titles
colnames(enf)[5] <- "ANO"
enf[enf == 9999] <- 0 # code NA with 0


##table with type of confiscated objects, CIDE

asegur <- read_csv("data/enfrentamientos/catalogo/tabla1-A-E.csv", skip = 1, col_names = c("ID", "asegur"))
##problems with gibberish from original file
## table coding corporations involved, CIDE

corpo <- read_csv("data/enfrentamientos/catalogo/tabla9-A-E.csv", skip = 1, col_names = c("ID", "corpo"))
##problems with gibberish from original file

## join database with coded tables

enf <- left_join(enf, asegur, by = "ID")
enf <- left_join(enf, corpo, by = "ID")

## database with attacks against armed forces, made public by CIDE
agr <- read_tsv("data/agresiones/BD_A-A.txt")
colnames(agr)[1] <- "ID"
agr$ID <- agr$ID+4000 # add a number for IDs that is larger from the enf database
colnames(agr)[5] <- "ANO"
agr[agr == 9999] <- 0
agr <- agr %>% 
  select(-IO,-AG,-RES,-GRA,-ARF) %>% 
  rename(VEH = VEHICULO)

##table with type of confiscated objects, CIDE
asegur1 <- read_csv("data/agresiones/catalogo/tabla1-A-A.csv", skip = 1, col_names = c("ID", "asegur"))
asegur1$ID <- asegur1$ID+4000 

## table coding the corporations involved,  CIDE

corpo1 <- read_csv("data/agresiones/catalogo/tabla9-A-A.csv", skip = 1, col_names = c("ID", "corpo"))
corpo1$ID <- corpo1$ID+4000 
agr <- left_join(agr, asegur1, by = "ID")
agr <- left_join(agr, corpo1, by = "ID")


## bind the attacks and the engagemnt database to get dataframe for all armed engagements
enf <- rbind(enf, agr)

## code confiscated objects and corporations from PDF codebook 
##comunicación (coms), armamento (weaponry), uniformes (uniforms), 
##dinero (money), vehículos (vehicles), mariguana (marihuana), cocaina (cocaine), 
## heroina (heorin), drogas (drugs), otras (other), 
## armas largas (assult weapons) y armas cortas (handguns) (1 or 0), 
## sedena, semar, base de operaciones mixtas, PF, policía estatal, municipal,
## ministerial, AFI
## create columns following codebook
enf <- enf %>%
  mutate(team = ifelse(asegur == 1 | asegur == 3, 1, 0), 
         com = ifelse(asegur == 1, 1, 0), 
         arm = ifelse(asegur == 2, 1, 0),
         uni = ifelse(asegur == 3, 1, 0), 
         din = ifelse(asegur == 4, 1, 0), 
         veh = ifelse(asegur == 5, 1, 0), 
         mar = ifelse(asegur == 6, 1, 0), 
         coc = ifelse(asegur == 7, 1, 0), 
         her = ifelse(asegur == 8, 1, 0), 
         dr = ifelse(asegur == 9, 1, 0), 
         otr = ifelse(asegur == 10, 1, 0), 
         arl_d = ifelse(ARL > 0, 1, 0), 
         arc_d = ifelse(ARC > 0, 1, 0), 
         sedena = ifelse(corpo == 1, 1, 0), 
         semar = ifelse(corpo == 2, 1, 0), 
         bom = ifelse(corpo == 3, 1, 0),
         bomu = ifelse(corpo == 4, 1, 0), 
         pfp = ifelse(corpo == 5, 1, 0), 
         pes = ifelse(corpo == 6, 1, 0), 
         pmu = ifelse(corpo == 8, 1, 0), 
         min = ifelse(corpo == 13, 1, 0), 
         afi = ifelse(corpo == 28, 1, 0))

## remove duplicate observations once columns were created
enf1 <- enf %>%
  group_by(ID) %>%
  summarise_each(funs(max)) %>%
  mutate(date = dmy(paste(DIA, MES, ANO, sep = "-")), muncode = ESTADO*1000 + Municipio)


## distance from state capital
capitales <- read_csv("data/capitales_distances.csv")

capitales <- capitales[4:6]

enf1 <- left_join(enf1, capitales, by = "muncode")

enf1 <- rename(enf1, year = ANO)

## census data 2010, INEGI
census <- read_delim("/Users/andres/Google Drive/empirical/data/ITER_NALTXT10.TXT", delim = "\t")


total_pop <- census %>%
  select(state = ENTIDAD, mun = MUN, NOM_MUN, twn = LOC, POBTOT, ALTITUD) %>%
  mutate(muncode = as.numeric(state)*1000 + as.numeric(mun),
         elev = as.numeric(ALTITUD), 
         pop = as.numeric(POBTOT)) %>%
  select(-state, -mun) %>%
  filter(twn != "0000", twn != "9999", twn != "9998") %>%
  group_by(muncode, NOM_MUN)  %>%
  summarise(pop = sum(pop))


enf1 <- left_join(enf1, total_pop, by = "muncode")

## party alignment (1, 0)
party <- read_csv("data/party_mixed.csv")
party <- select(party, 2,3,8,9)

## elevation data
zonal <- read_csv("data/zonal_stats.csv")
zonal <- select(zonal, 5, 12:14)

enf1 <- left_join(enf1, zonal)
enf1 <- left_join(enf1, party, by = c("muncode", "year"))

## road density at the municipal level
densidad <- read_csv("data/densidad_caminos_mpo.csv")
densidad <- select(densidad, 2, 7)
enf1 <- left_join(enf1, densidad)

## cartel presence, data from Coscia - Rios
cartel <- read_csv("data/CosciaRios2012_DataBase.csv") %>%
  mutate(cartel = ifelse(rowSums(.[4:13]) > 0, 1, 0)) %>%
  select(muncode = Code, year = Year, cartel)

cartel$year <- lead(cartel$year) # match with previous year

enf1 <- left_join(enf1, cartel, by = c("muncode", "year"))

## data for municipal poverty and inequality
coneval <- read_csv("~/Google Drive/empirical/data/3.3 Concentrado, indicadores de pobreza por municipio.csv", skip = 6, col_names = FALSE)
poor <- coneval %>%
  filter(!is.na(X3)) %>%
  select(muncode = X3, pct.poor = X6, 
         pct.extpoor = X10, gini = X45) 

enf1 <- left_join(enf1, poor)

enf1 <- mutate(enf1, gini = as.numeric(gini))

#write.csv(enf1, "enf1.csv")
#enf1 <- select(enf1, ID, muncode, uni, arl_d, dist_capital, STD, fed_party, densidad, gini, pct.poor)
#no_match <- enf1[which(!complete.cases(enf1)),] # check that cases are complete

## enf1 <- read_csv("enf1.csv") ## load data frame
## ols with log transformation
a <- lm(log(DOF+1) ~ uni + arl_d + log(dist_capital+1) + log(STD) + fed_party + densidad + gini + pct.poor + log(pop), data = enf1)
## poisson
b <- glm(DOF ~ uni + arl_d + log(dist_capital+1) + log(STD) + fed_party + densidad + gini + pct.poor, offset(log(pop)), family = "poisson", data = enf1)

## negative binomial
library(MASS)
c <- glm.nb(DOF ~ uni + arl_d + log(dist_capital+1) + log(STD) + fed_party + densidad + gini + pct.poor, offset(log(pop)), data = enf1)

library(stargazer)

##html regression tables
stargazer(a, b, c, header=FALSE, type = "html", title = "Regression results", dep.var.labels = c("Oponents Killed", "Oponents Killed", "Oponents Killed"),
          covariate.labels = c("Uniforms", "Assault Weapons", "Dist. to state capital", "Std. Dev. Altitude", "Aligned Party", "Road density", "Gini coef", "% Poor",
                              "Population"), flip = TRUE, star.char = c("", "", ""), notes.append = FALSE)
## text regression tables

stargazer(a, b, c, header=FALSE, type = "text", title = "Regression results", dep.var.labels = c("Oponents Killed", "Oponents Killed", "Oponents Killed"),
          covariate.labels = c("Uniforms", "Assault Weapons", "Dist. to state capital", "Std. Dev. Altitude", "Aligned Party", "Road density", "Gini coef", "% Poor",
                               "Population"), flip = TRUE, star.char = c("", "", ""), notes.append = FALSE)




# 

# código para agresiones previas a enfrentamientos ------------------------


# 
# ## crear fechas y códigos municipales
# agr1 <- agr %>%
#   mutate(date = dmy(paste(DIA, MES, ANO, sep = "-")), muncode = ESTADO*1000 + Municipio, tipo = rep(1, nrow(agr))) %>%
#   select(ID, TIMESTAMP, date, muncode, tipo)
# ## agregar tipo enfretamineto y o agreción  
# enf2 <- select(enf1, ID, TIMESTAMP, date, muncode) %>%
#   mutate(tipo = rep(0, nrow(enf1)))
# 
# ## junta agresiones y enfrentamientos
# tipo <- rbind(agr1, enf2)
# ## crear y arreglar variable sobre agresión previa
# tipo <- arrange(tipo, date) %>%
#   group_by(muncode) %>%
#   mutate(prev = lag(tipo)) %>%
#   ungroup() %>%
#   filter(tipo == 0)
# 
# tipo$prev[is.na(tipo$prev)] <- 0
# 
# tipo <- select(tipo, ID, prev, tipo)
# 
# enf1 <- left_join(enf1, tipo, by = "ID")
# 
# ## variable antes o después del 2009
# enf1 <- enf1 %>%
#   mutate(post2009 = ifelse(ANO < 2010, 0, 1))