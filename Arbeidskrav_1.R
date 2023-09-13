#Innlevering arbeidskrav SOK-2013

suppressPackageStartupMessages(library(tidyverse))



# Henter datasettet
datasett_1<-"https://raw.githubusercontent.com/uit-sok-2013-h23/Data_Arbeidskrav_1/main/datasett1.csv"
data_1<-read.csv(datasett_1)

#1.Kalkuler «openness» til alle land i datasettet. Rapporter åpenheten til Norge, og forklar hva tallet betyr. 
# Har Norge sett en endring i trade openness i perioden datasettet omhandler? (2018 – 2021)

# Beregner trade openness for alle land. Jeg tar vekk land med manglende verdier
data_to<-data_1 %>% 
  drop_na() %>% 
  mutate(trade_openness=(EXP+IMP)/gdp)

# Lager eget datasett for Norge:

to_norge<-data_to %>% 
  filter(LOCATION=="NOR")


# Sjekk at trade openness betyr utenrikshandelens andel av GDP.  Vi ser at trade openness for Norge
# økte fra 2018 til 2020 for deretter å avta i 2021 og 2022. 2022 var det laveste nivået i måleperioden.

# 2.Lag et scatter-plot for trade openness til alle land i 2020. 
# Legg til en kvadratisk regresjonslinje hvor «openness» er på y-aksen og BNP per capita er på x-aksen.
# Forklar kort hva grafen viser

# Plott for trade openness for alle land i 2020
# lager en ny variabel: gdp_per_cap

to_2020<-data_to %>%
  mutate(gdp_per_cap=gdp/pop) %>% 
  filter(TIME==2020)


# Lager enkelt plott:
ggplot(to_2020, aes(x=trade_openness, y= gdp_per_cap)) +
  geom_point() +
  labs(title="Forholdet mellom Trade openness og BNP per capita", 
       x= "Trade Openness", y= "BNP per innbygger")

# Plot med kvadratisk regresjonslinje:
ggplot(to_2020, aes(x=trade_openness, y= gdp_per_cap)) +
    geom_point() +
    labs(title="Forholdet mellom Trade openness og BNP per capita \nKvadratisk regresjon", 
         x= "Trade Openness",
       y= "BNP per innbygger") +
   # geom_text(aes(label = LOCATION))+
    geom_smooth(method = lm,formula = y ~ x + I(x^2),se=FALSE)

# 3. Gjenta deloppgave 2, men nå med logaritmen av BNP per capita på x-aksen i stedet. 
# Hva er forskjellen? Er det noen spesielt god grunn til å velge den ene over den andre i dette tilfellet?

# Plot med log(gdp_per_cap):
ggplot(to_2020, aes(x=trade_openness, y= log(gdp_per_cap))) +
  geom_point() +
  labs(title="Forholdet mellom Trade openness og BNP per capita \nmed log(BNP per capita)", 
       x= "Trade Openness",
       y= "log(BNP per innbygger)") +
  # geom_text(aes(label = LOCATION))+
  geom_smooth(method = lm,formula = y ~ x + I(x^2),se=FALSE)


# Oppgave 2:
# Henter datasettet: 

url_1 <- "https://raw.githubusercontent.com/uit-sok-2013-h23/Data_Arbeidskrav_1/main/datasett2.csv"
data_2 <- read.csv(url_1)


# rydder vekk alle rader med NA
data_2_ryddet<-data_2 %>% 
  drop_na()

#1:Bruk datasettet til å konstruere en graf med en kvadratisk regresjonslinje, hvor handelsoverlapp-verdiene er 
# på y-aksen og Similarity Indeks-verdiene er på x-aksen. Forklar kort hva grafen betyr.

# Plot med kvadratisk regresjonslinje:
ggplot(data_2_ryddet, aes(x=simil_index, y= overlap)) +
  geom_point() +
  labs(title="Forholdet mellom simil index og overlap", 
       x= "Simil index",
       y= "Overlap") +
  # geom_text(aes(label = LOCATION))+
  geom_smooth(method = lm,formula = y ~ x + I(x^2),se=FALSE)

#2:Legg til tekst som markerer Sverige, Norge, Danmark, og Finland sin plassering på grafen.
# Tolk hva Norges plassering på grafen betyr sammenlignet med de resterende landene.

# Henter relevante land:
countries <- c("NOR", "DNK", "FIN", "SWE")
countryfilter <- data_2_ryddet[data_2_ryddet$pcode %in% countries,]

ggplot(data_2_ryddet, aes(x=simil_index, y= overlap)) +
  geom_point() +
  geom_text(data = countryfilter, aes(label = pcode), size = 2, hjust = 0, vjust = 1)+
  labs(title="Forholdet mellom simil index og overlap", 
       x= "Simil index",
       y= "Overlap") +
  # geom_text(aes(label = LOCATION))+
  geom_smooth(method = lm,formula = y ~ x + I(x^2),se=FALSE)
