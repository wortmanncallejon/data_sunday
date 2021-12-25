library(tidyverse)
library(sf)
library(glue)
library(scales)
library(htmltools)
library(leaflet)




union <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           (Gruppenname == "CDU" | Gruppenname == "CSU")) %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(UNION = Prozent/100) %>% 
  select(ID, UNION)

spd <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname == "SPD") %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(SPD = Prozent/100) %>% 
  select(ID, SPD)

gruene <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname == "GRÜNE") %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(GRÜNE = Prozent/100) %>% 
  select(ID, GRÜNE)

fdp <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname == "FDP") %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(FDP = Prozent/100) %>% 
  select(ID, FDP)

afd <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname == "AfD") %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(AfD = Prozent/100) %>% 
  select(ID, AfD)

linke <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname == "DIE LINKE") %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Prozent") %>% 
  mutate(`DIE LINKE` = Prozent/100) %>% 
  select(ID, `DIE LINKE`)

g <- read_csv("data-lmBZp.csv") %>% 
  mutate(Name = str_glue("{Nachname}, {Vornamen}")) %>% 
  select(Gebietsnummer, Gruppenname, Name) %>% 
  rename(ID = Gebietsnummer,
         Partei = Gruppenname) %>% 
  mutate(Partei = factor(case_when(Partei == "CDU" ~ "UNION",
                                   Partei == "CSU" ~ "UNION",
                                   TRUE ~ Partei)))

e <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
           Stimme == 1 &
           Gruppenname %in% c("CDU", "CSU", "SPD", "GRÜNE", "FDP", "AfD", "DIE LINKE")) %>%
  mutate(ID = as.numeric(Gebietsnummer)) %>% 
  select("ID", "Gebietsname", "Gruppenname", "Prozent", "UegGebietsnummer") %>% 
  mutate(OstWest = factor(case_when(ID <= 11 ~ "West",
                                    ID >= 12 & ID <= 17 ~ "Ost",
                                    ID >= 18 & ID <= 55 ~ "West",
                                    ID >= 56 & ID <= 74 ~ "Ost",
                                    ID >= 87 & ID <= 148 ~ "West",
                                    ID >= 149 & ID <= 166 ~ "Ost",
                                    ID >= 167 & ID <= 188 ~ "West",
                                    ID >= 189 & ID <= 196 ~ "Ost",
                                    ID >= 197 & ID <= 299 ~ "West",
                                    ID %in% c(76,83,84,85,86) ~ "Ost",
                                    (ID >= 77 & ID <= 82) | ID == 75  ~ "West",
                                    TRUE ~ NA_character_)),
         Gewinner = Prozent/100,
         Partei = case_when(Gruppenname == "CDU" ~ "UNION",
                            Gruppenname == "CSU" ~ "UNION",
                            TRUE ~ Gruppenname))  %>% 
  select(ID, Partei, Gewinner, OstWest) %>% 
  inner_join(g, by = c("ID", "Partei"))

Länder <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16")) %>% 
  select(Gebietsnummer, Gebietsname) %>% 
  mutate(Land_ID = as.numeric(Gebietsnummer),
         Land = Gebietsname) %>% 
  group_by(Land) %>% 
  summarize(Land_ID = mean(Land_ID))

WK <- read_csv2("btw2021_kerg2.csv", skip = 9) %>% 
  filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99")) %>% 
  select(Gebietsnummer, Gebietsname, UegGebietsnummer) %>% 
  mutate(ID = as.numeric(Gebietsnummer),
         WK_Name = Gebietsname,
         Land_ID = as.numeric(UegGebietsnummer)) %>% 
  group_by(WK_Name) %>% 
  summarize(ID = mean(ID),
            Land_ID = mean(Land_ID)) %>% 
  inner_join(Länder, by = "Land_ID")

btw21 <- union %>%
  inner_join(WK, by = "ID") %>%
  inner_join(spd, by = "ID") %>%
  inner_join(gruene, by = "ID") %>% 
  inner_join(afd, by = "ID") %>%
  inner_join(fdp, by = "ID") %>%
  inner_join(linke, by = "ID") %>% 
  inner_join(e, by = "ID") %>% 
  mutate(Zweiter = case_when(Partei == "GRÜNE" ~ pmax(UNION, SPD, FDP, AfD, `DIE LINKE`, na.rm = T),
                             Partei == "UNION" ~ pmax(GRÜNE, SPD, FDP, AfD, `DIE LINKE`, na.rm = T),
                             Partei == "SPD" ~ pmax(UNION, GRÜNE, FDP, AfD, `DIE LINKE`, na.rm = T),
                             Partei == "FDP" ~ pmax(UNION, SPD, GRÜNE, AfD, `DIE LINKE`, na.rm = T),
                             Partei == "AfD" ~ pmax(UNION, SPD, FDP, GRÜNE, `DIE LINKE`, na.rm = T),
                             Partei == "DIE LINKE" ~ pmax(UNION, SPD, FDP, AfD, GRÜNE, na.rm = T),
                             TRUE ~ NA_real_)) %>% 
  mutate(Diff = (Gewinner - Zweiter),
         Alpha = (Gewinner - Zweiter)*10,
         Anteil = Gewinner) %>% 
  select(ID, WK_Name, Land_ID, Land, OstWest, Name, Partei, Anteil, Diff, Alpha)


rm(WK, afd, union, spd, e, fdp, g, gruene, Länder, linke)




