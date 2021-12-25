get_btw21 <- function() {
  require(tidyverse)
  b <- read_csv2("BTW/Data/btw2021_kerg2.csv", skip = 9) %>% 
    filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99") &
             Stimme == 2 &
             Gruppenname %in% c("CDU", "CSU", "SPD", "GRÜNE", "FDP", "AfD", "DIE LINKE")) %>%
    mutate(ID = as.numeric(Gebietsnummer)) %>% 
    select("ID", "Gebietsname", "Gruppenname", "Prozent") %>% 
    mutate(Zweit = Prozent,
           Partei = case_when(Gruppenname == "CDU" ~ "UNION",
                              Gruppenname == "CSU" ~ "UNION",
                              TRUE ~ Gruppenname)) %>% 
    select(ID, Partei, Zweit)
  
  g <- read_csv("BTW/Data/data-lmBZp.csv") %>% 
    mutate(Name = str_glue("{Nachname}, {Vornamen}")) %>% 
    select(Gebietsnummer, Gruppenname, Name) %>% 
    rename(ID = Gebietsnummer,
           Partei = Gruppenname) %>% 
    mutate(Partei = factor(case_when(Partei == "CDU" ~ "UNION",
                                     Partei == "CSU" ~ "UNION",
                                     TRUE ~ Partei)))
  
  Länder <- read_csv2("BTW/Data/btw2021_kerg2.csv", skip = 9) %>% 
    filter(Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16")) %>% 
    select(Gebietsnummer, Gebietsname) %>% 
    mutate(Land_ID = as.numeric(Gebietsnummer),
           Land = Gebietsname) %>% 
    group_by(Land) %>% 
    summarize(Land_ID = mean(Land_ID))
  
  WK <- read_csv2("BTW/Data/btw2021_kerg2.csv", skip = 9) %>% 
    filter(!Gebietsnummer %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15", "16", "99")) %>% 
    select(Gebietsnummer, Gebietsname) %>% 
    mutate(ID = as.numeric(Gebietsnummer),
           WK_Name = Gebietsname) %>% 
    group_by(WK_Name) %>% 
    summarize(ID = mean(ID))
  
  btw21 <- read_csv2("BTW/Data/btw2021_kerg2.csv", skip = 9) %>% 
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
           Erst = Prozent,
           Partei = case_when(Gruppenname == "CDU" ~ "UNION",
                              Gruppenname == "CSU" ~ "UNION",
                              TRUE ~ Gruppenname),
           Land_ID = as.numeric(UegGebietsnummer))  %>% 
    select(ID, Partei, Erst, OstWest, Land_ID) %>% 
    inner_join(b, by = c("ID", "Partei")) %>% 
    full_join(g, by = c("ID", "Partei")) %>% 
    inner_join(Länder, by = "Land_ID") %>%
    inner_join(WK, by = "ID") %>% 
    select(ID, OstWest, Land, WK_Name, Name, Partei, Erst, Zweit) %>% 
    mutate(Erst = Erst / 100,
           Zweit = Zweit / 100,
           Elected = factor(case_when(is.na(Name) ~ 0,
                                      !is.na(Name) ~ 1)),
           Alpha = case_when(is.na(Name) ~ 0.5,
                             !is.na(Name) ~ 1),
           D = Erst - Zweit,
           Log = log(Erst/(1-Erst)) - log(Zweit/(1-Zweit)),
           Name = case_when(ID == 100 & Partei == "FDP" ~ "Lindner, Christian",
                            ID == 205 & Partei == "DIE LINKE" ~ "Trabert, Gerhard",
                            ID == 100 & Partei == "FDP" ~ "Graf Lambsdorff, Alexander",
                            ID == 3 & Partei == "FDP" ~ "Kubicki, Wolfgang",
                            ID == 159 & Partei == "DIE LINKE" ~ "Kipping, Katja",
                            ID == 280 & Partei == "SPD" ~ "Esken, Saskia",
                            TRUE ~ as.character(Name))) %>%
    mutate(Label = str_glue("{Name} (WK {ID})")) %>% 
    arrange(desc(Log)) %>%
    rowid_to_column("N") %>%
    mutate(Name = fct_reorder(Name, Log))
  
  rm(b, g, Länder, WK)
  return(btw21)
}

get_Explainer_data <- function() {
  require(tidyverse)
  x <- runif(10000, 0, 1)
  y <- log(x * 2/(1-x * 2)) - log(x/(1-x))
  a <- log(x * 1.5/(1-x * 1.5)) - log(x/(1-x))
  b <- log(x * 1.1/(1-x * 1.1)) - log(x/(1-x))
  c <- log(x * 1.25/(1-x * 1.25)) - log(x/(1-x))
  
  
  y <- tibble(x,y) %>% mutate(type = "+100%") %>% rename(Log = y)
  a <- tibble(x,a) %>% mutate(type = "+50%") %>% rename(Log = a)
  b <- tibble(x,b) %>% mutate(type = "+10%") %>% rename(Log = b)
  c <- tibble(x,c) %>% mutate(type = "+25%") %>% rename(Log = c)
  
  data <- rbind(y,a,b,c) %>%
    tibble() %>%
    mutate(type = factor(type, levels = c("+10%", "+25%", "+50%", "+100%"))) %>% 
    filter(!is.na(Log))
  
  rm(x,y,a,b,c)
  return(data)
}