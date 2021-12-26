library(tidyverse)
library(readxl)

## Absolute ----
e <- read_xlsx("agh/Data/DL_BE_AGHBVV2021.xlsx", sheet =3) %>% 
  select("Bezirksnummer", "Bezirksname", "Abgeordneten-\r\nhauswahlkreis",
         "Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD") %>%
  mutate(N = as.numeric(`Abgeordneten-\r\nhauswahlkreis`)) %>% 
  mutate(ID = str_glue("{Bezirksnummer}{`Abgeordneten-\r\nhauswahlkreis`} - {Bezirksname} {N}")) %>% 
  group_by(ID) %>% 
  summarize_at(c("Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD"), sum) %>% 
  ungroup() %>% 
  pivot_longer(-ID) %>% 
  rename(Partei = name,
         Erst = value)

results <- read_xlsx("agh/Data/DL_BE_AGHBVV2021.xlsx", sheet = 4) %>% 
  select("Bezirksnummer", "Bezirksname", "Abgeordneten-\r\nhauswahlkreis",
         "Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD") %>%
  mutate(N = as.numeric(`Abgeordneten-\r\nhauswahlkreis`)) %>% 
  mutate(ID = str_glue("{Bezirksnummer}{`Abgeordneten-\r\nhauswahlkreis`} - {Bezirksname} {N}")) %>% 
  group_by(ID) %>% 
  summarize_at(c("Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD"), sum) %>% 
  ungroup() %>% 
  pivot_longer(-ID) %>% 
  rename(Partei = name,
         Zweit = value) %>% 
  inner_join(e, by = c("ID", "Partei")) %>% 
  inner_join(read_xlsx("agh/Data/Gewählte_Berlin.xlsx"), by = c("ID", "Partei")) %>% 
  mutate(P = Erst/Zweit,
         D = Erst-Zweit) %>% 
  mutate(Name = fct_reorder(Name, P))


ggplot(results, aes(Name, P, fill = Partei)) +
  scale_fill_manual(values = c("#005EA4", "black", "#CC0066", "#0E8C1D", "#C00000")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0.5, 1.5), oob=scales::rescale_none, "Verhältnis von Erst- zu Zweitstimmen") +
  geom_col() +
  geom_text(aes(label = Name), y = 0.5, color = "white", angle = 90, hjust = 0, size = 3) +
  xlab(NULL) +
  geom_hline(yintercept = 1) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

# Export as PNG
# ggsave("percent.png", device = "png", width = 16, height = 9, dpi = 600)

## Relative ----
e <- read_xlsx("agh/Data/DL_BE_AGHBVV2021.xlsx", sheet =3) %>% 
  select("Bezirksnummer", "Bezirksname", "Abgeordneten-\r\nhauswahlkreis",
         "Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD") %>%
  mutate(N = as.numeric(`Abgeordneten-\r\nhauswahlkreis`)) %>% 
  mutate(ID = str_glue("{Bezirksnummer}{`Abgeordneten-\r\nhauswahlkreis`} - {Bezirksname} {N}")) %>% 
  group_by(ID) %>% 
  summarize_at(c("Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD"), sum) %>%
  ungroup() %>%
  mutate(SPD = SPD / `Gültige Stimmen`,
         CDU = CDU / `Gültige Stimmen`,
         GRÜNE = GRÜNE / `Gültige Stimmen`,
         `DIE LINKE` = `DIE LINKE` / `Gültige Stimmen`,
         AfD = AfD / `Gültige Stimmen`) %>% 
  pivot_longer(-ID) %>% 
  rename(Partei = name,
         Erst = value)


results <- read_xlsx("agh/Data/DL_BE_AGHBVV2021.xlsx", sheet = 4) %>% 
  select("Bezirksnummer", "Bezirksname", "Abgeordneten-\r\nhauswahlkreis",
         "Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD") %>%
  mutate(N = as.numeric(`Abgeordneten-\r\nhauswahlkreis`)) %>% 
  mutate(ID = str_glue("{Bezirksnummer}{`Abgeordneten-\r\nhauswahlkreis`} - {Bezirksname} {N}")) %>% 
  group_by(ID) %>% 
  summarize_at(c("Gültige Stimmen", "SPD", "CDU", "GRÜNE", "DIE LINKE", "AfD"), sum) %>% 
  ungroup() %>%
  mutate(SPD = SPD / `Gültige Stimmen`,
         CDU = CDU / `Gültige Stimmen`,
         GRÜNE = GRÜNE / `Gültige Stimmen`,
         `DIE LINKE` = `DIE LINKE` / `Gültige Stimmen`,
         AfD = AfD / `Gültige Stimmen`) %>% 
  pivot_longer(-ID) %>%
  rename(Partei = name,
         Zweit = value) %>% 
  inner_join(e, by = c("ID", "Partei")) %>% 
  inner_join(read_xlsx("agh/Data/Gewählte_Berlin.xlsx"), by = c("ID", "Partei")) %>% 
  mutate(D = Erst-Zweit,
         P = Erst/Zweit,
         Mq = log(Erst/(1-Erst)) - log(Zweit/(1-Zweit))) %>% 
  mutate(Name = fct_reorder(Name, D)) %>% 
  mutate(label_pos = case_when(Mq > 0 ~ as.character(Name),
                               TRUE ~ NA_character_),
         label_neg = case_when(Mq < 0 ~ as.character(Name),
                               TRUE ~ NA_character_))


ggplot(results, aes(Name, D, fill = Partei)) +
  scale_fill_manual(values = c("#005EA4", "black", "#CC0066", "#0E8C1D", "#C00000")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits=c(-0.05, 0.15),
                     #limits = c(-0.25, 0.75),
                     oob=scales::rescale_none,
                     "Differenz aus Zweit- und Erststimmen") +
  geom_col() +
  geom_text(aes(label = label_pos), color = "black", angle = 90, hjust = -0.1, size = 3) +
  geom_text(aes(label = label_neg), color = "black", angle = 90, hjust = 1.1, size = 3) +
  xlab(NULL) +
  #geom_hline(yintercept = 1) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

# Export as PNG
# ggsave("log.png", device = "png", width = 16, height = 9, dpi = 600)
