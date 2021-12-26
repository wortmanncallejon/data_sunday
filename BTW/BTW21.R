library(ggrepel)
source("C:/Users/felix/Documents/GitHub/data_sunday/functions.R", encoding = "UTF-8")

btw21 <- get_btw21()

# Scatterplot for Erst- vs. Zweitstimme ----

p <- ggplot(btw21, aes(Zweit, Erst, color = Partei, label = Name)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#005EA4", "#CC0066", "#FFC000", "#0E8C1D", "#C00000", "black")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.55), breaks = c(0,0.1,0.2,0.3,0.4,0.5), "Erststimmenanteil") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.55), breaks = c(0,0.1,0.2,0.3,0.4,0.5), "Zweitstimmenanteil") +
  geom_point(aes(alpha = abs(Log))) +
  geom_label_repel(force = 10, family = "Avenir Next Condensed") +
  theme_bw() +
  theme(text = element_text(family = "Avenir Next Condensed", size = 14),
        legend.position = "none")

p

# Export plot as JSON/HTML/PNG
# plotly::plotly_json(plotly::ggplotly(p))
# 
# htmlwidgets::saveWidget(plotly::ggplotly(p), "Export/index.html")
# 
# ggsave("Export/scatter.png", device = "png", width = 18, height = 16, dpi = 600, plot = p)

rm(btw21, p)

## Explainer for the Performance Measure ----

# Using Gregor Gysi

x <- runif(10000, 0, 1)
y <- log(x/(1-x))

dat <- tibble(x,y)

E <- 0.35440575
Z <- 0.15958443
d <- log(E/(1-E)) - log(Z/(1-Z))

ggplot(dat, aes(x,y)) +
  geom_line() +
  scale_x_continuous("Stimmenanteil", labels = scales::percent_format(accuracy = 1)) +
  ylab("log(p/(1-p))") +
  ylim(c(-5,5))  +
  theme_bw() +
  theme(text = element_text(family = "Avenir Next Condensed")) +
  annotate("rect", xmin = Z, xmax = E, ymin = log(Z/(1-Z)), ymax = log(E/(1-E)), alpha = 0.2) +
  annotate("segment", x = E, xend = E, y = log(Z/(1-Z)), yend = log(E/(1-E)), color = "red", size = 1) +
  annotate("text", x = E, y = log(Z/(1-Z)) + (d/2), label = str_glue("d = {as.character(round(d, 4))}"), hjust = -0.2, family = "Avenir Next Condensed") +
  geom_point(x = E, y = log(E/(1-E)), color = "#CC0066") +
  annotate("text",x = E, y = log(E/(1-E)), label = "Erststimmenanteil\nGregor Gysi", vjust = -0.4, family = "Avenir Next Condensed") +
  geom_point(x = Z, y = log(Z/(1-Z)), color = "#CC0066") +
  annotate("text", x = Z - 0.05, y = log(Z/(1-Z)), label = "Zweitstimmenanteil\nGregor Gysi", vjust = 0, family = "Avenir Next Condensed")

# Export as PNG
# ggsave("Export/Gysi.png", device = "png", width = 16, height = 9, dpi = 600)

rm(x,y,d,E,Z,dat)

# PLot Example Measuremant Curves

data <- get_Explainer_data()
e <- get_btw21() %>% filter(Elected == 1)

ggplot(data, aes(x, Log, color = type)) +
  geom_vline(xintercept = summary(e$Zweit)[2] - 1.5 * IQR(e$Zweit), linetype = "dotdash") +
  geom_vline(xintercept = summary(e$Zweit)[5] + 1.5 * IQR(e$Zweit), linetype = "dotdash") +
  geom_line() +
  scale_color_discrete("Verhältnis von Zweit-\nzu Erststimmenanteil") +
  scale_x_continuous("Zweitstimmenanteil", labels = scales::percent_format(accuracy = 1)) +
  ylab("log(Erst/(1-Erst)) - log(Zweit/(1-Zweit)) bei Erst = x * Zwei") +
  ylim(c(-1,5))  +
  theme_bw() +
  theme(text = element_text(family = "Avenir Next Condensed"))

# Export as PNG
# ggsave("Export/Log.png", device = "png", width = 16, height = 9, dpi = 600)

rm(data)

# Plot Distribution

ggplot(e, aes(Zweit)) + 
  geom_boxplot() +
  #geom_histogram(bins = 50) +
  #geom_density() +
  scale_x_continuous("Zweitstimmenanteil", labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  ylim(c(-1,1)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(family = "Avenir Next Condensed"))

# Export as PNG
# ggsave("Export/Box.png", device = "png", width = 16, height = 9, dpi = 600)