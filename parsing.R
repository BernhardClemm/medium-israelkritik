# Author: Bernhard Clemm
# Date: 10 January 2021
# Purpose: Data for the blog post at https://bernhard-clemm.medium.com/israelkritik-in-den-deutschen-medien-64a450a89169

# Packages & Functions ####
# ------------------------

library(rvest)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(directlabels)
library(devtools)
devtools::source_url("https://github.com/BernhardClemm/factiva-parsing/blob/main/factiva-parser.R?raw=TRUE")

# Set locale ####
# --------------------------
Sys.setlocale("LC_TIME", "de_DE.UTF-8") 

# Import and parse data ####
# --------------------------

files <- list.files(path = "./data/Israel", 
                    pattern="*.html", full.names = TRUE, recursive = FALSE)
data_list <- lapply(files, factiva_parser) 
israel_data <- bind_rows(data_list, .id = "file")

chinakritik_year <- read.csv("./data/China/Factiva_summary.csv")

russlandkritik_year <- read.csv("./Israelkritik/data/Russland/Factiva_summary.csv")

# Clean data ####
# ---------------

# Clean list of sources
# length(table(data$source, exclude = NULL))

israel_data <- israel_data %>%
  mutate(source = case_when(
    source == "AACHE" ~ "Aachener Nachrichten",
    source == "AFPDE" ~ "Agence France Presse",
    source == "ARD Alpha Transkripte" ~ "ARD",
    source == "ARD Transkripte" ~ "ARD",
    source == "B.Z. am Sonntag" ~ "B.Z.",
    source == "Bayerisches Fernsehen Transkripte" ~ "BR",
    source == "Bayerischer Rundfunk Transkripte" ~ "BR",
    source == "Berliner Morgenpost Online" ~ "Berliner Morgenpost",
    source == "BERLRZ" ~ "Berliner Zeitung",
    source == "Berner Zeitung Online" ~ "Berliner Zeitung",
    source == "BILD am Sonntag" ~ "BILD",
    source == "BILD Berlin-Brandenburg" ~ "BILD",
    source == "BILD Düsseldorf" ~ "BILD",
    source == "BILD Hamburg" ~ "BILD",
    source == "BILD München" ~ "BILD",
    source == "BILD Plus" ~ "BILD",
    source == "bild.de" ~ "BILD",
    source == "Blick am Abend" ~ "Blick",
    source == "Blick Online" ~ "Blick",
    source == "BREMNA" ~ "Bremer Nachrichten",
    source == "dapd Basisdienst" ~ "dapd",
    source == "dapd Themendienste" ~ "dapd",
    source == "dapd Landesdienste" ~ "dapd",
    source == "ddp Basisdienst" ~ "ddp",
    source == "ddp Landesdienste" ~ "ddp",
    source == "ddp-Wirtschaftsdienst" ~ "ddp",
    source == "Der Tagesspiegel Online" ~ "Der Tagesspiegel",
    source == "DEUDE" ~ "Deutsche Welle",
    source == "Deutsche Welle Radio Transkripte" ~ "Deutsche Welle",
    source == "DIEZEI" ~ "DIE ZEIT",
    source == "ZEIT online" ~ "DIE ZEIT",
    source == "ZEIT Magazin" ~ "DIE ZEIT",
    source == "ZEITON" ~ "DIE ZEIT",
    source == "ZEIT im Osten" ~ "DIE ZEIT",
    source == "dpa-AFX ProFeed" ~ "dpa",
    source == "dpa-InfoLine" ~ "dpa",
    source == "dpa-StarLine" ~ "dpa",
    source == "DWELT" ~ "Die Welt",
    source == "Welt am Sonntag" ~ "Die Welt",
    source == "Welt Aktuell" ~ "Die Welt",
    source == "Welt kompakt" ~ "Die Welt",
    source == "WELT online" ~ "Die Welt",
    source == "WELTON" ~ "Die Welt",
    source == "WSONNT" ~ "Die Welt",
    source == "epd Basisdienst" ~ "epd",
    source == "epd medien" ~ "epd",
    source == "epd medien Newsletter" ~ "epd",
    source == "EPDBS" ~ "epd",
    source == "FOCUS" ~ "Focus",
    source == "Focus Online" ~ "Focus",
    source == "Frankfurter Rundschau Online" ~ "Frankfurter Rundschau",
    source == "FRARUN" ~ "Frankfurter Rundschau",
    source == "FTD" ~ "Financial Times Deutschland",
    source == "GNLZGR" ~ "General Anzeiger",
    source == "Göttinger Tageblatt / Eichsfelder Tageblatt Online" ~ "Göttinger Tageblatt / Eichsfelder Tageblatt",
    source == "HABEND" ~ "Hamburger Abendblatt",
    source == "Hamburger Abendblatt Online" ~ "Hamburger Abendblatt",
    source == "Hannoversche Allgemeine Zeitung Online" ~ "Hannoversche Allgemeine Zeitung",
    source == "Kölner Stadtanzeiger" ~ "Kölner Stadt-Anzeiger",
    source == "Kurier am Sonntag" ~ "Kurier",
    source == "Leipziger Volkszeitung Online" ~ "Leipziger Volkszeitung",
    source == "MARKAL" ~ "Märkische Allgemeine",
    source == "Märkische Allgemeine Zeitung Online" ~ "Märkische Allgemeine",
    source == "Neue Presse Online" ~ "Neue Presse",
    source == "NEUZZ" ~ "Neue Zürcher Zeitung",
    source == "NZZ am Sonntag" ~ "Neue Zürcher Zeitung",
    source == "NZZ International" ~ "Neue Zürcher Zeitung",
    source == "NURNBN" ~ "Nürnberger Nachrichten",
    source == "OSTSEZ" ~ "Ostsee-Zeitung",
    source == "news aktuell OTS - Originaltextservice" ~ "OTS",
    source == "OTS - Originaltextservice" ~ "OTS",
    source == "RBB Radio Transkripte" ~ "RBB",
    source == "RBB Transkripte" ~ "RBB",
    source == "Rheinische Post Online" ~ "Rheinische Post",
    source == "RHEPO" ~ "Rheinische Post",
    source == "SDDZ" ~ "Süddeutsche Zeitung",
    source == "Süddeutsche Zeitung Online" ~ "Süddeutsche Zeitung",
    source == "SPGL" ~ "Spiegel",
    source == "SPGLO" ~ "Spiegel",
    source == "Spiegel Online" ~ "Spiegel",
    source == "Spiegel Plus" ~ "Spiegel",
    source == "Spiegel Online (Deutsch)" ~ "Spiegel",
    source == "Tages Anzeiger Online" ~ "Tages Anzeiger",
    source == "taz - die tageszeitung" ~ "TAZ",
    TRUE ~ as.character(source)
  ))

# Summaries ####
# --------------

# By source

## Choose most common sources

israelkritk_source <- israel_data %>%
  mutate(date = as.POSIXct(date, origin = '1970-01-01',tz='UTC')) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  group_by(source) %>%
  summarize(israel_count = n()) %>%
  arrange(israel_count)

israelkritk_source_year <- israel_data %>%
  mutate(date = as.POSIXct(date, origin = '1970-01-01',tz='UTC')) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  filter(source == "Die Welt" | source == "TAZ" | source == "Süddeutsche Zeitung" | 
           source == "Der Tagesspiegel" | source == "DIE ZEIT") %>%
  group_by(source, year) %>%
  summarize(count = n()) %>%
  filter(year < as.Date("2021-01-01")) 

israelkritik_year_5 <- israel_data %>%
  mutate(date = as.POSIXct(date, origin = '1970-01-01',tz='UTC')) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  filter(source == "Die Welt" | source == "TAZ" | source == "Süddeutsche Zeitung" | 
           source == "Der Tagesspiegel" | source == "DIE ZEIT") %>%
  group_by(year) %>%
  summarize(count = n()/5) %>%
  filter(year < as.Date("2021-01-01")) 
  
israelkritik_year <- israel_data %>%
  mutate(date = as.POSIXct(date, origin = '1970-01-01',tz='UTC')) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  group_by(year) %>%
  summarize(israel_count = n()) %>%
  filter(year < as.Date("2021-01-01")) 

chinakritik_year <- chinakritik_year %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = lubridate::ymd(year, truncated = 2L))

russlandkritik_year <- russlandkritik_year %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = lubridate::ymd(year, truncated = 2L))

kritik <- left_join(israelkritik_year, chinakritik_year, by = "year") %>%
  left_join(., russlandkritik_year, by = "year") %>%
  pivot_longer(cols = c(israel_count, china_count, russia_count), 
               names_to = "country", values_to = "count") %>%
  mutate(country = case_when(
    country == "china_count" ~ "Chinakritik",
    country == "israel_count" ~ "Israelkritik",
    country == "russia_count" ~ "Russlandkritik"))

# Graphs ####
# ----------

sources_year <- ggplot(data = israelkritk_source_year, 
                       aes(x = year, y = count)) +
  geom_line(size = 0.75, alpha = 0.6, aes(group = source, color = source)) +
  geom_smooth(data = israelkritik_year_5, 
              aes(x = year, y = count), size = 1.5, se = FALSE, color = "#666666") +
  scale_x_datetime(expand = c(0.08, 0)) +
  labs(x = "Jahr", y = "Artikel") +
  geom_text(aes(x = as.POSIXct("2018-01-01"), y = 25, 
                label = "Welt", color = "Die Welt"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2009-01-01"), y = 17, 
                label = "Süddeutsche \n Zeitung", color = "Süddeutsche Zeitung"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2014-01-01"), y = 19, 
                label = "TAZ", color = "TAZ"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2020-01-01"), y = 18, 
                label = "Tagesspiegel", color = "Der Tagesspiegel"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2017-01-01"), y = 3,
                label = "Zeit", color = "DIE ZEIT"), 
            show.legend = F, size = 5) +
  # theme_economist_white() +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size = 0.2),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size = 16, margin = margin(t = 5, r = 0, b = 0, l = 0)))

israel_china_russia <- ggplot(kritik, aes(x = year, y = count, group = country, color = country)) +
  geom_line(size = 0.75, alpha = 0.6) + 
  labs(x = "Jahr", y = "Artikel") +
  scale_x_datetime(expand = c(0.05, 0)) +
  scale_color_manual(values = c("#df2407", "#0038b8", "#878787")) + 
  geom_text(aes(x = as.POSIXct("2012-01-01"), y = 200, 
                label = "Israelkritik", color = "Israelkritik"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2019-07-01"), y = 58, 
                label = "Chinakritik", color = "Chinakritik"), 
            show.legend = F, size = 5) +
  geom_text(aes(x = as.POSIXct("2014-01-01"), y = 65, 
                label = "Russlandkritik", color = "Russlandkritik"), 
            show.legend = F, size = 5) +
  # theme_economist_white() +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size = 0.2),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(size = 16, margin = margin(t = 5, r = 0, b = 0, l = 0)))

