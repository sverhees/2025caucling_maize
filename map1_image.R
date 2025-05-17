
# script for static map

# packages
library(tidyverse) #data manipulation
library(lingtypology) #linguistic maps

# data
maize <- read_tsv("maize.tsv")
villages <- read_tsv("~/Git/master_villages/villages.tsv")

# split feature dataset into dialect layers and ascribe color(shade)s

# village layer
feature_villages <- maize %>%
  filter(map == "yes") %>%
  select(idiom, type, value1, value2, example) %>%
  filter(type == "village") %>%
  mutate(value1_color = case_when(value1 == 'hajj-wheat' ~ '#16a085',
                                  value1 == 'other wheat' ~ '#773e8e',
                                  value1 == 'other' ~ '#f39c12',
                                  TRUE ~ 'white'))

# dialect_nt2 layer
feature_nt2 <- maize %>%
  filter(map == "yes") %>%
  select(idiom, type, value1, value2, example) %>%
  filter(type == "dialect_nt2") %>%
  mutate(value1_color = case_when(value1 == 'hajj-wheat' ~ '#1ccaa7',
                                  value1 == 'other wheat' ~ '#aa71c1',
                                  value1 == 'other' ~ '#f5ae3d',
                                  TRUE ~ 'white'))


# dialect_nt1 layer

feature_nt1 <- maize %>%
  filter(map == "yes") %>%
  select(idiom, type, value1, value2, example) %>%
  filter(type == "dialect_nt1") %>%
  mutate(value1_color = case_when(value1 == 'hajj-wheat' ~ '#35e3c1',
                                  value1 == 'other wheat' ~ '#bf95d0',
                                  value1 == 'other' ~ '#f7c36e',
                                  TRUE ~ 'white'))

# dialect_toplevel layer

feature_toplevel <- maize %>%
  filter(map == "yes") %>%
  select(idiom, type, value1, value2, example) %>%
  filter(type == "dialect_toplevel") %>%
  mutate(value1_color = case_when(value1 == 'hajj-wheat' ~ '#a5f3e3',
                                  value1 == 'other wheat' ~ '#d5b8e0',
                                  value1 == 'other' ~ '#fad79e',
                                  TRUE ~ 'white'))

# general language layer

feature_lang <- maize %>%
  filter(map == "yes") %>%
  select(idiom, lang, type, value1, value2, example) %>%
  filter(type == "lang") %>%
  mutate(value1_color = case_when(value1 == 'hajj-wheat' ~ '#16a085',
                                  value1 == 'other wheat' ~ '#773e8e',
                                  value1 == 'other' ~ '#f39c12',
                                  TRUE ~ 'white'))

# combine layers for the map

# subset coordinate data
coord <- villages %>%
  select(village, lat, lon, lang, gltc_lang, dialect_toplevel, dialect_nt1, dialect_nt2, village_dialect)

# combine dialect layers and coordinates
coord_features <- coord %>%
  left_join(feature_villages, by = c("village_dialect" = "idiom")) %>%
  left_join(feature_nt2, by = c("dialect_nt2" = "idiom")) %>%
  mutate(value1 = coalesce(value1.x, value1.y),
         value2 = coalesce(value2.x, value2.y),
         example = coalesce(example.x, example.y),
         type = coalesce(type.x, type.y),
         value1_color = coalesce(value1_color.x, value1_color.y)) %>%
  select(village, lat, lon, lang, gltc_lang, dialect_toplevel, dialect_nt1, dialect_nt2, village_dialect,
         value1, value2, type, example, value1_color) %>%
  left_join(feature_nt1, by = c("dialect_nt1" = "idiom")) %>%
  mutate(value1 = coalesce(value1.x, value1.y),
         value2 = coalesce(value2.x, value2.y),
         example = coalesce(example.x, example.y),
         type = coalesce(type.x, type.y),
         value1_color = coalesce(value1_color.x, value1_color.y)) %>%
  select(village, lat, lon, lang, gltc_lang, dialect_toplevel, dialect_nt1, dialect_nt2, village_dialect,
         value1, value2, type, example, value1_color) %>%
  left_join(feature_toplevel, by = c("dialect_toplevel" = "idiom")) %>%
  mutate(value1 = coalesce(value1.x, value1.y),
         value2 = coalesce(value2.x, value2.y),
         example = coalesce(example.x, example.y),
         type = coalesce(type.x, type.y),
         value1_color = coalesce(value1_color.x, value1_color.y)) %>%
  select(village, lat, lon, lang, gltc_lang, dialect_toplevel, dialect_nt1, dialect_nt2, village_dialect,
         value1, value2, type, example, value1_color) %>%
  left_join(feature_lang, by = c("lang" = "lang")) %>%
  mutate(value1 = coalesce(value1.x, value1.y),
         value2 = coalesce(value2.x, value2.y),
         example = coalesce(example.x, example.y),
         type = coalesce(type.x, type.y),
         value1_color = coalesce(value1_color.x, value1_color.y)) %>%
  select(village, lat, lon, lang, gltc_lang, idiom, dialect_toplevel, dialect_nt1, dialect_nt2, village_dialect,
         value1, value2, type, example, value1_color) %>%
  mutate(value1_type = paste(value1, type, sep=" - ")) %>%
  filter(!is.na(type)) %>%
  mutate(type_name = case_when(type == "lang" ~ idiom,
                               type == "village" ~ village_dialect,
                               type == "dialect_toplevel" ~ dialect_toplevel,
                               type == "dialect_nt1" ~ dialect_nt1,
                               type == "dialect_nt2" ~ dialect_nt2)) %>%
  select(village, lat, lon, lang, gltc_lang, 
         value1, value2, type, example, value1_type, value1_color, type_name)

# make a subset for the general language layer

lang_features <- coord_features %>%
  filter(type=='lang')

coord_features2 <- coord_features %>%
  filter(type!='lang')

# make a map

map.feature(lang.gltc(lang_features$gltc_lang),
            latitude = lang_features$lat,
            longitude = lang_features$lon,
            features = lang_features$value1_type,
            legend = FALSE,
            #control = lang_features$type,
            width = 5,
            zoom.level = 8,
            color = lang_features$value1_color,
            opacity = 0.1,
            popup = (paste(lang_features$village,"<br>",lang_features$type_name, lang_features$type, "<br>","<i>", lang_features$example, "(",lang_features$value1_type,")")),
            #zoom.control = TRUE,
            tile = "Esri.WorldGrayCanvas") %>%
  map.feature(lang.gltc(coord_features2$gltc_lang),
              latitude = coord_features2$lat,
              longitude = coord_features2$lon,
              features = coord_features2$value1_type,
              legend.position = "bottomleft",
              #control = coord_features2$type,
              width = 5,
              color = coord_features2$value1_color,
              opacity = 1,
              zoom.level = 8,
              popup = (paste(coord_features2$village,"<br>",coord_features2$type_name, coord_features2$type, "<br>","<i>", coord_features2$example, "(",coord_features2$value1_type,")")),
              zoom.control = TRUE,
              tile = "Esri.WorldGrayCanvas",
              pipe.data = .)

