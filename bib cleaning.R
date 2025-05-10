
# packages
library(tidyverse) #data manipulation

#data
biblib <- read_tsv("biblib.tsv")

maizebibs <-maize %>%
  select(BIBTEXKEY = example_source, example_page) %>%
  distinct()

bibmaize <- biblib %>%
  inner_join(maizebibs, by = "BIBTEXKEY")

bib_clean <- bibmaize %>%
  mutate(translation = case_when(TITLE_TRANSLATION != 'NA' ~ paste0(' [',TITLE_TRANSLATION,']'),
                                 TRUE ~ NA)) %>%
  mutate(author = case_when(is.na(AUTHOR) ~ EDITOR,
                            TRUE ~ AUTHOR)) %>%
  mutate(page = case_when(is.na(example_page) ~ '.',
                          TRUE ~ paste0(', ', example_page,'.'))) %>% 
  mutate(source = apply(cbind(author, ' (',YEAR,') ', TITLE, translation, '. ', ADDRESS,': ', PUBLISHER, page), 1, 
                        function(x) paste(x[!is.na(x)], collapse = "")))


print(bib_clean$source)


