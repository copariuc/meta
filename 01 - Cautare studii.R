if(!require(scholar)) install.packages("scholar")
if(!require(dplyr)) install.packages("dplyr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("elizagrames/litsearchr")
if(!require(bib2df)) install.packages("bib2df")
if(!require(PRISMA2020)) install.packages("PRISMA2020")
if(!require(revtools)) install.packages("revtools")
if(!require(flextable)) install.packages("flextable")
if(!require(writexl)) install.packages("writexl")
devtools::install_github("nealhaddaway/GSscraper")

library(scholar); library(dplyr); library(litsearchr)
library(bib2df); library(PRISMA2020); library(revtools)
library(flextable); library(writexl)

# Utilizarea pachetului „scholar” ####
id <- "20pY4EYAAAAJ"

# Extragerea publicatiilor si a numarului de citari
works <- get_publications(id); nrow(works)
cites <- get_citation_history(id)
sum(cites$cites); range(cites$year)

# Extragerea informatiilor despre autor, profil si indice hirsh
author <- get_profile(id)
author$name; author$affiliation; author$h_index; author$i10_index; author$total_cites; author$interests

# Compararea carierei autorilor
comparare <- compare_scholar_careers(ids = c(id, 'BXrDvr8AAAAJ'))
comparare %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(total_citari = sum(cites),
                   primul.an = min(year),
                   ultimul.an = max(year))

# Afisarea informatiilor despre jurnalele in care a publicat autorul
impact <- get_journalrank(journals = works$journal); table(is.na(impact$Rank))
range(as.numeric(impact$Rank), na.rm = T)
range(as.numeric(impact$SJR), na.rm = T)
range(as.numeric(impact$H.index), na.rm = T)
metrics <- get_impactfactor(journals = works$journal)
range(as.numeric(metrics$ImpactFactor), na.rm = T)
range(as.numeric(metrics$Eigenfactor), na.rm = T)


# Rafinarea cuvintelor cheie ####
# Importul selectiei 
surse <- import_results(file = "Minimal.bib")
# Extragerea termenilor din titlu, abstract si cuvinte cheie
extract_terms(text = surse$title, method = "fakerake", 
              min_freq = 3, language = "English")
extract_terms(text = surse$keywords, method = "fakerake", 
              min_freq = 1, language = "English")
extract_terms(text = surse$abstract, method = "fakerake", 
              min_freq = 3, language = "English")

# Crearea DFM
dfm <- create_dfm(elements = surse$abstract,
                  features = c("cyberbullying", "social network", "Facebook", "Twitter",
                               "YouTube", "Instagram", "Pinterest", "SnapChat", 
                               "dark tetrad","dark triad",
                               "social media", "triad personality traits", "triad traits", 
                               "triad personality"))
# Crearea retelei de asociere
retea.asociere <- create_network(search_dfm = as.matrix(dfm), min_studies = 1, min_occ = 1)
plot(retea.asociere, main = "Reteaua de asociere a cuvintelor cheie")
dfm <- create_dfm(elements = surse$abstract,
                  features = c("cyberbullying", "social media", "Facebook", "Twitter",
                               "YouTube", "Instagram", "Pinterest", "SnapChat", 
                               "dark tetrad","dark triad",
                               "social media", "triad personality traits", "triad traits", 
                               "triad personality"))
dfm <- create_dfm(elements = surse$abstract,
                  features = c("cyberbullying", "Facebook", "Twitter",
                               "YouTube", "Instagram", "Pinterest", "SnapChat", 
                               "dark triad", "triad personality traits", "triad traits", 
                               "triad personality"))
# Extragerea cuvintelor cheie
get_keywords(retea.asociere)

# Construirea si aranjarea tabelului centralizator ####
tabel.surse <- data.frame(ID = NA, Type = NA, Authors = NA, Year = NA, Title = NA,
                          Journal = NA, Abstract = NA, Keywords = NA, DB = NA, DOI = NA)
# Importul bibliografiei in R
sursa <- bib2df("Elsevier Science Direct.bib"); dbase <- paste("Elsevier Science Direct \n", date()); rec <- 1
sursa <- bib2df("Elsevier Scopus.bib"); dbase <- paste("Elsevier Scopus \n", date()); rec <- 1
sursa <- bib2df("Web of science.bib"); dbase <- paste("Web of science \n", date()); rec <- 1
sursa <- bib2df("Wiley Ebooks.bib"); dbase <- paste("Wiley Ebooks \n", date()); rec <- 1
sursa <- bib2df("Springer Link.bib"); dbase <- paste("Springer Link\n", date()); rec <- 1
while(rec <= nrow(sursa)) {
  rand <- sursa %>%
    filter(BIBTEXKEY == sursa$BIBTEXKEY[rec])
  id <- rand$BIBTEXKEY
  type <- rand$CATEGORY
  authors <- paste(unlist(rand$AUTHOR), collapse="; ")
  year <- rand$YEAR
  title <- gsub("[{|}]", "", rand$TITLE)
  journal <- rand$JOURNAL
  abstract <- rand$ABSTRACT
  keywords <- rand$KEYWORDS
  db.nume <- dbase
  doi <- rand$DOI
  # Actualizarea tabelului centralizator - ITERATIV
  tabel.surse <- rbind(tabel.surse, 
                       c(id, type, authors, year, title, journal,  
                         abstract, keywords, db.nume, doi))
  rec <- rec + 1   # Trecerea la urmatorul rand
}

# Stergerea primei inregistrari si a obiectelor inutile - LA FINAL
if (is.na(tabel.surse[1,])) tabel.surse <- tabel.surse[-1,]
rm(rand, sursa, abstract, authors, doi, id, journal,
   keywords, rec, title, type, year, db.nume, dbase)
save(tabel.surse, file = "Centralizator.Rdata"); total.db <- nrow(tabel.surse)

# Scrappingul datelor din alte surse de date ####
search.string <- 'cyberbullying AND (Facebook OR Twitter OR Youtube OR Instagram OR Pinterest OR SnapChat) AND ("dark triad" OR "triad personality traits" OR "triad traits" OR "triad personality")'
scrape_hits(search_terms = search.string,
            writefile = T, verbose = T, database = "ndltd",
            directory = "./")
registers <- 0; rm(search.string)

# Crearea structurii PRISMA ####
PRISMA.template <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
# Incarcarea informatiilor in sablonul PRISMA - TOTALUL SURSELOR
PRISMA.template <- PRISMA.template %>%
  mutate(boxtext = case_when(data == "identification"~"Identification", T~boxtext))
PRISMA.template$n[which(PRISMA.template$data == "database_results")] <- total.db
PRISMA.template$n[which(PRISMA.template$data == "register_results")] <- registers

# Desenarea si afisarea diagramei - FAZA INITIALA
PRISMA <- PRISMA_flowdiagram(PRISMA_data(PRISMA.template),
                             interactive = T, previous = F, other = F,
                             fontsize = 10, font = "Arial",
                             title_colour = "DarkOrange",         # Culoarea titului sectiunii - Baze de date
                             greybox_colour = "DarkOliveGreen",   # Culoarea intregii sectiuni - Alte surse
                             #main_colour = "Red",                # Culoarea bordurilor - Baze de date
                             arrow_colour = "SteelBlue",          # Culoarea sagetii
                             arrow_head = "vee",                  # Tipul varfului sagetii
                             #arrow_tail = "none",                # Tipul cozii sagetii
                             side_boxes = T)
PRISMA; PRISMA_save(PRISMA, overwrite = T, filename = "PRISMA.png", filetype = "PNG")

# Cautarea inregistrarilor duplicat ####
gasite <- find_duplicates(data = tabel.surse,
                          match_variable = "DOI",
                          match_function = "exact")
gasite <- extract_unique_references(tabel.surse, gasite)
duplicate <- sum(gasite$n_duplicates) - nrow(gasite)
# Scanare suplimentara a duplicatelor si generarea tabelului de analiza
rezult <- screen_duplicates(x= gasite)
duplicate <- duplicate + nrow(gasite) - nrow(rezult)
PRISMA.template$n[which(PRISMA.template$data == "duplicates")] <- duplicate

# Scanarea inregistrarilor dupa topic ####
rezult <- screen_topics(x = rezult)
temp <- rezult$raw %>%
  dplyr::filter(screened_topics == "selected")
del.topics <- nrow(rezult$raw) - nrow(temp)
PRISMA.template$n[which(PRISMA.template$data == "excluded_automatic")] <- del.topics

# Scanarea inregistrarilor dupa tirlu ####
rezult <- screen_titles(x = temp)
temp <- rezult %>%
  dplyr::filter(screened_titles == "selected")
del.titles <- nrow(rezult) - nrow(temp)
PRISMA.template$n[which(PRISMA.template$data == "excluded_other")] <- del.titles

# Salvarea datelor - FAZA I
# Exportul rezultatelor intr-un tabel Excel
write_xlsx(temp[, 1:11], path = "Centralizator.xlsx")
# Crearea si salvarea unui tabel
tabel <- flextable(data = temp[, 1:11]) %>%
  theme_vanilla(); tabel
tabel %>% 
  save_as_html(path = "Centralizator.html") %>%
  save_as_docx( path = "Centralizator.docx")

save(temp, file = "Temp.RData")
save(PRISMA.template, file = "PRISMA.Rdata")
