detach("package:CorMat2Es", unload = TRUE)
devtools::install_github("copariuc/CorMat2ES", force = T)
library(CorMat2Es)

# Functia de returnare a datelor demografice
ret.sample <- function (ref) {
  author <- bd.meta[which(bd.meta$label == ref),]$author
  year <- as.numeric(tabel.surse[which(tabel.surse$label == ref),]$year)
  n <- as.numeric(bd.meta[which(bd.meta$label == ref),]$n)
  prop.m <- as.numeric(bd.meta[which(bd.meta$label == ref),]$p.male)
  m.age <- as.numeric(bd.meta[which(bd.meta$label == ref),]$m.age)
  male <- round(prop.m * n)
  female <- n - male
  return(list(author = author, 
              year = year, 
              n = n,
              male = male,
              female = female,
              prop.m = prop.m, 
              m.age = m.age))}

# Incarcarea seturilor de date
load("Centralizator.Rdata"); load("Codari.Rdata"); eliminate <- 0

## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. Giammarco, P. A. Vernon (2012)
## Relationships between bullying behaviours and the Dark Triad: A study with adults
ref = "ref_004"; dem <- ret.sample(ref = ref)
matrice <- '
.89
.89 .86
.97 .76 .86
.94 .74 .97 .83
.80 .60 .83 .65 .69
.55 .49 .53 .51 .41 .78
.22 .21 .20 .20 .11 .42 .71
.35 .33 .34 .35 .22 .48 .35 .73
'
var <- c("Total Bullying", "Indirect Bullying", "Direct Bullying", "Verbal Direct Bullying",
        "Physical Direct Bullying", "Psychopathy", "Narcissism", "Machiavellianism")
sec <- c("Psychopathy", "Narcissism", "Machiavellianism"); N.x <- 5
efecte <- Mat2DF(Study = dem$author, Mat.Cor = matrice, Vars = var,  n.X = N.x, s.Y = sec, N = dem$n)$effects

## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) 
## Dark personalities on Facebook: Harmful online behaviors and language
ref = "ref_006"; dem <- ret.sample(ref = ref)
g1 <- beta2es(study = dem$author, type = "g", n1 = dem$male, n2 = dem$female,
             beta = 1.00, sd = 0.06 * sqrt(dem$n), 
             X = "Harmfull", Rxx = NA, Y = "Psychopathy", Ryy = .72)
g2 <- beta2es(study = dem$author, type = "g", n1 = dem$male, n2 = dem$female,
              beta = .01, sd = 0.05 * sqrt(dem$n), 
              X = "Harmfull", Rxx = NA, Y = "Machiavellianism", Ryy = .72)
g3 <- beta2es(study = dem$author, type = "g", n1 = dem$male, n2 = dem$female,
              beta = .01, sd = 0.05 * sqrt(dem$n), 
              X = "Harmfull", Rxx = NA, Y = "Narcissism", Ryy = .72)
efecte <- rbind(efecte, g1, g2, g3)
