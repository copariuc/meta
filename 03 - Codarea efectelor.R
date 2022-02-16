# Incarcarea pachetelor necesare^
if (!require(esc)) install.packages("esc")
if (!require(MAd)) install.packages("MAd")
library(esc); library(MAd); library(PRISMA2020)
library(dplyr)

# Demonstrarea marimii efectului ####
femei <- round(rnorm(n=19, mean=22, sd=3))
barbati <- round(rnorm(n=19, mean=20, sd=3))
mean(femei); mean(barbati)
sd(femei); sd(barbati)
t.test(femei, barbati, var.equal = T)

# Extragerea scorului z sau t situat la limita superioara a distributiei
qnorm(p = .0065, lower.tail = F)
qt(p = .0065, df=36, lower.tail = F)
qchisq(p=0.043, df=1, n=200, lower.tail = F)

# Codarea contrastului polinomial
contr.poly(3, contrasts = T)
contr.poly(4, contrasts = T) * 10
contr.poly(5, contrasts = T) * 10
contr.poly(6, contrasts = T) * 10

# Codarea efectelor folosindu-se pachetul "esc" ####
load("Centralizator.Rdata"); load("PRISMA.Rdata"); load("Codari.Rdata"); eliminate <- 0
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
# Functia de conversie a coeficientului de corelatie in marimea efectului
r2g <- function(ref, r, n, rxx, ryy, vx, vy) {
  es <- hedges_g(d = cohens_d(r = r), totaln = n)
  se <- 1/sqrt(n - 3)
  var <- se ^ 2
  ci <- convert_r2z(r) + c(-1, 1) *  se * qnorm((1 + .95) / 2)
  ci <- hedges_g(d = cohens_d(r = convert_z2r(ci)), totaln = n)
  w <- 1/(se ^ 2)
  study <- bd.meta[which(bd.meta$label == ref),]$author
  return(list(study = study,
              vx = vx, vy = vy,
              es = as.numeric(es),
              se = as.numeric(se),
              var = as.numeric(var),
              ci = as.numeric(ci),
              w = as.numeric(w),
              rxx = as.numeric(rxx),
              ryy = as.numeric(ryy)))}
# Functia de returnare a informatiilor suplimentare
info.g <- function(ref, vx, rxx, vy, ryy) {
  return(list(
    vx = vx, vy = vy,
    rxx = as.numeric(rxx),
    ryy = as.numeric(ryy)))}

## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. Giammarco, P. A. Vernon (2012)
## Relationships between bullying behaviours and the Dark Triad: A study with adults
ref = "ref_004"; dem <- ret.sample(ref = ref)
r = .35;  rxx <- .89; ryy = .73; x = "Bullying Total"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .22;  rxx <- .89; ryy = .71; x = "Bullying Total"; y = "Narcissism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .55;  rxx <- .89; ryy = .78; x = "Bullying Total"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .33;  rxx <- .76; ryy = .73; x = "Bullying Indirect"; y = "Machiavellianism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .21;  rxx <- .76; ryy = .71; x = "Bullying Indirect"; y = "Narcissism"
g5 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .49;  rxx <- .76; ryy = .78; x = "Bullying Indirect"; y = "Psychopathy"
g6 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .34;  rxx <- .86; ryy = .73; x = "Bullying Direct"; y = "Machiavellianism"
g7 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .20;  rxx <- .86; ryy = .71; x = "Bullying Direct"; y = "Narcissism"
g8 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .53;  rxx <- .86; ryy = .78; x = "Bullying Direct"; y = "Psychopathy"
g9 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .35;  rxx <- .83; ryy = .73; x = "Bullying Verbal Direct"; y = "Machiavellianism"
g10 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .20;  rxx <- .83; ryy = .71; x = "Bullying Verbal Direct"; y = "Narcissism"
g11 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .51;  rxx <- .83; ryy = .78; x = "Bullying Verbal Direct"; y = "Psychopathy"
g12 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .22;  rxx <- .69; ryy = .73; x = "Bullying Physical Direct"; y = "Machiavellianism"
g13 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .11;  rxx <- .69; ryy = .71; x = "Bullying Physical Direct"; y = "Narcissism"
g14 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .41;  rxx <- .69; ryy = .78; x = "Bullying Physical Direct"; y = "Psychopathy"
g15 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- data.frame(
  c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
    g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
    g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
    g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
    g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g5$study, g5$es, g5$w, dem$n, g5$se, g5$var, g5$ci[1], g5$ci[2], "g",
    g5$vx, g5$rxx, g5$vy, g5$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g6$study, g6$es, g6$w, dem$n, g6$se, g6$var, g6$ci[1], g6$ci[2], "g",
    g6$vx, g6$rxx, g6$vy, g6$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g7$study, g7$es, g7$w, dem$n, g7$se, g7$var, g7$ci[1], g7$ci[2], "g",
    g7$vx, g7$rxx, g7$vy, g7$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g8$study, g8$es, g8$w, dem$n, g8$se, g8$var, g8$ci[1], g8$ci[2], "g",
    g8$vx, g8$rxx, g8$vy, g8$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g9$study, g9$es, g9$w, dem$n, g9$se, g9$var, g9$ci[1], g9$ci[2], "g",
    g9$vx, g9$rxx, g9$vy, g9$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g10$study, g10$es, g10$w, dem$n, g10$se, g10$var, g10$ci[1], g10$ci[2], "g",
    g10$vx, g10$rxx, g10$vy, g10$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g11$study, g11$es, g11$w, dem$n, g11$se, g11$var, g11$ci[1], g11$ci[2], "g",
    g11$vx, g11$rxx, g11$vy, g11$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g12$study, g12$es, g12$w, dem$n, g12$se, g12$var, g12$ci[1], g12$ci[2], "g",
    g12$vx, g12$rxx, g12$vy, g12$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g13$study, g13$es, g13$w, dem$n, g13$se, g13$var, g13$ci[1], g13$ci[2], "g",
    g13$vx, g13$rxx, g13$vy, g13$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g14$study, g14$es, g14$w, dem$n, g14$se, g14$var, g14$ci[1], g14$ci[2], "g",
    g14$vx, g14$rxx, g14$vy, g14$ryy, dem$year, dem$m.age, dem$prop.m),
  c(g15$study, g15$es, g15$w, dem$n, g15$se, g15$var, g15$ci[1], g15$ci[2], "g",
    g15$vx, g15$rxx, g15$vy, g15$ryy, dem$year, dem$m.age, dem$prop.m))
efecte <- as.data.frame(t(efecte)); rownames(efecte) <- NULL
colnames(efecte) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                      "X", "Rxx", "Y", "Ryy", "year", "age", "male")
rm(list = ls(pattern = "^g"), dem)

## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) 
## Dark personalities on Facebook: Harmful online behaviors and language
ref = "ref_006"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Harmfull", rxx = NA, vy ="Psychopathy", ryy = .72)
g1 <- esc_beta(beta = 1.00, sdy = 0.06 * sqrt(dem$n), grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Harmfull", rxx = NA, vy ="Machiavellianism", ryy = .72)
g2 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(dem$n), grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Harmfull", rxx = NA, vy ="Narcissism", ryy = .72)
g3 <- esc_beta(beta = 0.01, sdy = 0.05 * sqrt(dem$n), grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx),
              c(gi1$vy, gi2$vy, gi3$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy),
              c(rep(dem$year, 3)), c(rep(dem$m.age, 3)), c(rep(dem$prop.m, 3)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 3. Studiul ref_012 - CODARE ####
## N. Craker, E. March (2016)
## The dark side of Facebook: The Dark Tetrad, negative social potency, and trolling behaviours
ref = "ref_012"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Trolling", rxx = .70, vy ="Sadism", ryy = .58)
g1 <- esc_beta(beta = 0.07, sdy = 4.51, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Trolling", rxx = .70, vy ="Machiavellianism", ryy = .80)
g2 <- esc_beta(beta = -0.001, sdy = 4.51, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Trolling", rxx = .70, vy ="Psychopathy", ryy = .75)
g3 <- esc_beta(beta = 0.06, sdy =  4.51, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Trolling", rxx = .70, vy ="Narcissism", ryy = .82)
g4 <- esc_beta(beta = -0.01, sdy =  4.51, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp)

r = .35;  rxx <- .70; ryy = .58; x = "Trolling"; y = "Sadism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .34;  rxx <- .70; ryy = .80; x = "Trolling"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .18;  rxx <- .70; ryy = .82; x = "Trolling"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .39;  rxx <- .70; ryy = .75; x = "Trolling"; y = "Psychopathy"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), temp, dem)

## 4. Studiul ref_020 - CODARE ####
## A. K. Goodboy, M. M. Martin (2015) 
## The personality profile of a cyberbully: Examining the Dark Triad
ref = "ref_020"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Visual Cyberbullying", rxx = .84, vy ="Machiavellianism", ryy = .79)
g1 <- esc_beta(beta = 0.09, sdy = 4.29, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Visual Cyberbullying", rxx = .84, vy ="Psychopathy", ryy = .80)
g2 <- esc_beta(beta = 0.27, sdy = 4.29, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Visual Cyberbullying", rxx = .84, vy ="Narcissism", ryy = .82)
g3 <- esc_beta(beta = 0.05, sdy = 4.29, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Text Cyberbullying", rxx = .87, vy ="Machiavellianism", ryy = .79)
g4 <- esc_beta(beta = 0.07, sdy = 5.13, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi5 <- info.g(ref, vx = "Text Cyberbullying", rxx = .87, vy ="Psychopathy", ryy = .80)
g5 <- esc_beta(beta = 0.30, sdy = 5.13, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi6 <- info.g(ref, vx = "Text Cyberbullying", rxx = .87, vy ="Narcissism", ryy = .82)
g6 <- esc_beta(beta = 0.12, sdy = 5.13, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4, g5, g6)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx, gi5$vx, gi6$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx, gi5$rxx, gi6$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy, gi5$vy, gi6$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy, gi6$ryy, gi6$ryy),
              c(rep(dem$year, 6)), c(rep(dem$m.age, 6)), c(rep(dem$prop.m, 6)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 5. Studiul ref_022 - CODARE ####
## C. J. Hand, Graham G. Scott b, Zara P. Brodie b, Xilei Ye c, Sara C. Sereno (2021) 
## Tweet valence, volume of abuse, and observers’ dark tetrad personality factors influence victim-blaming and the perceived severity of twitter cyberabuse
ref = "ref_022"; dem <- ret.sample(ref = ref)
r = .211;  rxx <- .90; ryy = .71; x = "Victim Blame"; y = "Psychopathy"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .164;  rxx <- .90; ryy = .73; x = "Victim Blame"; y = "Narcissism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .252;  rxx <- .90; ryy = .70; x = "Victim Blame"; y = "Machiavellianism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .265;  rxx <- .90; ryy = .85; x = "Victim Blame"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## 6. Studiul ref_029 - CODARE ####
## Kurek, A., Jose, P. E., & Stuart, J. (2019). 
## I did it for the LULZ: How the dark personality predicts online disinhibition and aggressive online behavior in adolescence.
ref = "ref_029"; dem <- ret.sample(ref = ref)
r = .17;  rxx <- .92; ryy = .93; x = "Cyber Aggression"; y = "Narcissism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .26;  rxx <- .92; ryy = .79; x = "Cyber Aggression"; y = "Sadism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .05;  rxx <- .92; ryy = .71; x = "Cyber Aggression"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## 7. Studiul ref_045 - CODARE ####
## Pabian, S., De Backer, C. J. S., & Vandebosch, H. (2015) 
## Dark Triad personality traits and adolescent cyber-aggression
ref = "ref_045"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Cyber Aggression", rxx = .84, vy ="Psychopathy", ryy = .77)
g1 <- esc_beta(beta = 0.60, sdy = 2.94, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Cyber Aggression", rxx = .84, vy ="Machiavellianism", ryy = .74)
g2 <- esc_beta(beta = -0.36, sdy = 2.94, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Cyber Aggression", rxx = .84, vy ="Narcissism", ryy = .61)
g3 <- esc_beta(beta = 0.23, sdy = 2.94, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx),
              c(gi1$vy, gi2$vy, gi3$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy),
              c(rep(dem$year, 3)), c(rep(dem$m.age, 3)), c(rep(dem$prop.m, 3)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp)
r = .30;  rxx <- .84; ryy = .74; x = "Cyber Aggression"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .29;  rxx <- .84; ryy = .61; x = "Cyber Aggression"; y = "Narcissism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .43;  rxx <- .84; ryy = .77; x = "Cyber Aggression"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m))

rm(list = ls(pattern = "^g"), temp, dem)

## 8. Studiul ref_053 - CODARE ####
## Stiff C. (2019)
## The Dark Triad and Facebook surveillance: How Machiavellianism, psychopathy, but not narcissism predict using Facebook to spy on others
ref = "ref_053"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Facebook tracking", rxx = .7, vy ="Machiavellianism", ryy = .7)
g1 <- esc_beta(beta = 0.16, sdy = 0.96, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Facebook investigating", rxx = .7, vy ="Machiavellianism", ryy = .7)
g2 <- esc_beta(beta = 0.04, sdy = 0.89, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Facebook tracking", rxx = .7, vy ="Psychopathy", ryy = .7)
g3 <- esc_beta(beta = 0.27, sdy = 0.96, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Facebook investigating", rxx = .7, vy ="Psychopathy", ryy = .7)
g4 <- esc_beta(beta = 0.10, sdy = 0.89, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 9.a. Studiul ref_059.a - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2019)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
ref = "ref_059"; dem <- ret.sample(ref = ref)
r = .71;  rxx <- .85; ryy = .89; x = "Trolling"; y = "Sadism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .32;  rxx <- .85; ryy = .82; x = "Trolling"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .26;  rxx <- .85; ryy = .76; x = "Trolling"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .63;  rxx <- .85; ryy = .82; x = "Trolling"; y = "Psychopathy"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .59;  rxx <- .68; ryy = .89; x = "Pain pleasure"; y = "Sadism"
g5 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .31;  rxx <- .68; ryy = .82; x = "Pain pleasure"; y = "Machiavellianism"
g6 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .18;  rxx <- .68; ryy = .76; x = "Pain pleasure"; y = "Narcissism"
g7 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .56;  rxx <- .68; ryy = .82; x = "Pain pleasure"; y = "Psychopathy"
g8 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g5$study, g5$es, g5$w, dem$n, g5$se, g5$var, g5$ci[1], g5$ci[2], "g",
                  g5$vx, g5$rxx, g5$vy, g5$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g6$study, g6$es, g6$w, dem$n, g6$se, g6$var, g6$ci[1], g6$ci[2], "g",
                  g6$vx, g6$rxx, g6$vy, g6$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g7$study, g7$es, g7$w, dem$n, g7$se, g7$var, g7$ci[1], g7$ci[2], "g",
                  g7$vx, g7$rxx, g7$vy, g7$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g8$study, g8$es, g8$w, dem$n, g8$se, g8$var, g8$ci[1], g8$ci[2], "g",
                  g8$vx, g8$rxx, g8$vy, g8$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## 9.b. Studiul ref_059.b - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018)
## Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
ref = "ref_059"; dem <- ret.sample(ref = ref); dem$n <- 223
r = .44;  rxx <- .91; ryy = .89; x = "Trolling"; y = "Sadism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .43;  rxx <- .91; ryy = .82; x = "Trolling"; y = "Psychopathy"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## 10. Studiul ref_067 - CODARE ####
## Brown W.M., Hazraty S., Palasinski M. (2019) 
## Examining the Dark Tetrad and Its Links to Cyberbullying
ref = "ref_067"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Narcissism", ryy = .88)
g1 <- esc_f(f = 24.40, totaln = dem$male, es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Narcissism", ryy = .88)
g2 <- esc_f(f = 42.67, totaln = dem$female, es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Machiavellianism", ryy = .88)
g3 <- esc_f(f = 52.93, totaln = dem$male, es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Machiavellianism", ryy = .88)
g4 <- esc_f(f = 129.7, totaln = dem$female, es.type = "g", study = dem$author)
gi5 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Psychopathy", ryy = .88)
g5 <- esc_f(f = 73.00, totaln = dem$male, es.type = "g", study = dem$author)
gi6 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Psychopathy", ryy = .88)
g6 <- esc_f(f = 118.10, totaln = dem$female, es.type = "g", study = dem$author)
gi7 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Sadism", ryy = .88)
g7 <- esc_f(f = 25.63, totaln = dem$male, es.type = "g", study = dem$author)
gi8 <- info.g(ref, vx = "Cyberbullying", rxx = .93, vy ="Sadism", ryy = .88)
g8 <- esc_f(f = 9.54, totaln = dem$female, es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4, g5, g6, g7, g8)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx, gi5$vx, gi6$vx, gi7$vx, gi8$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx, gi5$rxx, gi6$rxx, gi7$rxx, gi8$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy, gi5$vy, gi6$vy, gi7$vy, gi8$vy),
              c(gi1$ryy, gi2$ryy, gi4$ryy, gi3$ryy, gi5$ryy, gi6$ryy, gi7$ryy, gi8$ryy),
              c(rep(dem$year, 8)), c(rep(dem$m.age, 8)), c(rep(dem$prop.m, 8)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 11. Studiul ref_068 - CODARE ####
## Kircaburuna K., Jonasonb P.K., Griffithsc M.D. (2018)
## The Dark Tetrad traits and problematic social media use: The mediating role of cyberbullying and cyberstalking
ref = "ref_068"; dem <- ret.sample(ref = ref)
r = .46;  rxx <- .79; ryy = .82; x = "Cyberbullying"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .41;  rxx <- .79; ryy = .66; x = "Cyberbullying"; y = "Psychopathy"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .30;  rxx <- .79; ryy = .88; x = "Cyberbullying"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .47;  rxx <- .79; ryy = .74; x = "Cyberbullying"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .42;  rxx <- .83; ryy = .82; x = "Cybertrolling"; y = "Machiavellianism"
g5 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .38;  rxx <- .83; ryy = .66; x = "Cybertrolling"; y = "Psychopathy"
g6 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .28;  rxx <- .83; ryy = .88; x = "Cybertrolling"; y = "Narcissism"
g7 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .39;  rxx <- .83; ryy = .74; x = "Cybertrolling"; y = "Sadism"
g8 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g5$study, g5$es, g5$w, dem$n, g5$se, g5$var, g5$ci[1], g5$ci[2], "g",
                  g5$vx, g5$rxx, g5$vy, g5$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g6$study, g6$es, g6$w, dem$n, g6$se, g6$var, g6$ci[1], g6$ci[2], "g",
                  g6$vx, g6$rxx, g6$vy, g6$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g7$study, g7$es, g7$w, dem$n, g7$se, g7$var, g7$ci[1], g7$ci[2], "g",
                  g7$vx, g7$rxx, g7$vy, g7$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g8$study, g8$es, g8$w, dem$n, g8$se, g8$var, g8$ci[1], g8$ci[2], "g",
                  g8$vx, g8$rxx, g8$vy, g8$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## 12. Studiul ref_069 - CODARE ####
## Gylfason H.F., Sveinsdottir A.H., Vésteinsdóttir V., Sigurvinsdottir R. (2021) 
## Haters Gonna Hate, Trolls Gonna Troll: The Personality Profile of a Facebook Troll
ref = "ref_069"; dem <- ret.sample(ref = ref)
r = .449;  rxx <- .67; ryy = .63; x = "Trolling"; y = "Sadism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .367;  rxx <- .67; ryy = .80; x = "Trolling"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .285;  rxx <- .67; ryy = .74; x = "Trolling"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .105;  rxx <- .67; ryy = .86; x = "Trolling"; y = "Narcissism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))

gi5 <- info.g(ref, vx = "Trolling", rxx = .67, vy ="Sadism", ryy = .63)
g5 <- esc_beta(beta = .356, sdy = 50.53, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi6 <- info.g(ref, vx = "Trolling", rxx = .67, vy ="Machiavellianism", ryy = .80)
g6 <- esc_beta(beta = .242, sdy = 50.53, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi7 <- info.g(ref, vx = "Trolling", rxx = .67, vy ="Psychopathy", ryy = .74)
g7 <- esc_beta(beta = .063, sdy = 50.53, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi8 <- info.g(ref, vx = "Trolling", rxx = .67, vy ="Narcissism", ryy = .86)
g8 <- esc_beta(beta = -.110, sdy = 50.53, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g5, g6, g7, g8)
temp <- cbind(temp,
              c(gi5$vx, gi6$vx, gi7$vx, gi8$vx),
              c(gi5$rxx, gi6$rxx, gi7$rxx, gi8$rxx),
              c(gi5$vy, gi6$vy, gi7$vy, gi8$vy),
              c(gi5$ryy, gi6$ryy, gi7$ryy, gi8$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 13. Studiul ref_150 - CODARE ####
## Duncan Z., March E. (2019) 
## Examining the Dark Tetrad and Its Links to Cyberbullying
ref = "ref_150"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Antisocial-general", rxx = .85, vy ="Narcissism", ryy = .69)
g1 <- esc_beta(beta = .08, sdy = 3.10, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Antisocial-general", rxx = .85, vy ="Machiavellianism", ryy = .74)
g2 <- esc_beta(beta = .09, sdy = 3.10, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Antisocial-general", rxx = .85, vy ="Psychopathy", ryy = .73)
g3 <- esc_beta(beta = .21, sdy = 3.10, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Antisocial-general", rxx = .85, vy ="Sadism", ryy = .86)
g4 <- esc_beta(beta = .26, sdy = 3.10, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi5 <- info.g(ref, vx = "Antisocial-esteem", rxx = .89, vy ="Machiavellianism", ryy = .74)
g5 <- esc_beta(beta = .21, sdy = 4.57, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi6 <- info.g(ref, vx = "Antisocial-esteem", rxx = .89, vy ="Psychopathy", ryy = .73)
g6 <- esc_beta(beta = .06, sdy = 4.57, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi7 <- info.g(ref, vx = "Antisocial-esteem", rxx = .89, vy ="Sadism", ryy = .86)
g7 <- esc_beta(beta = .03, sdy = 4.57, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi8 <- info.g(ref, vx = "Antisocial-sexual", rxx = .67, vy ="Narcissism", ryy = .69)
g8 <- esc_beta(beta = .03, sdy = 2.47, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi9 <- info.g(ref, vx = "Antisocial-sexual", rxx = .67, vy ="Machiavellianism", ryy = .74)
g9 <- esc_beta(beta = .11, sdy = 2.47, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi10<- info.g(ref, vx = "Antisocial-sexual", rxx = .67, vy ="Psychopathy", ryy = .73)
g10 <- esc_beta(beta = .36, sdy = 2.47, grp1n = dem$male, grp2n = dem$female, 
                es.type = "g", study = dem$author)
gi11<- info.g(ref, vx = "Antisocial-sexual", rxx = .67, vy ="Sadism", ryy = .86)
g11 <- esc_beta(beta = .10, sdy = 2.47, grp1n = dem$male, grp2n = dem$female, 
                es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx, gi5$vx, gi6$vx, gi7$vx, gi8$vx, gi9$vx, gi10$vx, gi11$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx, gi5$rxx, gi6$rxx, gi7$rxx, gi8$rxx, gi9$rxx, gi10$rxx, gi11$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy, gi5$vy, gi6$vy, gi7$vy, gi8$vy, gi9$vy, gi10$vy, gi11$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy, gi5$ryy, gi6$ryy, gi7$ryy, gi8$ryy, gi9$ryy, gi10$ryy, gi11$ryy),
              c(rep(dem$year, 11)), c(rep(dem$m.age, 11)), c(rep(dem$prop.m, 11)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp)

r = .21;  rxx <- .85; ryy = .69; x = "Antisocial-general"; y = "Narcissism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .33;  rxx <- .85; ryy = .74; x = "Antisocial-general"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .42;  rxx <- .85; ryy = .73; x = "Antisocial-general"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .42;  rxx <- .85; ryy = .86; x = "Antisocial-general"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .12;  rxx <- .89; ryy = .69; x = "Antisocial-esteem"; y = "Narcissism"
g5 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .23;  rxx <- .89; ryy = .74; x = "Antisocial-esteem"; y = "Machiavellianism"
g6 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .17;  rxx <- .89; ryy = .73; x = "Antisocial-esteem"; y = "Psychopathy"
g7 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .12;  rxx <- .89; ryy = .86; x = "Antisocial-esteem"; y = "Sadism"
g8 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .20;  rxx <- .67; ryy = .69; x = "Antisocial-sexual"; y = "Narcissism"
g9 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .35;  rxx <- .67; ryy = .74; x = "Antisocial-sexual"; y = "Machiavellianism"
g10 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .49;  rxx <- .67; ryy = .73; x = "Antisocial-sexual"; y = "Psychopathy"
g11 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .36;  rxx <- .67; ryy = .86; x = "Antisocial-sexual"; y = "Sadism"
g11 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g5$study, g5$es, g5$w, dem$n, g5$se, g5$var, g5$ci[1], g5$ci[2], "g",
                  g5$vx, g5$rxx, g5$vy, g5$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g6$study, g6$es, g6$w, dem$n, g6$se, g6$var, g6$ci[1], g6$ci[2], "g",
                  g6$vx, g6$rxx, g6$vy, g6$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g7$study, g7$es, g7$w, dem$n, g7$se, g7$var, g7$ci[1], g7$ci[2], "g",
                  g7$vx, g7$rxx, g7$vy, g7$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g8$study, g8$es, g8$w, dem$n, g8$se, g8$var, g8$ci[1], g8$ci[2], "g",
                  g8$vx, g8$rxx, g8$vy, g8$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g9$study, g9$es, g9$w, dem$n, g9$se, g9$var, g9$ci[1], g9$ci[2], "g",
                  g9$vx, g9$rxx, g9$vy, g9$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g10$study, g10$es, g10$w, dem$n, g10$se, g10$var, g10$ci[1], g10$ci[2], "g",
                  g10$vx, g10$rxx, g10$vy, g10$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g11$study, g11$es, g11$w, dem$n, g11$se, g11$var, g11$ci[1], g11$ci[2], "g",
                  g11$vx, g11$rxx, g11$vy, g11$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## *** Studiul ref_159 - CODARE ####
## Ferenczi N., Marshall T.C., Bejanyan K. (2017) 
## Are sex differences in antisocial and prosocial Facebook use explained by narcissism and relational self-construal?
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## *** Studiul ref_195 - CODARE ####
## Hussain Z., Wegmann E., Grifths M.D. (2021) 
## The association between problematic  social networking site use, dark triad traits,and emotion dysregulation
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## *** Studiul ref_216 - CODARE ####
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## 14. Studiul ref_221 - CODARE ####
## Kircaburun K., Jonason P. Griffiths M.D., Aslanargun E., Emirtekin E., Tosuntas S.B., Billieux J. (2019)
## Childhood Emotional Abuse and Cyberbullying Perpetration: The Role of Dark Personality Traits
ref = "ref_221"; dem <- ret.sample(ref = ref)
r = .46;  rxx <- .79; ryy = .81; x = "Cyberbullying"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .41;  rxx <- .79; ryy = .67; x = "Cyberbullying"; y = "Psychopathy"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .30;  rxx <- .79; ryy = .88; x = "Cyberbullying"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .43;  rxx <- .79; ryy = .77; x = "Cyberbullying"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem, temp)

## *** Studiul ref_224 - CODARE ####
## Kircaburun K., Griffiths M.D. (2018)
## The dark side of internet: Preliminary evidence for the associations of dark personality traits with specific online activities and problematic internet use 
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## 15. Studiul ref_244 - CODARE ####
## March E., Grieve R., Marrington J., Jonason P.K. (2017)
## Trolling on Tinder (and other dating apps): Examining the role of the Dark Tetrad and impulsivity
ref = "ref_244"; dem <- ret.sample(ref = ref)
r = .11;  rxx <- .74; ryy = .80; x = "Trolling"; y = "Narcissism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .20;  rxx <- .74; ryy = .77; x = "Trolling"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .32;  rxx <- .74; ryy = .73; x = "Trolling"; y = "Psychopathy"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .25;  rxx <- .74; ryy = .84; x = "Trolling"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))

gi1 <- info.g(ref, vx = "Trolling", rxx = .74, vy ="Psychopathy", ryy = .73)
g1 <- esc_beta(beta = 0.17, sdy = 2.98, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Trolling", rxx = .74, vy ="Sadism", ryy = .84)
g2 <- esc_beta(beta = 0.16, sdy = 2.98, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Trolling", rxx = .74, vy ="Narcissism", ryy = .80)
g3 <- esc_beta(beta = 0.06, sdy = 2.98, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Trolling", rxx = .74, vy ="Machiavellianism", ryy = .77)
g4 <- esc_beta(beta = 0.06, sdy = 2.98, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 16. Studiul ref_245 - CODARE ####
## March E., Grieve R., Wagstaffc D., Slocumd A. (2020) - 
## Exploring anger as a moderator of narcissism and antisocial behaviour on tinder
ref = "ref_245"; dem <- ret.sample(ref = ref)
r = .10;  rxx <- NA; ryy = .75; x = "Harassment"; y = "Narcissism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .22;  rxx <- NA; ryy = .58; x = "Harassment"; y = "Narcissism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .12;  rxx <- NA; ryy = .75; x = "Aggression"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .13;  rxx <- NA; ryy = .58; x = "Aggression"; y = "Narcissism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))

gi1 <- info.g(ref, vx = "Aggression", rxx = NA, vy ="Narcissism", ryy = .75)
g1 <- esc_beta(beta = .22, sdy = 0.76, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Aggression", rxx = NA, vy ="Narcissism", ryy = .58)
g2 <- esc_beta(beta = .09, sdy = 0.76, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Harassment", rxx = NA, vy ="Narcissism", ryy = .75)
g3 <- esc_beta(beta = -.04, sdy = 0.40, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Harassment", rxx = NA, vy ="Narcissism", ryy = .58)
g4 <- esc_beta(beta = .29, sdy = 0.40, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 17. Studiul ref_249 - CODARE ####
## March E., Steele G. (2020) 
## High Esteem and Hurting Others Online: Trait Sadism Moderates the Relationship Between Self-Esteem and Internet Trolling
ref = "ref_249"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Trolling", rxx = .81, vy ="Psychopathy", ryy = .72)
g1 <- esc_beta(beta = .29, sdy = 4.92, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Trolling", rxx = .81, vy ="Sadism", ryy = .84)
g2 <- esc_beta(beta = .45, sdy = 4.92, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx),
              c(gi1$rxx, gi2$rxx),
              c(gi1$vy, gi2$vy),
              c(gi1$ryy, gi2$ryy),
              c(rep(dem$year, 2)), c(rep(dem$m.age, 2)), c(rep(dem$prop.m, 2)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp)

r = .56;  rxx <- .81; ryy = .72; x = "Trolling"; y = "Psychopathy"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .59;  rxx <- .81; ryy = .84; x = "Trolling"; y = "Sadism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), temp, dem)

## 18. Studiul ref_254 - CODARE ####
## Masui K. (2019) 
## Loneliness moderates the relationship between Dark Tetrad personality traits and internet trolling
ref = "ref_254"; dem <- ret.sample(ref = ref)
r = .37;  rxx <- .80; ryy = .85; x = "Trolling"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .30;  rxx <- .80; ryy = .60; x = "Trolling"; y = "Psychopathy"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .20;  rxx <- .80; ryy = .82; x = "Trolling"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .39;  rxx <- .80; ryy = .79; x = "Trolling"; y = "Sadism"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m))

gi1 <- info.g(ref, vx = "Trolling", rxx = .60, vy ="Psychopathy", ryy = .84)
g1 <- esc_beta(beta = .11, sdy = 0.49, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Trolling", rxx = .60, vy ="Narcissism", ryy = .82)
g2 <- esc_beta(beta = -.05, sdy = 0.49, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Trolling", rxx = .60, vy ="Sadism", ryy = .79)
g3 <- esc_beta(beta = .39, sdy = 0.49, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Trolling", rxx = .60, vy ="Machiavellianism", ryy = .85)
g4 <- esc_beta(beta = .12, sdy = 0.49, grp1n = dem$male, grp2n = dem$female, 
               es.type = "g", study = dem$author)

### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy),
              c(rep(dem$year, 4)), c(rep(dem$m.age, 4)), c(rep(dem$prop.m, 4)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## *** Studiul ref_289 - CODARE ####
## Petit J., Carcioppolo J. (2020) 
## Associations between the Dark Triad and online communication behavior: A brief report of preliminary findings
eliminate <- eliminate + 1
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
## 19. Studiul ref_290 - CODARE ####
## Pineda D., Galán M., Martínez-Martínez A.,Campagne D.,Piqueras J.A. (2021) 
## Same Personality, New Ways to Abuse:  How Dark Tetrad Personalities Are Connected With Cyber Intimate Partner Violence
ref = "ref_290"; dem <- ret.sample(ref = ref)
r = .143;  rxx <- .93; ryy = .68; x = "Control victimization"; y = "Sadism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .045;  rxx <- .93; ryy = .80; x = "Control victimization"; y = "Machiavellianism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .085;  rxx <- .93; ryy = .65; x = "Control victimization"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .096;  rxx <- .93; ryy = .72; x = "Control victimization"; y = "Psychopathy"
g4 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .211;  rxx <- .84; ryy = .68; x = "Direct aggression victimization"; y = "Sadism"
g5 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .072;  rxx <- .84; ryy = .80; x = "Direct aggression victimization"; y = "Machiavellianism"
g6 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .113;  rxx <- .84; ryy = .65; x = "Direct aggression victimization"; y = "Narcissism"
g7 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .137;  rxx <- .84; ryy = .72; x = "Direct aggression victimization"; y = "Psychopathy"
g8 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .132;  rxx <- .86; ryy = .68; x = "Control perpetration"; y = "Sadism"
g9 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .122;  rxx <- .86; ryy = .80; x = "Control perpetration"; y = "Machiavellianism"
g10 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .124;  rxx <- .86; ryy = .65; x = "Control perpetration"; y = "Narcissism"
g11 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .141;  rxx <- .86; ryy = .72; x = "Control perpetration"; y = "Psychopathy"
g12 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .265;  rxx <- .73; ryy = .68; x = "Direct aggression perpetration"; y = "Sadism"
g13 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .145;  rxx <- .73; ryy = .80; x = "Direct aggression perpetration"; y = "Machiavellianism"
g14 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .144;  rxx <- .73; ryy = .65; x = "Direct aggression perpetration"; y = "Narcissism"
g15 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .234;  rxx <- .73; ryy = .72; x = "Direct aggression perpetration"; y = "Psychopathy"
g16 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g4$study, g4$es, g4$w, dem$n, g4$se, g4$var, g4$ci[1], g4$ci[2], "g",
                  g4$vx, g4$rxx, g4$vy, g4$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g5$study, g5$es, g5$w, dem$n, g5$se, g5$var, g5$ci[1], g5$ci[2], "g",
                  g5$vx, g5$rxx, g5$vy, g5$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g6$study, g6$es, g6$w, dem$n, g6$se, g6$var, g6$ci[1], g6$ci[2], "g",
                  g6$vx, g6$rxx, g6$vy, g6$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g7$study, g7$es, g7$w, dem$n, g7$se, g7$var, g7$ci[1], g7$ci[2], "g",
                  g7$vx, g7$rxx, g7$vy, g7$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g8$study, g8$es, g8$w, dem$n, g8$se, g8$var, g8$ci[1], g8$ci[2], "g",
                  g8$vx, g8$rxx, g8$vy, g8$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g9$study, g9$es, g9$w, dem$n, g9$se, g9$var, g9$ci[1], g9$ci[2], "g",
                  g9$vx, g9$rxx, g9$vy, g9$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g10$study, g10$es, g10$w, dem$n, g10$se, g10$var, g10$ci[1], g10$ci[2], "g",
                  g10$vx, g10$rxx, g10$vy, g10$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g11$study, g11$es, g11$w, dem$n, g11$se, g11$var, g11$ci[1], g11$ci[2], "g",
                  g11$vx, g11$rxx, g11$vy, g11$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g12$study, g12$es, g12$w, dem$n, g12$se, g12$var, g12$ci[1], g12$ci[2], "g",
                  g12$vx, g12$rxx, g12$vy, g12$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g13$study, g13$es, g13$w, dem$n, g13$se, g13$var, g13$ci[1], g13$ci[2], "g",
                  g13$vx, g13$rxx, g13$vy, g13$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g14$study, g14$es, g14$w, dem$n, g14$se, g14$var, g14$ci[1], g14$ci[2], "g",
                  g14$vx, g14$rxx, g14$vy, g14$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g15$study, g15$es, g15$w, dem$n, g15$se, g15$var, g15$ci[1], g15$ci[2], "g",
                  g15$vx, g15$rxx, g15$vy, g15$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g16$study, g16$es, g16$w, dem$n, g16$se, g16$var, g16$ci[1], g16$ci[2], "g",
                  g16$vx, g16$rxx, g16$vy, g16$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)


## 20. Studiul ref_311 - CODARE ####
## Sest N., March E. (2017) 
## Constructing the cyber-troll: Psychopathy, sadism, and empathy
ref = "ref_311"; dem <- ret.sample(ref = ref)
r = .62;  rxx <- .84; ryy = .75; x = "Trolling"; y = "Psychopathy"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .62;  rxx <- .84; ryy = .87; x = "Trolling"; y = "Sadism"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m))
rm(list = ls(pattern = "^g"), dem)

## *** Studiul ref_323 - CODARE ####
## Sorokowski P., Kowal M., Zdybek P., Oleszkiewicz1 A. (2020)
## Are Online Haters Psychopaths? Psychological Predictors of Online Hating Behavior
# Eliminat deoarece nu contine masuri ale cyberbullying-ului
eliminate <- eliminate + 1
## 21. Studiul ref_360 - CODARE ####
## Yoon Lee S., Yao M.Z., Yi-Fan Su L. (2021)
## Expressing unpopular opinion or trolling: Can dark personalities differentiate them?
ref = "ref_360"; dem <- ret.sample(ref = ref)
gi1 <- info.g(ref, vx = "Vocal minority vs Troll", rxx = NA, vy ="Machiavellianism", ryy = .90)
g1 <- esc_mean_se(grp1m = 6.22, grp1se = 0.20, grp1n = 95,
                  grp2m = 6.08, grp2se = 0.29, grp2n = 46,
                  es.type = "g", study = dem$author)
gi2 <- info.g(ref, vx = "Social conformers vs Troll", rxx = NA, vy ="Machiavellianism", ryy = .90)
g2 <- esc_mean_se(grp1m = 5.26, grp1se = 0.18, grp1n = 117,
                  grp2m = 6.08, grp2se = 0.29, grp2n = 46,
                  es.type = "g", study = dem$author)
gi3 <- info.g(ref, vx = "Normative majority vs Troll", rxx = NA, vy ="Machiavellianism", ryy = .90)
g3 <- esc_mean_se(grp1m = 5.50, grp1se = 0.13, grp1n = 234,
                  grp2m = 6.08, grp2se = 0.29, grp2n = 46,
                  es.type = "g", study = dem$author)
gi4 <- info.g(ref, vx = "Silent minority vs Troll", rxx = NA, vy ="Machiavellianism", ryy = .90)
g4 <- esc_mean_se(grp1m = 5.50, grp1se = 0.23, grp1n = 75,
                  grp2m = 6.08, grp2se = 0.29, grp2n = 46,
                  es.type = "g", study = dem$author)
gi5 <- info.g(ref, vx = "Vocal minority vs Troll", rxx = NA, vy ="Psychopathy", ryy = .87)
g5 <- esc_mean_se(grp1m = 4.79, grp1se = 0.19, grp1n = 95,
                  grp2m = 5.03, grp2se = 0.27, grp2n = 46,
                  es.type = "g", study = dem$author)
gi6 <- info.g(ref, vx = "Normative majority vs Troll", rxx = NA, vy ="Psychopathy", ryy = .87)
g6 <- esc_mean_se(grp1m = 3.90, grp1se = 0.12, grp1n = 235,
                  grp2m = 5.03, grp2se = 0.27, grp2n = 46,
                  es.type = "g", study = dem$author)
gi7 <- info.g(ref, vx = "Social conformers vs Troll", rxx = NA, vy ="Psychopathy", ryy = .87)
g7 <- esc_mean_se(grp1m = 3.71, grp1se = 0.17, grp1n = 117,
                  grp2m = 5.03, grp2se = 0.27, grp2n = 46,
                  es.type = "g", study = dem$author)
gi8 <- info.g(ref, vx = "Silent minority vs Troll", rxx = NA, vy ="Psychopathy", ryy = .87)
g8 <- esc_mean_se(grp1m = 4.00, grp1se = 0.21, grp1n = 75,
                  grp2m = 5.03, grp2se = 0.27, grp2n = 46,
                  es.type = "g", study = dem$author)
gi9 <- info.g(ref, vx = "Vocal minority vs Troll", rxx = NA, vy ="Sadism", ryy = .96)
g9 <- esc_mean_se(grp1m = 4.00, grp1se = 0.24, grp1n = 95,
                  grp2m = 4.86, grp2se = 0.34, grp2n = 46,
                  es.type = "g", study = dem$author)
gi10 <- info.g(ref, vx = "Normative majority vs Troll", rxx = NA, vy ="Sadism", ryy = .96)
g10 <- esc_mean_se(grp1m = 4.08, grp1se = 0.15, grp1n = 235,
                   grp2m = 4.86, grp2se = 0.34, grp2n = 46,
                   es.type = "g", study = dem$author)
gi11 <- info.g(ref, vx = "Silent minority vs Troll", rxx = NA, vy ="Sadism", ryy = .96)
g11 <- esc_mean_se(grp1m = 3.37, grp1se = 0.27, grp1n = 75,
                   grp2m = 4.86, grp2se = 0.34, grp2n = 46,
                   es.type = "g", study = dem$author)
gi12 <- info.g(ref, vx = "Social conformers vs Troll", rxx = NA, vy ="Sadism", ryy = .96)
g12 <- esc_mean_se(grp1m = 2.87, grp1se = 0.21, grp1n = 117,
                   grp2m = 4.86, grp2se = 0.34, grp2n = 46,
                   es.type = "g", study = dem$author)
### Combinare efecte si construirea bazei de date a efectelor
temp <- combine_esc(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12)
temp <- cbind(temp,
              c(gi1$vx, gi2$vx, gi3$vx, gi4$vx, gi5$vx, gi6$vx, gi7$vx, gi8$vx, gi9$vx, gi10$vx, gi11$vx, gi12$vx),
              c(gi1$rxx, gi2$rxx, gi3$rxx, gi4$rxx, gi5$rxx, gi6$rxx, gi7$rxx, gi8$rxx, gi9$rxx, gi10$rxx, gi11$rxx, gi12$rxx),
              c(gi1$vy, gi2$vy, gi3$vy, gi4$vy, gi5$vy, gi6$vy, gi7$vy, gi8$vy, gi9$vy, gi10$vy, gi11$vy, gi12$vy),
              c(gi1$ryy, gi2$ryy, gi3$ryy, gi4$ryy, gi5$ryy, gi6$ryy, gi7$ryy, gi8$ryy, gi9$ryy, gi10$ryy, gi11$ryy, gi12$ryy),
              c(rep(dem$year, 12)), c(rep(dem$m.age, 12)), c(rep(dem$prop.m, 12)))
colnames(temp) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                    "X", "Rxx", "Y", "Ryy", "year", "age", "male")
temp$es <- -temp$es # Corectie de pozitionare a grupului de trolling al doilea
efecte <- rbind(efecte, temp); rm(list = ls(pattern = "^g"), temp, dem)

## 22. Studiul ref_366 - CODARE ####
## Zhanga H., Zhaob H. (2020) 
## Dark personality traits and cyber aggression in adolescents: A moderated mediation analysis of belief in virtuous humanity and self-control
ref = "ref_366"; dem <- ret.sample(ref = ref)
r = .32;  rxx <- .94; ryy = .84; x = "Cyberaggression"; y = "Machiavellianism"
g1 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .37;  rxx <- .94; ryy = .63; x = "Cyberaggression"; y = "Psychopathy"
g2 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)
r = .11;  rxx <- .94; ryy = .81; x = "Cyberaggression"; y = "Narcissism"
g3 <- r2g(ref = ref, r = r, n = dem$n, rxx = rxx, ryy = ryy, vx=x, vy=y)

### Combinare efecte si construirea bazei de date a efectelor
efecte <- rbind(efecte,
                c(g1$study, g1$es, g1$w, dem$n, g1$se, g1$var, g1$ci[1], g1$ci[2], "g",
                  g1$vx, g1$rxx, g1$vy, g1$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g2$study, g2$es, g2$w, dem$n, g2$se, g2$var, g2$ci[1], g2$ci[2], "g",
                  g2$vx, g2$rxx, g2$vy, g2$ryy, dem$year, dem$m.age, dem$prop.m),
                c(g3$study, g3$es, g3$w, dem$n, g3$se, g3$var, g3$ci[1], g3$ci[2], "g",
                  g3$vx, g3$rxx, g3$vy, g3$ryy, dem$year, dem$m.age, dem$prop.m))

# Salvarea finala a efectelor si actualizarea PRISMA ####
PRISMA.template$n[which(PRISMA.template$data == "dbr_excluded")] <- eliminate
PRISMA.template$n[which(PRISMA.template$data == "new_studies")] <- as.numeric(
  PRISMA.template$n[which(PRISMA.template$data == "dbr_assessed")]) - eliminate
# Desenarea si afisarea diagramei PRISMA
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
save(efecte, file = "Efecte.RData"); save(PRISMA.template, file = "PRISMA.Rdata")

# Puridicarea si conversia studiilor, salvarea datelor ####
efecte$es <- as.numeric(efecte$es); efecte$weight <- as.numeric(efecte$weight)
efecte$sample.size <- as.numeric(efecte$sample.size); efecte$se <- as.numeric(efecte$se)
efecte$var <- as.numeric(efecte$var); efecte$ci.lo <- as.numeric(efecte$ci.lo)
efecte$ci.hi <- as.numeric(efecte$ci.hi); efecte$Rxx <- as.numeric(efecte$Rxx)
efecte$Ryy <- as.numeric(efecte$Ryy); efecte$year <- as.numeric(efecte$year)
efecte$age <- as.numeric(efecte$age); efecte$male <- as.numeric(efecte$male)
efecte$X <- factor(efecte$X); efecte$Y <- factor(efecte$Y)

range(efecte$es)
ds.global <- efecte %>%
  dplyr::filter(!is.infinite(es)) %>%
  dplyr::filter(es < 1000)
range(ds.global$es); shapiro.test(ds.global$es)
save(ds.global, file = "Finala.RData")

rm(list = ls(pattern = "^g"), dem, barbati, femei, r, ref, rxx, ryy, x, y,  
   info.g, r2g, ret.sample, bd.meta, tabel.surse, eliminate)
