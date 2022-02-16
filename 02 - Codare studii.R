# Incarcarea setulului de date si a sablonului PRISMA
library(dplyr); load("Centralizator.Rdata")

# Construirea bazei de date cu articolele codate ####
bd.meta <- tabel.surse %>%
  dplyr::select(label, author, year) %>%
  dplyr::filter(
    label != "ref_077" &
      label != "ref_273" &
      label != "ref_031" &
      label != "ref_034")

## 1. Studiul ref_004 - CODARE ####
## H. M. Baughman, S. Dearing, E. Giammarco, P. A. Vernon (2012) - Relationships between bullying behaviours and the Dark Triad: A study with adults
# Caracteristici ale participantilor
bd.meta$author[1] <- "Baughman et al. (2012)"
bd.meta$n <- NA; bd.meta$n[1] <- 657
bd.meta$p.male <- NA; bd.meta$p.male[1] <- round(203 / 657, 4)
bd.meta$m.age <- NA; bd.meta$m.age[1] <- 23.1
bd.meta$pop <- NA; bd.meta$pop[1] <- "Adulti"
bd.meta$prize <- NA; bd.meta$prize[1] <- T
bd.meta$country <- NA; bd.meta$country[1] <- "Canada"
# Caracteristici de design
bd.meta$design <- NA; bd.meta$design[1] <- "Descriptiv"
bd.meta$subgroup<- NA; bd.meta$subgroup[1] <- "Gen"
bd.meta$sample<- NA; bd.meta$sample[1] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure <- NA; bd.meta$measure[1] <- "Standardizata"
bd.meta$method <- NA; bd.meta$method[1] <- "Chestionar"

## 2. Studiul ref_006 - CODARE ####
## O. Bogolyubova, P. Panicheva, R. Tikhonov, V. Ivanov, Y. Ledovaya (2018) - Dark personalities on Facebook: Harmful online behaviors and language
# Caracteristici ale participantilor
bd.meta$author[2] <- "Bogolyubova et al. (2018)"
bd.meta$n[2] <- 6724
bd.meta$p.male[2] <- round(1487 / 6724, 4)
bd.meta$m.age[2] <- 44.96
bd.meta$pop[2] <- "Adulti"
bd.meta$prize[2] <- F
bd.meta$country[2] <- "Rusia"
# Caracteristici de design
bd.meta$design[2] <- "Descriptiv"
bd.meta$subgroup[2] <- "Gen"
bd.meta$sample[2] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[2] <- "Standardizata"
bd.meta$method[2] <- "Chestionar"

## 3. Studiul ref_012 - CODARE ####
## N. Craker, E. March (2016) - The dark side of Facebook: The Dark Tetrad, negative social potency, and trolling behaviours
# Caracteristici ale participantilor
bd.meta$author[3] <- "Craker et al. (2016)"
bd.meta$n[3] <- 396
bd.meta$p.male[3] <- round(94 / 396, 4)
bd.meta$m.age[3] <- 34.41
bd.meta$pop[3] <- "Adulti"
bd.meta$prize[3] <- F
bd.meta$country[3] <- "Australia"
# Caracteristici de design
bd.meta$design[3] <- "Descriptiv"
bd.meta$subgroup[3] <- "Gen"
bd.meta$sample[3] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[3] <- "Standardizata"
bd.meta$method[3] <- "Chestionar"

## 4. Studiul ref_020 - CODARE ####
## A. K. Goodboy, M. M. Martin (2015) - The personality profile of a cyberbully: Examining the Dark Triad
# Caracteristici ale participantilor
bd.meta$author[4] <- "Goodboy et al. (2015)"
bd.meta$n[4] <- 227
bd.meta$p.male[4] <- round(104 / 227, 4)
bd.meta$m.age[4] <- 20.97
bd.meta$pop[4] <- "Studenti"
bd.meta$prize[4] <- F
bd.meta$country[4] <- "USA"
# Caracteristici de design
bd.meta$design[4] <- "Descriptiv"
bd.meta$subgroup[4] <- NA
bd.meta$sample[4] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[4] <- "Standardizata"
bd.meta$method[4] <- "Chestionar"

## 5. Studiul ref_022 - CODARE ####
## C. J. Hand, Graham G. Scott b, Zara P. Brodie b, Xilei Ye c, Sara C. Sereno (2021) - Tweet valence, volume of abuse, and observers’ dark tetrad personality factors influence victim-blaming and the perceived severity of twitter cyberabuse
# Caracteristici ale participantilor
bd.meta$author[5] <- "Hand et al. (2021)"
bd.meta$n[5] <- 125
bd.meta$p.male[5] <- round(39 / 125, 4)
bd.meta$m.age[5] <- 25.06
bd.meta$pop[5] <- "Studenti"
bd.meta$prize[5] <- F
bd.meta$country[5] <- "International"
# Caracteristici de design
bd.meta$design[5] <- "Factorial"
bd.meta$subgroup[5] <- "Valence, Abuse volume"
bd.meta$sample[5] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[5] <- "Standardizata"
bd.meta$method[5] <- "Chestionar"

## 6. Studiul ref_029 - CODARE ####
## Kurek, A., Jose, P. E., & Stuart, J. (2019). ‘I did it for the LULZ’: How the dark personality predicts online disinhibition and aggressive online behavior in adolescence.
# Caracteristici ale participantilor
bd.meta$author[6] <- "Kurek et al. (2019)"
bd.meta$n[6] <- 709
bd.meta$p.male[6] <- 0.495
bd.meta$m.age[6] <- 15.56
bd.meta$pop[6] <- "Adolescenti"
bd.meta$prize[6] <- F
bd.meta$country[6] <- "Noua Zeelanda"
# Caracteristici de design
bd.meta$design[6] <- "Factorial, Cale"
bd.meta$subgroup[6] <- "Gen, Grup varsta"
bd.meta$sample[6] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[6] <- "Standardizata"
bd.meta$method[6] <- "Chestionar"

## 7. Studiul ref_031 - SE ELIMINA
## M. Lyons, N. Gillies, G. Brewer (2019) - Dark Triad traits, Facebook intensity, and intrasexual competition
## 7. Studiul ref_034 - SE ELIMINA - Capitol din carte
## March, Evita (2022) - 	20 - Psychopathy: Cybercrime and Cyber Abuse
## 7. Studiul ref_045 - CODARE ####
## Pabian, S., De Backer, C. J. S., & Vandebosch, H. (2015) - Dark Triad personality traits and adolescent cyber-aggression
# Caracteristici ale participantilor
bd.meta$author[7] <- "Pabian et al. (2015)"
bd.meta$n[7] <- 324
bd.meta$p.male[7] <- 1 - 0.630
bd.meta$m.age[7] <- 16.05
bd.meta$pop[7] <- "Adolescenti"
bd.meta$prize[7] <- F
bd.meta$country[7] <- "Belgia"
# Caracteristici de design
bd.meta$design[7] <- "SEM"
bd.meta$subgroup[7] <- NA
bd.meta$sample[7] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[7] <- "Standardizata"
bd.meta$method[7] <- "Chestionar"

## 8. Studiul ref_053 - CODARE ####
## Stiff C. (2019) - The Dark Triad and Facebook surveillance: How Machiavellianism, psychopathy, but not narcissism predict using Facebook to spy on others
# Caracteristici ale participantilor
bd.meta$author[8] <- "Stiff et al. (2019)"
bd.meta$n[8] <- 259
bd.meta$p.male[8] <- 0.416
bd.meta$m.age[8] <- 20.49
bd.meta$pop[8] <- "Adulti"
bd.meta$prize[8] <- T
bd.meta$country[8] <- "Anglia"
# Caracteristici de design
bd.meta$design[8] <- "SEM"
bd.meta$subgroup[8] <- "Gen"
bd.meta$sample[8] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[8] <- "Standardizata"
bd.meta$method[8] <- "Chestionar"

## 9. Studiul ref_059.a - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018) - Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
# Caracteristici ale participantilor
bd.meta$author[9] <- "Buckels et al. (2019)"
bd.meta$n[9] <- 345
bd.meta$p.male[9] <- 0.482
bd.meta$m.age[9] <- 34.4
bd.meta$pop[9] <- "Adulti"
bd.meta$prize[9] <- T
bd.meta$country[9] <- "USA"
# Caracteristici de design
bd.meta$design[9] <- "SEM"
bd.meta$subgroup[9] <- NA 
bd.meta$sample[9] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[9] <- "Standardizata"
bd.meta$method[9] <- "Chestionar"
temp <- bd.meta[1:9,]

## 10. Studiul ref_059.b - CODARE ####
## Buckels E.E. Trapnell P. D., Andjelovic T, Paulhus D.L. (2018) - Internet Trolling and Everyday Sadism: Parallel Effects on Pain Perception and Moral Judgment
# Caracteristici ale participantilor
temp <- rbind(temp, temp[9,])
temp$author[10] <- "Buckels et al. (2019)"
temp$label[10] <- "ref_059.b"
temp$n[10] <- 1134 + 236
temp$p.male[10] <- 0.287
temp$m.age[10] <- 18.50
temp$pop[10] <- "Tineri"
temp$prize[10] <- T
temp$country[10] <- "USA"
# Caracteristici de design
temp$design[10] <- "Cvasiexperiment"
temp$subgroup[10] <- NA
temp$sample[10] <- "Convenienta"
# Caracteristici de masurare
temp$measure[10] <- "Standardizata"
temp$method[10] <- "Chestionar"
bd.meta <- rbind(temp, bd.meta[10:28,]); rm(temp)

## 11. Studiul ref_067 - CODARE ####
## Brown W.M., Hazraty S., Palasinski M. (2019) - Examining the Dark Tetrad and Its Links to Cyberbullying
# Caracteristici ale participantilor
bd.meta$author[11] <- "Brown et al. (2019)"
bd.meta$n[11] <- 1464
bd.meta$p.male[11] <- 0.539
bd.meta$m.age[11] <- 22.48
bd.meta$pop[11] <- "Generala"
bd.meta$prize[11] <- F
bd.meta$country[11] <- "Anglia"
# Caracteristici de design
bd.meta$design[11] <- "Regresii"
bd.meta$subgroup[11] <- "Rasa"
bd.meta$sample[11] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[11] <- "Standardizata"
bd.meta$method[11] <- "Chestionar"

## 12. Studiul ref_068 - CODARE ####
## Kircaburuna K., Jonasonb P.K., Griffithsc M.D. (2018) - The Dark Tetrad traits and problematic social media use: The mediating role of cyberbullying and cyberstalking
# Caracteristici ale participantilor
bd.meta$author[12] <- "Kircaburuna et al. (2018)"
bd.meta$n[12] <- 761
bd.meta$p.male[12] <- 0.360
bd.meta$m.age[12] <- 20.70
bd.meta$pop[12] <- "Studenti"
bd.meta$prize[12] <- F
bd.meta$country[12] <- "Turcia"
# Caracteristici de design
bd.meta$design[12] <- "SEM"
bd.meta$subgroup[12] <- NA
bd.meta$sample[12] <- "Lot cercetare"
# Caracteristici de masurare
bd.meta$measure[12] <- "Standardizata"
bd.meta$method[12] <- "Chestionar"

## 13. Studiul ref_069 - CODARE ####
## Gylfason H.F., Sveinsdottir A.H., Vésteinsdóttir V., Sigurvinsdottir R. (2021) - Haters Gonna Hate, Trolls Gonna Troll: The Personality Profile of a Facebook Troll
# Caracteristici ale participantilor
bd.meta$author[13] <- "Gylfason et al. (2021)"
bd.meta$n[13] <- 139
bd.meta$p.male[13] <- 0.122
bd.meta$m.age[13] <- NA
bd.meta$pop[13] <- "Generala"
bd.meta$prize[13] <- F
bd.meta$country[13] <- "Islanda"
# Caracteristici de design
bd.meta$design[13] <- "SEM"
bd.meta$subgroup[13] <- NA
bd.meta$sample[13] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[13] <- "Standardizata"
bd.meta$method[13] <- "Chestionar"

## 14. Studiul ref_150 - CODARE ####
## Duncan Z., March E. (2019) - Examining the Dark Tetrad and Its Links to Cyberbullying
# Caracteristici ale participantilor
bd.meta$author[14] <- "Duncan et al. (2019)"
bd.meta$n[14] <- 587
bd.meta$p.male[14] <- 0.210 
bd.meta$m.age[14] <- 23.75
bd.meta$pop[14] <- "Generala"
bd.meta$prize[14] <- F
bd.meta$country[14] <- "Australia"
# Caracteristici de design
bd.meta$design[14] <- "Regresii"
bd.meta$subgroup[14] <- F
bd.meta$sample[14] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[14] <- "Standardizata"
bd.meta$method[14] <- "Chestionar"

## 15. Studiul ref_159 - CODARE ####
## Ferenczi N., Marshall T.C., Bejanyan K. (2016) - Are sex differences in antisocial and prosocial Facebook use explained by narcissism and relational self-construal?
# Caracteristici ale participantilor
bd.meta$author[15] <- "Ferenczi et al. (2016)"
bd.meta$n[15] <- 573
bd.meta$p.male[15] <- 1 - 0.59
bd.meta$m.age[15] <- 30.79
bd.meta$pop[15] <- "Generala"
bd.meta$prize[15] <- T
bd.meta$country[15] <- "USA"
# Caracteristici de design
bd.meta$design[15] <- "Regresii"
bd.meta$subgroup[15] <- "Gen"
bd.meta$sample[15] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[15] <- "Standardizata"
bd.meta$method[15] <- "Chestionar"

## 16. Studiul ref_195 - CODARE ####
## Hussain Z., Wegmann E., Grifths M.D. (2021) - The association between problematic  social networking site use, dark triad traits,and emotion dysregulation
# Caracteristici ale participantilor
bd.meta$author[16] <- "Hussain et al. (2021)"
bd.meta$n[16] <- 555
bd.meta$p.male[16] <- 0.524
bd.meta$m.age[16] <- 33.32
bd.meta$pop[16] <- "Generala"
bd.meta$prize[16] <- F
bd.meta$country[16] <- "Anglia" 
# Caracteristici de design
bd.meta$design[16] <- "SEM"
bd.meta$subgroup[16] <- NA
bd.meta$sample[16] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[16] <- "Standardizata"
bd.meta$method[16] <- "Chestionar"

## 17. Studiul ref_216 - CODARE ####
## Kircaburuna K., Jonasonb P.K., Griffithsc M.D. (2018) - The Dark Tetrad traits and problematic online gaming: The mediating role of online gaming motives and moderating role of game types
# Caracteristici ale participantilor
bd.meta$author[17] <- "Kircaburuna et al. (2018)"
bd.meta$n[17] <- 421
bd.meta$p.male[17] <- 1.000
bd.meta$m.age[17] <- 20.82
bd.meta$pop[17] <- "Generala"
bd.meta$prize[17] <- F
bd.meta$country[17] <- "Turcia"
# Caracteristici de design
bd.meta$design[17] <- "SEM"
bd.meta$subgroup[17] <- NA
bd.meta$sample[17] <- "Lot de cercetare"
# Caracteristici de masurare
bd.meta$measure[17] <- "Standardizata"
bd.meta$method[17] <- "Chestionar"

## 18. Studiul ref_221 - CODARE ####
## Kircaburun K., Jonason P., Griffiths M.D., Aslanargun E. (2019) - Examining the Dark Tetrad and Its Links to Cyberbullying
# Caracteristici ale participantilor
bd.meta$author[18] <- "Kircaburun et al. (2019)"
bd.meta$n[18] <- 280 + 492
bd.meta$p.male[18] <- round(280 / (280 + 492), 4)
bd.meta$m.age[18] <- 20.72
bd.meta$pop[18] <- "Studenti"
bd.meta$prize[18] <- F
bd.meta$country[18] <- "Turcia"
# Caracteristici de design
bd.meta$design[18] <- "SEM"
bd.meta$subgroup[18] <- "Gen"
bd.meta$sample[18] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[18] <- "Standardizata"
bd.meta$method[18] <-"Chestionar"

## 19. Studiul ref_224 - CODARE ####
## Kircaburun K., Jonason P., Griffiths M.D. (2018) - The dark side of internet: Preliminary evidence for the associations of dark personality traits with specific online activities and problematic internet use
# Caracteristici ale participantilor
bd.meta$author[19] <- "Kircaburun et al. (2018)"
bd.meta$n[19] <- 772
bd.meta$p.male[19] <- 1 - 0.64 
bd.meta$m.age[19] <- 20.72
bd.meta$pop[19] <- "Studenti"
bd.meta$prize[19] <- F
bd.meta$country[19] <- "Turcia"
# Caracteristici de design
bd.meta$design[19] <- "SEM"
bd.meta$subgroup[19] <- "Gen"
bd.meta$sample[19] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[19] <- "Standardizata"
bd.meta$method[19] <- "Chestionar"

## 20. Studiul ref_244 - CODARE ####
## March E., Grieve R., Marrington J., Jonason P.K. (2017) - Trolling on Tinder (and other dating apps): Examining the role of the Dark Tetrad and impulsivity
# Caracteristici ale participantilor
bd.meta$author[20] <- "March et al. (2017)"
bd.meta$n[20] <- 357
bd.meta$p.male[20] <- 0.29 
bd.meta$m.age[20] <- 22.5
bd.meta$pop[20] <- "Generala"
bd.meta$prize[20] <- F
bd.meta$country[20] <- "Australia"
# Caracteristici de design
bd.meta$design[20] <- "Regresii"
bd.meta$subgroup[20] <- "Gen"
bd.meta$sample[20] <-  "Convenienta"
# Caracteristici de masurare
bd.meta$measure[20] <- "Standardizata"
bd.meta$method[20] <- "Chestionar"

## 21. Studiul ref_245 - CODARE ####
## Marcha E., Grieve R., Wagstaffc D., Slocumd A. (2020) - Exploring anger as a moderator of narcissism and antisocial behaviour on tinder
# Caracteristici ale participantilor
bd.meta$author[21] <- "March et al. (2020)"
bd.meta$n[21] <- 1001
bd.meta$p.male[21] <- 0.463 
bd.meta$m.age[21] <- 22.42
bd.meta$pop[21] <- "Generala"
bd.meta$prize[21] <- F
bd.meta$country[21] <- "Australia"
# Caracteristici de design
bd.meta$design[21] <- "Regresii"
bd.meta$subgroup[21] <- "Gen"
bd.meta$sample[21] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[21] <- "Standardizata"
bd.meta$method[21] <- "Chestionar"

## 22. Studiul ref_249 - CODARE ####
## March E., Steele G. (2020) - High Esteem and Hurting Others Online: Trait Sadism Moderates the Relationship Between Self-Esteem and Internet Trolling
# Caracteristici ale participantilor
bd.meta$author[22] <- "March and Steele (2020)"
bd.meta$n[22] <- 400
bd.meta$p.male[22] <- 0.325 
bd.meta$m.age[22] <- 24.97
bd.meta$pop[22] <- "Generala"
bd.meta$prize[22] <- F
bd.meta$country[22] <- "Australia"
# Caracteristici de design
bd.meta$design[22] <- "Regresii"
bd.meta$subgroup[22] <- "Gen"
bd.meta$sample[22] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[22] <- "Standardizata"
bd.meta$method[22] <- "Chestionar"

## 23. Studiul ref_254 - CODARE ####
## Masui K. (2019) - Loneliness moderates the relationship between Dark Tetrad personality traits and internet trolling
# Caracteristici ale participantilor
bd.meta$author[23] <- "Masui (2019)"
bd.meta$n[23] <- 513
bd.meta$p.male[23] <- 1 - 0.511
bd.meta$m.age[23] <- 46.8
bd.meta$pop[23] <- "Generala"
bd.meta$prize[23] <- T
bd.meta$country[23] <- "Japonia"
# Caracteristici de design
bd.meta$design[23] <- "Regresii"
bd.meta$subgroup[23] <- "Gen"
bd.meta$sample[23] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[23] <- "Standardizata"
bd.meta$method[23] <- "Chestionar"

## 24. Studiul ref_289 - CODARE ####
## Petit J., Carcioppolo J. (2020) - Associations between the Dark Triad and online communication behavior: A brief report of preliminary findings
# Caracteristici ale participantilor
bd.meta$author[24] <- "Petit et al. (2020)"
bd.meta$n[24] <- 147
bd.meta$p.male[24] <- 0.20
bd.meta$m.age[24] <- NA
bd.meta$pop[24] <- "Studenti"
bd.meta$prize[24] <- T
bd.meta$country[24] <- NA
# Caracteristici de design
bd.meta$design[24] <- "Regresii"
bd.meta$subgroup[24] <- "Gen"
bd.meta$sample[24] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[24] <- "Standardizata"
bd.meta$method[24] <- "Chestionar"

## 25. Studiul ref_290 - CODARE ####
## Pineda D., Galán M., Martínez-Martínez A.,Campagne D.,Piqueras J.A. (2021) - Same Personality, New Ways to Abuse:  How Dark Tetrad Personalities Are Connected With Cyber Intimate Partner Violence
# Caracteristici ale participantilor
bd.meta$author[25] <- "Pineda et al. (2019)"
bd.meta$n[25] <- 1189
bd.meta$p.male[25] <- round(261 / 1189, 4)
bd.meta$m.age[25] <- 29.36
bd.meta$pop[25] <- "Generala"
bd.meta$prize[25] <- F
bd.meta$country[25] <- 
  # Caracteristici de design
  bd.meta$design[25] <- "SEM"
bd.meta$subgroup[25] <- "Gen"
bd.meta$sample[25] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[25] <- "Standardizata"
bd.meta$method[25] <- "Chestionar"

## 26. Studiul ref_311 - CODARE ####
## Sest N., March E. (2017) - Constructing the cyber-troll: Psychopathy, sadism, and empathy
# Caracteristici ale participantilor
bd.meta$author[26] <- "Sest et al. (2017)"
bd.meta$n[26] <- 415
bd.meta$p.male[26] <- 0.36
bd.meta$m.age[26] <- 23.37
bd.meta$pop[26] <- "Generala"
bd.meta$prize[26] <- F
bd.meta$country[26] <- "Australia" 
# Caracteristici de design
bd.meta$design[26] <- "Corelational"
bd.meta$subgroup[26] <- "Gen" 
bd.meta$sample[26] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[26] <- "Standardizata" 
bd.meta$method[26] <- "Chestionar"

## 27. Studiul ref_323 - CODARE ####
## Sorokowski P., Kowal M., Zdybek P., Oleszkiewicz1 A. (2020) - Are Online Haters Psychopaths? Psychological Predictors of Online Hating Behavior
# Caracteristici ale participantilor
bd.meta$author[27] <- "Sorokowski et al. (2020)"
bd.meta$n[27] <- 94
bd.meta$p.male[27] <- 1 - 0.41
bd.meta$m.age[27] <- 33.4
bd.meta$pop[27] <- "Generala"
bd.meta$prize[27] <- F
bd.meta$country[27] <- "Polonia" 
# Caracteristici de design
bd.meta$design[27] <- "Corelational"
bd.meta$subgroup[27] <- "Gen" 
bd.meta$sample[27] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[27] <- "Standardizata" 
bd.meta$method[27] <- "Chestionar"

## 28. Studiul ref_360 - CODARE ####
## Yoon Lee S., Yao M.Z., Yi-Fan Su L. (2021) - Expressing unpopular opinion or trolling: Can dark personalities differentiate them?
# Caracteristici ale participantilor
bd.meta$author[28] <- "Yoon Lee et al. (2021)"
bd.meta$n[28] <- 599
bd.meta$p.male[28] <- .55
bd.meta$m.age[28] <- 35.61
bd.meta$pop[28] <- "Generala"
bd.meta$prize[28] <- F
bd.meta$country[28] <- "USA" 
# Caracteristici de design
bd.meta$design[28] <- "MANOVA"
bd.meta$subgroup[28] <- "Gen" 
bd.meta$sample[28] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[28] <- "Standardizata" 
bd.meta$method[28] <- "Chestionar"

## 29. Studiul ref_366 - CODARE ####
## Zhanga H., Zhaob H. (2020) - Dark personality traits and cyber aggression in adolescents: A moderated mediation analysis of belief in virtuous humanity and self-control
# Caracteristici ale participantilor
bd.meta$author[29] <- "Zhanga et al. (2020)"
bd.meta$n[29] <- 675
bd.meta$p.male[29] <- 296 / 675
bd.meta$m.age[29] <- 19.64
bd.meta$pop[29] <- "Studenti"
bd.meta$prize[29] <- F
bd.meta$country[29] <- "China" 
# Caracteristici de design
bd.meta$design[29] <- "SEM"
bd.meta$subgroup[29] <- NA
bd.meta$sample[29] <- "Convenienta"
# Caracteristici de masurare
bd.meta$measure[29] <- "Standardizata" 
bd.meta$method[29] <- "Chestionar"

# Salvarea datelor
save(bd.meta, file = "Codari.RData"); rm(tabel.surse)

# Andreou, E. (2004). Bully/victim problems and their association with Machiavellianism and self-efficacy in Greek primary school children. British Journal of Educational Psychology, 74(2), 297–309.
# Ang, R. P., Tan, K. A., & Mansor, A. T. (2011). Normative beliefs about aggression as a mediator of narcissistic exploitativeness and cyberbullying. Journal of Interpersonal Violence, 26(13), 2619–2634.
# Ang, R. P., Ong, E. Y., Lim, J. C., & Lim, E. W. (2010). From narcissistic exploitativeness to bullying behavior: The mediating role of approval-of-aggression beliefs. Social Development, 19(4), 721–735.
# Abell, L., & Brewer, G. (2014). Machivellianism, self-monitoring, self-promotion and relational aggression on Facebook. Computers in Human Behavior, 36, 258–262. <http://dx.doi.org/10.1016/j.chb.2014.03.076>.
# Brown, W. M., Hazraty, S., & Palasinski, M. (2019). Examining the dark tetrad and its links to cyberbullying. Cyberpsychology, Behavior, and Social Networking, 22(8), 552–557.
#	Bushman, B. J., & Baumeister, R. F. (1998). Threatened egoism, narcissism, selfes-teem, and direct and displaced aggression: Does self-love or self-hate lead to vio-lence? Journal of Personality and Social Psychology, 75, 219–229.
# Carpenter, C. J. (2012). Narcissism on Facebook: Self-promotional and anti-social behavior. Personality and Individual Differences, 52(4), 482–486. http://dx.doi.org/10.1016/j.paid.2011.11.011.
# Choi, M., Panek, E. T., Nardis, Y., & Toma, C. L. (2015). When social media isn't social: Friends' responsiveness to narcissists on Facebook. Personality and Individual Differences, 77, 209–214. https://doi.org/10.1016/j.paid.2014.12.056.
# Craker, N., & March, E. (2016). The dark side of Facebook®: The Dark Tetrad, negative social potency, and trolling behaviours. Personality and Individual Differences, 102, 79–84.
# Eksi, F. (2012). Examination of narcissistic personality traits’ predicting level of internet addiction and cyber bullying through path analysis. Educational Sciences: Theory and Practice, 12(3), 1694–1706.
# Fanti, K. A., & Kimonis, E. R. (2012). Bullying and victimization: The role of conduct problems and psychopathic traits. Journal of Research on Adolescence, 22(4), 617–631.
# Fanti, K. A., & Henrich, C. C. (2014). Effects of self-esteem and narcissism on bullying and victimization during early adolescence. Journal of Early Adolescence. in press. 
# Garcia, D., & Sikstr€om, S. (2014). The dark side of Facebook: Semantic representations of status updates predict the Dark Triad of personality. Personality and Individual Differences, 67, 92–96. https://doi.org/10.1016/j.paid.2013.10.001
# Giammarco, E. A., & Vernon, P. A. (2014). Vengeance and the Dark Triad: The role of empathy and perspective taking in trait forgivingness. Personality and Individual Differences, 67, 23–29. <http://dx.doi.org/10.1016.j.paid.2014.02.010>.
# Gumpel, T. P. (2014). Linking psychopathy and school aggression in a nonclinical sample of adolescents. Journal of School Violence, 13(4), 377–395.
#	Kerig, P. K., & Stellwagen, K. K. (2010). Roles of callous-unemotional traits, narcis-sism, and Machiavellianism in childhood aggression. Journal of Psychopathological Behavior Assessment, 32, 343–352.
# Lopes, B., & Yu, H. (2017). Who do you troll and why: An investigation into the relationship between the dark triad personalities and online trolling behaviours towards popular and less popular Facebook profiles. Computers in Human Behavior, 77, 69–76. https://doi.org/10.1016/j.chb.2017.08.036.
# Madan, A. O. (2014). Cyber aggression/cyber bullying and the Dark Triad: Effect on workplace behavior/performance. International Journal of Computer and Systems Engineering, 8(6), 1740–1745. Retrieved from https://waset.org/publications/9998533/cyber-aggression-cyber-bullying-and-the-dark-triad-effect-on-workplacebehavior-performance.
#	March, E., Grieve, R., Marrington, J., & Jonason, P. K. (2017). Trolling on Tinder (and other dating apps): Examining the role of the Dark Tetrad and impulsivity. Personality and Indi-vidual Differences, 110, 139e143. https://doi.org/10.1016/j.paid.2017.01.025.
# Nevin, A. D. (2015). Cyber-Psychopathy: Examining the relationship between dark E-personality and online misconduct. Electronic thesis and dissertation repository, paper 2926 (Retrieved from) http://ir.lib.uwo.ca/etd/2926
# # Sest, N., & March, E. (2017). Constructing the cyber-troll: Psychopathy, sadism, and empathy. Personality and Individual Differences, 119, 69–72. https://doi.org/10.1016/j.paid.2017.06.038.
# Scott, G. G., Brodie, Z. P., Wilson, M. J., Ivory, L., Hand, C. J., & Sereno, S. C. (2020). Celebrity abuse on Twitter: The impact of tweet valence, volume of abuse, and dark triad personality factors on victim blaming and perceptions of severity. Computers in Human Behavior, 103, 109–119. https://doi.org/10.1016/j.chb.2019.09.020
# Sutton, J., & Keogh, E. (2000). Social competition in school: Relationships with bullying, Machiavellianism and personality. British Journal of Educational Psychology, 70, 443–456. <http://dx.doi.org/10.1348/000709900158227>.

