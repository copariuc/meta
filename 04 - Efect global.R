# Incarcarea pachetelor necesare
if (!require(meta)) install.packages("meta")
devtools::install_github("MathiasHarrer/dmetar")
library(meta); library(dplyr); library(MAd)
library(dmetar)

load("Finala.RData")

# Agregarea studiilor
efecte.agr <- agg(data = ds.global, id = study, method = "BHHR")

n <- ds.global %>%
  group_by(study) %>%
  summarise(n = round(mean(sample.size)))

# efecte.agr <- cbind(efecte.agr, n$n)
efecte.agr <- efecte.agr %>%
  arrange(id) %>%
  mutate(n = n$n) %>%
  mutate(se = sqrt(var) / sqrt(n))

rm(n)

# Analiza unui model cu efecte fixe
fixe <- metagen(data = efecte.agr, TE = es, seTE = se, studlab = id,
                fixed = T, random = F); summary(fixe)
# Analiza unui model cu efecte aleatorii
aleatorii <- metagen(data = efecte.agr, TE = es, seTE = se, studlab = id,
                fixed = F, random = T, w.random = T, prediction = T); summary(aleatorii)


# Desenarea graficului de tip forest plot
forest(aleatorii, layout = "meta", sortvar = TE, xlim = c(-2.0, 2.0),
       rightlabs = c("g","95% CI", "Weight"),
       leftlabs = c("Authors and year", "g", "Std. err."),
       col.fixed = "red", col.diamond = "orange")

# Corectia de artefact
ds.global <- atten(data = ds.global, g = es, xx = Rxx, yy = Ryy)
ds.global <- transform(ds.global,
                       es.cor = ifelse(is.na(g.corrected), es, g.corrected))
ds.global$g.corrected <- NULL

# Agregarea pentru analiza cu moderare
moderare.agg <- ds.global %>%
  group_by(study, Y) %>%
  summarise(es = mean(es),
            es.cor = mean(es.cor),
            se = mean(se))

# Analiza unui model cu moderatori categoriali
moderare <- metagen(data = moderare.agg, TE = es, seTE = se, studlab = study,
                    fixed = F, random = T, prediction = T, subgroup = Y); summary(moderare)
subgroup.analysis.mixed.effects(x = moderare, subgroups = moderare$subgroup)

# Desenarea graficului de tip forest plot
png(filename = "Efecte.png", height = 1300, width = 700)
forest(moderare, layout = "meta", sortvar = TE, xlim = c(-2.0, 2.0),
       rightlabs = c("g","95% CI", "Weight"),
       leftlabs = c("Authors and year", "g", "Std. err."),
       col.fixed = "red", col.diamond = "orange",
       subgroup.name = "Dark Tetrad Factors"); dev.off()

#  Analiza unui model de meta-regresie
regresie.agg <- ds.global %>%
  group_by(study, Y) %>%
  summarise(es = mean(es),
            se = mean(se),
            year = mean(year),
            cases = mean(sample.size),
            age = mean(age))
# Analiza unui model cu efecte aleatorii
aleatorii <- metagen(data = regresie.agg, TE = es, seTE = se, studlab = study,
                     fixed = F, random = T, w.random = T, prediction = T); summary(aleatorii)
regresie <- metareg(x = aleatorii, formula = age + cases, method.tau = "REML",
                    control = list(verbose = T)); summary(regresie)
forest(regresie)
bubble(regresie, studlab = T, regline = T)

# Analiza unui model bazat pe efecte de interactiune - Regresii ierarhice
if(!require(metafor)) install.packages("metafor")
library(metafor)

model.1 <- rma(yi = es, sei = se, data = regresie.agg,
               mods = ~ cases); model.1
model.2 <- rma(yi = es, sei = se, data = regresie.agg,
               mods = ~ age); model.2
model.3 <- rma(yi = es, sei = se, data = regresie.agg,
               mods = ~ age + cases); model.3
regresie.agg$c.cases <- regresie.agg$cases - mean(regresie.agg$cases)
regresie.agg$c.age <- regresie.agg$age - mean(regresie.agg$age, na.rm = T)
model.4 <- rma(yi = es, sei = se, data = regresie.agg,
               mods = ~ age + cases + c.age * c.cases); model.4

anova(model.4, model.3)

# Utilizarea testului permutarilor pentru stabilitatea modelului
permutest(model.3, iter = 100, permci = T)
