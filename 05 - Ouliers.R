library(dmetar)

# Analiza studiilor cu efecte extreme
no.out <- find.outliers(moderare); 
summary(no.out)
# Desenarea graficului de tip forest plot
png(filename = "Efecte.png", height = 1300, width = 700)
forest(no.out); dev.off()

# Analiza studiilor influentiale
influence <- InfluenceAnalysis(moderare, random = T)

pdf(file = "Efecte.pdf", height = 5, width = 10)
plot(influence, "baujat"); dev.off()

pdf(file = "Efecte.pdf", height = 7, width = 12)
plot(influence, "influence"); dev.off()

pdf(file = "Efecte.pdf", height = 9, width = 9)
plot(influence, "es"); dev.off()

# Analiza GOSH
date.gosh <- rma(yi = aleatorii$TE, sei = aleatorii$seTE,
                 method = aleatorii$method.tau)
parallel::detectCores()
rez.gosh <- gosh(date.gosh, progbar = T, ncpus = 7, parallel = "snow")
plot(rez.gosh, alpha = .05)
diag.gosh <- gosh.diagnostics(rez.gosh, km=T, verbose = T)
plot(diag.gosh)

