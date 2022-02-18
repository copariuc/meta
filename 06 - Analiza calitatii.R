library(meta); library(dmetar)

# Desenarea graficului de tip funnel-plot
funnel.meta(fixe, xlim = c(-.5, 1.8), studlab = F,
            contour = c(.95, .99), col.contour = c("orange", "green"))
legend(x =1.6, y = 0, legend = c("p<.05", "p<.01"), fill = c("orange", "green"))

# Testul regresiei Egger
metabias(fixe, plotit = T, method.bias = "Egger")
eggers.test(fixe)

# Analiza magnitudinii biasarii
fixe$I2
aleatorii$I2
trim <- trimfill(fixe); summary(trim)
forest(trim)
funnel.meta(trim, xlim = c(-2.0, 2.0), studlab = F,
            contour = c(.95, .99), col.contour = c("orange", "green"))
legend(x =1.6, y = 0, legend = c("p<.05", "p<.01"), fill = c("orange", "green"))

# Metoda Ruker
if (!require(metasens)) install.packages("metasens")
library(metasens)

l.meta <- limitmeta(aleatorii); summary(l.meta)
funnel.limitmeta(l.meta, shrunken = T)

# Analiza curbei p
pcurve(aleatorii)
