# EpiModel Tutoriais

#install.packages("EpiModel", dependencies = TRUE)
#devtools::install_github("statnet/EpiModel")
library(EpiModel)

## Deterministic Compartimental Models ##

## SI (Not Run)

param <- param.dcm(inf.prob = 0.2, act.rate = 0.25)
init <- init.dcm(s.num = 500, i.num = 1)
control <- control.dcm(type = "SI", nsteps =300)

mod <- dcm(param, init, control)

plot(mod)

summary(mod, at = 130)


## SIR c/ demografia


param <- param.dcm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/20,
                   b.rate = 1/95, ds.rate = 1/100, di.rate = 1/80, dr.rate = 1/100)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 700, dt = 0.5)
mod <- dcm(param, init, control)



## SIR sem demografia

param <- param.dcm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/20,
                   b.rate = 0, ds.rate = 0, di.rate = 0, dr.rate = 0)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 400, dt = 0.5)
mod1 <- dcm(param, init, control)

summary(mod1, at = 2)

par(mfrow = c(1, 1))
comp_plot(mod1, at = 50, digits = 1)


## Stochastic individual contact model ## 

## SI (Not run)

param <- param.icm(inf.prob = 0.2, act.rate = 0.25)
init <- init.icm(s.num = 500, i.num = 1)
control <- control.icm(type = "SI", nsims = 10, nsteps = 300)
mod2 <- icm(param, init, control)

summary(mod, at = 125)
plot(mod2, y = "i.num", mean.line = FALSE, sim.lines = TRUE)

plot(mod2, sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE, mean.line = FALSE)

# Plot incidence
par(mfrow = c(1, 2))
plot(mod, y = "si.flow", mean.smooth = TRUE, grid = TRUE)
plot(mod, y = "si.flow", qnts.smooth = FALSE, qnts = 1)


## SIR ICM

param <- param.icm(inf.prob = 0.2, act.rate = 1, rec.rate = 1/50,
                   b.rate = 1/100, ds.rate = 1/100, di.rate = 1/90,
                   dr.rate = 1/100)
init <- init.icm(s.num = 1000, i.num = 1, r.num = 0)
control <- control.icm(type = "SIR", nsteps = 250, nsims = 10)
sim <- icm(param, init, control)


### Final figure ###

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(3, 2))

plot(mod1, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes", col = c("royalblue1", "sienna2", "gold1"),
     legend = "n")
title(main = "a)", cex.main = 1.5, adj = 0)
legend("topright", legend = c("S","I","R"), col = c("royalblue1", "sienna2", "gold1"),
       lty=1, cex=1, lwd = 2)
plot(mod1, main = "Disease Incidence", y = "si.flow", lwd = 4, col = "firebrick", legend = "n")


plot(mod, popfrac = FALSE, alpha = 0.5,lwd = 4, 
     col = c("royalblue1", "sienna2", "gold1"), legend = "n")
title(main = "b)", cex.main = 1.5, adj = 0)
legend("topright", legend = c("S","I","R"), col = c("royalblue1", "sienna2", "gold1"),
       lty=1, cex=1, lwd = 2)
plot(mod, y = "si.flow", lwd = 4, col = "firebrick", legend = "n")


plot(sim, qnts = FALSE, sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE, 
     mean.line = TRUE, legend = FALSE, sim.col = c("royalblue1", "sienna2", "gold1"),
     mean.col = c("royalblue1", "sienna2", "gold1"))
title(main = "c)", cex.main = 1.5, adj = 0)
legend("topright", legend = c("S","I","R"), col = c("royalblue1", "sienna2", "gold1"),
       lty=1, cex=1, lwd = 2)
plot(sim, y = "si.flow", qnts.smooth = FALSE, qnts = 1, mean.col  = "firebrick",
     qnts.col = "firebrick")




