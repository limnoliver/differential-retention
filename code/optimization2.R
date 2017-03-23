library(FME)

# set parameter boundaries  
lowerBound = c(0,-1)
upperBound = c(10,0)
a.start = 1.12
b.start = -0.52

# a.start = start value for par a (educated guess; 1.12 from Brett & Benjamin)
# b.start = best guess for par b (-0.53 from Brett & Benjamin)
# dat.in = data frame with all variables needed (tp_in_conc, tp_out_conc, Rp, res_time)
# type = "concentration" for fitting model by minimizing TP_out_conc residuals,"retention"
# for minimizing Rp residuals
  
P.ret.mod.fit <- function(dat.in, type) {
a <- a.start
b <- b.start
pars = c(a,b)

d <- dat.in

if (type == "concentration"){
  yy=d$tp_out_conc
  xx=d$tp_in_conc
  tt=d$res_time
  
  model <- function(a,b){
    t = tt
    x = xx
    y = x/(1+(a*(t^b)*t))
  }
  
  out_diff <- function(pars){
    modeled = log(model(pars[1], pars[2]))
    resTPout = modeled - log(yy)
    return(resTPout)
  }
} else {
  yy = d$Rp
  tt = d$res_time
  
  model <- function(a,b){
    t = tt
    y = 1-(1/(1+(a*(t^b)*t)))
  }
  out_diff <- function(pars){
    modeled = model(pars[1], pars[2])
    resTPout = modeled - yy
    return(resTPout)
  }
}
Fit <- modFit(f = out_diff, p=parStart,method = 'Newton', 
               lower= lowerBound,
               upper= upperBound)
return(Fit)

}

Fit.Brett <- P.ret.mod.fit(dat.in = brett, type = "concentration")
Fit.all <- P.ret.mod.fit(dat.in = dat.p, type = "concentration")
Fit.real <- P.ret.mod.fit(dat.in = dat.p.real, type = "concentration")
Fit.pos <- P.ret.mod.fit(dat.in = dat.p.pos, type = "concentration")
Fit.Rp <- P.ret.mod.fit(dat.in = dat.p, type = "retention")
Fit.Rp.real <- P.ret.mod.fit(dat.in = dat.p.real, type = "retention")
Fit.Rp.pos <- P.ret.mod.fit(dat.in = dat.p.pos, type = "retention")

curve(1-(1/(1+(Fit.Rp.real$par[1]*(x^(1+Fit.Rp.real$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

curve(1-(1/(1+(Fit.Brett$par[1]*(x^(1+Fit.Brett$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,xaxt = "n", 
      cex.lab = 2, cex.axis = 1.3, add = TRUE)
curve(1-(1/(1+(Fit.real$par[1]*(x^(1+Fit.real$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "orange", ylim = c(-1, 1.1), lwd = 4,xaxt = "n", 
      cex.lab = 2, cex.axis = 1.3, add = TRUE)

yy = dat.p.pos$Rp
tt = dat.p.pos$res_time
Fit.Rp.pos <- modFit(f = TP_out_diff, p=parStart,method = 'Newton', 
                      lower= lowerBound,
                      upper= upperBound)
curve(1-(1/(1+(Fit.Rp.pos$par[1]*(x^(1+Fit.Rp.pos$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "blue", ylim = c(-1, 1.1), lwd = 4,xaxt = "n", 
      cex.lab = 2, cex.axis = 1.3, add = TRUE)


