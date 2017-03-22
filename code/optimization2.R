b <- -0.53
a <- 1.12
pars = c(a,b)

yy=dat.p$tp_out_conc
xx=dat.p$tp_in_conc
tt=dat.p$res_time

model_tp_out <- function(a,b){
  t = tt
  x = xx
  y = x/(1+(a*(t^b)*t))
}

TP_out_diff <- function(pars){
  modeled = log(model_tp_out(pars[1], pars[2]))
  resTPout = modeled - log(yy)
  return(resTPout)
}

# Starting parameters cannot be negative, because of bounds we set 
parStart = pars
lowerBound = c(0,-1)
upperBound = c(10,0)

# For difficult problems it may be efficient to perform some iterations with Pseudo, which will bring the algorithm 
# near the vicinity of a (the) minimum, after which the default algorithm (Marq) is used to locate the minimum more precisely.
Fit2 <- modFit(f = TP_out_diff, p=parStart,method = 'Newton', 
               lower= lowerBound,
               upper= upperBound)

Fit2par = Fit2$par

curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

points(dat.p$Rp~dat.p$res_time, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1.1), lwd = 4,add=TRUE)
curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      col = col.p, lwd = 4,add=TRUE)


# optimize model with vals >-1
yy=dat.p$tp_out_conc[dat.p$Rp>0]
xx=dat.p$tp_in_conc[dat.p$Rp>0]
tt=dat.p$res_time[dat.p$Rp>0]
Fit2 <- modFit(f = TP_out_diff, p=parStart,method = 'Newton', 
               lower= lowerBound,
               upper= upperBound)


curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

points(dat.p$Rp~dat.p$res_time, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1.1), lwd = 4,add=TRUE)
curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      col = col.p, lwd = 4,add=TRUE)

# now try optimizing for R
yy=brett$Rp_calculated
#xx=dat.p$tp_in_conc[dat.p$Rp>0]
tt=brett$res_time

model_tp_out <- function(a,b){
  t = tt
  #x = xx
  y = 1-(1/(1+(a*(t^b)*t)))
}

TP_out_diff <- function(pars){
  modeled = model_tp_out(pars[1], pars[2])
  resTPout = modeled - yy
  return(resTPout)
}

Fit2 <- modFit(f = TP_out_diff, p=parStart,method = 'Newton', 
               lower= lowerBound,
               upper= upperBound)
curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

points(dat.p$Rp~dat.p$res_time, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(1.12*(x^.47)))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = "red", ylim = c(-1, 1.1), lwd = 4,add=TRUE)
curve(1-(1/(1+(Fit2$par[1]*(x^(1+Fit2$par[2]))))), 0.001, 1000, log = "x",
      col = col.p, lwd = 4,add=TRUE)
