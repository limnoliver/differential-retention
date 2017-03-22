## optimize parameters

# use all values where res time and retention are available for Rp
dat.p <- dat.all[!is.na(dat.all$Rp)&!is.na(dat.all$res_time)&!is.na(dat.all$tp_in_conc)&!is.na(dat.all$tp_out_conc), ]


yy=log(dat.p$tp_out_conc)
xx=log(dat.p$tp_in_conc)
tt=log(dat.p$res_time)

funk=function(param,x,y,t){
  a=param[1]
  b=param[2]
  fit = sum((y-(x/1+(a*((abs(t)^b)*sign(t)))))^2)
  return(fit)
}

op <- optim(par=c(1.12,.47), fn=funk, y=yy, t=tt, x=xx)


fit = sum((y-(1-(1/(1+(a*((abs(x)^b)*sign(x))))))^2)

curve(1-(1/(1+(1.59*(x^.41)))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)

points(dat.p$Rp~dat.p$res_time, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))
curve(1-(1/(1+(0.968*(x^1.0)))), 0.001, 1000, log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(-1, 1.1), lwd = 4,add=TRUE)


curve(1-(1/(1+(1.2*(x^0.90)))), 0.001,1000,log = "x",
      col = col.p, ylim = c(0, 1.1), lwd = 4,add = TRUE)

# lake mendota
curve(1-(exp((-Vf*x)/12.8)), .5, 10, log = "x"))

curve(1-(exp((-Vf*x)/12.8)), 0.001,1000,log = "x",
      ylab = "Rn", xlab = "Residence Time (y)", 
      col = col.n, ylim = c(-.1, 1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
curve(1-(1/(1+(1.12*(x^.47)))), 0.001,1000,log = "x",
      ylab = "Rp", xlab = "Residence Time (y)", 
      col = col.p, add=TRUE, lwd=4)
abline(v=1, col="gray", lty=2)
# week
abline(v=2, col="gray", lty=2)
# month
abline(v=3, col="gray", lty=2)
abline(v=4, col="gray", lty=2)
