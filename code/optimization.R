## optimize parameters

# use all values where res time and retention are available for Rp
dat.p <- dat.all[!is.na(dat.all$Rp)&!is.na(dat.all$res_time)&dat.all$Rp>0&dat.all$Rp<1, ]


xx=dat.p$res_time
yy=dat.p$Rp

funk=function(param,x,y){
  a=param[1]
  b=param[2]
  fit = sum((y-(1-(1/(1+(a*x*(abs(x)^b*sign(x)))))))^2)
  return(fit)
}

op <- optim(par=c(1.12,-0.5), fn=funk, x=xx, y=yy, lower = c(0.01,-.99), upper = c(10,10), 
      method = "L-BFGS-B")


fit = sum((y-(1-(1/(1+(a*x*(x^b))))))^2)

curve(1-(1/(1+(1.17*(x^0.292)))), 0.001,1000,log = "x",
      ylab = "Retention", xlab = "Residence Time (y)", 
      col = col.p, ylim = c(0, 1.1), lwd = 4,xaxt = "n", cex.lab = 2, cex.axis = 1.3)
points(dat.p$Rp~dat.p$res_time, xlog = TRUE, pch = 21, cex = 1.5,
       bg = rgb(222,222,222,max=255,alpha=200))

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
