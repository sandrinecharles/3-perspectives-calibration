niter <- 1000
# Simulation of internal concentrations
# considering the above probability distributions
# and a fixed sigma value

sigma <- 10

pred.para <- c()
pred.total <- c()
for(k in 1:niter){
  tmp.ku <- sample(ku, 1)
  tmp.ke <- sample(ke, 1)
  tmp.theo <- bioacc(parameters = c(tmp.ku, tmp.ke),
                     expw = expw, tc = 1, tmax = tmax)
  
  alea <- rnorm(n = 300, 0, sigma)
  
  pred.para <- rbind(pred.para, tmp.theo$conc)
  pred.total <- rbind(pred.total, tmp.theo$conc + alea)
}
colnames(pred.para) <- colnames(pred.total) <- tmp.theo$time

Qpred.para <- t(apply(pred.para, MARGIN = 2,
                      FUN = function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))
Qpred.total <- t(apply(pred.total, MARGIN = 2,
                       FUN = function(x) quantile(x, probs = c(0.025, 0.5, 0.975))))

Qpred <- as.data.frame(rbind(Qpred.para, Qpred.total))
Qpred$type <- rep(c("confidence interval", "prediction interval"), each=300)
Qpred$type <- factor(Qpred$type,
                     levels=c("prediction interval", "confidence interval"))
Qpred$time <- rep(tmp.theo$time, 2)
colnames(Qpred) <- c("lower", "median", "upper", "type", "time")

ggplot(data = Qpred, aes(x = time, y = median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill=type),
              linetype = "dashed") +
  geom_line()