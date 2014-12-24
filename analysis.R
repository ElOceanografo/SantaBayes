library(xtable)

evidence <- c("None", "Beard", "Date", "Suit", "Hat", "Nose", 
              "Laugh", "Soon",  "Reindeer")
p.evidence <- c(NA, 0.01,  1/365,  0.001,  0.1,   0.3,    
                0.2,     0.8,   1e-9)
p.santa <- rep(0, length(p.evidence))
likelihood <- c(NA, rep(1, length(p.evidence) - 1))

tab <- data.frame(evidence, p.evidence, likelihood, 
                  p.santa, p.santa, p.santa, p.santa)

priors <- c(0.9, 0.1, 0.001, 1e-9)
lty <- 1:length(priors)

plot.new()
plot.window(xlim=c(1, 9), ylim=c(0, 1))
axis(1, at=1:length(p.evidence), labels=evidence)
mtext("Evidence", 1, 3)
axis(2)
mtext("P(Santa)", 2, 3)

for (i in 1:length(priors)) {
  tab[1, i + 3] <- priors[i]
  for (j in 2:length(p.evidence)) {
    tab[j, i+3] <- likelihood[j] * tab[j-1, i+3] / 
      ((1 - tab[j-1, i+3]) * p.evidence[j] + likelihood[j] * tab[j-1, i+3])
  }
  lines(tab[ , i+3], lty=lty[i])
  points(tab[ , i+3], pch=lty[i])
}


labels <- c("D", "P(D)", "P(D|S)", 
            "P(S)=0.9", "P(S)=0.5", "P(S)=0.1", "P(S)=1E-9")
colnames(tab) <- labels
print(xtable(tab, digits=3, display=c("s", "s", rep("G", 6))))