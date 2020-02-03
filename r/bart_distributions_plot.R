library(viridis)

# EV curves in the task design
balloons <- 1:64

# conditional probabilities
cond_uniform_p <- NULL
cond_normal_p_high <- NULL
cond_normal_p_moderate <- NULL
cond_normal_p_low <- NULL

for (pump in 0:63) {
        cond_normal_p_high <- c(cond_normal_p_high,
                        dnorm((pump+1), mean = 32, sd = 18) /
                                sum(dnorm((pump+1):64, mean = 32, sd = 18)))
        cond_normal_p_moderate <- c(cond_normal_p_moderate,
                                dnorm((pump+1), mean = 32, sd = 12) /
                                        sum(dnorm((pump+1):64, mean = 32, sd = 12)))
        cond_normal_p_low <- c(cond_normal_p_low,
                                dnorm((pump+1), mean = 32, sd = 6) /
                                        sum(dnorm((pump+1):64, mean = 32, sd = 6)))
        cond_uniform_p <- c(cond_uniform_p,
                        dunif((pump+1), min = 1, max = 64) /
                                sum(dunif((pump+1):64, min = 1, max = 64)))
}


# cumulative probabilities
cum_uniform_p <- punif(balloons, 1, 64)
cum_normal_p_high <- pnorm(balloons, 32, 18)
cum_normal_p_moderate <- pnorm(balloons, 32, 12)
cum_normal_p_low <- pnorm(balloons, 32, 6)

# density
dens_uniform_p <- dunif(balloons, 1, 64)
dens_normal_p_high <- dnorm(balloons, 32, 18)
dens_normal_p_moderate <- dnorm(balloons, 32, 12)
dens_normal_p_low <- dnorm(balloons, 32, 6)

# EVs
ev_uniform_p <- balloons * (1 - cum_uniform_p)
ev_normal_p_high <- balloons * (1 - cum_normal_p_high)
ev_normal_p_moderate <- balloons * (1 - cum_normal_p_moderate)
ev_normal_p_low <- balloons * (1 - cum_normal_p_low)

cols <- viridis(4, end = .8,  alpha = .8)
names(cols) <- c("unif", "norm_low", "norm_mod", "norm_high")

# plot

pdf("plots/bart_architecture.pdf", height = 6.5, width = 9.25)

layout(matrix(c(1, 1, 2, 2, 5,
                3, 3, 4, 4, 5), byrow = TRUE, ncol = 5))
par(mar = c(4, 7, .5, 1))

### plot conditional probabilities
plot(balloons, cond_uniform_p, ylim = c(0, 1), bty = "l", las = 1, cex.axis = 1.4,
     cex.lab = 1.8, xlab = "", ylab = "",
     pch = 15, col = cols["unif"])
mtext("Conditional p(explosion)", 2, cex = 1.2, line = 3, padj = -.5)
mtext("N Inflations", 1, cex = 1.2, line = 3, padj = -.25)
text("a)", font = 2, xpd = TRUE, x = -20, y = 1, cex = 2)

points(balloons, cond_normal_p_low, col = cols["norm_low"], pch = 16)
points(balloons, cond_normal_p_moderate, col = cols["norm_mod"], pch = 17)
points(balloons, cond_normal_p_high, col = cols["norm_high"], pch = 18)

### plot probability densities

plot(balloons, dens_uniform_p, ylim = c(0, .08), bty = "l", las = 1, cex.axis = 1.4,
     cex.lab = 1.8, xlab = "", ylab = "",
     pch = 15, col = cols["unif"])
mtext("p(explosion) Mass", 2, cex = 1.2, line = 3, padj = -.5)
mtext("N Inflations", 1, cex = 1.2, line = 3, padj = -.25)
text("b)", font = 2, xpd = TRUE, x = -20, y = .08, cex = 2)

points(balloons, dens_normal_p_low, col = cols["norm_low"], pch = 16)
points(balloons, dens_normal_p_moderate, col = cols["norm_mod"], pch = 17)
points(balloons, dens_normal_p_high, col = cols["norm_high"], pch = 18)

### plot cumulative probabilities

plot(balloons, cum_uniform_p, ylim = c(0, 1), bty = "l", las = 1, cex.axis = 1.4,
     cex.lab = 1.8, xlab = "", ylab = "",
     pch = 15, col = cols["unif"])
mtext("Cumulative p(explosion)", 2, cex = 1.2, line = 3, padj = -.5)
mtext("N Inflations", 1, cex = 1.2, line = 3, padj = -.25)
text("c)", font = 2, xpd = TRUE, x = -20, y = 1, cex = 2)

points(balloons, cum_normal_p_low, col = cols["norm_low"], pch = 16)
points(balloons, cum_normal_p_moderate, col = cols["norm_mod"], pch = 17)
points(balloons, cum_normal_p_high, col = cols["norm_high"], pch = 18)


### Plot EVs


plot(balloons, ev_uniform_p, ylim = c(0, 22), bty = "l", las = 1, cex.axis = 1.4,
     cex.lab = 1.8, xlab = "", ylab = "",
     pch = 15, col = cols["unif"])
mtext("Expected Payoff", 2, cex = 1.2, line = 3, padj = -.5)
mtext("N Inflations", 1, cex = 1.2, line = 3, padj = -.25)
text("d)", font = 2, xpd = TRUE, x = -20, y = 22, cex = 2)

points(balloons, ev_normal_p_low, col = cols["norm_low"], pch = 16)
points(balloons, ev_normal_p_moderate, col = cols["norm_mod"], pch = 17)
points(balloons, ev_normal_p_high, col = cols["norm_high"], pch = 18)

maximums <- c(which.max(ev_uniform_p), which.max(ev_normal_p_low),
              which.max(ev_normal_p_moderate), which.max(ev_normal_p_high))
segments(maximums, -1, maximums, .05, col = cols, lwd = 2.5)

# Legend
par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
points(rep(.1, 4), y = seq(.2, .8, length.out = 4), pch = 15:18, col = cols,
       cex = 2.4)
# text(x = rep(.1, 4) + .1, y = seq(.2, .8, length.out = 4), cex = 1.6,
#      labels = c(expression(paste(BART[uniform],symbol("\u003A"), sep = "")),
#                 expression(paste(BART[normal-L],symbol("\u003A"), sep = "")),
#                 expression(paste(BART[normal-M],symbol("\u003A"), sep = "")),
#                 expression(paste(BART[normal-H],symbol("\u003A"), sep = ""))),
#      offset = 0, adj = 0)
text(x = rep(.15, 4) + .1, y = seq(.2, .8, length.out = 4)-.05, cex = 2,
     labels = c("~ U(1, 64)", "~ N(32, 6)", "~ N(32, 12)", "~ N(32, 18)"),
     offset = 0, adj = 0, vfont=c("script", "bold"))
dev.off()