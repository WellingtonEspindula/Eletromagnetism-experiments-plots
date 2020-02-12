library(readr)

# constants
u0 <- 4 * pi * 10 ^ (-7)
R <- 5
degree <- pi / 180
Bterra <- 1.70192 * 10 ^ (-5)

# get data
metric_r_cte <-
  read_delim("../data/metric_r_cte.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)

# calculating Bexp and Bnom
Bexp = Bterra * tan(metric_r_cte[, 2] * degree)
colnames(Bexp) <- c("Bexp (T)")
Bnom <- metric_r_cte[, 1] * ((u0) / (2 * pi * R))
colnames(Bnom) <- c("Bnom (T)")

table = metric_r_cte[, 1]
table[, 2] <- Bexp * (10 ^ 6)
colnames(table) <- c("i", "Bexp")

# fitting curve
fit = lm(Bexp ~ i, data = table)

# getting info from the fit
summary(fit)

# create sequence for x coordinate
xx <- seq(0, 401, length = 50)

# save to file
png("../plots_and_figures/ex1_teta_per_i.png")
# png("../plots_and_figures/ex1_teta_per_i.png", width = 640, height = 480)


# plotting
plot(
  table,
  pch = 19,
  main = "Campo Magnético do fio x Corrente Elétrica",
  xlab = "i (mA)",
  ylab = "B (mT)"
)

# add adjested curve to the plot
lines(xx, predict(fit, data.frame(i = xx)), col = "red")

# adding subtitles to this graph
legend("topleft", inset=.05, c("B medido", "B ajustado"), col=c("black", "red"), lty=c(NA,1), pch=c(20,NA), cex=1.2, box.lty=0)


# save in device
dev.off()

# mount the table
table = metric_r_cte
table[, 3] <- Bexp * (10 ^ 6)
table[, 4] <- Bnom * (10 ^ 6)
colnames(table) <-
  c("i (mA)", "Deflexão ()", "Bexp (mT)", "Bnom (mT)")

write.table(
  table,
  file = "../plots_and_figures/output_table_exp1.csv",
  sep = ";",
  quote = FALSE,
  row.names = TRUE
)
