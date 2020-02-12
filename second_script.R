library(readr)

# constants
u0 <- 4 * pi * 10 ^ (-7)
degree <- pi / 180
i <- 351.1
Bterra <- 1.70192 * 10 ^ (-5)

# get data
metric_i_cte <-
  read_delim(
    "../data/metric_i_cte_r-em-cm.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

r_mm = metric_i_cte[, 1] * 10
teta = metric_i_cte[, 2]

# calculating Bexp and Bnom
Bexp = Bterra * tan(teta * degree)
colnames(Bexp) <- c("Bexp (T)")
Bnom <- ((u0 * i) / (2 * pi * r_mm))
colnames(Bnom) <- c("Bnom (T)")

table = r_mm
table[, 2] <- Bexp * (10 ^ 6)
colnames(table) <- c("r", "Bexp")

# fitting curve
fit = lm(Bexp ~ I(1 / r), data = table)

# getting info from the fit
summary(fit)

# write summary result into a file
#write(summary(fit), "../plots_and_figures/ex2_output.txt")

# create sequence for x coordinate
xx <- seq(5, 50, length = 50)

# save to file
png("../plots_and_figures/ex2_teta_per_r.png")
# png("../plots_and_figures/ex2_teta_per_r.png", width = 640, height = 480)

# plotting
plot(
  table,
  pch = 19,
  xlab = "r (mm)",
  ylab = "B (mT)",
  main = "Campo Magnético do fio x Distância ao fio"
)
# add adjested curve to the plot
lines(xx, predict(fit, data.frame(r = xx)), col = "red")

# adding subtitles to this graph
legend(
  "topright",
  inset = .05,
  c("B medido", "B ajustado"),
  col = c("black", "red"),
  lty = c(NA, 1),
  pch = c(20, NA),
  cex = 1.2,
  box.lty = 0
)


# save in device
dev.off()

table = r_mm
table[, 2] <- teta
table[, 3] <- Bexp * (10 ^ 6)
table[, 4] <- Bnom * (10 ^ 6)
colnames(table) <-
  c("r (mm)", "Deflexão ()", "Bexp (mT)", "Bnom (mT)")

write.table(
  table,
  file = "../plots_and_figures/output_table_exp2.csv",
  sep = ";",
  quote = FALSE,
  row.names = TRUE
)
