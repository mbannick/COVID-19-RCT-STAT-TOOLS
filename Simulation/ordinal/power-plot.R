library(data.table)
library(ggplot2)
library(reshape2)
library(ggbrace)

df <- fread("~/Documents/FileZilla/BIOST572/RESULTS-binary.csv")
df2 <- copy(df)
df <- data.table::melt(df, id.vars=c("n", "seed", "trt_effect", "dgp", "wmean_truth"),
                       measure.vars=c("wmean_unadj_est", "wmean_adj_est"))

df[, variable := lapply(.SD, function(x) gsub("wmean_", "", x)), .SDcols=c("variable")]
df[, variable := lapply(.SD, function(x) gsub("_est", "", x)), .SDcols=c("variable")]
df <- df[!is.na(trt_effect)]
df[, mse := (value - wmean_truth)^2]

# ggplot(data=df) + geom_boxplot(aes(x=variable, y=value)) +
#   facet_wrap(trt_effect ~ dgp)

rel <- df[, lapply(.SD, mean), .SDcols="mse", by=c("n", "trt_effect",
   "dgp", "variable")]
rel <- rel[, mse.scale := mse * sqrt(n)]

rel2 <- dcast(rel, n + trt_effect + dgp ~ variable, value.var="mse") %>% data.table

rel2[, relative := adj / unadj]
rel2[, sampsize := lapply(.SD, function(x) paste0("N = ", x)), .SDcols="n"]
rel2[, sampsize := factor(sampsize, levels=c("N = 100",
                                                "N = 200",
                                                "N = 500",
                                                "N = 1000"))]
rel2[dgp == 1, dgp_name := "Hospitalized"]
rel2[dgp == 3, dgp_name := "Non-Hospitalized"]
rel2[, relative.gain := 1 - relative]

real.tx <- mapply(binary.tx.eff, dgp=rel2$dgp, n=rel2$n, eff=rel2$trt_effect)
rel2[, real := real.tx]

ggplot(data=rel2) +
  geom_point(aes(x=real.tx, y=relative.gain,
                 color=dgp_name, group=dgp_name)) +
  geom_line(aes(x=real.tx, y=relative.gain,
                color=dgp_name, group=dgp_name), linetype='dashed') +
  facet_grid( ~ sampsize, scales="free_x") +
  geom_hline(yintercept=0, color='red', linetype='dashed') +
  scale_colour_manual(values=c("#3279a8", "#a8329e")) +
  theme_minimal() +
  #scale_x_continuous(breaks=c(0, 0.5, 1)) +
  labs(x="Treatment Effect",
       y="Percent Decrease in MSE", color="Population") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Relative Efficiency for Risk Difference Estimand (Binary Outcome)")

df <- fread("~/Documents/FileZilla/BIOST572/RESULTS-binary.csv")
df2 <- copy(df)
df2[dgp == 1, dgp_name := "Hospitalized"]
df2[dgp == 3, dgp_name := "Non-Hospitalized"]
df2[, sampsize := lapply(.SD, function(x) paste0("N = ", x)), .SDcols="n"]
df2[, sampsize := factor(sampsize, levels=c("N = 100",
                                             "N = 200",
                                             "N = 500",
                                             "N = 1000"))]

df2[, power_adj := (wmean_adj_bca_ciu < 0) | (wmean_adj_bca_cil > 0)]
df2[, power_unj := wmean_unadj_pval < 0.05]
df2 <- df2[!is.na(trt_effect)]

df2 <- data.table::melt(df2,
                        id.vars=c("n", "seed", "trt_effect", "dgp", "wmean_truth"),
                        measure.vars=c("power_adj", "power_unj"))
summ2 <- df2[, lapply(.SD, function(x) mean(x, na.rm=TRUE)),
             .SDcols="value",
             by=c("n", "trt_effect", "dgp", "wmean_truth", "variable")]

summ3 <- dcast(summ2, n + trt_effect + dgp ~ variable, value.var="value") %>% data.table

pdf("power-plot.pdf", height=5, width=7)
ggplot(data=summ3) +
  geom_abline(slope=1, intercept=0, linetype='solid', color='black',
              size=0.4) +
  geom_segment(aes(x=power_unj, xend=power_unj, y=power_adj, yend=power_unj),
               color="black", linetype='dashed', size=0.4) +
  geom_point(aes(x=power_unj, y=power_adj, color=factor(n)), size=2) +
  theme_minimal() + #coord_fixed() +
  theme(legend.position="bottom") +
  ggtitle("Power to Reject Null Hypothesis\n(Binary Outcomes, Risk Difference)") +
  labs(color="Sample Size", x="Power: Unadjusted Test",
       y="Power: Adjusted Test") +
  annotate("text", x = 0.35, y = 0.2, label = "Equal Power") +
  annotate("segment", x = 0.2, y = 0.2,
           xend=0.25, yend=0.2, size=0.4) +
  geom_brace(xstart=0.10, xend=0.11, ystart=0.03, yend=0.07,
             rotate = 90, size=0.4) +
  annotate("text", x=0.20, y=0.05, label = "Type I Error")
dev.off()
