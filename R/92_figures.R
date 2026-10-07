######################################################################
## 92_figures.R  -  Figuras del paper (Bloque 2)
## F3: AUC de todos los modelos, PRE (A) y EoTx (B), IC95%
## F4: valor incremental (clinico vs +hemograma, PRE/EoTx/combinado)
## F5: SHAP (XGBoost; + random forest si treeshap disponible), PRE/EoTx
## F6: forest plot de los RR ajustados (modelos finales PRE y EoTx)
## F7: dinamica de parametros por desenlace (eos%, monocitos, IG, plaquetas)
## OUTPUT: figures/paper/*.png
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(dplyr) })
destf <- file.path("figures", "paper"); if (!dir.exists(destf)) dir.create(destf, recursive = TRUE)

tema <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13),
        strip.background = element_rect(fill = "grey92", color = NA),
        legend.position = "top")
col_fail <- "#C0392B"; col_cure <- "#2E86C1"
col_up   <- "#C0392B"; col_down <- "#2E86C1"

mm <- read.csv("outputs/91_master_metrics/master_metrics.csv", check.names = TRUE)
mm$Model <- factor(mm$Model, levels = c("Parsimonious logistic","LASSO","Random Forest","XGBoost","SuperLearner"))

## ---------- F3: AUC de todos los modelos, PRE y EoTx (clinico+hemograma)
orden_modelos <- c("Parsimonious logistic","LASSO","Random Forest","XGBoost","SuperLearner")
f3_panel <- function(scn, titulo) {
  d <- mm[mm$Scenario == scn, ]
  ## orden fijo pedido por D. Rebellon: logistica, LASSO, RF, XGBoost, SuperLearner
  ## (rev -> la logistica queda ARRIBA en el eje y)
  d <- d[match(orden_modelos, d$Model), ]
  d$Model <- factor(d$Model, levels = rev(orden_modelos))
  ggplot(d, aes(x = ROC_AUC, y = Model)) +
    geom_errorbarh(aes(xmin = AUC_lo, xmax = AUC_hi), height = 0.18, color = "grey45") +
    geom_point(size = 3.1, color = col_fail) +
    geom_text(aes(label = sprintf("%.2f", ROC_AUC)), vjust = -0.9, size = 3.4) +
    scale_x_continuous(limits = c(0.45, 0.9), breaks = seq(0.5, 0.9, 0.1)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey70") +
    labs(title = titulo, x = "Out-of-sample ROC-AUC (95% CI)", y = NULL) + tema
}
F3 <- f3_panel("PRE clinical + blood count", "A. Pre-treatment (clinical + blood count)") +
      f3_panel("EoTx clinical + blood count", "B. End of treatment (clinical + blood count)")
ggsave(file.path(destf, "Figure3_AUC_all_models.png"), F3, width = 11, height = 4.2, dpi = 300)
message("[92] F3 ok")

## ---------- F4: valor incremental (modelo clave = logistica parsimoniosa)
lg <- mm[mm$Model == "Parsimonious logistic", ]
f4 <- data.frame(
  Moment = c("Pre-treatment","Pre-treatment","End of treatment","End of treatment","Combined\nPre + EoTx"),
  Set    = c("Clinical only","Clinical + blood count","Clinical only","Clinical + blood count","Clinical + blood count"),
  AUC = NA, lo = NA, hi = NA)
pick <- function(s){ r<-lg[lg$Scenario==s,]; c(r$ROC_AUC, r$AUC_lo, r$AUC_hi) }
f4[1,3:5]<-pick("PRE clinical only");           f4[2,3:5]<-pick("PRE clinical + blood count")
f4[3,3:5]<-pick("EoTx clinical only");          f4[4,3:5]<-pick("EoTx clinical + blood count")
f4[5,3:5]<-pick("Combined PRE+EoTx blood count")
f4$Moment <- factor(f4$Moment, levels = c("Pre-treatment","End of treatment","Combined\nPre + EoTx"))
f4$Set <- factor(f4$Set, levels = c("Clinical only","Clinical + blood count"))
F4 <- ggplot(f4, aes(x = Moment, y = AUC, fill = Set)) +
  geom_col(position = position_dodge(0.7), width = 0.62, color = "grey30") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.7), width = 0.18) +
  geom_text(aes(label = sprintf("%.2f", AUC)), position = position_dodge(0.7), vjust = -0.6, size = 3.4) +
  scale_fill_manual(values = c("Clinical only" = "#AEB6BF", "Clinical + blood count" = "#C0392B"), name = NULL) +
  coord_cartesian(ylim = c(0.5, 0.85)) +
  labs(title = "Incremental value of blood count parameters (parsimonious logistic model)",
       x = NULL, y = "Out-of-sample ROC-AUC (95% CI)") + tema
ggsave(file.path(destf, "Figure4_incremental_value.png"), F4, width = 9, height = 5, dpi = 300)
message("[92] F4 ok")

## ---------- F5: SHAP (XGBoost) PRE/EoTx, + RF si treeshap -------------
shap_panel <- function(csv, titulo, topn = 10) {
  s <- read.csv(csv, check.names = TRUE)
  s$dir <- ifelse(grepl("^\\+", s$direction), "Higher -> more failure", "Higher -> less failure")
  s <- s[order(-s$mean_abs_shap), ][seq_len(min(topn, nrow(s))), ]
  s$feature <- factor(s$feature, levels = rev(s$feature))
  ggplot(s, aes(x = mean_abs_shap, y = feature, fill = dir)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = c("Higher -> more failure" = col_up, "Higher -> less failure" = col_down), name = NULL) +
    labs(title = titulo, x = "Mean |SHAP| (impact on predicted failure)", y = NULL) + tema
}
## Orden pedido por D. Rebellon: random forest antes que XGBoost.
F5 <- (shap_panel("outputs/70_interpretability_shap/rf_shap_importance_pre.csv",  "A. Random forest - Pre-treatment") +
       shap_panel("outputs/70_interpretability_shap/rf_shap_importance_eotx.csv", "B. Random forest - End of treatment")) /
      (shap_panel("outputs/70_interpretability_shap/shap_importance_pre.csv",  "C. XGBoost - Pre-treatment") +
       shap_panel("outputs/70_interpretability_shap/shap_importance_eotx.csv", "D. XGBoost - End of treatment"))
ggsave(file.path(destf, "Figure5_SHAP_ml.png"), F5, width = 12, height = 9, dpi = 300)
message("[92] F5 (xgb+rf) ok")

## ---------- F6: forest plot de RR ajustados (modelos finales) --------
rr <- readRDS("outputs/21_logistic_multivariate/RR_models.rds")
etq <- c("sexoMale"="Male sex","edad_c"="Age (per year)","edad"="Age (per year)",
         "adenopatia_evalbasYes"="Lymphadenopathy (baseline)",
         "infeccion_concom_evalbasYes"="Concomitant infection (baseline)",
         "infeccion_concom_fttoYes"="Concomitant infection (EoTx)",
         "ig_bajoLow (<=cut)"="Immature granulocytes <=0.01 (pre)",
         "plaq_bajoLow (<=cut)"="Low platelets (pre)",
         "eos_alto>=cut"="Eosinophils >=14% (EoTx)",
         "mono_sube>=cut"="Monocyte ratio >=1.07 (EoTx)",
         "tratamientoMiltefosine"="Miltefosine (vs Glucantime)")
prep <- function(df, momento) {
  df <- df[df$term != "(Intercept)", ]
  df$lab <- ifelse(df$term %in% names(etq), etq[df$term], df$term)
  df$Moment <- momento; df
}
fdf <- rbind(prep(rr$pre, "Pre-treatment model"), prep(rr$eotx, "EoTx model"))
fdf$lab <- factor(fdf$lab, levels = rev(unique(fdf$lab)))
F6 <- ggplot(fdf, aes(x = RR, y = lab)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, color = "grey45") +
  geom_point(aes(color = RR > 1), size = 3) +
  scale_color_manual(values = c("TRUE" = col_up, "FALSE" = col_down), guide = "none") +
  scale_x_log10(breaks = c(0.25,0.5,1,2,4,8)) +
  facet_wrap(~Moment, scales = "free_y", ncol = 1) +
  labs(title = "Adjusted risk ratios for therapeutic failure (final multivariate models)",
       x = "Adjusted RR (log scale, 95% CI)", y = NULL) + tema
ggsave(file.path(destf, "Figure6_forest_RR.png"), F6, width = 9, height = 6.5, dpi = 300)
message("[92] F6 ok")

## ---------- F7: dinamica de parametros por desenlace -----------------
d <- read.csv("data/Clean_data_csv/final_df_cleaned.csv", check.names = TRUE)
d$Outcome <- factor(ifelse(d$estado_final == "Therapeutic failure", "Failure", "Cure"), levels = c("Cure","Failure"))
tot <- function(s) d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]
d$eos_pct_pre  <- 100*d$Eosinofilos_pre/tot("pre")
d$eos_pct_post <- 100*d$Eosinofilos_post/tot("post")
long_pp <- function(pre, post, nombre) {
  rbind(data.frame(Outcome=d$Outcome, Moment="Pre",  val=d[[pre]],  Param=nombre),
        data.frame(Outcome=d$Outcome, Moment="EoTx", val=d[[post]], Param=nombre))
}
pp <- rbind(
  long_pp("eos_pct_pre","eos_pct_post","Eosinophils (%)"),
  long_pp("Monocitos_pre","Monocitos_post","Monocytes (x10^3/uL)"),
  long_pp("Granulocitos_pre","Granulocitos_post","Immature granulocytes (x10^3/uL)")
)
pp$Moment <- factor(pp$Moment, levels = c("Pre","EoTx"))
p_dyn <- ggplot(pp[is.finite(pp$val), ], aes(x = Moment, y = val, fill = Outcome)) +
  geom_boxplot(outlier.size = 0.6, width = 0.6) +
  scale_fill_manual(values = c("Cure" = col_cure, "Failure" = col_fail), name = NULL) +
  facet_wrap(~Param, scales = "free_y", nrow = 1) +
  labs(title = "A. Blood count dynamics by therapeutic outcome", x = NULL, y = NULL) + tema
plt <- data.frame(Outcome = d$Outcome, val = d$Plaquetas_pre)
p_plt <- ggplot(plt[is.finite(plt$val), ], aes(x = Outcome, y = val, fill = Outcome)) +
  geom_boxplot(outlier.size = 0.6, width = 0.5) +
  scale_fill_manual(values = c("Cure" = col_cure, "Failure" = col_fail), guide = "none") +
  labs(title = "B. Pre-treatment platelets", x = NULL, y = "Platelets (x10^3/uL)") + tema
F7 <- p_dyn / p_plt + plot_layout(heights = c(1, 0.9))
ggsave(file.path(destf, "Figure7_dynamics_by_outcome.png"), F7, width = 11, height = 8, dpi = 300)
message("[92] F7 ok")
message("[92] DONE. Figuras en ", destf)
