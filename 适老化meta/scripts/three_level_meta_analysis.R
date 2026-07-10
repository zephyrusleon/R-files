#!/usr/bin/env Rscript
# =============================================================================
# Three-Level Meta-Analysis: Urban Built Environment and Physical Activity
# among Community-Dwelling Older Adults
# =============================================================================

# ---- Setup ----
rm(list = ls())
library(readxl)
library(metafor)
library(dplyr)
library(ggplot2)

# ---- Load data ----
data <- read_excel("data.xlsx", sheet = "Sheet1")
# Keep only rows with valid study_id
data <- data[!is.na(data[[1]]), ]
# Ensure numeric columns are numeric
data$study_id <- as.numeric(data[[1]])
data$esid     <- as.numeric(data[[2]])
data$OR       <- as.numeric(data[[16]])
data$lower_ci <- as.numeric(data[[17]])
data$upper_ci <- as.numeric(data[[18]])

# Remove rows missing key info
data <- data[!is.na(data$study_id) & !is.na(data$OR), ]

# ---- Compute logOR and sampling variance ----
# If logOR column (col 19, 0-indexed = 18) already computed, use it
# Otherwise compute from OR and CI
logor_col <- as.numeric(data[[19]])
vi_col     <- as.numeric(data[[39]])  # col 38 in 0-indexed = col 39 in R

data$logOR <- ifelse(!is.na(logor_col), logor_col, log(data$OR))
data$vi    <- ifelse(!is.na(vi_col), vi_col,
                     ((log(data$upper_ci) - log(data$lower_ci)) / (2 * 1.96))^2)

# Remove rows where vi cannot be computed
data <- data[!is.na(data$vi) & data$vi > 0, ]

# Study label
data$Study <- data[[3]]

cat("\n=== DATA SUMMARY ===\n")
cat("Studies:", length(unique(data$study_id)), "\n")
cat("Effect sizes:", nrow(data), "\n")
cat("ES per study:\n")
print(table(data$study_id))

# ---- Three-Level Meta-Analysis Model ----
# Level 3: Between-study variance
# Level 2: Within-study (between-ES) variance
# Level 1: Sampling variance (vi)

m_multi <- rma.mv(
  yi     = logOR,
  V      = vi,
  random = ~ 1 | study_id / esid,
  method = "REML",
  test   = "t",
  dfs    = "contain",
  data   = data
)

cat("\n\n========== THREE-LEVEL MODEL ==========\n")
print(summary(m_multi))

# Convert to OR scale for reporting
pooled_OR  <- exp(m_multi$b)
pooled_ci_lb <- exp(m_multi$ci.lb)
pooled_ci_ub <- exp(m_multi$ci.ub)

cat("\n--- Pooled Effect on OR Scale ---\n")
cat(sprintf("Pooled OR = %.3f [%.3f, %.3f]\n", pooled_OR, pooled_ci_lb, pooled_ci_ub))

# ---- Variance Decomposition (I²) ----
# Manual computation of I² for three-level models
# Total variance = sigma2_level3 + sigma2_level2 + typical_sampling_variance
sigma2_L3 <- m_multi$sigma2[1]  # between-study
sigma2_L2 <- m_multi$sigma2[2]  # within-study (between-ES)
# Typical sampling variance (Higgins & Thompson, 2002)
# Using the formula: v_typical = (k-1) / sum(1/vi)
k <- nrow(data)
v_typical <- (k - 1) / sum(1 / data$vi)

total_var <- sigma2_L3 + sigma2_L2 + v_typical
I2_L3 <- sigma2_L3 / total_var * 100  # % between studies
I2_L2 <- sigma2_L2 / total_var * 100  # % within studies
I2_L1 <- v_typical / total_var * 100   # % sampling error

cat("\n\n========== VARIANCE DECOMPOSITION (I^2) ==========\n")
cat(sprintf("Level 3 (Between-study):  sigma^2 = %.4f, I^2 = %.1f%%\n", sigma2_L3, I2_L3))
cat(sprintf("Level 2 (Within-study):   sigma^2 = %.4f, I^2 = %.1f%%\n", sigma2_L2, I2_L2))
cat(sprintf("Level 1 (Sampling error): v_type = %.4f, I^2 = %.1f%%\n", v_typical, I2_L1))
cat(sprintf("Total variance:           %.4f\n", total_var))

# ---- Likelihood Ratio Tests ----
# Proper LRT: compare full 3-level model to reduced models with one variance
# component fixed at 0. Both models use identical random structure; only
# difference is whether one sigma^2 is constrained.
# Reference: Assink & Wibbelink (2016), The Quantitative Methods for Psychology

# Test Level 3 (sigma^2_between-study = 0)
# Fix sigma^2_1 (study level) at 0, freely estimate sigma^2_2 (ES level)
m_noL3 <- rma.mv(
  yi = logOR, V = vi,
  random = ~ 1 | study_id / esid,
  method = "REML", test = "t", dfs = "contain",
  data = data,
  sigma2 = c(0, NA)
)

# Test Level 2 (sigma^2_within-study = 0)
# Freely estimate sigma^2_1, fix sigma^2_2 at 0
m_noL2 <- rma.mv(
  yi = logOR, V = vi,
  random = ~ 1 | study_id / esid,
  method = "REML", test = "t", dfs = "contain",
  data = data,
  sigma2 = c(NA, 0)
)

# Compute LRT statistics using logLik()
ll_full  <- as.numeric(logLik(m_multi))
ll_noL3  <- as.numeric(logLik(m_noL3))
ll_noL2  <- as.numeric(logLik(m_noL2))

lrt_L3 <- max(0, -2 * (ll_noL3 - ll_full))  # guard against floating-point negatives
lrt_L2 <- max(0, -2 * (ll_noL2 - ll_full))

# One-sided p-values: variance is bounded at 0, so the test statistic follows
# a 50:50 mixture of chi^2_0 and chi^2_1 (Self & Liang, 1987)
p_L3 <- if (lrt_L3 < 1e-8) 1.0 else 0.5 * pchisq(lrt_L3, df = 1, lower.tail = FALSE)
p_L2 <- if (lrt_L2 < 1e-8) 1.0 else 0.5 * pchisq(lrt_L2, df = 1, lower.tail = FALSE)

cat("\n\n========== LIKELIHOOD RATIO TESTS ==========\n")
cat(sprintf("Full model (3-level) logLik = %.4f (df = 3)\n", ll_full))
cat(sprintf("No-Level-3 model logLik    = %.4f (df = 2, sigma^2_3 fixed at 0)\n", ll_noL3))
cat(sprintf("No-Level-2 model logLik    = %.4f (df = 2, sigma^2_2 fixed at 0)\n", ll_noL2))
cat(sprintf("\nLRT for Level 3 (between-study) variance:\n"))
cat(sprintf("  chi^2(1) = %.4f, p = %.4f\n", lrt_L3, p_L3))
cat(sprintf("  %s\n", ifelse(p_L3 < 0.05,
  "→ Level 3 variance is significant — retain 3-level structure.",
  "→ Level 3 variance NOT significant — 2-level model may suffice.")))
cat(sprintf("\nLRT for Level 2 (within-study, between-ES) variance:\n"))
cat(sprintf("  chi^2(1) = %.4f, p = %.4f\n", lrt_L2, p_L2))
cat(sprintf("  %s\n", ifelse(p_L2 < 0.05,
  "→ Level 2 variance is significant — retain within-study heterogeneity component.",
  "→ Level 2 variance NOT significant — simple random-effects model may suffice.")))

# ---- Study-level pooled effects ----
cat("\n\n========== STUDY-LEVEL ORs ==========\n")
for (sid in sort(unique(data$study_id))) {
  dd <- data[data$study_id == sid, ]
  n_es <- nrow(dd)
  if (n_es == 1) {
    s_or <- dd$OR
    s_lo <- dd$lower_ci
    s_hi <- dd$upper_ci
  } else {
    sm <- rma(yi = dd$logOR, vi = dd$vi, method = "REML")
    s_or <- exp(sm$b)
    s_lo <- exp(sm$ci.lb)
    s_hi <- exp(sm$ci.ub)
  }
  cat(sprintf("  %s (sid=%d): OR=%.3f [%.3f, %.3f], k=%d\n",
              dd$Study[1], sid, s_or, s_lo, s_hi, n_es))
}

# =============================================================================
# FIGURES
# =============================================================================
dir.create("figures", showWarnings = FALSE)

# ---- Figure 1: Forest Plot of All Effect Sizes ----
cat("\n\nGenerating forest plot...\n")

# Prepare data: compute per-study summary via REML
studies <- unique(data$study_id)
forest_data <- data.frame(
  study_id = integer(),
  author   = character(),
  k        = integer(),
  OR       = numeric(),
  lower    = numeric(),
  upper    = numeric(),
  weight   = numeric(),
  stringsAsFactors = FALSE
)

for (sid in sort(studies)) {
  dd <- data[data$study_id == sid, ]
  if (nrow(dd) == 1) {
    s_or  <- dd$OR[1]
    s_lo  <- dd$lower_ci[1]
    s_hi  <- dd$upper_ci[1]
    s_wt  <- 1 / dd$vi[1]
  } else {
    sm <- rma(yi = dd$logOR, vi = dd$vi, method = "REML")
    s_or <- exp(sm$b)
    s_lo <- exp(sm$ci.lb)
    s_hi <- exp(sm$ci.ub)
    s_wt <- 1 / sm$se^2
  }
  forest_data <- rbind(forest_data, data.frame(
    study_id = sid,
    author   = dd$Study[1],
    k        = nrow(dd),
    OR       = s_or,
    lower    = s_lo,
    upper    = s_hi,
    weight   = s_wt,
    stringsAsFactors = FALSE
  ))
}

# Convert OR to log scale for symmetric display
forest_data$logOR  <- log(forest_data$OR)
forest_data$logL   <- log(forest_data$lower)
forest_data$logU   <- log(forest_data$upper)

# Add overall pooled estimate
forest_data <- rbind(forest_data, data.frame(
  study_id = NA, author = "RE Model", k = NA,
  OR = pooled_OR, lower = pooled_ci_lb, upper = pooled_ci_ub,
  weight = NA,
  logOR = log(pooled_OR), logL = log(pooled_ci_lb), logU = log(pooled_ci_ub),
  stringsAsFactors = FALSE
))

forest_data$author <- factor(forest_data$author, levels = rev(forest_data$author))
forest_data$label  <- paste0(forest_data$author, " (k = ", forest_data$k, ")")
forest_data$label[is.na(forest_data$k)] <- "RE Model"
forest_data$label <- factor(forest_data$label, levels = rev(forest_data$label))

# Determine x-axis limits
xmin <- min(forest_data$logL, na.rm = TRUE) - 0.5
xmax <- max(forest_data$logU, na.rm = TRUE) + 0.5

p_forest <- ggplot(forest_data, aes(x = logOR, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(size = weight), shape = 15, na.rm = TRUE) +
  geom_errorbarh(aes(xmin = logL, xmax = logU), height = 0.2, size = 0.8) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  scale_x_continuous(
    breaks = log(c(0.5, 0.75, 1, 1.5, 2, 3, 5, 10, 20)),
    labels = c("0.5", "0.75", "1", "1.5", "2", "3", "5", "10", "20")
  ) +
  labs(
    title = "Forest Plot: Built Environment and Physical Activity",
    subtitle = "Three-level meta-analysis (OR scale)",
    x = "Odds Ratio (log scale)", y = ""
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40")
  )

ggsave("figures/forest_plot.png", p_forest, width = 10, height = 6, dpi = 150)
cat("Saved: figures/forest_plot.png\n")

# ---- Figure 2: Funnel Plot (contour-enhanced) ----
cat("Generating funnel plot...\n")

# Standard errors of individual ES
data$yi <- data$logOR
data$sei <- sqrt(data$vi)

# Create funnel data with pseudo-confidence contours
funnel_data <- data.frame(
  yi = data$yi,
  sei = data$sei
)

p_funnel <- ggplot(funnel_data, aes(x = yi, y = sei)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = m_multi$b, linetype = "dashed", color = "red", size = 0.8) +
  # Add pseudo confidence region lines for the pooled estimate
  stat_function(
    fun = function(x) abs(x - m_multi$b) / 1.96,
    xlim = range(funnel_data$yi),
    color = "grey60", linetype = "dotted"
  ) +
  stat_function(
    fun = function(x) abs(x - m_multi$b) / 1.96 * 1.5,
    xlim = range(funnel_data$yi),
    color = "grey70", linetype = "dotted"
  ) +
  scale_y_reverse() +
  labs(
    title = "Funnel Plot",
    subtitle = "Each point = one effect size; red line = pooled estimate",
    x = "Log Odds Ratio",
    y = "Standard Error"
  ) +
  theme_bw()

ggsave("figures/funnel_plot.png", p_funnel, width = 8, height = 6, dpi = 150)
cat("Saved: figures/funnel_plot.png\n")

# ---- Figure 3: Baujat Plot (heterogeneity contribution) ----
cat("Generating Baujat plot...\n")

baujat_data <- data.frame(
  es_id = seq_len(nrow(data)),
  study = data$Study,
  yi    = data$yi,
  vi    = data$vi
)

# Compute influence for each ES
# Contribution to heterogeneity: (yi - mu)^2 / vi
baujat_data$squared_pearson_resid <- (baujat_data$yi - m_multi$b)^2 / baujat_data$vi

# Influence on pooled estimate: leave-one-out style
baujat_data$influence <- NA
for (i in seq_len(nrow(baujat_data))) {
  d_sub <- baujat_data[-i, ]
  m_sub <- tryCatch({
    rma.mv(yi = yi, V = vi, random = ~ 1 | study_id / esid,
           method = "REML", test = "t", dfs = "contain",
           data = data[-i, ])
  }, error = function(e) NULL)
  if (!is.null(m_sub)) {
    baujat_data$influence[i] <- abs(m_sub$b - m_multi$b)
  }
}

p_baujat <- ggplot(baujat_data, aes(x = influence, y = squared_pearson_resid)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(
    data = subset(baujat_data, squared_pearson_resid > quantile(squared_pearson_resid, 0.9, na.rm = TRUE)),
    aes(label = paste(study, "ES", es_id)), size = 3, hjust = -0.1, vjust = -0.5
  ) +
  labs(
    title = "Baujat Plot: Heterogeneity Contribution",
    x = "Influence on Pooled Estimate",
    y = "Squared Pearson Residual (Contribution to Heterogeneity)"
  ) +
  theme_bw()

ggsave("figures/baujat_plot.png", p_baujat, width = 9, height = 6, dpi = 150)
cat("Saved: figures/baujat_plot.png\n")

# ---- Figure 4: Caterpillar Plot of Individual Effect Sizes ----
cat("Generating caterpillar plot...\n")

caterpillar_data <- data.frame(
  es_id = seq_len(nrow(data)),
  author = paste0(data$Study, " [ES", data$esid, "]"),
  study_id = data$study_id,
  yi = data$yi,
  lower = data$yi - 1.96 * data$sei,
  upper = data$yi + 1.96 * data$sei
)
caterpillar_data <- caterpillar_data[order(caterpillar_data$yi), ]
caterpillar_data$order <- seq_len(nrow(caterpillar_data))

p_caterpillar <- ggplot(caterpillar_data, aes(x = yi, y = order)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = m_multi$b, color = "red", size = 0.8) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, size = 0.6) +
  scale_y_continuous(breaks = caterpillar_data$order, labels = caterpillar_data$author) +
  labs(
    title = "Caterpillar Plot: All Effect Sizes",
    subtitle = paste0("Red line = pooled logOR (", round(m_multi$b, 3), ")"),
    x = "Log Odds Ratio", y = ""
  ) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))

ggsave("figures/caterpillar_plot.png", p_caterpillar, width = 10, height = 10, dpi = 150)
cat("Saved: figures/caterpillar_plot.png\n")

# =============================================================================
# Export results tables
# =============================================================================

# Model summary
sink("output/model_summary.txt")
cat("Three-Level Meta-Analysis Model Summary\n")
cat("========================================\n\n")
print(summary(m_multi))
cat("\n\nPooled OR = ", round(pooled_OR, 4),
    " [", round(pooled_ci_lb, 4), ", ", round(pooled_ci_ub, 4), "]\n", sep = "")
cat("\n--- Variance Decomposition ---\n")
cat(sprintf("Level 3 (Between-study): sigma^2 = %.6f, I^2 = %.1f%%\n", sigma2_L3, I2_L3))
cat(sprintf("Level 2 (Within-study):  sigma^2 = %.6f, I^2 = %.1f%%\n", sigma2_L2, I2_L2))
cat(sprintf("Level 1 (Sampling):      v_typ  = %.6f, I^2 = %.1f%%\n", v_typical, I2_L1))
cat(sprintf("Total:                   %.6f\n", total_var))
sink()
cat("Saved: output/model_summary.txt\n")

# Study-level table
study_summary <- forest_data[!is.na(forest_data$study_id), ]
write.csv(study_summary, "output/study_level_effects.csv", row.names = FALSE)
cat("Saved: output/study_level_effects.csv\n")

# =============================================================================
# Done
# =============================================================================
cat("\n\n=== ANALYSIS COMPLETE ===\n")
cat("Output files:\n")
cat("  figures/forest_plot.png\n")
cat("  figures/funnel_plot.png\n")
cat("  figures/baujat_plot.png\n")
cat("  figures/caterpillar_plot.png\n")
cat("  output/model_summary.txt\n")
cat("  output/study_level_effects.csv\n")
