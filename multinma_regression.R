suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(multinma)
  library(coda)
})

# ============================================================
# Progress / timing helpers
# ============================================================
t_start_all <- Sys.time()

step_msg <- function(txt) {
  cat("\n", strrep("=", 72), "\n", txt, "\n", strrep("=", 72), "\n", sep = "")
}

step_time <- function(t0, label) {
  dt <- difftime(Sys.time(), t0, units = "mins")
  cat(sprintf("[DONE] %s in %.2f min\n", label, as.numeric(dt)))
}

step_run <- function(label, expr) {
  step_msg(label)
  t0 <- Sys.time()
  val <- force(expr)
  step_time(t0, label)
  val
}

# ============================================================
# 0. Paths
# ============================================================
base_dir <- "C:/Users/fredr/OneDrive/Desktop/nma_project/mavranezouli"
in_data  <- file.path(base_dir, "clean_data", "combined_long_mean_change_dataset_ms.csv")
in_map   <- file.path(base_dir, "clean_data", "trt_to_class_ms.csv")
out_dir  <- file.path(base_dir, "multinma_class_strict_smd_metareg")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ============================================================
# 1. Read data
# ============================================================
dat <- read_csv(in_data, show_col_types = FALSE)
trt_map <- read_delim(in_map, delim = ";", col_types = cols(.default = col_character()))

# ============================================================
# 2. Convert types
# ============================================================
dat <- dat %>%
  mutate(
    na = as.numeric(na),
    arm = as.numeric(arm),
    treatment = as.numeric(treatment),
    n = as.numeric(n),
    mean_change = as.numeric(mean_change),
    sd_change = as.numeric(sd_change),
    source = as.character(source)
  )

trt_map <- trt_map %>%
  mutate(
    trtcode = as.numeric(trtcode),
    classcode = as.numeric(classcode)
  )

# ============================================================
# 3. Deterministic treatment -> class mapping
#    (same as your gemtc script)
# ============================================================
trt_map <- trt_map %>%
  mutate(
    classcode = case_when(
      trtcode == 1 ~ 1, trtcode == 2 ~ 2, trtcode == 3 ~ 3, trtcode == 4 ~ 4, trtcode == 5 ~ 5,
      trtcode == 6 ~ 6, trtcode == 7 ~ 7, trtcode %in% c(8, 9) ~ 8,
      trtcode %in% c(10, 11, 12, 13) ~ 9, trtcode == 14 ~ 10, trtcode == 15 ~ 11, trtcode == 16 ~ 12,
      trtcode == 17 ~ 13, trtcode == 18 ~ 14, trtcode == 19 ~ 15,
      trtcode %in% c(20, 21, 22, 23) ~ 16, trtcode %in% c(24, 25, 26, 27) ~ 17,
      trtcode %in% c(28, 29) ~ 18, trtcode == 30 ~ 19, trtcode == 31 ~ 20, trtcode == 32 ~ 21,
      trtcode == 33 ~ 22, trtcode %in% c(34, 35) ~ 23, trtcode == 36 ~ 24, trtcode == 37 ~ 25,
      trtcode == 38 ~ 26, trtcode %in% c(39, 40, 41, 42, 43, 44) ~ 27,
      trtcode %in% c(45, 46, 47, 48, 49, 50) ~ 28, trtcode %in% c(51, 52) ~ 29,
      trtcode == 53 ~ 30, trtcode %in% c(54, 55, 56) ~ 31, trtcode %in% c(57, 58, 59) ~ 32,
      trtcode %in% c(60, 61, 62) ~ 33, trtcode %in% c(63, 64) ~ 34, trtcode == 65 ~ 35,
      trtcode == 66 ~ 36, trtcode %in% c(67, 68) ~ 37,
      trtcode %in% c(69, 70, 71, 72, 73, 74, 75) ~ 38, trtcode == 76 ~ 39,
      trtcode %in% c(77, 78) ~ 40, trtcode %in% c(79, 80, 81) ~ 41, trtcode %in% c(82, 83) ~ 42,
      trtcode == 84 ~ 43, trtcode == 85 ~ 44, trtcode == 86 ~ 45,
      trtcode %in% c(87, 88, 89) ~ 46, trtcode %in% c(90, 91) ~ 47, trtcode == 92 ~ 48,
      trtcode %in% c(93, 94, 95, 96, 97) ~ 49, trtcode %in% c(98, 99) ~ 50,
      TRUE ~ NA_real_
    ),
    class = case_when(
      trtcode == 1 ~ "Placebo",
      trtcode == 2 ~ "Attention placebo",
      trtcode == 3 ~ "No treatment",
      trtcode == 4 ~ "Waitlist",
      trtcode == 5 ~ "TAU",
      trtcode == 6 ~ "Mirtazapine",
      trtcode == 7 ~ "Trazodone",
      trtcode %in% c(8, 9) ~ "Behavioural therapies individual",
      trtcode %in% c(10, 11, 12, 13) ~ "Cognitive and cognitive behavioural therapies individual",
      trtcode == 14 ~ "Cognitive and cognitive behavioural therapies group",
      trtcode == 15 ~ "Problem solving individual",
      trtcode == 16 ~ "Problem solving group",
      trtcode == 17 ~ "Counselling individual",
      trtcode == 18 ~ "Interpersonal psychotherapy (IPT) individual",
      trtcode == 19 ~ "Psychoeducation group",
      trtcode %in% c(20, 21, 22, 23) ~ "Self-help",
      trtcode %in% c(24, 25, 26, 27) ~ "Self-help with support",
      trtcode %in% c(28, 29) ~ "Short-term psychodynamic psychotherapies individual",
      trtcode == 30 ~ "Music therapy group",
      trtcode == 31 ~ "Mindfulness or meditation group",
      trtcode == 32 ~ "Peer support group",
      trtcode == 33 ~ "Any psychotherapy",
      trtcode %in% c(34, 35) ~ "Cognitive and cognitive behavioural therapies individual + placebo",
      trtcode == 36 ~ "Interpersonal psychotherapy (IPT) individual + placebo",
      trtcode == 37 ~ "Counselling individual + placebo",
      trtcode == 38 ~ "Relaxation individual + placebo",
      trtcode %in% c(39, 40, 41, 42, 43, 44) ~ "SSRIs",
      trtcode %in% c(45, 46, 47, 48, 49, 50) ~ "TCAs",
      trtcode %in% c(51, 52) ~ "SNRIs",
      trtcode == 53 ~ "Any AD",
      trtcode %in% c(54, 55, 56) ~ "Sham acupuncture",
      trtcode %in% c(57, 58, 59) ~ "Acupuncture",
      trtcode %in% c(60, 61, 62) ~ "Exercise individual",
      trtcode %in% c(63, 64) ~ "Exercise group",
      trtcode == 65 ~ "Yoga group",
      trtcode == 66 ~ "Light therapy",
      trtcode %in% c(67, 68) ~ "Behavioural therapies individual + AD",
      trtcode %in% c(69, 70, 71, 72, 73, 74, 75) ~ "Cognitive and cognitive behavioural therapies individual + AD",
      trtcode == 76 ~ "Cognitive and cognitive behavioural therapies group + AD",
      trtcode %in% c(77, 78) ~ "Interpersonal psychotherapy (IPT) individual + AD",
      trtcode %in% c(79, 80, 81) ~ "Counselling individual + AD",
      trtcode %in% c(82, 83) ~ "Short-term psychodynamic psychotherapies individual + AD",
      trtcode == 84 ~ "Psychoeducation group + AD",
      trtcode == 85 ~ "Peer support group + AD",
      trtcode == 86 ~ "Relaxation individual + AD",
      trtcode %in% c(87, 88, 89) ~ "Exercise individual + AD",
      trtcode %in% c(90, 91) ~ "Exercise group + AD",
      trtcode == 92 ~ "Yoga group + AD",
      trtcode %in% c(93, 94, 95, 96, 97) ~ "Acupuncture + AD",
      trtcode %in% c(98, 99) ~ "Light therapy + AD",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 4. Join + clean + collapse same-class duplicate arms
# ============================================================
collapse_source <- function(x) {
  ux <- unique(na.omit(x))
  if (length(ux) == 0) return(NA_character_)
  if (length(ux) == 1) return(ux)
  "mixed_source"
}

dat_class <- dat %>%
  left_join(trt_map %>% select(treatment = trtcode, class), by = "treatment") %>%
  filter(
    !is.na(studyid), !is.na(class),
    !is.na(n), n > 0,
    !is.na(mean_change),
    !is.na(sd_change), sd_change > 0
  )

collapsed <- dat_class %>%
  group_by(studyid, class) %>%
  summarise(
    n = sum(n),
    mean_change = sum(mean_change * n) / sum(n),
    sd_change = sqrt(sum((n - 1) * sd_change^2) / sum(n - 1)),
    source = collapse_source(source),
    .groups = "drop"
  ) %>%
  group_by(studyid) %>%
  filter(n_distinct(class) >= 2) %>%
  ungroup()

stopifnot("Placebo" %in% collapsed$class)

# ============================================================
# 5. Strict within-study standardization
# ============================================================
study_sd <- collapsed %>%
  group_by(studyid) %>%
  summarise(
    pooled_sd_study = sqrt(sum((n - 1) * sd_change^2, na.rm = TRUE) /
                             sum((n - 1), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(!is.na(pooled_sd_study), pooled_sd_study > 0)

collapsed_std <- collapsed %>%
  inner_join(study_sd, by = "studyid") %>%
  mutate(
    z_mean_change = mean_change / pooled_sd_study,
    z_sd_change = 1
  ) %>%
  filter(is.finite(z_mean_change), abs(z_mean_change) <= 8) %>%
  group_by(studyid) %>%
  filter(n_distinct(class) >= 2) %>%
  ungroup()

# ============================================================
# 6. Safe treatment IDs + study-level covariates
# ============================================================
trt_lookup <- collapsed_std %>%
  distinct(class) %>%
  arrange(class) %>%
  mutate(trt = sprintf("trt_%03d", row_number())) %>%
  select(trt, class)

placebo_trt <- trt_lookup %>% filter(class == "Placebo") %>% pull(trt)
if (length(placebo_trt) != 1) stop("Could not uniquely identify Placebo.")

analysis_df <- collapsed_std %>%
  left_join(trt_lookup, by = "class") %>%
  group_by(studyid) %>%
  mutate(
    source_study = collapse_source(source),
    src_binary = ifelse(source_study == "binary_response_converted", 1, 0),
    src_basefu = ifelse(source_study == "baseline_followup", 1, 0)
  ) %>%
  ungroup() %>%
  transmute(
    study = as.character(studyid),
    trt = trt,
    y = z_mean_change,
    se = z_sd_change / sqrt(n),   # = 1/sqrt(n) after strict standardization
    n = n,
    src_binary = as.numeric(src_binary),
    src_basefu = as.numeric(src_basefu)
  )

# ============================================================
# 7. Build multinma network (arm-based normal outcome)
# ============================================================
net_data <- analysis_df %>%
  left_join(trt_lookup, by = "trt") %>%
  rename(trt_class = class)

net <- set_agd_arm(
  data = net_data,
  study = study,
  trt = trt,
  y = y,
  se = se,
  sample_size = n,
  trt_class = trt_class,
  trt_ref = placebo_trt
)

# ============================================================
# 8. Fit models (Stan) -- patched controls for treedepth/ESS
# ============================================================

fit_main <- step_run("Fitting main NMA model", nma(
  net,
  trt_effects = "random",
  prior_intercept = normal(scale = 10),
  prior_trt = normal(scale = 5),
  prior_het = half_normal(scale = 1),
  chains = 4, iter = 8000, warmup = 4000, thin = 1,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  seed = 20260813,
  refresh = 50
))

fit_reg_binary <- step_run("Fitting meta-regression: src_binary", nma(
  net,
  trt_effects = "random",
  regression = ~ src_binary,
  prior_reg = normal(scale = 2),
  prior_intercept = normal(scale = 10),
  prior_trt = normal(scale = 5),
  prior_het = half_normal(scale = 1),
  chains = 4, iter = 8000, warmup = 4000, thin = 1,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  seed = 20260813,
  refresh = 50
))

fit_reg_basefu <- step_run("Fitting meta-regression: src_basefu", nma(
  net,
  trt_effects = "random",
  regression = ~ src_basefu,
  prior_reg = normal(scale = 2),
  prior_intercept = normal(scale = 10),
  prior_trt = normal(scale = 5),
  prior_het = half_normal(scale = 1),
  chains = 4, iter = 8000, warmup = 4000, thin = 1,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  seed = 20260813,
  refresh = 50
))

# ============================================================
# 9. Relative effects vs placebo
# ============================================================

rel_main <- step_run("Computing relative effects: main", 
                     relative_effects(fit_main, trt_ref = placebo_trt))

rel_bin_0 <- relative_effects(
  fit_reg_binary,
  trt_ref = placebo_trt,
  newdata = data.frame(src_binary = 0)
)

rel_bin_1 <- relative_effects(
  fit_reg_binary,
  trt_ref = placebo_trt,
  newdata = data.frame(src_binary = 1)
)

rel_bfu_0 <- relative_effects(
  fit_reg_basefu,
  trt_ref = placebo_trt,
  newdata = data.frame(src_basefu = 0)
)

rel_bfu_1 <- relative_effects(
  fit_reg_basefu,
  trt_ref = placebo_trt,
  newdata = data.frame(src_basefu = 1)
)

# ============================================================
# 10. Ranking outputs (version-robust)
# ============================================================

rk_main  <- step_run("Computing posterior ranks: main", 
                     posterior_ranks(fit_main, lower_better = TRUE))

if ("rank_probs" %in% getNamespaceExports("multinma")) {
  rk_prob_main <- multinma::rank_probs(rk_main) %>%
    as_tibble()
} else {
  # Fallback if rank_probs() is unavailable in installed multinma version
  rk_df <- as.data.frame(rk_main)
  trts <- names(rk_df)
  k <- length(trts)
  
  rk_prob_main <- bind_rows(lapply(trts, function(tt) {
    tibble(
      trt = tt,
      rank = 1:k,
      prob = sapply(1:k, function(r) mean(rk_df[[tt]] == r, na.rm = TRUE))
    )
  }))
}

calc_sucra <- function(rank_probs_df) {
  k <- max(rank_probs_df$rank, na.rm = TRUE)
  rank_probs_df %>%
    group_by(trt) %>%
    summarise(
      SUCRA = sum((k - rank) * prob, na.rm = TRUE) / (k - 1),
      .groups = "drop"
    )
}

sucra_main <- calc_sucra(rk_prob_main) %>%
  left_join(trt_lookup, by = "trt") %>%
  arrange(desc(SUCRA))

# ============================================================
# 10b. Basic diagnostics export (recommended)
# ============================================================
diag_main <- summary(fit_main)$summary
diag_bin  <- summary(fit_reg_binary)$summary
diag_bfu  <- summary(fit_reg_basefu)$summary

write_csv(as_tibble(diag_main, rownames = "parameter"), file.path(out_dir, "diag_fit_main.csv"))
write_csv(as_tibble(diag_bin,  rownames = "parameter"), file.path(out_dir, "diag_fit_reg_binary.csv"))
write_csv(as_tibble(diag_bfu,  rownames = "parameter"), file.path(out_dir, "diag_fit_reg_basefu.csv"))

# ============================================================
# 11. Save outputs
# ============================================================
write_csv(trt_lookup, file.path(out_dir, "treatment_legend.csv"))
write_csv(analysis_df, file.path(out_dir, "multinma_analysis_df.csv"))

write_csv(as_tibble(rel_main), file.path(out_dir, "rel_main_vs_placebo.csv"))
write_csv(as_tibble(rel_bin_0), file.path(out_dir, "rel_reg_binary_src0_vs_placebo.csv"))
write_csv(as_tibble(rel_bin_1), file.path(out_dir, "rel_reg_binary_src1_vs_placebo.csv"))
write_csv(as_tibble(rel_bfu_0), file.path(out_dir, "rel_reg_basefu_src0_vs_placebo.csv"))
write_csv(as_tibble(rel_bfu_1), file.path(out_dir, "rel_reg_basefu_src1_vs_placebo.csv"))

write_csv(as_tibble(rk_prob_main), file.path(out_dir, "rank_probs_main.csv"))
write_csv(sucra_main, file.path(out_dir, "sucra_main_labeled.csv"))

capture.output(summary(fit_main), file = file.path(out_dir, "summary_fit_main.txt"))
capture.output(summary(fit_reg_binary), file = file.path(out_dir, "summary_fit_reg_binary.txt"))
capture.output(summary(fit_reg_basefu), file = file.path(out_dir, "summary_fit_reg_basefu.txt"))

saveRDS(
  list(
    trt_lookup = trt_lookup,
    analysis_df = analysis_df,
    net = net,
    fit_main = fit_main,
    fit_reg_binary = fit_reg_binary,
    fit_reg_basefu = fit_reg_basefu,
    rel_main = rel_main,
    rel_bin_0 = rel_bin_0,
    rel_bin_1 = rel_bin_1,
    rel_bfu_0 = rel_bfu_0,
    rel_bfu_1 = rel_bfu_1,
    rk_prob_main = rk_prob_main,
    sucra_main = sucra_main
  ),
  file.path(out_dir, "multinma_metareg_all_objects.rds")
)

cat("\nDone.\n")
cat("Output folder:", out_dir, "\n")
cat("Placebo treatment:", placebo_trt, "\n")
cat("Studies:", dplyr::n_distinct(analysis_df$study), "\n")
cat("Treatments:", dplyr::n_distinct(analysis_df$trt), "\n")