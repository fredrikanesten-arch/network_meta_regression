library(dplyr)
library(readr)
library(netmeta)

# -----------------------------
# 0. Paths (Windows-safe)
# -----------------------------
base_dir <- "C:/Users/fredr/OneDrive/Desktop/nma_project/mavranezouli"
in_data  <- file.path(base_dir, "clean_data", "combined_long_mean_change_dataset_ms.csv")
in_map   <- file.path(base_dir, "clean_data", "trt_to_class_ms.csv")
out_dir  <- file.path(base_dir, "netmeta_class_ms")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -----------------------------
# 1. Read data
# -----------------------------
dat <- read_csv(in_data, show_col_types = FALSE)

trt_map <- read_delim(
  in_map,
  delim = ";",
  col_types = cols(.default = col_character())
)

# -----------------------------
# 2. Convert types
# -----------------------------
dat <- dat %>%
  mutate(
    na = as.numeric(na),
    arm = as.numeric(arm),
    treatment = as.numeric(treatment),
    n = as.numeric(n),
    mean_change = as.numeric(mean_change),
    sd_change = as.numeric(sd_change)
  )

trt_map <- trt_map %>%
  mutate(
    trtcode = as.numeric(trtcode),
    classcode = as.numeric(classcode)
  )

# -----------------------------
# 3. Deterministic treatment -> class mapping
# -----------------------------
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

# -----------------------------
# 4. Join + keep analyzable rows
# -----------------------------
dat_class <- dat %>%
  left_join(
    trt_map %>% select(treatment = trtcode, classcode, class),
    by = "treatment"
  ) %>%
  filter(
    !is.na(studyid),
    !is.na(classcode),
    !is.na(class),
    !is.na(n),
    !is.na(mean_change),
    !is.na(sd_change)
  )

# -----------------------------
# 5. Collapse duplicate same-class arms within study
# -----------------------------
collapsed_class <- dat_class %>%
  group_by(studyid, classcode, class) %>%
  summarise(
    n = sum(n),
    mean_change = sum(mean_change * n) / sum(n),
    sd_change = sqrt(sum((n - 1) * sd_change^2) / sum(n - 1)),
    .groups = "drop"
  ) %>%
  group_by(studyid) %>%
  filter(n_distinct(class) >= 2) %>%
  ungroup()

stopifnot("Placebo" %in% collapsed_class$class)

# -----------------------------
# 6. Pairwise contrasts + NMA
# -----------------------------
pw <- pairwise(
  treat = class,
  mean = mean_change,
  sd = sd_change,
  n = n,
  studlab = studyid,
  data = collapsed_class,
  sm = "SMD"
)

nma <- netmeta(
  TE = pw$TE,
  seTE = pw$seTE,
  treat1 = pw$treat1,
  treat2 = pw$treat2,
  studlab = pw$studlab,
  data = pw,
  sm = "SMD",
  random = TRUE,
  common = FALSE,
  reference.group = "Placebo",
  verbose = TRUE
)

# -----------------------------
# 7. Summary table vs Placebo
# -----------------------------
make_nma_excel_table_full <- function(nma, collapsed_class, ref = "Placebo",
                                      small.values = "good", digits = 2) {
  if (!ref %in% nma$trts) stop("Reference treatment '", ref, "' not found in nma$trts")
  
  n_tbl <- collapsed_class %>%
    group_by(class) %>%
    summarise(N = sum(n, na.rm = TRUE), .groups = "drop") %>%
    rename(Treatment = class) %>%
    mutate(`Treatment class` = Treatment)
  
  TE_mat    <- if (!is.null(nma$TE.random)) nma$TE.random    else nma$TE.common
  lower_mat <- if (!is.null(nma$lower.random)) nma$lower.random else nma$lower.common
  upper_mat <- if (!is.null(nma$upper.random)) nma$upper.random else nma$upper.common
  se_mat    <- if (!is.null(nma$seTE.random)) nma$seTE.random else nma$seTE.common
  
  extract_against_ref <- function(mat, ref) {
    if (ref %in% colnames(mat)) {
      data.frame(Treatment = rownames(mat), value = as.numeric(mat[, ref]), row.names = NULL)
    } else if (ref %in% rownames(mat)) {
      data.frame(Treatment = colnames(mat), value = as.numeric(mat[ref, ]), row.names = NULL)
    } else stop("Reference treatment not found in matrix dimnames.")
  }
  
  eff  <- extract_against_ref(TE_mat, ref)
  low  <- extract_against_ref(lower_mat, ref)
  high <- extract_against_ref(upper_mat, ref)
  se   <- extract_against_ref(se_mat, ref)
  
  effects_tbl <- eff %>%
    rename(SMD = value) %>%
    left_join(rename(low, lower_CI = value), by = "Treatment") %>%
    left_join(rename(high, upper_CI = value), by = "Treatment") %>%
    left_join(rename(se, seTE = value), by = "Treatment") %>%
    mutate(
      SMD = ifelse(Treatment == ref, 0, SMD),
      lower_CI = ifelse(Treatment == ref, 0, lower_CI),
      upper_CI = ifelse(Treatment == ref, 0, upper_CI),
      seTE = ifelse(Treatment == ref, NA, seTE),
      z = ifelse(!is.na(seTE) & seTE > 0, SMD / seTE, NA_real_),
      p_value = ifelse(!is.na(z), 2 * stats::pnorm(-abs(z)), NA_real_)
    )
  
  rnk <- netmeta::netrank(nma, small.values = small.values)
  ps <- if (!is.null(rnk$ranking.random)) rnk$ranking.random else rnk$ranking.common
  
  rank_tbl <- data.frame(
    Treatment = names(ps),
    P_score = as.numeric(ps),
    Rank = rank(-as.numeric(ps), ties.method = "average"),
    row.names = NULL
  )
  
  out <- data.frame(Treatment = nma$trts) %>%
    left_join(n_tbl, by = "Treatment") %>%
    left_join(effects_tbl, by = "Treatment") %>%
    left_join(rank_tbl, by = "Treatment") %>%
    mutate(`Treatment class` = ifelse(is.na(`Treatment class`), Treatment, `Treatment class`))
  
  fmt <- function(x) formatC(x, digits = digits, format = "f")
  out[[paste0("SMD vs ", ref, " (mean, 95% CI)")]] <- ifelse(
    is.na(out$SMD), NA_character_,
    paste0(fmt(out$SMD), " (", fmt(out$lower_CI), " to ", fmt(out$upper_CI), ")")
  )
  
  out %>%
    arrange(Rank) %>%
    select(
      Treatment, `Treatment class`, N, SMD, lower_CI, upper_CI,
      all_of(paste0("SMD vs ", ref, " (mean, 95% CI)")),
      z, p_value, P_score, Rank
    ) %>%
    rename(`lower CI` = lower_CI, `upper CI` = upper_CI, `p-value` = p_value, `P-score` = P_score)
}

summary_table_full <- make_nma_excel_table_full(
  nma = nma,
  collapsed_class = collapsed_class,
  ref = "Placebo"
)

# -----------------------------
# 8. Save key outputs
# -----------------------------
write_csv(summary_table_full, file.path(out_dir, "nma_summary_table_full_v2.csv"))

saveRDS(
  list(dat = dat, trt_map = trt_map, collapsed_class = collapsed_class, pw = pw, nma = nma),
  file = file.path(out_dir, "nma_class_level_analysis_objects.rds")
)

# -----------------------------
# 9. Class mapping consistency checks
# -----------------------------

dat_join_check <- dat %>%
  left_join(trt_map %>% select(treatment = trtcode, classcode, class), by = "treatment")
write_csv(dat_join_check, file.path(out_dir, "dat_join_check.csv"))

unmapped <- dat %>%
  distinct(treatment) %>%
  left_join(trt_map %>% select(treatment = trtcode, classcode, class), by = "treatment") %>%
  filter(is.na(classcode) | is.na(class))
write_csv(unmapped, file.path(out_dir, "unmapped_treatments.csv"))

map_consistency <- trt_map %>%
  group_by(trtcode) %>%
  summarise(n_class = n_distinct(classcode), .groups = "drop") %>%
  filter(n_class != 1)
write_csv(map_consistency, file.path(out_dir, "mapping_inconsistencies.csv"))

trt_class_counts <- dat_class %>%
  count(treatment, classcode, class, sort = TRUE)
write_csv(trt_class_counts, file.path(out_dir, "trt_class_counts_used.csv"))

dat_audit <- dat %>%
  left_join(trt_map %>% select(treatment = trtcode, classcode, class), by = "treatment") %>%
  mutate(
    drop_reason = case_when(
      is.na(studyid) ~ "missing_studyid",
      is.na(classcode) | is.na(class) ~ "unmapped_class",
      is.na(n) ~ "missing_n",
      is.na(mean_change) ~ "missing_mean_change",
      is.na(sd_change) ~ "missing_sd_change",
      TRUE ~ "kept"
    )
  )
write_csv(count(dat_audit, drop_reason), file.path(out_dir, "drop_reason_counts.csv"))