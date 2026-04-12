required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "tibble", "janitor",
  "rstatix", "afex", "emmeans", "effectsize", "writexl", "ggplot2",
  "officer", "flextable", "forcats", "magrittr", "stringi", "patchwork"
)

install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  missing_pkgs <- setdiff(pkgs, installed)
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, dependencies = TRUE, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

options(contrasts = c("contr.sum", "contr.poly"))
afex::set_sum_contrasts()

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  normalizePath(getwd())
}

root_dir <- get_script_dir()
data_dir <- file.path(root_dir, "INT")
pre_file <- file.path(data_dir, "all.xlsx")
post_file <- file.path(data_dir, "all_post.xlsx")
output_dir <- file.path(data_dir, "anova_outputs")
figures_dir <- file.path(data_dir, "figures")
report_file <- file.path(data_dir, "repeated_measures_anova_report.docx")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

jama_group_colors <- c(
  "CON" = "#374E55FF",
  "INTG" = "#DF8F44FF"
)

jama_group_fills <- c(
  "CON" = grDevices::adjustcolor("#374E55FF", alpha.f = 0.22),
  "INTG" = grDevices::adjustcolor("#DF8F44FF", alpha.f = 0.28)
)

existing_output_files <- list.files(output_dir, full.names = TRUE, recursive = FALSE)
if (length(existing_output_files) > 0) {
  invisible(file.remove(existing_output_files))
}

existing_figure_files <- list.files(figures_dir, full.names = TRUE, recursive = FALSE)
if (length(existing_figure_files) > 0) {
  invisible(file.remove(existing_figure_files))
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

format_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f"), x))
}

format_stat_label <- function(prefix, x, digits = 2) {
  ifelse(is.na(x), "", paste0(prefix, " = ", sprintf(paste0("%.", digits, "f"), x)))
}

format_ci_text <- function(estimate, low, high, digits = 2) {
  ifelse(
    is.na(estimate) | is.na(low) | is.na(high),
    "NA",
    paste0(
      sprintf(paste0("%.", digits, "f"), estimate),
      " (",
      sprintf(paste0("%.", digits, "f"), low),
      " to ",
      sprintf(paste0("%.", digits, "f"), high),
      ")"
    )
  )
}

format_mean_sd <- function(mean, sd, digits = 2) {
  ifelse(
    is.na(mean) | is.na(sd),
    "NA",
    sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean, sd)
  )
}

format_count_pct <- function(n, pct) {
  sprintf("%d (%.1f%%)", n, pct)
}

format_pct_change <- function(pre, post, digits = 1) {
  ifelse(
    is.na(pre) | is.na(post) | pre == 0,
    "NA",
    sprintf(paste0("%+.", digits, "f%%"), (post - pre) / pre * 100)
  )
}

classify_cohens_d <- function(d_value) {
  if (is.na(d_value)) {
    return(NA_character_)
  }
  abs_d <- abs(d_value)
  if (abs_d < 0.20) {
    "trivial"
  } else if (abs_d < 0.50) {
    "small"
  } else if (abs_d < 0.80) {
    "moderate"
  } else {
    "large"
  }
}

calc_pre_post_d <- function(pre_mean, post_mean, pre_sd, post_sd) {
  pooled_sd <- sqrt((pre_sd^2 + post_sd^2) / 2)
  ifelse(
    is.na(pooled_sd) | pooled_sd <= 0,
    NA_real_,
    (post_mean - pre_mean) / pooled_sd
  )
}

format_d_with_label <- function(d_value) {
  if (is.na(d_value)) {
    return("NA")
  }
  paste0(format_num(d_value, 2), " (", classify_cohens_d(d_value), ")")
}

p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

interaction_marker <- function(p) {
  ifelse(!is.na(p) & p < 0.05, "\u2020", "")
}

sanitize_ascii_text <- function(x) {
  x <- as.character(x)
  stringr::str_replace_all(
    x,
    c(
      "脳" = "x",
      "畏p虏" = "partial eta squared",
      "ηp²" = "partial eta squared",
      "Δx" = "Delta x",
      "Δt" = "Delta t"
    )
  )
}

sanitize_text_df <- function(df) {
  dplyr::mutate(df, dplyr::across(where(is.character), sanitize_ascii_text))
}

parse_stat_numeric <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "<\\.", "0.")
  x <- stringr::str_replace_all(x, "<0\\.", "0.")
  x <- stringr::str_replace_all(x, "[^0-9.\\-]", "")
  suppressWarnings(as.numeric(x))
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_nfkc(x)
  x <- stringi::stri_replace_all_regex(x, "\\p{Z}+", " ")
  stringi::stri_trim_both(x)
}

normalize_group <- function(x) {
  x <- stringr::str_to_upper(normalize_text(x))
  dplyr::case_when(
    x %in% c("CON", "CONTROL", "CG") ~ "CON",
    x %in% c("INTG", "INT", "INTERVENTION", "IG") ~ "INTG",
    TRUE ~ x
  )
}

safe_shapiro <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 3 || length(x) > 5000 || length(unique(x)) < 3) {
    return(NA_real_)
  }
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}

safe_levene <- function(data, formula) {
  tryCatch(
    rstatix::levene_test(data, formula),
    error = function(e) tibble::tibble(
      df1 = NA_real_,
      df2 = NA_real_,
      statistic = NA_real_,
      p = NA_real_
    )
  )
}

safe_t_test <- function(formula, data) {
  tryCatch(stats::t.test(formula, data = data), error = function(e) NULL)
}

safe_fisher_or_chisq <- function(tab) {
  if (any(tab < 5)) {
    out <- fisher.test(tab)
    list(method = "Fisher's exact test", p = out$p.value)
  } else {
    out <- chisq.test(tab, correct = FALSE)
    list(method = "Chi-square test", p = out$p.value)
  }
}

make_subject_key <- function(df) {
  df %>%
    mutate(
      number = normalize_text(number),
      name = normalize_text(name),
      group = normalize_group(group),
      sex = dplyr::if_else(is.na(sex), NA_character_, stringr::str_to_title(normalize_text(sex))),
      subject_id = paste(number, name, group, sep = "__")
    )
}

display_lookup <- c(
  number = "Participant number",
  name = "Participant name",
  group = "Group",
  sex = "Sex",
  height = "Height",
  weight = "Weight",
  age = "Age",
  training_session = "Training experience",
  bmi = "Body mass index",
  time = "15-m sprint time",
  cmj = "Countermovement jump",
  sj = "Squat jump",
  slj = "Standing long jump",
  distance = "Water-entry distance",
  dx = "Horizontal displacement (Δx)",
  dt = "Flight-time difference (Δt)",
  vxh = "Horizontal velocity",
  fms_deep_squat = "FMS Deep Squat",
  fms_hurdle_l = "FMS Hurdle Step, left",
  fms_hurdle_r = "FMS Hurdle Step, right",
  fms_lunge_l = "FMS In-line Lunge, left",
  fms_lunge_r = "FMS In-line Lunge, right",
  fms_shoulder_l = "FMS Shoulder Mobility, left",
  fms_shoulder_r = "FMS Shoulder Mobility, right",
  fms_aslr_l = "FMS Active Straight-Leg Raise, left",
  fms_aslr_r = "FMS Active Straight-Leg Raise, right",
  fms_trunk_push_up = "FMS Trunk Stability Push-Up",
  fms_rotary_l = "FMS Rotary Stability, left",
  fms_rotary_r = "FMS Rotary Stability, right",
  fms_total = "FMS Total",
  fms_total_raw = "Bilateral FMS sum (raw)"
)

unit_lookup <- c(
  number = "", name = "", group = "", sex = "", height = "cm", weight = "kg",
  age = "years", training_session = "years", bmi = "kg/m^2", time = "s",
  cmj = "cm", sj = "cm", slj = "cm", distance = "cm", dx = "cm", dt = "s",
  vxh = "m/s", fms_deep_squat = "score", fms_hurdle_l = "score",
  fms_hurdle_r = "score", fms_lunge_l = "score", fms_lunge_r = "score",
  fms_shoulder_l = "score", fms_shoulder_r = "score", fms_aslr_l = "score",
  fms_aslr_r = "score", fms_trunk_push_up = "score", fms_rotary_l = "score",
  fms_rotary_r = "score", fms_total = "score", fms_total_raw = "score"
)

category_lookup <- c(
  number = "baseline", name = "baseline", group = "baseline", sex = "baseline",
  height = "baseline", weight = "baseline", age = "baseline",
  training_session = "baseline", bmi = "baseline", time = "performance",
  cmj = "performance", sj = "performance", slj = "performance",
  distance = "performance", dx = "performance", dt = "performance",
  vxh = "performance", fms_deep_squat = "FMS", fms_hurdle_l = "FMS",
  fms_hurdle_r = "FMS", fms_lunge_l = "FMS", fms_lunge_r = "FMS",
  fms_shoulder_l = "FMS", fms_shoulder_r = "FMS", fms_aslr_l = "FMS",
  fms_aslr_r = "FMS", fms_trunk_push_up = "FMS", fms_rotary_l = "FMS",
  fms_rotary_r = "FMS", fms_total = "FMS", fms_total_raw = "audit"
)

display_lookup[c("dx", "dt")] <- c(
  "Horizontal displacement (Delta x)",
  "Flight-time difference (Delta t)"
)

analysis_display_order <- c(
  time = 1, distance = 2, dx = 3, vxh = 4, cmj = 5, sj = 6, slj = 7, dt = 8,
  fms_total = 9, fms_hurdle_r = 10, fms_hurdle_l = 11, fms_lunge_r = 12,
  fms_deep_squat = 13, fms_lunge_l = 14, fms_shoulder_l = 15,
  fms_shoulder_r = 16, fms_aslr_l = 17, fms_aslr_r = 18,
  fms_trunk_push_up = 19, fms_rotary_l = 20, fms_rotary_r = 21
)

lower_limb_outcomes <- c("sj", "cmj", "slj")
start_kinematic_outcomes <- c("time", "distance", "dx", "vxh")
start_main_figure_outcomes <- c("time", "distance", "vxh")
primary_performance <- start_kinematic_outcomes
primary_fms <- c("fms_total", "fms_hurdle_r", "fms_hurdle_l", "fms_lunge_r")
primary_main_outcomes <- c(primary_performance, primary_fms)
sensitivity_main_outcomes <- c(primary_performance, "fms_total")
primary_main_lookup <- stats::setNames(rep(FALSE, length(display_lookup)), names(display_lookup))
primary_main_lookup[primary_main_outcomes] <- TRUE

higher_is_better <- c(
  time = FALSE, distance = TRUE, dx = TRUE, vxh = TRUE, fms_total = TRUE,
  fms_hurdle_r = TRUE, fms_hurdle_l = TRUE, fms_lunge_r = TRUE
)

pre_raw <- readxl::read_excel(pre_file)
post_raw <- readxl::read_excel(post_file)

clean_name_map <- tibble::tibble(
  raw_name = names(pre_raw),
  clean_name = janitor::make_clean_names(names(pre_raw))
)

compute_standard_fms_total <- function(df) {
  df %>%
    mutate(
      fms_total_raw = fms_total,
      fms_total =
        fms_deep_squat +
        pmin(fms_hurdle_l, fms_hurdle_r) +
        pmin(fms_lunge_l, fms_lunge_r) +
        pmin(fms_shoulder_l, fms_shoulder_r) +
        pmin(fms_aslr_l, fms_aslr_r) +
        fms_trunk_push_up +
        pmin(fms_rotary_l, fms_rotary_r)
    )
}

pre <- janitor::clean_names(pre_raw) %>%
  compute_standard_fms_total() %>%
  make_subject_key()
post <- janitor::clean_names(post_raw) %>%
  compute_standard_fms_total() %>%
  make_subject_key()

required_vars <- c("number", "name", "group")
if (!all(required_vars %in% names(pre)) || !all(required_vars %in% names(post))) {
  stop("Required matching fields were not found in both input files.")
}

duplicate_pre <- pre %>% count(subject_id) %>% filter(n > 1)
duplicate_post <- post %>% count(subject_id) %>% filter(n > 1)
if (nrow(duplicate_pre) > 0 || nrow(duplicate_post) > 0) {
  stop("Duplicate participant keys were detected in the input data.")
}

pre_keys <- pre %>% select(all_of(required_vars))
post_keys <- post %>% select(all_of(required_vars))
unmatched_pre <- anti_join(pre_keys, post_keys, by = required_vars)
unmatched_post <- anti_join(post_keys, pre_keys, by = required_vars)
if (nrow(unmatched_pre) > 0 || nrow(unmatched_post) > 0) {
  stop("Pre- and post-test records did not match one-to-one by number, name, and group.")
}

pre <- pre %>% arrange(group, number, name)
post <- post %>% arrange(group, number, name)

baseline_vars <- c(
  "number", "name", "group", "sex", "height", "weight", "age",
  "training_session", "bmi", "subject_id"
)

analysis_vars <- intersect(setdiff(names(pre), baseline_vars), names(post))
analysis_vars <- setdiff(analysis_vars, "visit")
analysis_vars <- setdiff(analysis_vars, "fms_total_raw")
analysis_vars <- analysis_vars[analysis_vars %in% names(analysis_display_order)]
analysis_vars <- names(sort(analysis_display_order[analysis_vars]))

pre <- pre %>%
  mutate(
    group = factor(group, levels = c("CON", "INTG")),
    across(all_of(analysis_vars), as.numeric)
  )
post <- post %>%
  mutate(
    group = factor(group, levels = c("CON", "INTG")),
    across(all_of(analysis_vars), as.numeric)
  )

if (length(analysis_vars) != 21) {
  stop(sprintf("Expected 21 analyzable outcomes, but found %d.", length(analysis_vars)))
}

variable_dictionary_map <- bind_rows(
  clean_name_map %>% filter(clean_name != "fms_total"),
  tibble::tibble(raw_name = "Derived from standard 7-item FMS scoring", clean_name = "fms_total"),
  tibble::tibble(raw_name = "FMS_Total", clean_name = "fms_total_raw")
)

variable_dictionary <- variable_dictionary_map %>%
  mutate(
    english_display_name = dplyr::coalesce(unname(display_lookup[clean_name]), raw_name),
    unit = dplyr::coalesce(unname(unit_lookup[clean_name]), ""),
    variable_category = dplyr::coalesce(unname(category_lookup[clean_name]), "other"),
    is_primary_main = dplyr::coalesce(unname(primary_main_lookup[clean_name]), FALSE),
    display_order = dplyr::coalesce(unname(analysis_display_order[clean_name]), NA_real_)
  ) %>%
  arrange(variable_category, display_order, clean_name)

long_data <- bind_rows(
  pre %>%
    mutate(visit = "Pre") %>%
    select(subject_id, group, sex, height, weight, age, training_session, bmi, visit, all_of(analysis_vars)),
  post %>%
    mutate(visit = "Post") %>%
    select(subject_id, group, sex, height, weight, age, training_session, bmi, visit, all_of(analysis_vars))
) %>%
  pivot_longer(cols = all_of(analysis_vars), names_to = "outcome", values_to = "value") %>%
  mutate(
    visit = factor(visit, levels = c("Pre", "Post")),
    outcome = factor(outcome, levels = analysis_vars),
    outcome_label = dplyr::recode(as.character(outcome), !!!display_lookup),
    domain = dplyr::recode(as.character(outcome), !!!category_lookup),
    is_primary_main = as.character(outcome) %in% primary_main_outcomes
  )

wide_data <- pre %>%
  select(subject_id, group, sex, height, weight, age, training_session, bmi, all_of(analysis_vars)) %>%
  rename_with(~ paste0(.x, "_pre"), all_of(analysis_vars)) %>%
  left_join(
    post %>%
      select(subject_id, all_of(analysis_vars)) %>%
      rename_with(~ paste0(.x, "_post"), all_of(analysis_vars)),
    by = "subject_id"
  )

analysis_completeness <- purrr::map_dfr(analysis_vars, function(v) {
  tibble(
    outcome = v,
    outcome_label = display_lookup[[v]],
    pre_non_missing = sum(!is.na(pre[[v]])),
    post_non_missing = sum(!is.na(post[[v]])),
    paired_non_missing = sum(!is.na(wide_data[[paste0(v, "_pre")]]) & !is.na(wide_data[[paste0(v, "_post")]]))
  )
})

qc_summary <- tibble(
  metric = c(
    "Paired participants",
    "CON participants",
    "INTG participants",
    "Duplicate keys in pre-test file",
    "Duplicate keys in post-test file",
    "Unmatched pre-test records",
    "Unmatched post-test records",
    "Analyzable outcomes",
    "Outcomes with complete paired values"
  ),
  value = c(
    dplyr::n_distinct(pre$subject_id),
    sum(pre$group == "CON"),
    sum(pre$group == "INTG"),
    nrow(duplicate_pre),
    nrow(duplicate_post),
    nrow(unmatched_pre),
    nrow(unmatched_post),
    length(analysis_vars),
    sum(analysis_completeness$paired_non_missing == dplyr::n_distinct(pre$subject_id))
  )
)

baseline_cont_vars <- c("age", "height", "weight", "bmi", "training_session")
baseline_cat_vars <- c("sex")

baseline_desc_cont <- pre %>%
  group_by(group) %>%
  summarise(
    across(
      all_of(baseline_cont_vars),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

baseline_tests_cont <- purrr::map_dfr(baseline_cont_vars, function(v) {
  dat <- pre %>% select(group, all_of(v)) %>% filter(!is.na(.data[[v]]))
  tt <- safe_t_test(stats::as.formula(paste(v, "~ group")), dat)
  tibble(
    variable = v,
    method = "Independent samples t-test",
    statistic = if (!is.null(tt)) unname(tt$statistic) else NA_real_,
    p = if (!is.null(tt)) tt$p.value else NA_real_
  )
})

baseline_desc_cat <- purrr::map_dfr(baseline_cat_vars, function(v) {
  pre %>%
    count(group, .data[[v]], name = "n") %>%
    group_by(group) %>%
    mutate(percent = 100 * n / sum(n), variable = v) %>%
    ungroup() %>%
    rename(level = !!v)
})

baseline_tests_cat <- purrr::map_dfr(baseline_cat_vars, function(v) {
  tab <- table(pre$group, pre[[v]])
  res <- safe_fisher_or_chisq(tab)
  tibble(
    variable = v,
    method = res$method,
    statistic = NA_real_,
    p = res$p
  )
})

baseline_table3_spec <- tibble::tribble(
  ~section, ~variable, ~row_label, ~row_type,
  "Participant characteristics", "age", NA_character_, "continuous",
  "Participant characteristics", "height", NA_character_, "continuous",
  "Participant characteristics", "weight", NA_character_, "continuous",
  "Participant characteristics", "training_session", "Training experience (years)", "continuous",
  "Participant characteristics", "bmi", "BMI (kg/m^2)", "continuous",
  "Participant characteristics", "sex", "Female, n (%)", "categorical",
  "Lower-limb explosive performance", "sj", "SJ (cm)", "continuous",
  "Lower-limb explosive performance", "cmj", "CMJ (cm)", "continuous",
  "Lower-limb explosive performance", "slj", "SLJ (cm)", "continuous",
  "Start performance", "time", "15-m sprint time (s)", "continuous",
  "Start performance", "distance", "Water-entry distance (cm)", "continuous",
  "Start performance", "dx", "Horizontal displacement (Delta x) (cm)", "continuous",
  "Start performance", "vxh", "Horizontal velocity (m/s)", "continuous",
  "Functional movement", "fms_total", "FMS Total (0-21)", "continuous"
)

baseline_table3_cont_vars <- baseline_table3_spec %>%
  filter(row_type == "continuous") %>%
  pull(variable) %>%
  unique()

baseline_table3_tests_cont <- purrr::map_dfr(baseline_table3_cont_vars, function(v) {
  dat <- pre %>% select(group, all_of(v)) %>% filter(!is.na(.data[[v]]))
  tt <- safe_t_test(stats::as.formula(paste(v, "~ group")), dat)
  tibble(
    variable = v,
    method = "Independent samples t-test",
    statistic = if (!is.null(tt)) unname(tt$statistic) else NA_real_,
    p = if (!is.null(tt)) tt$p.value else NA_real_
  )
})

summary_table <- long_data %>%
  group_by(outcome, outcome_label, domain, is_primary_main, group, visit) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mean_sd = format_mean_sd(mean, sd))

change_table <- purrr::map_dfr(analysis_vars, function(v) {
  wide_data %>%
    transmute(
      subject_id,
      group,
      outcome = v,
      outcome_label = display_lookup[[v]],
      domain = category_lookup[[v]],
      is_primary_main = v %in% primary_main_outcomes,
      pre = .data[[paste0(v, "_pre")]],
      post = .data[[paste0(v, "_post")]],
      change = post - pre
    ) %>%
    group_by(group, outcome, outcome_label, domain, is_primary_main) %>%
    summarise(
      n = sum(!is.na(change)),
      change_mean = mean(change, na.rm = TRUE),
      change_sd = sd(change, na.rm = TRUE),
      change_median = median(change, na.rm = TRUE),
      change_iqr = IQR(change, na.rm = TRUE),
      .groups = "drop"
    )
})

anova_results <- list()
posthoc_results <- list()
assumption_results <- list()
ancova_results <- list()
outlier_results <- list()

for (v in analysis_vars) {
  dat_long <- long_data %>%
    filter(outcome == v) %>%
    select(subject_id, group, visit, value) %>%
    filter(!is.na(value))

  complete_ids <- dat_long %>%
    count(subject_id) %>%
    filter(n == 2) %>%
    pull(subject_id)

  dat_long <- dat_long %>% filter(subject_id %in% complete_ids)
  if (nrow(dat_long) == 0) {
    next
  }

  fit <- tryCatch(
    afex::aov_ez(
      id = "subject_id",
      dv = "value",
      data = dat_long,
      within = "visit",
      between = "group",
      type = 3,
      factorize = FALSE,
      return = "afex_aov"
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    next
  }

  anova_results[[v]] <- afex::nice(fit, es = "pes") %>%
    janitor::clean_names() %>%
    mutate(
      mse = parse_stat_numeric(mse),
      f = parse_stat_numeric(f),
      pes = parse_stat_numeric(pes),
      p_value = parse_stat_numeric(p_value),
      outcome = v,
      outcome_label = display_lookup[[v]],
      domain = category_lookup[[v]],
      is_primary_main = v %in% primary_main_outcomes,
      .before = 1
    )

  emm_visit <- emmeans::emmeans(fit, ~ visit | group)
  emm_group <- emmeans::emmeans(fit, ~ group | visit)

  posthoc_results[[v]] <- bind_rows(
    as.data.frame(pairs(emm_visit, adjust = "holm")) %>%
      janitor::clean_names() %>%
      mutate(
        outcome = v,
        outcome_label = display_lookup[[v]],
        domain = category_lookup[[v]],
        is_primary_main = v %in% primary_main_outcomes,
        comparison_type = "within_group_pre_post",
        .before = 1
      ),
    as.data.frame(pairs(emm_group, adjust = "holm")) %>%
      janitor::clean_names() %>%
      mutate(
        outcome = v,
        outcome_label = display_lookup[[v]],
        domain = category_lookup[[v]],
        is_primary_main = v %in% primary_main_outcomes,
        comparison_type = "between_group_each_time",
        .before = 1
      )
  )

  dat_wide <- wide_data %>%
    transmute(
      subject_id,
      group,
      pre = .data[[paste0(v, "_pre")]],
      post = .data[[paste0(v, "_post")]],
      change = post - pre
    ) %>%
    filter(!is.na(pre) & !is.na(post))

  shapiro_tbl <- dat_wide %>%
    group_by(group) %>%
    summarise(
      n = n(),
      p = safe_shapiro(change),
      .groups = "drop"
    ) %>%
    mutate(
      outcome = v,
      outcome_label = display_lookup[[v]],
      domain = category_lookup[[v]],
      test = "Shapiro-Wilk test for change scores",
      visit = NA_character_,
      .before = 1
    )

  levene_change <- safe_levene(dat_wide, change ~ group) %>%
    mutate(
      outcome = v,
      outcome_label = display_lookup[[v]],
      domain = category_lookup[[v]],
      test = "Levene's test for change scores",
      visit = NA_character_,
      .before = 1
    )

  levene_each_time <- dat_long %>%
    group_by(visit) %>%
    group_modify(~ safe_levene(.x, value ~ group)) %>%
    ungroup() %>%
    mutate(
      outcome = v,
      outcome_label = display_lookup[[v]],
      domain = category_lookup[[v]],
      test = "Levene's test by visit",
      .before = 1
    )

  assumption_results[[v]] <- bind_rows(shapiro_tbl, levene_change, levene_each_time)

  ancova_dat <- dat_wide %>%
    mutate(group = factor(group, levels = c("CON", "INTG")))
  contrasts(ancova_dat$group) <- stats::contr.treatment(2, base = 1)

  ancova_fit <- tryCatch(stats::lm(post ~ group + pre, data = ancova_dat), error = function(e) NULL)
  if (!is.null(ancova_fit)) {
    ancova_anova_tbl <- as.data.frame(stats::anova(ancova_fit)) %>%
      tibble::rownames_to_column("term") %>%
      janitor::clean_names() %>%
      mutate(
        outcome = v,
        outcome_label = display_lookup[[v]],
        domain = category_lookup[[v]],
        is_primary_main = v %in% primary_main_outcomes,
        .before = 1
      )

    ancova_effect_tbl <- tryCatch(
      effectsize::eta_squared(ancova_fit, partial = TRUE) %>%
        as.data.frame() %>%
        janitor::clean_names() %>%
        mutate(
          outcome = v,
          outcome_label = display_lookup[[v]],
          domain = category_lookup[[v]],
          is_primary_main = v %in% primary_main_outcomes,
          .before = 1
        ),
      error = function(e) NULL
    )

    ancova_coef_tbl <- as.data.frame(summary(ancova_fit)$coefficients) %>%
      tibble::rownames_to_column("term") %>%
      janitor::clean_names() %>%
      mutate(
        outcome = v,
        outcome_label = display_lookup[[v]],
        domain = category_lookup[[v]],
        is_primary_main = v %in% primary_main_outcomes,
        .before = 1
      )

    ancova_ci_tbl <- tryCatch(
      {
        ci_tbl <- as.data.frame(stats::confint(ancova_fit))
        ci_tbl$term <- rownames(ci_tbl)
        tibble::as_tibble(ci_tbl[, c("term", "2.5 %", "97.5 %")]) %>%
          rename(conf_low = `2.5 %`, conf_high = `97.5 %`)
      },
      error = function(e) {
        tibble::tibble(
          term = ancova_coef_tbl$term,
          conf_low = NA_real_,
          conf_high = NA_real_
        )
      }
    )

    ancova_coef_tbl <- ancova_coef_tbl %>%
      left_join(ancova_ci_tbl, by = "term") %>%
      mutate(
        residual_sd = stats::sigma(ancova_fit),
        std_estimate = estimate / residual_sd,
        std_conf_low = conf_low / residual_sd,
        std_conf_high = conf_high / residual_sd
      )

    ancova_results[[v]] <- list(
      anova = ancova_anova_tbl,
      effect = ancova_effect_tbl,
      coef = ancova_coef_tbl
    )

    studentized <- tryCatch(stats::rstudent(ancova_fit), error = function(e) rep(NA_real_, nrow(ancova_dat)))
    outlier_results[[v]] <- ancova_dat %>%
      transmute(
        subject_id,
        group,
        outcome = v,
        outcome_label = display_lookup[[v]],
        domain = category_lookup[[v]],
        studentized_residual = studentized,
        outlier_flag = abs(studentized_residual) > 3
      )
  }
}

anova_table_all <- bind_rows(anova_results)
if (!"p" %in% names(anova_table_all) && "p_value" %in% names(anova_table_all)) {
  anova_table_all$p <- anova_table_all$p_value
}
anova_table_all <- anova_table_all %>%
  mutate(
    df = as.character(df),
    num_df = suppressWarnings(as.numeric(stringr::str_trim(stringr::str_split_fixed(df, ",", 2)[, 1]))),
    den_df = suppressWarnings(as.numeric(stringr::str_trim(stringr::str_split_fixed(df, ",", 2)[, 2])))
  )

posthoc_table_all <- bind_rows(posthoc_results)
assumption_table_all <- bind_rows(assumption_results)
ancova_anova_all <- bind_rows(lapply(ancova_results, `[[`, "anova"))
if (!"p" %in% names(ancova_anova_all) && "pr_f" %in% names(ancova_anova_all)) {
  ancova_anova_all$p <- ancova_anova_all$pr_f
}
ancova_effect_all <- bind_rows(lapply(ancova_results, `[[`, "effect"))
ancova_coef_all <- bind_rows(lapply(ancova_results, `[[`, "coef"))
outlier_table_all <- bind_rows(outlier_results)

interaction_table <- anova_table_all %>%
  filter(effect %in% c("group:visit", "visit:group")) %>%
  rename(
    interaction_num_df = num_df,
    interaction_den_df = den_df,
    interaction_f = f,
    interaction_p = p,
    interaction_pes = pes
  ) %>%
  select(
    outcome, outcome_label, domain, is_primary_main,
    interaction_num_df, interaction_den_df, interaction_f, interaction_p, interaction_pes
  )

within_posthoc <- posthoc_table_all %>%
  filter(comparison_type == "within_group_pre_post") %>%
  transmute(outcome, group, within_p = p_value) %>%
  pivot_wider(
    names_from = group,
    values_from = within_p,
    names_glue = "{group}_within_p"
  )

between_posthoc <- posthoc_table_all %>%
  filter(comparison_type == "between_group_each_time") %>%
  transmute(outcome, visit, between_p = p_value) %>%
  pivot_wider(
    names_from = visit,
    values_from = between_p,
    names_glue = "{visit}_between_p"
  )

ancova_group_table <- ancova_anova_all %>%
  filter(term == "group") %>%
  transmute(outcome, ancova_group_p = p) %>%
  left_join(
    ancova_effect_all %>%
      filter(parameter == "group") %>%
      transmute(outcome, ancova_partial_eta2 = eta2_partial),
    by = "outcome"
  ) %>%
  left_join(
    ancova_coef_all %>%
      filter(stringr::str_detect(term, "^group") & term != "group") %>%
      group_by(outcome) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(
        outcome,
        ancova_group_beta = estimate,
        ancova_group_ci_low = conf_low,
        ancova_group_ci_high = conf_high,
        ancova_group_std_beta = std_estimate,
        ancova_group_std_ci_low = std_conf_low,
        ancova_group_std_ci_high = std_conf_high
      ),
    by = "outcome"
  )

sensitivity_plot_data <- ancova_group_table %>%
  filter(outcome %in% sensitivity_main_outcomes) %>%
  mutate(
    outcome_label = unname(display_lookup[outcome]),
    domain = unname(category_lookup[outcome]),
    is_primary_main = outcome %in% sensitivity_main_outcomes,
    favors_higher = dplyr::coalesce(as.logical(unname(higher_is_better[outcome])), TRUE),
    adjusted_effect_favoring_intg = ifelse(favors_higher, ancova_group_std_beta, -ancova_group_std_beta),
    ci_low_favoring_intg = ifelse(favors_higher, ancova_group_std_ci_low, -ancova_group_std_ci_high),
    ci_high_favoring_intg = ifelse(favors_higher, ancova_group_std_ci_high, -ancova_group_std_ci_low),
    ci_low_plot = pmin(ci_low_favoring_intg, ci_high_favoring_intg, na.rm = TRUE),
    ci_high_plot = pmax(ci_low_favoring_intg, ci_high_favoring_intg, na.rm = TRUE),
    outcome_label = factor(
      outcome_label,
      levels = rev(unname(display_lookup[sensitivity_main_outcomes]))
    ),
    p_label = ifelse(
      is.na(ancova_group_p),
      "p = NA",
      ifelse(ancova_group_p < 0.001, "p < 0.001", paste0("p = ", format_p(ancova_group_p)))
    ),
    ci_label = format_ci_text(adjusted_effect_favoring_intg, ci_low_plot, ci_high_plot, digits = 2)
  ) %>%
  arrange(desc(outcome_label))

group_visit_summary <- summary_table %>%
  select(outcome, outcome_label, domain, is_primary_main, group, visit, mean_sd) %>%
  pivot_wider(
    names_from = c(group, visit),
    values_from = mean_sd,
    names_glue = "{group}_{visit}"
  )

group_change_summary <- change_table %>%
  mutate(change_mean_sd = format_mean_sd(change_mean, change_sd)) %>%
  select(outcome, group, change_mean_sd) %>%
  pivot_wider(
    names_from = group,
    values_from = change_mean_sd,
    names_glue = "{group}_change"
  )

main_result_table <- group_visit_summary %>%
  left_join(group_change_summary, by = "outcome") %>%
  left_join(interaction_table, by = c("outcome", "outcome_label", "domain", "is_primary_main")) %>%
  left_join(within_posthoc, by = "outcome") %>%
  left_join(between_posthoc, by = "outcome") %>%
  left_join(ancova_group_table, by = "outcome") %>%
  mutate(
    outcome_label = factor(as.character(outcome_label), levels = unname(display_lookup[analysis_vars]))
  ) %>%
  arrange(match(outcome, analysis_vars))

baseline_table_doc <- purrr::map_dfr(unique(baseline_table3_spec$section), function(section_name) {
  rows <- baseline_table3_spec %>% filter(section == section_name)

  section_header <- tibble(
    Variable = section_name,
    INTG = "",
    CON = "",
    P = ""
  )

  section_rows <- purrr::pmap_dfr(rows, function(section, variable, row_label, row_type) {
    if (row_type == "continuous") {
      con_mean <- mean(pre[[variable]][pre$group == "CON"], na.rm = TRUE)
      con_sd <- stats::sd(pre[[variable]][pre$group == "CON"], na.rm = TRUE)
      intg_mean <- mean(pre[[variable]][pre$group == "INTG"], na.rm = TRUE)
      intg_sd <- stats::sd(pre[[variable]][pre$group == "INTG"], na.rm = TRUE)
      stat_row <- baseline_table3_tests_cont %>% filter(variable == !!variable)
      tibble(
        Variable = dplyr::coalesce(row_label, paste0(display_lookup[[variable]], ifelse(unit_lookup[[variable]] == "", "", paste0(" (", unit_lookup[[variable]], ")")))),
        INTG = format_mean_sd(intg_mean, intg_sd),
        CON = format_mean_sd(con_mean, con_sd),
        P = format_p(stat_row$p[[1]])
      )
    } else {
      female_rows <- baseline_desc_cat %>%
        filter(variable == !!variable, as.character(level) == "Female") %>%
        mutate(value = format_count_pct(n, percent)) %>%
        select(group, value) %>%
        pivot_wider(names_from = group, values_from = value)
      cat_test <- baseline_tests_cat %>% filter(variable == !!variable)
      tibble(
        Variable = row_label,
        INTG = dplyr::coalesce(female_rows$INTG[[1]], "0 (0.0%)"),
        CON = dplyr::coalesce(female_rows$CON[[1]], "0 (0.0%)"),
        P = format_p(cat_test$p[[1]])
      )
    }
  })

  bind_rows(section_header, section_rows)
})

all_performance <- analysis_vars[analysis_vars %in% names(category_lookup[category_lookup == "performance"])]
main_performance_figure_outcomes <- c(lower_limb_outcomes, start_kinematic_outcomes)
all_fms <- analysis_vars[analysis_vars %in% names(category_lookup[category_lookup == "FMS"])]
significant_fms <- interaction_table %>%
  filter(domain == "FMS", interaction_p < 0.05) %>%
  arrange(match(outcome, analysis_vars)) %>%
  pull(outcome)
summary_numeric <- summary_table %>%
  select(outcome, group, visit, mean, sd) %>%
  pivot_wider(
    names_from = c(group, visit),
    values_from = c(mean, sd),
    names_sep = "__"
  )

within_group_d <- summary_numeric %>%
  transmute(
    outcome,
    CON_post_pre_d_raw = calc_pre_post_d(mean__CON__Pre, mean__CON__Post, sd__CON__Pre, sd__CON__Post),
    INTG_post_pre_d_raw = calc_pre_post_d(mean__INTG__Pre, mean__INTG__Post, sd__INTG__Pre, sd__INTG__Post)
  ) %>%
  mutate(
    CON_post_pre_d = purrr::map_chr(CON_post_pre_d_raw, format_d_with_label),
    INTG_post_pre_d = purrr::map_chr(INTG_post_pre_d_raw, format_d_with_label)
  )

main_result_table <- main_result_table %>%
  left_join(within_group_d, by = "outcome")

build_display_table_doc <- function(outcomes) {
  purrr::map_dfr(outcomes, function(outcome_name) {
    stats_row <- main_result_table %>%
      filter(outcome == outcome_name) %>%
      slice(1)
    num_row <- summary_numeric %>%
      filter(outcome == outcome_name) %>%
      slice(1)

    outcome_title <- paste0(
      unname(display_lookup[[outcome_name]]),
      ifelse(unit_lookup[[outcome_name]] == "", "", paste0(" (", unname(unit_lookup[[outcome_name]]), ")"))
    )

    con_pre <- format_mean_sd(num_row$mean__CON__Pre[[1]], num_row$sd__CON__Pre[[1]])
    con_post <- paste0(
      format_mean_sd(num_row$mean__CON__Post[[1]], num_row$sd__CON__Post[[1]]),
      p_to_stars(stats_row$CON_within_p[[1]]),
      ifelse(!is.na(stats_row$Post_between_p[[1]]) && stats_row$Post_between_p[[1]] < 0.05, "#", "")
    )
    intg_pre <- format_mean_sd(num_row$mean__INTG__Pre[[1]], num_row$sd__INTG__Pre[[1]])
    intg_post <- paste0(
      format_mean_sd(num_row$mean__INTG__Post[[1]], num_row$sd__INTG__Post[[1]]),
      p_to_stars(stats_row$INTG_within_p[[1]]),
      ifelse(!is.na(stats_row$Post_between_p[[1]]) && stats_row$Post_between_p[[1]] < 0.05, "#", "")
    )

    tibble(
      Outcome = c(outcome_title, "", "", ""),
      Metric = c("Pre", "Post", "Post-pre difference (%)", "Post-pre ES (d)"),
      CON = c(
        con_pre,
        con_post,
        format_pct_change(num_row$mean__CON__Pre[[1]], num_row$mean__CON__Post[[1]]),
        stats_row$CON_post_pre_d[[1]]
      ),
      INTG = c(
        intg_pre,
        intg_post,
        format_pct_change(num_row$mean__INTG__Pre[[1]], num_row$mean__INTG__Post[[1]]),
        stats_row$INTG_post_pre_d[[1]]
      ),
      `Group x Visit P` = c(format_p(stats_row$interaction_p[[1]]), "", "", ""),
      `Partial eta squared` = c(format_num(stats_row$interaction_pes[[1]], 3), "", "", "")
    )
  })
}

lower_limb_table_doc <- build_display_table_doc(lower_limb_outcomes)
start_kinematic_table_doc <- build_display_table_doc(start_kinematic_outcomes)
fms_main_table_doc <- build_display_table_doc(all_fms)

supplementary_posthoc_doc <- main_result_table %>%
  transmute(
    Outcome = as.character(outcome_label),
    Domain = domain,
    `CON Pre` = CON_Pre,
    `CON Post` = CON_Post,
    `INTG Pre` = INTG_Pre,
    `INTG Post` = INTG_Post,
    `CON Change` = CON_change,
    `INTG Change` = INTG_change,
    `CON Post-pre ES (d)` = CON_post_pre_d,
    `INTG Post-pre ES (d)` = INTG_post_pre_d,
    `Group x Visit P` = format_p(interaction_p),
    `Partial eta squared` = format_num(interaction_pes, 3),
    `CON Pre-Post P` = format_p(CON_within_p),
    `INTG Pre-Post P` = format_p(INTG_within_p),
    `Pre Between-Group P` = format_p(Pre_between_p),
    `Post Between-Group P` = format_p(Post_between_p),
    `ANCOVA Group P` = format_p(ancova_group_p)
  )

appendix_anova_doc <- anova_table_all %>%
  transmute(
    Outcome = outcome_label,
    Domain = domain,
    Effect = effect,
    `Num df` = format_num(num_df, 0),
    `Den df` = format_num(den_df, 0),
    `F` = format_num(f, 3),
    `P value` = format_p(p),
    `Partial eta squared` = format_num(pes, 3)
  )

appendix_posthoc_doc <- posthoc_table_all %>%
  mutate(stratum = dplyr::coalesce(as.character(group), as.character(visit), "")) %>%
  transmute(
    Outcome = outcome_label,
    Domain = domain,
    Comparison = comparison_type,
    Stratum = stratum,
    Contrast = contrast,
    Estimate = format_num(estimate, 3),
    `P value` = format_p(p_value)
  )

appendix_assumptions_doc <- assumption_table_all %>%
  transmute(
    Outcome = outcome_label,
    Domain = domain,
    Test = test,
    Visit = dplyr::coalesce(as.character(visit), ""),
    Statistic = format_num(statistic, 3),
    `P value` = format_p(p)
  )

appendix_outliers_doc <- outlier_table_all %>%
  transmute(
    Outcome = outcome_label,
    Domain = domain,
    `Subject ID` = subject_id,
    Group = as.character(group),
    `Studentized residual` = format_num(studentized_residual, 3),
    `Outlier flag` = ifelse(outlier_flag, "Yes", "No")
  )

appendix_ancova_doc <- ancova_anova_all %>%
  filter(outcome %in% sensitivity_main_outcomes) %>%
  transmute(
    Outcome = outcome_label,
    Domain = domain,
    Term = term,
    Df = format_num(df, 0),
    `Sum Sq` = format_num(sum_sq, 3),
    `Mean Sq` = format_num(mean_sq, 3),
    `F value` = format_num(f_value, 3),
    `P value` = format_p(p)
  )

supplementary_ancova_ci_doc <- sensitivity_plot_data %>%
  transmute(
    Outcome = as.character(outcome_label),
    Domain = as.character(domain),
    `Adjusted effect favoring INTG` = format_num(adjusted_effect_favoring_intg, 2),
    `95% CI` = ci_label,
    `Group P` = p_label
  )

supplementary_assumptions_outliers_doc <- bind_rows(
  appendix_assumptions_doc %>%
    mutate(`Subject ID` = "", Group = "", `Studentized residual` = "", `Outlier flag` = "") %>%
    select(Outcome, Domain, Test, Visit, Statistic, `P value`, `Subject ID`, Group, `Studentized residual`, `Outlier flag`),
  appendix_outliers_doc %>%
    transmute(
      Outcome,
      Domain,
      Test = "Outlier screening",
      Visit = "",
      Statistic = "",
      `P value` = "",
      `Subject ID`,
      Group,
      `Studentized residual`,
      `Outlier flag`
    )
)

variable_dictionary <- sanitize_text_df(variable_dictionary)
summary_table <- sanitize_text_df(summary_table)
change_table <- sanitize_text_df(change_table)
within_group_d <- sanitize_text_df(within_group_d)
anova_table_all <- sanitize_text_df(anova_table_all)
interaction_table <- sanitize_text_df(interaction_table)
posthoc_table_all <- sanitize_text_df(posthoc_table_all)
assumption_table_all <- sanitize_text_df(assumption_table_all)
outlier_table_all <- sanitize_text_df(outlier_table_all)
ancova_anova_all <- sanitize_text_df(ancova_anova_all)
ancova_effect_all <- sanitize_text_df(ancova_effect_all)
main_result_table <- sanitize_text_df(main_result_table)
baseline_table_doc <- sanitize_text_df(baseline_table_doc)
lower_limb_table_doc <- sanitize_text_df(lower_limb_table_doc)
start_kinematic_table_doc <- sanitize_text_df(start_kinematic_table_doc)
fms_main_table_doc <- sanitize_text_df(fms_main_table_doc)
supplementary_posthoc_doc <- sanitize_text_df(supplementary_posthoc_doc)
appendix_anova_doc <- sanitize_text_df(appendix_anova_doc)
appendix_posthoc_doc <- sanitize_text_df(appendix_posthoc_doc)
appendix_assumptions_doc <- sanitize_text_df(appendix_assumptions_doc)
appendix_outliers_doc <- sanitize_text_df(appendix_outliers_doc)
appendix_ancova_doc <- sanitize_text_df(appendix_ancova_doc)
supplementary_ancova_ci_doc <- sanitize_text_df(supplementary_ancova_ci_doc)
supplementary_assumptions_outliers_doc <- sanitize_text_df(supplementary_assumptions_outliers_doc)
sensitivity_plot_data <- sanitize_text_df(sensitivity_plot_data)

power_note <- paste(
  "Sample-size planning was aligned to the final mixed repeated-measures ANOVA framework.",
  "Using G*Power, the unified settings were F tests / ANOVA: Repeated measures, within-between interaction,",
  "with alpha = 0.05, power = 0.80, two groups, two measurements, and a medium effect size (f = 0.25).",
  "The 15-m sprint time outcome was used as the representative primary endpoint because it most directly reflects swimming-start performance and was therefore the most clinically relevant target for the intervention."
)

write.csv(variable_dictionary, file.path(output_dir, "00_variable_dictionary.csv"), row.names = FALSE)
write.csv(qc_summary, file.path(output_dir, "01_participant_matching_qc.csv"), row.names = FALSE)
write.csv(analysis_completeness, file.path(output_dir, "02_analysis_completeness.csv"), row.names = FALSE)
write.csv(baseline_table_doc, file.path(output_dir, "03_baseline_characteristics.csv"), row.names = FALSE)
write.csv(summary_table, file.path(output_dir, "04_pre_post_descriptive_summary.csv"), row.names = FALSE)
write.csv(change_table, file.path(output_dir, "05_change_score_summary.csv"), row.names = FALSE)
write.csv(within_group_d, file.path(output_dir, "05b_within_group_post_pre_d.csv"), row.names = FALSE)
write.csv(anova_table_all, file.path(output_dir, "06_mixed_anova_all_effects.csv"), row.names = FALSE)
write.csv(interaction_table, file.path(output_dir, "07_group_by_visit_interactions.csv"), row.names = FALSE)
write.csv(posthoc_table_all, file.path(output_dir, "08_posthoc_comparisons.csv"), row.names = FALSE)
write.csv(assumption_table_all, file.path(output_dir, "09_assumption_checks.csv"), row.names = FALSE)
write.csv(outlier_table_all, file.path(output_dir, "10_outlier_checks.csv"), row.names = FALSE)
write.csv(ancova_anova_all, file.path(output_dir, "11_ancova_sensitivity.csv"), row.names = FALSE)
write.csv(ancova_effect_all, file.path(output_dir, "12_ancova_effect_sizes.csv"), row.names = FALSE)
write.csv(main_result_table, file.path(output_dir, "13_main_result_table_full.csv"), row.names = FALSE)
write.csv(supplementary_posthoc_doc, file.path(output_dir, "14_supplementary_main_outcomes_table.csv"), row.names = FALSE)
write.csv(sensitivity_plot_data, file.path(output_dir, "15_sensitivity_plot_data.csv"), row.names = FALSE)
write.csv(lower_limb_table_doc, file.path(output_dir, "16_table4_lower_limb_display.csv"), row.names = FALSE)
write.csv(start_kinematic_table_doc, file.path(output_dir, "17_table5_start_kinematic_display.csv"), row.names = FALSE)
write.csv(fms_main_table_doc, file.path(output_dir, "18_table6_fms_display.csv"), row.names = FALSE)

writexl::write_xlsx(
  list(
    variable_dictionary = variable_dictionary,
    participant_matching_qc = qc_summary,
    analysis_completeness = analysis_completeness,
    baseline_characteristics = baseline_table_doc,
    descriptive_summary = summary_table,
    change_scores = change_table,
    within_group_post_pre_d = within_group_d,
    mixed_anova = anova_table_all,
    interactions = interaction_table,
    posthoc = posthoc_table_all,
    assumptions = assumption_table_all,
    outliers = outlier_table_all,
    ancova = ancova_anova_all,
    ancova_effects = ancova_effect_all,
    main_results = main_result_table,
    supplementary_posthoc = supplementary_posthoc_doc,
    ancova_ci = supplementary_ancova_ci_doc,
    assumptions_outliers = supplementary_assumptions_outliers_doc,
    sensitivity_plot_data = sensitivity_plot_data,
    table4_lower_limb_display = lower_limb_table_doc,
    table5_start_kinematic_display = start_kinematic_table_doc,
    table6_fms_display = fms_main_table_doc
  ),
  path = file.path(output_dir, "all_results_summary.xlsx")
)

build_main_figure <- function(selected_outcomes, outfile, width = 8.6, height = NULL) {
  if (is.null(height)) {
    height <- max(5.8, length(selected_outcomes) * 1.65)
  }

  build_group_cell <- function(outcome_name, group_name, show_title = TRUE) {
    group_color <- unname(jama_group_colors[[group_name]])
    group_fill <- unname(jama_group_fills[[group_name]])
    visit_colors <- c(Pre = "#6FAF73", Post = "#8A6BC1")
    line_color <- "#3F3F3F"
    stats_row <- main_result_table %>%
      filter(outcome == outcome_name) %>%
      slice(1)

    outcome_data <- long_data %>%
      filter(outcome == outcome_name) %>%
      mutate(
        visit = factor(as.character(visit), levels = c("Pre", "Post")),
        visit_num = c(1, 2)[match(as.character(visit), c("Pre", "Post"))]
      )

    panel_data <- outcome_data %>%
      filter(group == group_name)

    y_limits <- range(outcome_data$value, na.rm = TRUE)
    y_span <- diff(y_limits)
    if (!is.finite(y_span) || y_span <= 0) {
      y_span <- 1
    }
    y_upper <- y_limits[2] + y_span * 0.26
    y_lower <- y_limits[1] - y_span * 0.08

    within_p <- if (group_name == "CON") stats_row$CON_within_p[[1]] else stats_row$INTG_within_p[[1]]
    within_star <- p_to_stars(within_p)
    es_label <- paste0(
      "ES = ",
      format_num(stats_row$interaction_pes[[1]], 3)
    )
    violin_width <- if (stats::sd(panel_data$value, na.rm = TRUE) < 0.08 * y_span) 0.60 else 0.68

    base_theme <- theme_classic(base_size = 8.8) +
      theme(
        axis.line = element_line(linewidth = 0.30, color = "#5C5C5C"),
        axis.text.x = element_text(size = 7.5),
        axis.text.y = element_text(size = 7.4),
        axis.title = element_blank(),
        plot.margin = margin(3, 3, 3, 3),
        plot.title = if (show_title) element_text(face = "bold", size = 9.2, hjust = 0.5) else element_blank(),
        panel.grid = element_blank()
      )

    line_plot <- ggplot(panel_data, aes(x = visit_num, y = value)) +
      geom_line(
        aes(group = subject_id),
        color = line_color,
        alpha = 0.34,
        linewidth = 0.34
      ) +
      geom_point(
        aes(fill = visit),
        position = position_jitter(width = 0.035, height = 0),
        shape = 21,
        stroke = 0.28,
        color = "black",
        size = 1.0,
        alpha = 0.62
      ) +
      scale_fill_manual(values = visit_colors, guide = "none") +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("Pre", "Post"),
        expand = expansion(mult = c(0.10, 0.10))
      ) +
      coord_cartesian(ylim = c(y_lower, y_upper), clip = "off") +
      labs(title = NULL) +
      base_theme

    if (within_star != "") {
      within_label <- if (group_name == "INTG") paste0(within_star, es_label) else within_star
      line_plot <- line_plot +
        annotate(
          "segment",
          x = 1, xend = 2,
          y = y_limits[2] + y_span * 0.09,
          yend = y_limits[2] + y_span * 0.09,
          linewidth = 0.30,
          color = "#4A4A4A"
        ) +
        annotate(
          "segment",
          x = 1, xend = 1,
          y = y_limits[2] + y_span * 0.09,
          yend = y_limits[2] + y_span * 0.05,
          linewidth = 0.30,
          color = "#4A4A4A"
        ) +
        annotate(
          "segment",
          x = 2, xend = 2,
          y = y_limits[2] + y_span * 0.09,
          yend = y_limits[2] + y_span * 0.05,
          linewidth = 0.30,
          color = "#4A4A4A"
        ) +
        annotate(
          "text",
          x = 1.5,
          y = y_limits[2] + y_span * 0.17,
          label = within_label,
          size = 2.7,
          fontface = "bold"
        )
    } else if (group_name == "INTG") {
      line_plot <- line_plot +
        annotate(
          "text",
          x = 1.5,
          y = y_limits[2] + y_span * 0.17,
          label = es_label,
          size = 2.75,
          fontface = "bold"
        )
    }

    box_plot <- ggplot(panel_data, aes(x = visit_num, y = value, group = visit, fill = visit)) +
      stat_boxplot(
        geom = "errorbar",
        width = 0.18,
        linewidth = 0.42,
        color = "black"
      ) +
      geom_boxplot(
        width = 0.42,
        outlier.shape = NA,
        linewidth = 0.42,
        color = "black"
      ) +
      scale_fill_manual(values = visit_colors, guide = "none") +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("Pre", "Post"),
        expand = expansion(mult = c(0.14, 0.14))
      ) +
      coord_cartesian(ylim = c(y_lower, y_upper), clip = "off") +
      base_theme +
      theme(
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
      )

    violin_edge_data <- tibble(
      visit_num = c(1, 2),
      ymin = y_lower,
      ymax = y_upper
    )

    violin_plot <- ggplot(panel_data, aes(x = visit_num, y = value, group = visit, fill = visit)) +
      geom_violin(
        trim = FALSE,
        alpha = 0.75,
        color = "black",
        linewidth = 0.34,
        width = violin_width
      ) +
      geom_rect(
        data = tibble(
          xmin = c(0.66, 1.66),
          xmax = c(1.00, 2.00),
          ymin = -Inf,
          ymax = Inf
        ),
        inherit.aes = FALSE,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "white",
        color = NA
      ) +
      geom_segment(
        data = violin_edge_data,
        inherit.aes = FALSE,
        aes(x = visit_num, xend = visit_num, y = ymin, yend = ymax),
        color = "black",
        linewidth = 0.34
      ) +
      scale_fill_manual(values = visit_colors, guide = "none") +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("Pre", "Post"),
        expand = expansion(mult = c(0.24, 0.24))
      ) +
      coord_cartesian(ylim = c(y_lower, y_upper), clip = "on") +
      base_theme +
      theme(
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
      )

    inner_plots <- patchwork::wrap_plots(
      list(line_plot, box_plot, violin_plot),
      ncol = 3,
      widths = c(1.35, 0.95, 1.00)
    )

    if (!show_title) {
      return(inner_plots)
    }

    title_plot <- ggplot() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
      annotate("text", x = 0.5, y = 0.52, label = group_name, fontface = "bold", size = 3.35) +
      theme_void() +
      theme(plot.margin = margin(0, 2, -2, 2))

    patchwork::wrap_plots(
      list(title_plot, inner_plots),
      ncol = 1,
      heights = c(0.14, 1)
    )
  }

  build_row_annotation <- function(outcome_name) {
    ggplot() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
      annotate(
        "text",
        x = 0.01,
        y = 0.52,
        hjust = 0,
        label = unname(display_lookup[[outcome_name]]),
        fontface = "bold",
        size = 3.35
      ) +
      theme_void() +
      theme(plot.margin = margin(0, 6, -4, 6))
  }

  build_row <- function(outcome_name) {
    annotation_plot <- build_row_annotation(outcome_name)
    intg_plot <- build_group_cell(outcome_name, "INTG", show_title = TRUE)
    con_plot <- build_group_cell(outcome_name, "CON", show_title = TRUE)

    group_row <- patchwork::wrap_plots(
      list(intg_plot, con_plot),
      ncol = 2,
      widths = c(1, 1)
    )

    patchwork::wrap_plots(
      list(annotation_plot, group_row),
      ncol = 1,
      heights = c(0.18, 1)
    )
  }

  plot_rows <- lapply(selected_outcomes, build_row)
  plot_obj <- patchwork::wrap_plots(plot_rows, ncol = 1)
  ggplot2::ggsave(outfile, plot_obj, width = width, height = height, dpi = 300, bg = "white")
}

build_supplementary_figure <- function(domain_name, outfile, width, height) {
  domain_vars <- names(category_lookup[category_lookup == domain_name])
  domain_vars <- domain_vars[domain_vars %in% analysis_vars]
  plot_data <- summary_table %>%
    filter(outcome %in% domain_vars) %>%
    mutate(
      outcome = factor(outcome, levels = domain_vars),
      outcome_label = factor(outcome_label, levels = unname(display_lookup[domain_vars]))
    )

  plot_obj <- ggplot(plot_data, aes(x = visit, y = mean, color = group, group = group)) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 2.0) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.08, linewidth = 0.4) +
    facet_wrap(~ outcome_label, scales = "free_y", ncol = ifelse(domain_name == "performance", 2, 3)) +
    scale_color_manual(values = jama_group_colors, drop = FALSE) +
    labs(x = "Visit", y = "Mean (SD)", color = "Group") +
    theme_bw(base_size = 10.5) +
    theme(
      panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold"),
      legend.position = "top"
    )

  ggplot2::ggsave(outfile, plot_obj, width = width, height = height, dpi = 300, bg = "white")
}

build_sensitivity_forest <- function(outfile, width = 7.6, height = 5.8) {
  plot_data <- sensitivity_plot_data

  x_range <- range(c(plot_data$ci_low_plot, plot_data$ci_high_plot), na.rm = TRUE)
  x_span <- diff(x_range)
  if (!is.finite(x_span) || x_span <= 0) {
    x_span <- 1
  }
  label_x <- x_range[2] + x_span * 0.22

  plot_obj <- ggplot(plot_data, aes(y = outcome_label, x = adjusted_effect_favoring_intg)) +
    geom_vline(xintercept = 0, color = "#9E9E9E", linewidth = 0.4, linetype = "dashed") +
    geom_segment(
      aes(x = ci_low_plot, xend = ci_high_plot, yend = outcome_label),
      linewidth = 0.7,
      color = "#3E3E3E"
    ) +
    geom_point(
      aes(fill = ancova_group_p < 0.05),
      shape = 21,
      size = 2.8,
      stroke = 0.45,
      color = "#303030"
    ) +
    geom_text(
      aes(x = label_x, label = p_label),
      hjust = 0,
      size = 3.0,
      color = "#303030"
    ) +
    scale_fill_manual(
      values = c(`TRUE` = jama_group_colors[["INTG"]], `FALSE` = "#FFFFFF"),
      guide = "none"
    ) +
    labs(
      x = "Adjusted group effect favoring INTG (standardized units)",
      y = NULL
    ) +
    coord_cartesian(
      xlim = c(x_range[1] - x_span * 0.08, label_x + x_span * 0.14),
      clip = "off"
    ) +
    theme_bw(base_size = 10.5) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#ECECEC", linewidth = 0.25),
      axis.text.y = element_text(size = 9.1),
      axis.text.x = element_text(size = 8.8),
      plot.margin = margin(8, 54, 8, 8)
    )

  ggplot2::ggsave(outfile, plot_obj, width = width, height = height, dpi = 300, bg = "white")
}

lower_limb_main_figure <- file.path(figures_dir, "lower_limb_main.png")
start_main_figure <- file.path(figures_dir, "start_main.png")
fms_main_figure <- file.path(figures_dir, "fms_main.png")
performance_supp_figure <- file.path(figures_dir, "performance_supplementary.png")
fms_supp_figure <- file.path(figures_dir, "fms_supplementary.png")
sensitivity_forest_figure <- file.path(figures_dir, "sensitivity_forest.png")

build_main_figure(lower_limb_outcomes, lower_limb_main_figure, width = 10.2, height = 5.7)
build_main_figure(start_main_figure_outcomes, start_main_figure, width = 10.4, height = 5.6)
build_main_figure(significant_fms, fms_main_figure, width = 10.2, height = 7.0)
build_supplementary_figure("performance", performance_supp_figure, width = 11, height = 8.5)
build_supplementary_figure("FMS", fms_supp_figure, width = 12.5, height = 10)
build_sensitivity_forest(sensitivity_forest_figure, width = 7.4, height = 5.8)

build_primary_sentence <- function(outcome_name) {
  row <- main_result_table %>% filter(outcome == outcome_name)
  if (nrow(row) == 0) {
    return(NULL)
  }
  label <- as.character(row$outcome_label[[1]])
  better_high <- higher_is_better[[outcome_name]]
  con_change <- change_table %>% filter(outcome == outcome_name, group == "CON") %>% pull(change_mean)
  intg_change <- change_table %>% filter(outcome == outcome_name, group == "INTG") %>% pull(change_mean)

  direction_text <- if (isTRUE(better_high)) {
    if (intg_change > con_change) {
      "The trajectory favored INTG, with a larger increase from pre to post."
    } else {
      "The trajectory did not show a larger increase in INTG than in CON."
    }
  } else {
    if (intg_change < con_change) {
      "The trajectory favored INTG, with a larger reduction from pre to post."
    } else {
      "The trajectory did not show a larger reduction in INTG than in CON."
    }
  }

  paste0(
    label, " showed ",
    ifelse(row$interaction_p[[1]] < 0.05, "a statistically significant", "no statistically significant"),
    " group-by-visit interaction (F(",
    format_num(row$interaction_num_df[[1]], 0), ", ",
    format_num(row$interaction_den_df[[1]], 0), ") = ",
    format_num(row$interaction_f[[1]], 2), ", p = ",
    format_p(row$interaction_p[[1]]), ", ηp² = ",
    format_num(row$interaction_pes[[1]], 3), "). ",
    direction_text
  )
}

performance_results_text <- paste(vapply(primary_performance, build_primary_sentence, character(1)), collapse = " ")
fms_results_text <- paste(vapply(primary_fms, build_primary_sentence, character(1)), collapse = " ")
performance_results_text <- sanitize_ascii_text(performance_results_text)
fms_results_text <- sanitize_ascii_text(fms_results_text)

primary_ancova <- main_result_table %>%
  filter(outcome %in% sensitivity_main_outcomes) %>%
  mutate(ancova_sig = !is.na(ancova_group_p) & ancova_group_p < 0.05)

ancova_sig_labels <- primary_ancova %>%
  filter(ancova_sig) %>%
  pull(outcome_label) %>%
  as.character()

sensitivity_text <- if (length(ancova_sig_labels) > 0) {
  paste0(
    "The ANCOVA sensitivity analyses were consistent with the primary mixed ANOVA findings. ",
    "After baseline adjustment, statistically significant group effects remained evident for ",
    paste(ancova_sig_labels, collapse = ", "),
    ". These models were treated as confirmatory sensitivity analyses rather than replacements for the interaction-based primary analyses."
  )
} else {
  paste0(
    "The ANCOVA sensitivity analyses did not materially contradict the mixed ANOVA pattern, ",
    "but no primary outcome retained a statistically significant adjusted group effect at the predefined threshold. ",
    "These models were treated as supportive checks that were consistent with the primary interaction-based analyses."
  )
}
sensitivity_text <- sanitize_ascii_text(sensitivity_text)

overall_methods_text <- paste(
  "Pre- and post-test records were matched one-to-one using participant number, participant name, and group after standardizing spacing, full-width and half-width characters, letter case, and group codes.",
  "Descriptive statistics were summarized as mean (SD) for continuous variables and n (%) for categorical variables.",
  "The primary analytic framework was a 2 × 2 mixed repeated-measures ANOVA with Group (CON vs INTG) as the between-subject factor and Visit (Pre vs Post) as the within-subject factor, with the Group × Visit interaction treated as the primary effect of interest.",
  "Holm-adjusted post hoc comparisons were computed for within-group pre-post contrasts and between-group contrasts at each visit, with the main text restricted to the prespecified primary outcomes.",
  "Assumption checks included normality and homogeneity assessments, while potential outliers were screened using studentized residuals from ANCOVA models with an absolute threshold greater than 3.",
  "Sensitivity analyses were performed with ANCOVA models specified as Post ~ Group + Pre.",
  "The significance threshold was set at p < 0.05."
)

participant_results_text <- paste(
  "Participant matching confirmed 17 paired participants, including 8 in CON and 9 in INTG.",
  "No duplicate keys or unmatched participant records were detected.",
  "All 21 prespecified outcomes had complete paired values available for the primary analyses."
)
overall_methods_text <- sanitize_ascii_text(overall_methods_text)

performance_time_row <- main_result_table %>% filter(outcome == "time") %>% slice(1)
performance_distance_row <- main_result_table %>% filter(outcome == "distance") %>% slice(1)
performance_dx_row <- main_result_table %>% filter(outcome == "dx") %>% slice(1)
performance_vxh_row <- main_result_table %>% filter(outcome == "vxh") %>% slice(1)
sj_row <- main_result_table %>% filter(outcome == "sj") %>% slice(1)
cmj_row <- main_result_table %>% filter(outcome == "cmj") %>% slice(1)
slj_row <- main_result_table %>% filter(outcome == "slj") %>% slice(1)
fms_total_row <- main_result_table %>% filter(outcome == "fms_total") %>% slice(1)
fms_hurdle_r_row <- main_result_table %>% filter(outcome == "fms_hurdle_r") %>% slice(1)
fms_hurdle_l_row <- main_result_table %>% filter(outcome == "fms_hurdle_l") %>% slice(1)
fms_lunge_r_row <- main_result_table %>% filter(outcome == "fms_lunge_r") %>% slice(1)

lower_limb_results_text <- paste(
  "The lower-limb explosive power findings are included in Table 2 and Figure 1.",
  paste0(
    "No statistically significant Group x Visit interactions were observed for squat jump (F(1,15) = ",
    format_num(sj_row$interaction_f, 2), ", p = ", format_p(sj_row$interaction_p),
    ", partial eta squared = ", format_num(sj_row$interaction_pes, 3), "), countermovement jump (F(1,15) = ",
    format_num(cmj_row$interaction_f, 2), ", p = ", format_p(cmj_row$interaction_p),
    ", partial eta squared = ", format_num(cmj_row$interaction_pes, 3), "), or standing long jump (F(1,15) = ",
    format_num(slj_row$interaction_f, 2), ", p = ", format_p(slj_row$interaction_p),
    ", partial eta squared = ", format_num(slj_row$interaction_pes, 3), ")."
  ),
  "These findings indicate that changes over time did not differ significantly between INTG and CON. Detailed main-effect and post hoc results are provided in the Supplementary Materials."
)

performance_results_text <- paste(
  "The main start-performance and kinematic findings are summarized in Table 3 and Figure 2.",
  paste0(
    "Significant Group x Visit interactions were observed for water-entry distance (F(1,15) = ",
    format_num(performance_distance_row$interaction_f, 2), ", p = ", format_p(performance_distance_row$interaction_p),
    ", partial eta squared = ", format_num(performance_distance_row$interaction_pes, 3), "), horizontal velocity (F(1,15) = ",
    format_num(performance_vxh_row$interaction_f, 2), ", p = ", format_p(performance_vxh_row$interaction_p),
    ", partial eta squared = ", format_num(performance_vxh_row$interaction_pes, 3), "), 15-m sprint time (F(1,15) = ",
    format_num(performance_time_row$interaction_f, 2), ", p = ", format_p(performance_time_row$interaction_p),
    ", partial eta squared = ", format_num(performance_time_row$interaction_pes, 3), "), and horizontal displacement (Delta x) (F(1,15) = ",
    format_num(performance_dx_row$interaction_f, 2), ", p = ", format_p(performance_dx_row$interaction_p),
    ", partial eta squared = ", format_num(performance_dx_row$interaction_pes, 3), "). These interaction effects were in favor of INTG."
  ),
  paste0(
    "INTG reduced 15-m sprint time from ", performance_time_row$INTG_Pre, " s to ", performance_time_row$INTG_Post,
    " s, whereas CON changed from ", performance_time_row$CON_Pre, " s to ", performance_time_row$CON_Post,
    " s. Water-entry distance increased from ", performance_distance_row$INTG_Pre, " cm to ", performance_distance_row$INTG_Post,
    " cm in INTG, while the corresponding change in CON was small."
  ),
  paste0(
    "A comparable pattern was observed for the kinematic outcomes. INTG showed larger gains in horizontal displacement (",
    performance_dx_row$INTG_Pre, " cm to ", performance_dx_row$INTG_Post, " cm) and horizontal velocity (",
    performance_vxh_row$INTG_Pre, " m/s to ", performance_vxh_row$INTG_Post,
    " m/s), whereas CON displayed only modest displacement changes together with a decline in horizontal velocity."
  ),
  "Holm-adjusted post hoc comparisons showed significant pre-to-post improvements in INTG for the four primary start-performance outcomes, whereas the clearest post-test between-group separation was observed for horizontal velocity.",
  "Figure 2 displays, from left to right within each group cell, paired trajectories, boxplots, and half-violin summaries for the Pre and Post distributions; inferential conclusions were based on the mixed repeated-measures ANOVA rather than on the visual summaries alone."
)

fms_results_text <- paste(
  "The functional movement findings are presented in Table 4 and Figure 3.",
  paste0(
    "Significant Group x Visit interactions were found for FMS Total (F(1,15) = ",
    format_num(fms_total_row$interaction_f, 2), ", p = ", format_p(fms_total_row$interaction_p),
    ", partial eta squared = ", format_num(fms_total_row$interaction_pes, 3), "), FMS Hurdle Step, right (F(1,15) = ",
    format_num(fms_hurdle_r_row$interaction_f, 2), ", p = ", format_p(fms_hurdle_r_row$interaction_p),
    ", partial eta squared = ", format_num(fms_hurdle_r_row$interaction_pes, 3), "), FMS Hurdle Step, left (F(1,15) = ",
    format_num(fms_hurdle_l_row$interaction_f, 2), ", p = ", format_p(fms_hurdle_l_row$interaction_p),
    ", partial eta squared = ", format_num(fms_hurdle_l_row$interaction_pes, 3), "), and FMS In-line Lunge, right (F(1,15) = ",
    format_num(fms_lunge_r_row$interaction_f, 2), ", p = ", format_p(fms_lunge_r_row$interaction_p),
    ", partial eta squared = ", format_num(fms_lunge_r_row$interaction_pes, 3), "). The pattern of change was more favorable in INTG than in CON."
  ),
  paste0(
    "The standard FMS Total score increased from ", fms_total_row$INTG_Pre, " to ", fms_total_row$INTG_Post,
    " in INTG, whereas CON changed from ", fms_total_row$CON_Pre, " to ", fms_total_row$CON_Post,
    ". Improvements were also apparent in unilateral control tasks, including FMS Hurdle Step, right (",
    fms_hurdle_r_row$INTG_Pre, " to ", fms_hurdle_r_row$INTG_Post, "), FMS Hurdle Step, left (",
    fms_hurdle_l_row$INTG_Pre, " to ", fms_hurdle_l_row$INTG_Post, "), and FMS In-line Lunge, right (",
    fms_lunge_r_row$INTG_Pre, " to ", fms_lunge_r_row$INTG_Post, ")."
  ),
  "Only the outcomes with significant interaction effects are described in detail here; the full FMS panel is provided in Table 4. Figure 3 is restricted to the FMS outcomes with statistically significant interaction effects."
)

overall_methods_text <- paste(
  "Pre- and post-test records were matched one-to-one using participant number, participant name, and group after standardizing spacing, full-width and half-width characters, letter case, and group codes.",
  "Descriptive statistics were summarized as mean (SD) for continuous variables and n (%) for categorical variables.",
  "The primary analytic framework was a 2 x 2 mixed repeated-measures ANOVA with Group (CON vs INTG) as the between-subject factor and Visit (Pre vs Post) as the within-subject factor, with the Group x Visit interaction treated as the primary effect of interest.",
  "Within-group pre-to-post standardized changes were additionally summarized using Cohen's d based on the pooled pre/post standard deviation.",
  "Holm-adjusted post hoc comparisons were computed for within-group pre-post contrasts and between-group contrasts at each visit, with the main text restricted to the prespecified primary outcomes.",
  "Assumption checks included normality and homogeneity assessments, while potential outliers were screened using studentized residuals from ANCOVA models with an absolute threshold greater than 3.",
  "Sensitivity analyses were performed with ANCOVA models specified as Post ~ Group + Pre.",
  "The significance threshold was set at p < 0.05."
)

participant_results_text <- paste(
  "No sports-related injuries occurred during testing or training. Training compliance was high in both groups (INTG: 95.5%; CON: 96.0%), and all 17 participants completed the intervention (INTG: n = 9; CON: n = 8).",
  "All 21 prespecified outcomes had complete paired values, and no missing outcome data were identified; therefore, all analyses were conducted on complete cases.",
  "Baseline characteristics and pre-intervention performance measures are summarized in Table 1, and no statistically significant between-group differences were observed at baseline."
)

lower_limb_results_text <- sanitize_ascii_text(lower_limb_results_text)
performance_results_text <- sanitize_ascii_text(performance_results_text)
fms_results_text <- sanitize_ascii_text(fms_results_text)
overall_methods_text <- sanitize_ascii_text(overall_methods_text)
participant_results_text <- sanitize_ascii_text(participant_results_text)

mk_ft <- function(df, font_size = 8.5) {
  flextable::flextable(df) %>%
    flextable::theme_booktabs() %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::autofit() %>%
    flextable::line_spacing(space = 1.0, part = "all") %>%
    flextable::padding(padding = 3, part = "all")
}

mk_display_ft <- function(df, font_size = 8.1) {
  ft <- flextable::flextable(df) %>%
    flextable::theme_booktabs() %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::align(j = 1:2, align = "left", part = "all") %>%
    flextable::align(j = 3:ncol(df), align = "center", part = "all") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::line_spacing(space = 1.0, part = "all") %>%
    flextable::padding(padding = 3, part = "all") %>%
    flextable::autofit()

  outcome_rows <- which(df$Outcome != "")
  if (length(outcome_rows) > 0) {
    ft <- flextable::bold(ft, i = outcome_rows, j = 1, bold = TRUE, part = "body")
    ft <- flextable::hline(
      ft,
      i = outcome_rows,
      border = officer::fp_border(width = 0.9, color = "#595959"),
      part = "body"
    )
  }

  ft
}

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Statistical Analysis Report for the INT Study", style = "heading 1")
doc <- officer::body_add_par(doc, paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "."), style = "Normal")
doc <- officer::body_add_par(doc, "1. Statistical Methods", style = "heading 2")
doc <- officer::body_add_par(doc, overall_methods_text, style = "Normal")
doc <- officer::body_add_par(doc, power_note, style = "Normal")
doc <- officer::body_add_par(doc, "2. Results", style = "heading 2")
doc <- officer::body_add_par(doc, "2.1 Participant matching and data completeness", style = "heading 3")
doc <- officer::body_add_par(doc, participant_results_text, style = "Normal")
doc <- flextable::body_add_flextable(doc, mk_ft(qc_summary, font_size = 9))
doc <- officer::body_add_par(doc, "2.2 Baseline characteristics", style = "heading 3")
doc <- officer::body_add_par(doc, "Table 1. Baseline characteristics and pre-intervention performance variables.", style = "Normal")
doc <- flextable::body_add_flextable(doc, mk_ft(baseline_table_doc, font_size = 9))
doc <- officer::body_add_par(doc, "Note. Data are presented as mean (SD) or n (%). Baseline p values are descriptive rather than decisive evidence of comparability. BMI = body mass index; SJ = squat jump; CMJ = countermovement jump; SLJ = standing long jump; FMS = Functional Movement Screen.", style = "Normal")
doc <- officer::body_add_par(doc, "2.3 Lower-limb explosive power outcomes", style = "heading 3")
doc <- officer::body_add_par(doc, lower_limb_results_text, style = "Normal")
doc <- officer::body_add_par(doc, "Table 2. Lower-limb explosive power outcomes from the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_end_section_landscape(doc)
doc <- flextable::body_add_flextable(doc, mk_display_ft(lower_limb_table_doc, font_size = 8.1))
doc <- officer::body_add_par(doc, "Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen's d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Full main effects, change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials. SJ = squat jump; CMJ = countermovement jump; SLJ = standing long jump.", style = "Normal")
doc <- officer::body_add_par(doc, "Figure 1. Lower-limb explosive power outcomes arranged by rows and groups by columns. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Inferential conclusions were based on the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_add_img(doc, src = lower_limb_main_figure, width = 8.5, height = 5.1)
doc <- officer::body_end_section_portrait(doc)
doc <- officer::body_add_par(doc, "2.4 Start-performance and kinematic outcomes", style = "heading 3")
doc <- officer::body_add_par(doc, performance_results_text, style = "Normal")
doc <- officer::body_add_par(doc, "Table 3. Start-performance and kinematic outcomes from the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_end_section_landscape(doc)
doc <- flextable::body_add_flextable(doc, mk_display_ft(start_kinematic_table_doc, font_size = 8.0))
doc <- officer::body_add_par(doc, "Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen's d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Full main effects, change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials.", style = "Normal")
doc <- officer::body_add_par(doc, "Figure 2. Start-performance outcomes arranged by rows and groups by columns. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Horizontal displacement (Delta x) and Delta t are presented in the Supplementary Materials, and inferential conclusions were based on the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_add_img(doc, src = start_main_figure, width = 8.5, height = 5.5)
doc <- officer::body_end_section_portrait(doc)
doc <- officer::body_add_par(doc, "2.5 Functional movement outcomes", style = "heading 3")
doc <- officer::body_add_par(doc, fms_results_text, style = "Normal")
doc <- officer::body_add_par(doc, "Table 4. Functional movement outcomes from the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_end_section_landscape(doc)
doc <- flextable::body_add_flextable(doc, mk_display_ft(fms_main_table_doc, font_size = 7.4))
doc <- officer::body_add_par(doc, "Note. Data are presented as mean (SD). Post-pre difference (%) was calculated as ((Post - Pre) / Pre) x 100. Post-pre ES (d) represents the within-group standardized change from pre to post, calculated as (Post - Pre) / sqrt((SD_pre^2 + SD_post^2)/2). Cohen's d magnitudes were interpreted as trivial for |d| < 0.20, small for 0.20-0.49, moderate for 0.50-0.79, and large for >= 0.80. The primary inference is the Group x Visit interaction from the mixed repeated-measures ANOVA. * p < 0.05, ** p < 0.01, and *** p < 0.001 for Holm-adjusted within-group Pre vs Post comparisons; # indicates a Holm-adjusted between-group difference at Post. Detailed change scores, Holm-adjusted post hoc p values, and ANCOVA outputs are reported in the Supplementary Materials. FMS = Functional Movement Screen.", style = "Normal")
doc <- officer::body_add_par(doc, "Figure 3. FMS outcomes with statistically significant Group x Visit interactions displayed in the same row-by-column layout as Figures 1 and 2. Within each group cell, paired trajectories, boxplots, and half-violin distributions are displayed from left to right for the Pre and Post measurements. ES annotations are displayed above the trajectory panels, and asterisks denote significance levels based on Holm-adjusted within-group Pre vs Post comparisons. Inferential conclusions were based on the mixed repeated-measures ANOVA.", style = "Normal")
doc <- officer::body_add_img(doc, src = fms_main_figure, width = 8.5, height = 6.6)
doc <- officer::body_end_section_portrait(doc)
doc <- officer::body_add_par(doc, "2.6 Sensitivity analyses", style = "heading 3")
doc <- officer::body_add_par(doc, sensitivity_text, style = "Normal")
doc <- officer::body_add_par(doc, "Figure 4. Baseline-adjusted confirmatory analyses for the selected performance outcomes and FMS Total. Positive standardized effects indicate a direction favoring INTG after baseline adjustment, and horizontal lines denote 95% confidence intervals. These estimates were consistent with the primary mixed-ANOVA analyses.", style = "Normal")
doc <- officer::body_add_img(doc, src = sensitivity_forest_figure, width = 6.7, height = 5.2)
doc <- officer::body_add_par(doc, "3. Supplementary statistical outputs", style = "heading 2")
doc <- officer::body_add_par(doc, "Supplementary Table S1. Full mixed ANOVA effects for all outcomes, including Group, Visit, and Group x Visit terms.", style = "Normal")
doc <- officer::body_end_section_landscape(doc)
doc <- flextable::body_add_flextable(doc, mk_ft(appendix_anova_doc, font_size = 7.2))
doc <- officer::body_add_par(doc, "Supplementary Table S2. Detailed descriptive results, change scores, and Holm-adjusted post hoc comparisons for all outcomes.", style = "Normal")
doc <- flextable::body_add_flextable(doc, mk_ft(supplementary_posthoc_doc, font_size = 7.2))
doc <- officer::body_add_par(doc, "Supplementary Table S3. ANCOVA sensitivity analyses for the selected performance outcomes and FMS Total, with adjusted effect estimates and 95% confidence intervals.", style = "Normal")
doc <- flextable::body_add_flextable(doc, mk_ft(supplementary_ancova_ci_doc, font_size = 7.2))
doc <- flextable::body_add_flextable(doc, mk_ft(appendix_ancova_doc, font_size = 7.0))
doc <- officer::body_add_par(doc, "Supplementary Table S4. Assumption checks and outlier screening for all outcomes.", style = "Normal")
doc <- flextable::body_add_flextable(doc, mk_ft(supplementary_assumptions_outliers_doc, font_size = 6.9))
doc <- officer::body_end_section_portrait(doc)
doc <- officer::body_add_par(doc, "Supplementary Figure S1. Performance outcomes across the full analyzed performance panel, including Flight-time difference (Delta t).", style = "Normal")
doc <- officer::body_add_img(doc, src = performance_supp_figure, width = 6.8, height = 5.2)
doc <- officer::body_add_par(doc, "Supplementary Figure S2. FMS outcomes across all analyzed FMS variables.", style = "Normal")
doc <- officer::body_add_img(doc, src = fms_supp_figure, width = 6.8, height = 5.4)

print(doc, target = report_file)

cat("Analysis completed successfully.\n")
cat("Report:", report_file, "\n")
cat("Output directory:", output_dir, "\n")
cat("Figures directory:", figures_dir, "\n")
