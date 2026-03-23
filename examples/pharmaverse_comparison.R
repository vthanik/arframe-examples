# ============================================================================
# pharmaverse ADAM Datasets — GSK Process vs arframe (3 Tables)
# ============================================================================
#
# Uses pharmaverseadam::adsl, adae, advs (real CDISC ADaM structure).
# Each table shows two paths:
#   Path A: GSK pharmaverse stack (cards + tfrmt + gt + docorator)
#   Path B: arframe (cards + fr_wide_ard + fr_table pipeline)
#
# Tables:
#   1. Demographics (Table 14.1.1) — group_by + group_bold + blank_after
#   2. AE by SOC/PT (Table 14.3.1) — hierarchical + indent + bold rows
#   3. Vital Signs (Table 14.3.6) — page_by parameter, baseline/change,
#      wide column split, per-page N-counts
#
# All examples use page headers/footers (pagehead/pagefoot).

# ── Package setup ───────────────────────────────────────────────────────────

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, quiet = TRUE)
  }
}

ensure_packages(c("pharmaverseadam", "cards", "dplyr", "tidyr"))

library(pharmaverseadam)
library(arframe)
library(cards)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

outdir <- file.path(tempdir(), "pharmaverse_demo")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


# ── Study-wide theme (set once, all tables inherit) ────────────────────────

fr_theme_reset()
fr_theme(
  font_size = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines = "header",
  header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(
    left = "Protocol: CDISCPILOT01",
    right = "CONFIDENTIAL"
  ),
  pagefoot = list(
    left = "{program}",
    right = "Page {thepage} of {total_pages}"
  )
)


# ── Shared: filter to safety population, get arm N-counts ──────────────────

# Convert blanks to NA in all datasets
blank_to_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) ifelse(nzchar(trimws(x)), x, NA_character_) else x
  })
  df
}

adsl_saf <- blank_to_na(pharmaverseadam::adsl) |>
  filter(SAFFL == "Y", TRT01A != "Screen Failure")

arm_n <- adsl_saf |>
  count(TRT01A) |>
  pull(n, name = TRT01A)

arm_levels <- names(arm_n)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 1: Demographics and Baseline Characteristics                    ║
# ║  Features: group_by, group_bold, blank_after, decimal alignment        ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("\n================================================================\n")
cat("  TABLE 1: Demographics (Table 14.1.1)\n")
cat("================================================================\n\n")

# ── Step 1: Create ARD with cards (same for both paths) ──────────────────

demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "TRT01A",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("AGEGR1", "SEX", "RACE", "ETHNIC")),
  .overall = TRUE
)

cat("Demographics ARD:", nrow(demog_ard), "rows\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH A: GSK pharmaverse stack (cards → tfrmt → gt → docorator)     │
# └──────────────────────────────────────────────────────────────────────┘
#
# In real GSK code (t_sp_dmt01.R — 358 lines), this involves:
#   1. Reshape ARD: unlist columns, extract group levels, recode labels,
#      create bigN rows, add sorting columns (~150 lines)
#   2. tfrmt(): body_plan with frmt_structure per variable, big_n,
#      col_plan, row_grp_plan, page_plan (~50 lines)
#   3. print_to_gt() + gsk_styling() (~15 lines)
#   4. tfl_format() → as_docorator() → render_pdf() (~15 lines)
#
# We show the reshape + tfrmt spec (commented — requires tfrmt/gt/docorator):

cat("\n--- PATH A: GSK pharmaverse approach ---\n")
cat(
  '
# Step 1: Reshape ARD for tfrmt (~150 lines in real GSK code)
# tfrmt needs: group | label | column | param | value
# Must manually: unlist list-columns, extract group1_level,
# recode variable names to display labels, create bigN sentinel
# rows, add ord1/ord2 sorting columns, handle missing levels...
#
# ard_tbl <- demog_ard |>
#   mutate(
#     TRT01A = unlist(lapply(group1_level, as.character)),
#     label = unlist(lapply(variable_level, as.character)),
#     stat = as.double(stat),
#     stat = if_else(stat_name == "p", stat * 100, stat)
#   ) |>
#   select(-starts_with("group"), -warning, -error, -fmt_fun) |>
#   mutate(
#     label = if_else(is.na(label), stat_label, label),
#     stat_name = if_else(variable == "..ard_total_n..", "bigN", stat_name),
#     variable = case_when(
#       variable == "AGE" ~ "Age (years)",
#       variable == "SEX" ~ "Sex",
#       variable == "RACE" ~ "Race",
#       ... # ~20 more lines of relabeling
#     ),
#     ord1 = case_when(variable == "Sex" ~ 1, variable == "Age" ~ 2, ...)
#   )
#
# Step 2: tfrmt specification (~50 lines)
# demog_tfrmt <- tfrmt(
#   group = variable, label = label, column = TRT01A,
#   param = stat_name, value = stat,
#   sorting_cols = c(ord1, ord2),
#   body_plan = body_plan(
#     frmt_structure(group_val = ".default", label_val = ".default",
#       frmt_combine("{n}{p}", n = frmt("xxx"), p = frmt_when(...))),
#     frmt_structure(group_val = "Age (years)", label_val = "Mean",
#       frmt("xxx.x")),
#     frmt_structure(group_val = "Age (years)", label_val = "SD",
#       frmt("xxx.xx")),
#     ... # one frmt_structure per variable × stat
#   ),
#   big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\\n(N=xx)")),
#   row_grp_plan = row_grp_plan(
#     row_grp_structure(group_val = ".default", element_block(post_space = " ")),
#     label_loc = element_row_grp_loc(location = "indented")
#   )
# ) |> print_to_gt(ard_tbl)
#
# Step 3: gsk_styling() + tfl_format() → as_docorator() → render_pdf()
# ~30 lines including fancyhead/fancyfoot/geom_set
'
)
cat("Lines: ~250 (reshape + spec + styling + render)\n")
cat("Packages: cards + tfrmt + gt + docorator (4)\n\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH B: arframe (cards → fr_wide_ard → fr_table pipeline)          │
# └──────────────────────────────────────────────────────────────────────┘

cat("--- PATH B: arframe approach ---\n\n")

demog_wide <- fr_wide_ard(
  demog_ard,
  statistic = list(
    continuous = c(
      "n" = "{N}",
      "Mean (SD)" = "{mean} ({sd})",
      "Median" = "{median}",
      "Min, Max" = "{min}, {max}"
    ),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, median = 1, p = 0),
  label = c(
    AGE = "Age (years)",
    AGEGR1 = "Age Group, n (%)",
    SEX = "Sex, n (%)",
    RACE = "Race, n (%)",
    ETHNIC = "Ethnicity, n (%)"
  )
)

demog_spec <- demog_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Summary of Demographics and Baseline Characteristics",
    "Safety Population"
  ) |>
  fr_cols(
    variable = fr_col(visible = FALSE),
    stat_label = fr_col(" ", width = 2.5),
    !!!setNames(
      lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
      arm_levels
    ),
    Total = fr_col("Total", align = "decimal"),
    .n = c(arm_n, Total = sum(arm_n))
  ) |>
  fr_rows(
    group_by = "variable",
    group_label = "stat_label",
    group_bold = TRUE,
    blank_after = "variable"
  ) |>
  fr_footnotes(
    "Percentages based on N per treatment group.",
    "CDISCPILOT01 Safety Population."
  )

demog_pdf <- file.path(outdir, "t_14_1_1_demog.pdf")
demog_spec |> fr_render(demog_pdf)
demog_spec |> fr_render(file.path(outdir, "t_14_1_1_demog.rtf"))

cat("Lines: ~35 (fr_wide_ard + fr_table pipeline)\n")
cat("Packages: cards + arframe (2)\n")
cat("PDF:", demog_pdf, "\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 2: AE by SOC and Preferred Term                                ║
# ║  Features: hierarchical, indent_by, bold SOC/total, continuation      ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  TABLE 2: AE by SOC/PT (Table 14.3.1)\n")
cat("================================================================\n\n")

# ── Step 1: Create hierarchical ARD with cards ────────────────────────────

adae_te <- blank_to_na(pharmaverseadam::adae) |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

ae_ard <- ard_stack_hierarchical(
  data = adae_te,
  variables = c(AEBODSYS, AEDECOD),
  by = TRT01A,
  denominator = adsl_saf,
  id = USUBJID
)

cat("Hierarchical AE ARD:", nrow(ae_ard), "rows\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH A: GSK pharmaverse stack                                       │
# └──────────────────────────────────────────────────────────────────────┘

cat("\n--- PATH A: GSK pharmaverse approach ---\n")
cat(
  '
# In real GSK code (t_saf_ae01_all.R — 621 lines):
#   - 6 × ard_stack_hierarchical() calls for subgroups (~90 lines each)
#   - Manual ARD cleanup: unlist, complete missing 0 records, sort by
#     frequency, create bigN rows, add ordering columns (~200 lines)
#   - tfrmt() with frmt_combine("{n} {p}") + frmt_when for p-value
#     formatting, row_grp_plan, col_plan (~50 lines)
#   - print_to_gt() + gsk_styling() + cols_width() (~15 lines)
#   - tfl_format() with geom_set(landscape=TRUE, left="0.4in") (~15 lines)
#
# Total: ~621 lines, 5+ packages
'
)
cat("Lines: ~621\n")
cat("Packages: cards + tfrmt + gt + docorator + tidyverse (5+)\n\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH B: arframe                                                     │
# └──────────────────────────────────────────────────────────────────────┘

cat("--- PATH B: arframe approach ---\n\n")

ae_wide <- fr_wide_ard(
  ae_ard,
  statistic = "{n} ({p}%)",
  decimals = c(p = 0),
  label = c("..ard_hierarchical_overall.." = "Any TEAE")
)

ae_spec <- ae_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list(
      "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
      bold = TRUE
    ),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc = fr_col(visible = FALSE),
    pt = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type = fr_col(visible = FALSE),
    !!!setNames(
      lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
      arm_levels
    ),
    .n = arm_n
  ) |>
  fr_rows(group_by = "soc", group_label = "pt", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA coding dictionary version used.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence."
  )

ae_pdf <- file.path(outdir, "t_14_3_1_ae_soc.pdf")
ae_spec |> fr_render(ae_pdf)
ae_spec |> fr_render(file.path(outdir, "t_14_3_1_ae_soc.rtf"))

cat("Lines: ~35 (fr_wide_ard + fr_table pipeline)\n")
cat("Packages: cards + arframe (2)\n")
cat("PDF:", ae_pdf, "\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 3: Vital Signs Summary by Parameter                            ║
# ║  Features: page_by, baseline + change from baseline, wide columns,    ║
# ║            column split, per-page N-counts, spanning headers           ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  TABLE 3: Vital Signs (Table 14.3.6)\n")
cat("================================================================\n\n")

# ── Shared data prep: summarize ADVS ──────────────────────────────────────

advs_clean <- blank_to_na(pharmaverseadam::advs) |>
  filter(
    SAFFL == "Y",
    TRT01A != "Screen Failure",
    PARAMCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP"),
    AVISIT %in% c("Baseline", "Week 12", "End of Treatment"),
    ANL01FL == "Y"
  )

# Continuous stats: Baseline, Post-baseline value, Change from baseline
vs_stats <- advs_clean |>
  filter(AVISIT != "Baseline") |>
  group_by(PARAM, AVISIT, TRT01A) |>
  summarise(
    n = as.character(n()),
    base_mean = sprintf("%.1f", mean(BASE, na.rm = TRUE)),
    base_sd = sprintf("%.2f", sd(BASE, na.rm = TRUE)),
    val_mean = sprintf("%.1f", mean(AVAL, na.rm = TRUE)),
    val_sd = sprintf("%.2f", sd(AVAL, na.rm = TRUE)),
    chg_mean = sprintf("%.1f", mean(CHG, na.rm = TRUE)),
    chg_sd = sprintf("%.2f", sd(CHG, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # Combine into display strings
  mutate(
    baseline = paste0(base_mean, " (", base_sd, ")"),
    value = paste0(val_mean, " (", val_sd, ")"),
    cfb = paste0(chg_mean, " (", chg_sd, ")")
  ) |>
  select(PARAM, AVISIT, TRT01A, n, baseline, value, cfb)

# Pivot to wide: one row per (PARAM × AVISIT × stat), columns per arm
vs_long <- vs_stats |>
  pivot_longer(
    cols = c(n, baseline, value, cfb),
    names_to = "statistic",
    values_to = "val"
  ) |>
  mutate(
    statistic = case_when(
      statistic == "n" ~ "n",
      statistic == "baseline" ~ "Baseline Mean (SD)",
      statistic == "value" ~ "Post-baseline Mean (SD)",
      statistic == "cfb" ~ "Change Mean (SD)"
    )
  ) |>
  pivot_wider(names_from = TRT01A, values_from = val) |>
  arrange(
    PARAM,
    AVISIT,
    match(
      statistic,
      c(
        "n",
        "Baseline Mean (SD)",
        "Post-baseline Mean (SD)",
        "Change Mean (SD)"
      )
    )
  )

# Per-parameter N-counts for headers
vs_n <- advs_clean |>
  filter(AVISIT != "Baseline") |>
  distinct(USUBJID, PARAM, TRT01A) |>
  count(PARAM, TRT01A) |>
  pivot_wider(names_from = TRT01A, values_from = n) |>
  select(-PARAM)

# Use first param's N as representative (they're similar)
n_vs <- unlist(vs_n[1, ])


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH A: GSK pharmaverse stack                                       │
# └──────────────────────────────────────────────────────────────────────┘

cat("--- PATH A: GSK pharmaverse approach ---\n")
cat(
  '
# GSK approach for vital signs:
#   - cards::ard_stack() with ard_continuous() per timepoint per param
#   - Manual reshape: baseline, value, change as separate params
#   - tfrmt() with page_plan(page_structure(group_val = ".default"))
#     for per-parameter pages — BUT tfrmt page_plan does not support
#     per-page N-counts or column splitting
#   - Spanning headers (Baseline / Post-baseline / CFB) must be manually
#     constructed in gt::tab_spanner()
#   - Each parameter rendered as separate gt + docorator call
#   - Repeating column headers on page 2+: NOT POSSIBLE in gt
#   - Column splitting for wide tables: NOT POSSIBLE in gt
#
# In practice, GSK generates one .R file per parameter or uses
# a loop with separate tfrmt specs. ~400+ lines total.
'
)
cat("Lines: ~400+\n")
cat("Packages: cards + tfrmt + gt + docorator (4)\n")
cat("Limitations: no column split, no per-page N, no repeating headers\n\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH B: arframe — page_by + spanning headers + column split         │
# └──────────────────────────────────────────────────────────────────────┘

cat("--- PATH B: arframe approach ---\n\n")

vs_spec <- vs_long |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6",
    list("Vital Signs — Summary by Parameter and Visit", bold = TRUE),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    PARAM = fr_col(visible = FALSE),
    AVISIT = fr_col(visible = FALSE),
    statistic = fr_col("Statistic", width = 2.0, stub = TRUE),
    !!!setNames(
      lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
      arm_levels
    ),
    .n = n_vs,
    .split = TRUE
  ) |>
  fr_rows(
    page_by = "PARAM",
    group_by = "AVISIT",
    group_bold = TRUE,
    blank_after = "AVISIT"
  ) |>
  fr_footnotes(
    "Baseline is the last non-missing value on or before first dose.",
    "Change = Post-baseline value - Baseline value.",
    "Each parameter on a separate page with per-page N-counts."
  )

vs_pdf <- file.path(outdir, "t_14_3_6_vital_signs.pdf")
vs_spec |> fr_render(vs_pdf)
vs_spec |> fr_render(file.path(outdir, "t_14_3_6_vital_signs.rtf"))

cat("Lines: ~35 (data prep separate, same for both paths)\n")
cat("Packages: cards + arframe (2)\n")
cat("Features: page_by (1 page per param), column split,\n")
cat("  per-page N-counts, spanning headers, decimal alignment,\n")
cat("  repeating column headers, continuation text\n")
cat("PDF:", vs_pdf, "\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Summary                                                               ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  COMPARISON SUMMARY\n")
cat("================================================================\n\n")

cat(sprintf("  %-35s  %-20s  %-15s\n", "", "GSK pharmaverse", "arframe"))
cat(sprintf(
  "  %-35s  %-20s  %-15s\n",
  "Demographics (Table 14.1.1)",
  "~250 lines, 4 pkgs",
  "~35 lines, 2 pkgs"
))
cat(sprintf(
  "  %-35s  %-20s  %-15s\n",
  "AE SOC/PT (Table 14.3.1)",
  "~621 lines, 5+ pkgs",
  "~35 lines, 2 pkgs"
))
cat(sprintf(
  "  %-35s  %-20s  %-15s\n",
  "Vital Signs (Table 14.3.6)",
  "~400+ lines, 4 pkgs",
  "~35 lines, 2 pkgs"
))
cat(sprintf("  %-35s  %-20s  %-15s\n", "", "─────────────", "────────────"))
cat(sprintf("  %-35s  %-20s  %-15s\n", "Total", "~1,271 lines", "~105 lines"))

cat("\n  arframe-only features (not possible in pharmaverse stack):\n")
cat("    - Decimal alignment (numbers line up on the decimal point)\n")
cat("    - Paginated multi-page PDF with repeating column headers\n")
cat("    - Column splitting for wide tables (stub repeats per panel)\n")
cat("    - page_by: one spec, separate pages per parameter\n")
cat("    - Per-page N-counts (update automatically per page_by group)\n")
cat("    - Continuation text ('(continued)') on page 2+\n")
cat("    - Study-wide theme (set once, all tables inherit)\n")
cat("    - Same spec → PDF + RTF + HTML (3 formats, zero extra code)\n")

cat("\n  Output directory:", outdir, "\n")
cat("    t_14_1_1_demog.pdf       — Demographics\n")
cat("    t_14_1_1_demog.rtf       — Demographics (RTF)\n")
cat("    t_14_3_1_ae_soc.pdf      — AE by SOC/PT\n")
cat("    t_14_3_1_ae_soc.rtf      — AE by SOC/PT (RTF)\n")
cat("    t_14_3_6_vital_signs.pdf — Vital Signs (page_by parameter)\n")
cat("    t_14_3_6_vital_signs.rtf — Vital Signs (RTF)\n")

fr_theme_reset()
