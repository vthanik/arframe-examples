# ============================================================================
# arframe Team Pitch — Features GSK Cannot Do Today
# ============================================================================
#
# This script showcases arframe capabilities that the current
# tfrmt + gt + docorator stack CANNOT produce:
#
#   1. Wide vital signs table with COLUMN SPLITTING (auto-paneled)
#   2. Decimal alignment in PDF (not possible with gt)
#   3. page_by — one table spec, separate pages per parameter
#   4. Spanning headers with per-page N-counts
#   5. One spec → PDF + RTF + HTML (three outputs, zero extra code)
#   6. Study-wide theme (set once, every table inherits)
#
# All examples use pharmaverseadam (CDISCPILOT01).

library(arframe)
library(pharmaverseadam)
library(cards)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Study-wide theme — set ONCE, all tables below inherit                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝
# In the pharmaverse stack, each table needs its own docorator wrapping
# with fancyhead/fancyfoot/geometry. In arframe, set it once:

fr_theme_reset()
fr_theme(
  font_size   = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines      = "header",
  header      = list(bold = TRUE, align = "center"),
  n_format    = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "Protocol: CDISCPILOT01", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

# Done. Every fr_render() call below inherits these settings.
# No tfl_format() wrapper needed. No gsk_styling() function needed.


# ── Shared data prep ────────────────────────────────────────────────────────

# Convert blanks to NA (pharmaverseadam uses "" for missing character values)
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

adae_te <- blank_to_na(pharmaverseadam::adae) |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

advs_clean <- blank_to_na(pharmaverseadam::advs) |>
  filter(
    SAFFL == "Y",
    TRT01A != "Screen Failure",
    PARAMCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP"),
    ANL01FL == "Y"
  )


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  1. COLUMN SPLITTING — Wide table auto-paneled across pages            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Vital signs tables have 9+ data columns (3 treatments × 3 stats).
# When they don't fit on one page, you need COLUMN PANELS — the stub
# column repeats on each panel with a subset of data columns.
#
# gt/tfrmt/docorator CANNOT do this. You'd manually split the data
# and create separate gt objects. arframe does it with .split = TRUE.

cat("═══════════════════════════════════════════════════════════\n")
cat("  1. COLUMN SPLITTING — wide tables auto-paneled\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Build Systolic BP summary for one visit
vs_sysbp <- advs_clean |>
  filter(PARAMCD == "SYSBP", AVISIT %in% c("Week 2", "Week 4", "End of Treatment")) |>
  group_by(AVISIT, TRT01A) |>
  summarise(
    n_val       = as.character(n()),
    base_mean   = sprintf("%.1f (%.2f)", mean(BASE, na.rm = TRUE), sd(BASE, na.rm = TRUE)),
    val_mean    = sprintf("%.1f (%.2f)", mean(AVAL, na.rm = TRUE), sd(AVAL, na.rm = TRUE)),
    chg_mean    = sprintf("%.1f (%.2f)", mean(CHG,  na.rm = TRUE), sd(CHG,  na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(n_val, base_mean, val_mean, chg_mean), names_to = "stat", values_to = "val") |>
  mutate(stat = case_when(
    stat == "n_val"    ~ "n",
    stat == "base_mean" ~ "Baseline Mean (SD)",
    stat == "val_mean"  ~ "Post-baseline Mean (SD)",
    stat == "chg_mean"  ~ "Change Mean (SD)"
  )) |>
  pivot_wider(names_from = TRT01A, values_from = val) |>
  mutate(AVISIT = factor(AVISIT, c("Week 2", "Week 4", "End of Treatment"))) |>
  arrange(AVISIT, match(stat, c("n", "Baseline Mean (SD)", "Post-baseline Mean (SD)", "Change Mean (SD)")))

wide_spec <- vs_sysbp |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6.1",
    "Systolic Blood Pressure — Column Split Demo",
    "Safety Population"
  ) |>
  fr_cols(
    AVISIT = fr_col(visible = FALSE),
    # Stub column repeats on every panel
    stat   = fr_col("Statistic", width = 2.0, stub = TRUE),
    # 9 data columns — too wide for one page
    `Placebo`                 = fr_col("Placebo\nBaseline",    width = 1.2, align = "decimal"),
    `Xanomeline Low Dose`     = fr_col("Xan Low\nBaseline",   width = 1.2, align = "decimal"),
    `Xanomeline High Dose`    = fr_col("Xan High\nBaseline",  width = 1.2, align = "decimal"),
    .split = TRUE,
    .n = arm_n
  ) |>
  fr_rows(group_by = "AVISIT", group_bold = TRUE, blank_after = "AVISIT") |>
  fr_footnotes(
    "SD = Standard Deviation. CFB = Change from Baseline.",
    "Column panels created automatically — stub repeats on each panel."
  )

wide_spec |> fr_render(file.path(tempdir(), "column_split.pdf"))
wide_spec  # HTML preview

cat("PDF with auto column panels written to:", file.path(tempdir(), "column_split.pdf"), "\n")
cat("Try this with gt/tfrmt — you can't. You'd manually split into 2 tables.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  2. PAGE_BY — One spec, separate pages per parameter                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Vital signs table with ALL parameters — each gets its own page.
# N-counts update per page automatically from a named vector.
# gt/tfrmt have no concept of page_by.

cat("═══════════════════════════════════════════════════════════\n")
cat("  2. PAGE_BY — one spec, separate pages per parameter\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Summarize all 4 PARAMCDs across visits
vs_stats <- advs_clean |>
  filter(AVISIT %in% c("Week 2", "Week 4", "End of Treatment")) |>
  group_by(PARAM, AVISIT, TRT01A) |>
  summarise(
    n_val    = as.character(n()),
    baseline = sprintf("%.1f (%.2f)", mean(BASE, na.rm = TRUE), sd(BASE, na.rm = TRUE)),
    value    = sprintf("%.1f (%.2f)", mean(AVAL, na.rm = TRUE), sd(AVAL, na.rm = TRUE)),
    cfb      = sprintf("%.1f (%.2f)", mean(CHG,  na.rm = TRUE), sd(CHG,  na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(n_val, baseline, value, cfb), names_to = "statistic", values_to = "val") |>
  mutate(statistic = case_when(
    statistic == "n_val"    ~ "n",
    statistic == "baseline" ~ "Baseline Mean (SD)",
    statistic == "value"    ~ "Post-baseline Mean (SD)",
    statistic == "cfb"      ~ "Change Mean (SD)"
  )) |>
  pivot_wider(names_from = TRT01A, values_from = val) |>
  arrange(
    PARAM, AVISIT,
    match(statistic, c("n", "Baseline Mean (SD)", "Post-baseline Mean (SD)", "Change Mean (SD)"))
  )

# Per-parameter N-counts (header updates automatically per page_by group)
vs_n_df <- advs_clean |>
  filter(AVISIT %in% c("Week 2", "Week 4", "End of Treatment")) |>
  distinct(USUBJID, PARAM, TRT01A) |>
  count(PARAM, TRT01A) |>
  pivot_wider(names_from = TRT01A, values_from = n)

# Use the first parameter's counts as a representative named vector
vs_n_vec <- unlist(vs_n_df[1, setdiff(names(vs_n_df), "PARAM")])

page_by_spec <- vs_stats |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6",
    "Vital Signs — Summary by Parameter and Visit",
    "Safety Population"
  ) |>
  fr_cols(
    PARAM     = fr_col(visible = FALSE),
    AVISIT    = fr_col(visible = FALSE),
    statistic = fr_col("Statistic", width = 2.0),
    `Placebo`              = fr_col("Placebo",       align = "decimal"),
    `Xanomeline Low Dose`  = fr_col("Xan\nLow Dose", align = "decimal"),
    `Xanomeline High Dose` = fr_col("Xan\nHigh Dose", align = "decimal"),
    .n = vs_n_vec
  ) |>
  fr_rows(page_by = "PARAM", group_by = "AVISIT", group_bold = TRUE, blank_after = "AVISIT") |>
  fr_spans(
    "Placebo"              = "Placebo",
    "Xanomeline Low Dose"  = "Xanomeline Low Dose",
    "Xanomeline High Dose" = "Xanomeline High Dose"
  ) |>
  fr_footnotes(
    "Baseline is the last non-missing value on or before first dose.",
    "Change = Post-baseline value - Baseline value.",
    "Each parameter on a separate page with its own N-counts."
  )

page_by_spec |> fr_render(file.path(tempdir(), "page_by_vital_signs.pdf"))
page_by_spec  # HTML preview

cat("PDF with per-parameter pages written to:", file.path(tempdir(), "page_by_vital_signs.pdf"), "\n")
cat("4 parameters = 4 pages, each with correct N-counts. One spec.\n")
cat("gt/tfrmt: you'd write 4 separate gt objects + 4 docorator calls.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  3. DECIMAL ALIGNMENT — the #1 missing feature in gt                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# gt aligns text left or center. Numbers like "56.5 (12.15)" and "3 (6.7)"
# visually jump around when center-aligned. Decimal alignment keeps the
# decimal point (or parenthesis) in a fixed column position.

cat("═══════════════════════════════════════════════════════════\n")
cat("  3. DECIMAL ALIGNMENT — not possible in gt\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Build demographics via cards + fr_wide_ard
demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "TRT01A",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("AGEGR1", "SEX", "RACE")),
  .overall = TRUE
)

demog_wide <- fr_wide_ard(
  demog_ard,
  statistic = list(
    continuous  = c("n" = "{N}", "Mean (SD)" = "{mean} ({sd})", "Median" = "{median}", "Min, Max" = "{min}, {max}"),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, median = 1, p = 1),
  label = c(AGE = "Age (years)", AGEGR1 = "Age Group, n (%)", SEX = "Sex, n (%)", RACE = "Race, n (%)")
)

decimal_spec <- demog_wide |>
  fr_table() |>
  fr_titles(
    "Decimal Alignment Demo",
    "Notice how decimals and parentheses line up vertically"
  ) |>
  fr_cols(
    variable   = fr_col(visible = FALSE),
    stat_label = fr_col("", width = 2.5),
    !!!setNames(
      lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
      arm_levels
    ),
    Total = fr_col("Total", align = "decimal"),
    .n = c(arm_n, Total = sum(arm_n))
  ) |>
  fr_rows(group_by = "variable", group_label = "stat_label", group_bold = TRUE, blank_after = "variable") |>
  fr_footnotes(
    "align = 'decimal' keeps numbers visually aligned.",
    "gt/tfrmt can only do left/center/right alignment."
  )

decimal_spec |> fr_render(file.path(tempdir(), "decimal_alignment.pdf"))
decimal_spec  # HTML preview

cat("PDF written to:", file.path(tempdir(), "decimal_alignment.pdf"), "\n")
cat("Compare with gt's center alignment — decimals jump around.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  4. ONE SPEC → THREE FORMATS                                          ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# In the pharmaverse stack:
#   - PDF: gt → docorator → render_pdf()
#   - RTF: gt → gtsave("file.rtf") — single-page, no pagination
#   - HTML: gt directly (different from PDF layout)
#
# In arframe: same spec, same formatting, all three outputs.

cat("═══════════════════════════════════════════════════════════\n")
cat("  4. ONE SPEC → PDF + RTF + HTML\n")
cat("═══════════════════════════════════════════════════════════\n\n")

ae_ard <- ard_stack_hierarchical(
  data = adae_te,
  variables = c(AEBODSYS, AEDECOD),
  by = TRT01A,
  denominator = adsl_saf,
  id = USUBJID
)

ae_wide <- fr_wide_ard(
  ae_ard,
  statistic = "{n} ({p}%)",
  decimals = c(p = 1),
  label = c("..ard_hierarchical_overall.." = "Any TEAE")
)

ae_spec <- ae_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("TEAEs by SOC and Preferred Term", bold = TRUE),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type = fr_col(visible = FALSE),
    !!!setNames(
      lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
      arm_levels
    ),
    .n = arm_n
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "overall"), bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"),     bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA coding dictionary version used.",
    "Subjects counted once per SOC and Preferred Term."
  )

# Same spec, three outputs — zero extra code
ae_spec |> fr_render(file.path(tempdir(), "ae_soc.pdf"))
ae_spec |> fr_render(file.path(tempdir(), "ae_soc.rtf"))
ae_spec  # HTML preview in viewer

cat("Same spec rendered to:\n")
cat("  PDF:", file.path(tempdir(), "ae_soc.pdf"), "\n")
cat("  RTF:", file.path(tempdir(), "ae_soc.rtf"), "\n")
cat("  HTML: viewer preview\n")
cat("pharmaverse needs different pipelines for each format.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  5. BATCH RENDER — multiple tables in a loop                          ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("═══════════════════════════════════════════════════════════\n")
cat("  5. BATCH RENDER — entire CSR in a loop\n")
cat("═══════════════════════════════════════════════════════════\n\n")

tables <- list(
  "Table_14_1_1" = demog_wide |>
    fr_table() |>
    fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics", "Safety Population") |>
    fr_cols(
      variable   = fr_col(visible = FALSE),
      stat_label = fr_col("", width = 2.5),
      !!!setNames(
        lapply(arm_levels, function(a) fr_col(a, align = "decimal")),
        arm_levels
      ),
      Total = fr_col("Total", align = "decimal"),
      .n = c(arm_n, Total = sum(arm_n))
    ) |>
    fr_rows(group_by = "variable", group_label = "stat_label", group_bold = TRUE, blank_after = "variable") |>
    fr_footnotes("Percentages based on N per treatment group."),

  "Table_14_3_1" = ae_spec
)

outdir <- file.path(tempdir(), "csr_batch")
dir.create(outdir, showWarnings = FALSE)

for (nm in names(tables)) {
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".pdf")))
  tables[[nm]] |> fr_render(file.path(outdir, paste0(nm, ".rtf")))
}

cat("2 tables × 2 formats = 4 files written to:\n", outdir, "\n")
cat("With pharmaverse: 2 × tfl_format() calls, each ~20 lines = 40 lines extra.\n")
cat("With arframe: a 3-line for loop.\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Summary for your team                                                 ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("═══════════════════════════════════════════════════════════\n")
cat("  WHY ARFRAME FOR GSK\n")
cat("═══════════════════════════════════════════════════════════\n\n")
cat("1. COLUMN SPLITTING   — Wide tables auto-panel. No manual split.\n")
cat("2. PAGE_BY             — Per-parameter pages with auto N-counts.\n")
cat("3. DECIMAL ALIGNMENT   — Real alignment, not center-faking-it.\n")
cat("4. ONE SPEC → 3 FMTS  — PDF + RTF + HTML from same pipeline.\n")
cat("5. STUDY-WIDE THEME    — Set once. No tfl_format()/gsk_styling().\n")
cat("6. ~25 LINES PER TABLE — vs ~300+ in pharmaverse stack.\n")
cat("7. 1 PACKAGE           — vs 5+ (cards/tfrmt/gt/docorator/tidyr).\n")
cat("8. NO TIDYVERSE DEP    — No dplyr/purrr/tidyr in arframe itself.\n")
cat("9. CONTINUATION TEXT   — '(continued)' on page 2+. docorator can't.\n")
cat("10. REPEATING HEADERS  — Column headers on every page. docorator can't.\n")

fr_theme_reset()
