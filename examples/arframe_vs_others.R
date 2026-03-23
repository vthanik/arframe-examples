# ============================================================================
# arframe vs tfrmt + gt + docorator — Real-World Comparison
# Two tables, two approaches, same data, same output
# ============================================================================
#
# This script produces TWO production tables from pharmaverseadam datasets:
#   Table 1: Demographics (t_sp_dmt01 equivalent)
#   Table 2: AE by SOC/PT (t_saf_ae01_all equivalent)
#
# For each table we show:
#   Path A — arframe: wide summary → fr_table() pipeline → PDF
#   Path B — pharmaverse: ARD → tfrmt → gt → docorator → PDF
#
# The data prep uses tidyverse (dplyr/tidyr) — same for both paths.
# The difference is what happens AFTER you have summary statistics.
#
# Based on real GSK production code from study 52427_219230-INTERNAL_01:
#   t_sp_dmt01.R    — 358 lines (pharmaverse)
#   t_saf_ae01_all.R — 621 lines (pharmaverse)

library(arframe)
library(pharmaverseadam)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# Helper: replace blank/whitespace-only strings with NA
blank_to_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) ifelse(nzchar(trimws(x)), x, NA_character_) else x
  })
  df
}


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 1: Demographics and Baseline Characteristics                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝

# ── Shared data prep (same effort for both approaches) ───────────────────
# Summarize ADSL into display-ready statistics.

adsl_saf <- pharmaverseadam::adsl |>
  blank_to_na() |>
  filter(SAFFL == "Y", TRT01A != "Screen Failure")

n_by_arm <- adsl_saf |> count(TRT01A) |> pull(n, name = TRT01A)
n_total  <- nrow(adsl_saf)

# Continuous: Age
age_stats <- adsl_saf |>
  group_by(TRT01A) |>
  summarise(
    n          = as.character(n()),
    `Mean (SD)` = sprintf("%.1f (%.2f)", mean(AGE), sd(AGE)),
    Median     = sprintf("%.1f", median(AGE)),
    `Min, Max` = sprintf("%d, %d", min(AGE), max(AGE)),
    .groups = "drop"
  ) |>
  pivot_longer(-TRT01A, names_to = "stat_label", values_to = "value") |>
  pivot_wider(names_from = TRT01A, values_from = value)

# Add total column
age_total <- adsl_saf |>
  summarise(
    n          = as.character(n()),
    `Mean (SD)` = sprintf("%.1f (%.2f)", mean(AGE), sd(AGE)),
    Median     = sprintf("%.1f", median(AGE)),
    `Min, Max` = sprintf("%d, %d", min(AGE), max(AGE))
  ) |>
  pivot_longer(everything(), names_to = "stat_label", values_to = "Total")

age_wide <- left_join(age_stats, age_total, by = "stat_label") |>
  mutate(variable = "Age (years)", .before = 1)

# Categorical: Sex
sex_stats <- adsl_saf |>
  count(TRT01A, SEX) |>
  group_by(TRT01A) |>
  mutate(pct = sprintf("%d (%.1f)", n, n / sum(n) * 100)) |>
  ungroup() |>
  select(TRT01A, SEX, pct) |>
  pivot_wider(names_from = TRT01A, values_from = pct) |>
  rename(stat_label = SEX)

sex_total <- adsl_saf |>
  count(SEX) |>
  mutate(Total = sprintf("%d (%.1f)", n, n / nrow(adsl_saf) * 100)) |>
  select(stat_label = SEX, Total)

sex_wide <- left_join(sex_stats, sex_total, by = "stat_label") |>
  mutate(variable = "Sex", .before = 1)

# Categorical: Race
race_stats <- adsl_saf |>
  count(TRT01A, RACE) |>
  group_by(TRT01A) |>
  mutate(pct = sprintf("%d (%.1f)", n, n / sum(n) * 100)) |>
  ungroup() |>
  select(TRT01A, RACE, pct) |>
  pivot_wider(names_from = TRT01A, values_from = pct) |>
  rename(stat_label = RACE)

race_total <- adsl_saf |>
  count(RACE) |>
  mutate(Total = sprintf("%d (%.1f)", n, n / nrow(adsl_saf) * 100)) |>
  select(stat_label = RACE, Total)

race_wide <- left_join(race_stats, race_total, by = "stat_label") |>
  mutate(variable = "Race", .before = 1)

# Combine all demographics (fill NAs from unbalanced factor levels)
demog_wide <- bind_rows(age_wide, sex_wide, race_wide) |>
  mutate(stat_label = paste0("  ", stat_label)) |>
  mutate(across(where(is.character) & !c(variable, stat_label), ~ replace_na(.x, "0")))

cat("── Demographics summary (wide format) ──\n")
print(demog_wide, n = 20)

arms  <- setdiff(names(demog_wide), c("variable", "stat_label", "Total"))
n_vec <- setNames(c(n_by_arm[arms], Total = n_total), c(arms, "Total"))


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH A: arframe — 20 lines from wide summary to PDF                │
# └──────────────────────────────────────────────────────────────────────┘

fr_theme_reset()
fr_theme(
  font_size = 9, font_family = "Courier New", orientation = "landscape",
  hlines = "header", header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})", footnote_separator = FALSE,
  pagehead = list(left = "CDISCPILOT01", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

demog_arframe <- demog_wide |>
  fr_table() |>
  fr_titles("Table 14.1.1", "Demographics and Baseline Characteristics",
            "Safety Population") |>
  fr_cols(
    variable   = fr_col(visible = FALSE),
    stat_label = fr_col("", width = 2.5, align = "left"),
    !!!setNames(
      lapply(arms, function(a) fr_col(a, align = "decimal")),
      arms
    ),
    Total = fr_col("Total", align = "decimal"),
    .n = n_vec
  ) |>
  fr_rows(group_by = "variable", group_label = "stat_label", group_bold = TRUE) |>
  fr_footnotes("Percentages based on N per treatment group.")

demog_arframe |> fr_render(file.path(tempdir(), "arframe_demog.pdf"))
demog_arframe  # HTML preview

cat("\n== PATH A: arframe demographics ==\n")
cat("Lines after data prep: ~20\n")
cat("Packages: arframe (1)\n")
cat("Output: PDF written to", file.path(tempdir(), "arframe_demog.pdf"), "\n\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH B: tfrmt + gt + docorator — ~80 lines from ARD to PDF         │
# └──────────────────────────────────────────────────────────────────────┘
# We show the REAL code structure, commented out since you may not have
# tfrmt/gt/docorator installed. The line counts are from real GSK code.

cat("== PATH B: tfrmt + gt + docorator demographics ==\n\n")

cat("── Step 0: Reshape wide → ARD (one row per stat) ~30 lines ──\n")
cat('
# The wide summary we already have CANNOT be used by tfrmt.
# tfrmt requires ARD format: group | label | column | param | value
# So we must reshape BACK to long format:

demog_ard <- demog_wide |>
  pivot_longer(
    cols = c(all_of(arms), "Total"),
    names_to  = "column",
    values_to = "value_str"
  ) |>
  # tfrmt needs numeric values, so we parse n and pct separately:
  mutate(
    n   = as.numeric(gsub(" \\\\(.*", "", value_str)),
    pct = as.numeric(gsub(".*\\\\(|\\\\)|%", "", value_str))
  ) |>
  # Now pivot to one-row-per-stat (n and pct as separate rows):
  pivot_longer(
    cols = c(n, pct),
    names_to  = "param",
    values_to = "stat"
  ) |>
  select(variable, stat_label, column, param, stat)

# For continuous stats (Mean, SD, etc.) this is even more complex
# because tfrmt needs frmt() patterns per stat type.
# In the GSK codebase, this reshape is ~100 lines of mutate/filter/select.
')

cat("\n── Step 1: tfrmt format specification ~40 lines ──\n")
cat('
# library(tfrmt)
# demog_tfrmt <- tfrmt(
#   group   = variable,
#   label   = stat_label,
#   column  = column,
#   param   = param,
#   value   = stat,
#
#   body_plan = body_plan(
#     # Categorical: n (pct%)
#     frmt_structure(
#       group_val = ".default",
#       label_val = ".default",
#       frmt_combine(
#         "{n} ({pct})",
#         n   = frmt("xxx"),
#         pct = frmt_when(
#           "==0"   ~ "",
#           "==100" ~ "100%",
#           "<1"    ~ "<1%",
#           ">99"   ~ ">99%",
#           TRUE    ~ frmt("xx.x%")
#         )
#       )
#     ),
#     # Continuous: Mean (SD) — different format
#     frmt_structure(
#       group_val = "Age (years)",
#       label_val = "Mean (SD)",
#       frmt("xxx.x (xx.xx)")
#     ),
#     # Continuous: Median
#     frmt_structure(
#       group_val = "Age (years)",
#       label_val = "Median",
#       frmt("xxx.x")
#     )
#   ),
#
#   # N-counts in headers — manual, not automatic
#   big_n = big_n_structure(
#     param_val = "bigN",
#     n_frmt = frmt("\\n(N=xx)")
#   ),
#
#   # Row grouping
#   row_grp_plan = row_grp_plan(
#     row_grp_structure(
#       group_val = ".default",
#       element_block(post_space = " ")
#     ),
#     label_loc = element_row_grp_loc(location = "indented")
#   ),
#
#   # Column order
#   col_plan = col_plan(
#     stat_label,
#     "Placebo",
#     "Xanomeline Low Dose",
#     "Xanomeline High Dose",
#     Total
#   )
# )
#
# demog_gt <- print_to_gt(demog_tfrmt, demog_ard)
')

cat("\n── Step 2: gt post-processing ~15 lines ──\n")
cat('
# # tfrmt output needs manual gt fixes:
# demog_gt <- demog_gt |>
#   gt::tab_options(
#     table.font.size  = gt::px(12),
#     table.font.names = c("Courier", gt::default_fonts()),
#     table.width      = "100%"
#   ) |>
#   gt::cols_align(align = "left", columns = everything()) |>
#   # No decimal alignment possible in gt — left-align is GSK default
#   gt::tab_style(
#     style = gt::cell_text(v_align = "top"),
#     locations = gt::cells_body()
#   )
')

cat("\n── Step 3: docorator wrapping ~20 lines ──\n")
cat('
# library(docorator)
# demog_gt |>
#   as_docorator(
#     display_name = "t_sp_dmt01",
#     display_loc  = tempdir(),
#     header = fancyhead(
#       fancyrow(left = "Protocol: CDISCPILOT01", right = doc_pagenum()),
#       fancyrow(left = "Analysis Set: Safety"),
#       fancyrow(left = NA, center = "Table 14.1.1"),
#       fancyrow(left = NA, center = "Demographics and Baseline Characteristics")
#     ),
#     footer = fancyfoot(
#       fancyrow(left = "Percentages based on N per treatment group."),
#       fancyrow(left = "t_sp_dmt01.pdf", right = format(Sys.Date(), "%d%b%Y"))
#     ),
#     tbl_stub_pct = 0.3,
#     tbl_scale = TRUE,
#     geometry = geom_set(landscape = TRUE)
#   ) |>
#   render_pdf(escape_latex = FALSE)
')

cat("\nPATH B total: ~105 lines after data prep (+ ~30 lines ARD reshape)\n")
cat("Packages: tfrmt + gt + docorator (3) + tidyr for reshape\n")
cat("Real GSK code (t_sp_dmt01.R): 358 lines total\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  TABLE 2: AE by SOC and Preferred Term                                ║
# ╚══════════════════════════════════════════════════════════════════════════╝

# ── Shared data prep: summarize ADAE into wide format ────────────────────

adae_te <- pharmaverseadam::adae |>
  blank_to_na() |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

adsl_saf <- pharmaverseadam::adsl |>
  blank_to_na() |>
  filter(SAFFL == "Y", TRT01A != "Screen Failure")

n_by_arm <- adsl_saf |> count(TRT01A) |> pull(n, name = TRT01A)

# SOC-level: subjects with any AE per SOC
soc_counts <- adae_te |>
  distinct(USUBJID, TRT01A, AEBODSYS) |>
  count(TRT01A, AEBODSYS, name = "n_subj") |>
  left_join(tibble(TRT01A = names(n_by_arm), N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, AEBODSYS, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat, values_fill = "0 (0.0)") |>
  mutate(row_type = "soc", pt = AEBODSYS, soc = AEBODSYS)

# PT-level: subjects per SOC/PT
pt_counts <- adae_te |>
  distinct(USUBJID, TRT01A, AEBODSYS, AEDECOD) |>
  count(TRT01A, AEBODSYS, AEDECOD, name = "n_subj") |>
  left_join(tibble(TRT01A = names(n_by_arm), N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, AEBODSYS, AEDECOD, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat, values_fill = "0 (0.0)") |>
  mutate(row_type = "pt", pt = AEDECOD, soc = AEBODSYS)

# Total "Any TEAE" row
any_ae <- adae_te |>
  distinct(USUBJID, TRT01A) |>
  count(TRT01A, name = "n_subj") |>
  left_join(tibble(TRT01A = names(n_by_arm), N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat) |>
  mutate(row_type = "total", pt = "Any TEAE", soc = "")

# Sort SOC by descending frequency (total across arms)
soc_order <- adae_te |>
  distinct(USUBJID, AEBODSYS) |>
  count(AEBODSYS, name = "freq") |>
  arrange(desc(freq)) |>
  pull(AEBODSYS)

# Sort PTs within SOC by descending frequency
pt_order <- adae_te |>
  distinct(USUBJID, AEBODSYS, AEDECOD) |>
  count(AEBODSYS, AEDECOD, name = "freq") |>
  arrange(AEBODSYS, desc(freq))

# Assemble final wide table
arms <- names(n_by_arm)
ae_wide <- bind_rows(
  any_ae,
  bind_rows(
    lapply(soc_order, function(s) {
      soc_row <- soc_counts |> filter(soc == s)
      pt_rows <- pt_counts |>
        filter(soc == s) |>
        left_join(pt_order |> filter(AEBODSYS == s), by = c("soc" = "AEBODSYS", "pt" = "AEDECOD")) |>
        arrange(desc(freq)) |>
        select(-freq)
      bind_rows(soc_row, pt_rows)
    })
  )
) |>
  select(soc, pt, row_type, all_of(arms))

cat("── AE SOC/PT summary (wide format) ──\n")
print(ae_wide, n = 15)


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH A: arframe — 25 lines from wide summary to PDF                │
# └──────────────────────────────────────────────────────────────────────┘

n_safety <- setNames(n_by_arm, arms)

ae_arframe <- ae_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
         bold = TRUE),
    "Safety Population"
  ) |>
  fr_page(continuation = "(continued)") |>
  fr_cols(
    soc      = fr_col(visible = FALSE),
    pt       = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
    row_type = fr_col(visible = FALSE),
    !!!setNames(
      lapply(arms, function(a) fr_col(a, align = "decimal")),
      arms
    ),
    .n = n_safety
  ) |>
  fr_rows(group_by = "soc", indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "total"), bold = TRUE),
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"),   bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA version 26.0.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence."
  )

ae_arframe |> fr_render(file.path(tempdir(), "arframe_ae_soc.pdf"))
ae_arframe  # HTML preview

cat("\n== PATH A: arframe AE SOC/PT ==\n")
cat("Lines after data prep: ~25\n")
cat("Packages: arframe (1)\n")
cat("Output: PDF written to", file.path(tempdir(), "arframe_ae_soc.pdf"), "\n\n")


# ┌──────────────────────────────────────────────────────────────────────┐
# │  PATH B: tfrmt + gt + docorator — ~130 lines from ARD to PDF        │
# └──────────────────────────────────────────────────────────────────────┘

cat("== PATH B: tfrmt + gt + docorator AE SOC/PT ==\n\n")

cat("── Step 0: Reshape wide → ARD (~50 lines) ──\n")
cat('
# tfrmt CANNOT use this wide table. It needs ARD format.
# The GSK code uses cards::ard_stack_hierarchical() which returns
# one row per (subject group × SOC × PT × stat). For this study
# with sex subgroups + 3 treatment arms + LA/ULA periods, this
# means 6 separate ard_stack_hierarchical() calls, each ~15 lines:

# ae_ard1 <- ard_stack_hierarchical(
#   data = adae_clean,
#   by = c(SEXBRTHG, TRTSEQA),
#   variables = c("AEBODSYS", "AEDECOD"),
#   id = USUBJID,
#   denominator = adsl_clean,
#   overall = FALSE,
#   over_variables = FALSE
# ) |> shuffle_ard()
#
# ae_ard2 <- ard_stack_hierarchical(   # CAB LA only
#   data = adae_la, ...
# ) |> shuffle_ard()
#
# ae_ard3 <- ard_stack_hierarchical(   # CAB ULA only
#   data = adae_ula, ...
# ) |> shuffle_ard()
#
# ae_ard4, ae_ard5, ae_ard6 ...        # Same again for Overall
#
# ae_ard_all <- bind_rows(ae_ard1, ..., ae_ard6)
#
# # Then: fix missing 0 records, create bigN rows, sort by frequency
# ae_ard_final <- ae_ard_all |>
#   complete(SEXBRTHG, TRTA, nesting(AEBODSYS, variable_level), stat_name,
#            fill = list(stat = 0)) |>
#   mutate(stat = ifelse(stat_name == "p", stat * 100, stat)) |>
#   left_join(ordering_aebodsys) |>
#   left_join(ordering_aeterm) |>
#   mutate(label = variable_level, ...)
#
# In the real GSK code (t_saf_ae01_all.R), this is lines 36–506:
# >>> 470 LINES of data manipulation <<<
')

cat("\n── Step 1: tfrmt format specification (~45 lines) ──\n")
cat('
# tfrmt_ae <- tfrmt(
#   group   = "AEBODSYS",
#   label   = "label",
#   column  = c("SEXBRTHG", "TRTA"),
#   param   = "stat_name",
#   value   = "stat",
#   sorting_cols = c(ord1, ord2),
#
#   body_plan = body_plan(
#     frmt_structure(
#       group_val = ".default",
#       label_val = ".default",
#       frmt_combine(
#         "{n} {p}",
#         n = frmt("x"),
#         p = frmt_when(
#           "==0"   ~ "",
#           "<1"    ~ "(<1%)",
#           "<10"   ~ frmt("(x%)"),
#           "==100" ~ frmt("(100%)"),
#           ">99"   ~ frmt("(>99%)"),
#           TRUE    ~ frmt("(xx%)")
#         )
#       )
#     )
#   ),
#
#   col_plan = col_plan(
#     "System Organ Class\\n  Preferred Term" = AEBODSYS,
#     label, "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose",
#     -ord1, -ord2, -context, -V, -stat_label
#   ),
#
#   big_n = big_n_structure(param_val = "bigN", n_frmt = frmt("\\n(N=xx)")),
#
#   row_grp_plan = row_grp_plan(
#     row_grp_structure(group_val = ".default", element_block(post_space = " ")),
#     label_loc = element_row_grp_loc(location = "indented")
#   )
# )
#
# tfrmt_ae_gt <- tfrmt_ae |> print_to_gt(ae_ard_final)
')

cat("\n── Step 2: gt post-processing (~10 lines) ──\n")
cat('
# tfrmt_ae_gt <- tfrmt_ae_gt |>
#   gt::cols_align(align = "left", columns = everything()) |>
#   gsk_styling()   # 30-line shared function for font/padding/borders
')

cat("\n── Step 3: docorator wrapping (~20 lines) ──\n")
cat('
# tfrmt_ae_gt |>
#   tfl_format(
#     filename  = "t_saf_ae01_all.pdf",
#     path      = output,
#     protocol  = "CDISCPILOT01",
#     id        = "Table 8.3.4",
#     title     = "Summary of AEs by SOC and Preferred Term",
#     popfl     = "SAFFL",
#     tbl_scale = TRUE,
#     engine    = "as_docorator",
#     geometry  = geom_set(landscape = TRUE, left = "0.4in", right = "0.4in")
#   )
#
# tfl_format() itself is a 327-line wrapper function that:
#   - maps popfl codes to labels
#   - builds fancyhead/fancyfoot rows
#   - calls as_docorator() + render_pdf()
')

cat("\nPATH B total: ~595 lines (470 data reshape + 45 tfrmt + 10 gt + 20 docorator + tfl_format helper)\n")
cat("Real GSK code (t_saf_ae01_all.R): 621 lines\n")
cat("Packages: cards + tfrmt + gt + docorator + tidyverse (5+)\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  FINAL SCOREBOARD                                                      ║
# ╚══════════════════════════════════════════════════════════════════════════╝

scoreboard <- data.frame(
  Metric = c(
    "Demographics table",
    "  Lines (after data prep)",
    "  Packages needed",
    "  Data format required",
    "  Decimal alignment",
    "  N-counts in headers",
    "  Group label bold",
    "  Output formats",
    "",
    "AE SOC/PT table",
    "  Lines (total with data prep)",
    "  Lines (table formatting only)",
    "  Packages needed",
    "  Data format required",
    "  SOC/PT indentation",
    "  SOC bold + total bold",
    "  Continuation text (page 2+)",
    "  Repeating column headers",
    "  Page X of Y",
    "  Decimal alignment",
    "  Output formats",
    "",
    "Shared infrastructure",
    "  Study-wide theme",
    "  Reusable config file",
    "  tfl_format() helper needed",
    "  gsk_styling() helper needed"
  ),
  arframe = c(
    "",
    "~20", "1", "Wide summary", "YES", "Automatic", "group_bold = TRUE",
    "PDF + RTF + HTML",
    "",
    "",
    "~80 (55 prep + 25 table)", "~25", "1 + dplyr/tidyr",
    "Wide summary", "indent_by = 'pt'", "fr_row_style() + fr_rows_matches()",
    "YES", "YES", "YES", "YES", "PDF + RTF + HTML",
    "",
    "",
    "fr_theme() — 10 lines, all tables inherit", "YES (_arframe.yml)", "NO", "NO"
  ),
  pharmaverse = c(
    "",
    "~105", "3 + tidyr", "ARD (long)", "NO", "Manual big_n_structure()",
    "Manual gt::tab_style()", "PDF only (docorator)",
    "",
    "",
    "~621 (470 ARD + 45 tfrmt + gt + docorator)", "~75", "5+ (cards/tfrmt/gt/docorator/tidyverse)",
    "ARD (one row per stat)", "row_grp_plan(location = 'indented')",
    "Not available in tfrmt, manual gt",
    "NO", "NO", "YES (docorator)", "NO (left-align only)",
    "PDF only (docorator)",
    "",
    "",
    "Not possible — each table wrapped individually", "NO",
    "YES — 327 lines", "YES — 30 lines"
  ),
  stringsAsFactors = FALSE
)

# Render scoreboard as arframe table
score_spec <- scoreboard |>
  fr_table() |>
  fr_titles(
    "arframe vs tfrmt + gt + docorator",
    list("Real-World Comparison from GSK Study 52427_219230", bold = TRUE)
  ) |>
  fr_cols(
    Metric     = fr_col("", width = 3.5, align = "left"),
    arframe    = fr_col("arframe\n(1 package)", width = 2.5, align = "left"),
    pharmaverse = fr_col("tfrmt + gt +\ndocorator (3+)", width = 2.5, align = "left")
  ) |>
  fr_header(bold = TRUE, align = "center") |>
  fr_hlines("header") |>
  fr_page(orientation = "landscape", font_family = "Courier New", font_size = 9) |>
  fr_styles(
    fr_style_if(
      condition = ~ grepl("^(YES|~[12])", .x),
      cols = "arframe",
      bold = TRUE
    )
  )

score_spec |> fr_render(file.path(tempdir(), "Scoreboard.pdf"))
score_spec  # HTML preview

cat("\n================================================================\n")
cat("  SCOREBOARD: PDFs written to", tempdir(), "\n")
cat("================================================================\n")
cat("  arframe_demog.pdf    — Demographics (arframe, ~20 lines)\n")
cat("  arframe_ae_soc.pdf   — AE SOC/PT (arframe, ~25 lines)\n")
cat("  Scoreboard.pdf       — Feature comparison\n")
cat("\n  Real GSK equivalents:\n")
cat("  t_sp_dmt01.R         — 358 lines (tfrmt + gt + docorator)\n")
cat("  t_saf_ae01_all.R     — 621 lines (tfrmt + gt + docorator)\n")
cat("  tfl_format.R         — 327 lines (shared docorator wrapper)\n")
cat("  gsk_styling.R        —  30 lines (shared gt styling)\n")
cat("  TOTAL pharmaverse:   1,336 lines for 2 tables + helpers\n")
cat("  TOTAL arframe:          45 lines for 2 tables (+ 10 line theme)\n")
cat("================================================================\n")

fr_theme_reset()
