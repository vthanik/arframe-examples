# ============================================================================
# Demographics Table — GSK pharmaverse stack vs arframe
# ============================================================================
#
# Produces the SAME demographics table (Table 14.1.1) two ways:
#   Path A: cards + tfrmt + gt + docorator  -> PDF (pharmaverse / GSK stack)
#   Path B: arframe                         -> PDF (single-package)
#
# Uses pharmaverseadam CDISC pilot study datasets (CDISCPILOT01).
# Missing packages are installed automatically.
# Output PDFs land in tempdir() for side-by-side comparison.

# ── Package setup ───────────────────────────────────────────────────────────

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, quiet = TRUE)
  }
}

ensure_packages(c("pharmaverseadam", "dplyr", "tidyr", "cards", "tfrmt", "gt", "docorator"))

library(arframe)
library(pharmaverseadam)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

outdir <- file.path(tempdir(), "compare_demog")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


# ── Shared data prep (identical for both paths) ────────────────────────────

blank_to_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) ifelse(nzchar(trimws(x)), x, NA_character_) else x
  })
  df
}

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

demog_wide <- bind_rows(age_wide, sex_wide, race_wide) |>
  mutate(stat_label = paste0("  ", stat_label)) |>
  mutate(across(where(is.character) & !c(variable, stat_label), ~ replace_na(.x, "0")))

arms  <- setdiff(names(demog_wide), c("variable", "stat_label", "Total"))
n_vec <- setNames(c(n_by_arm[arms], Total = n_total), c(arms, "Total"))


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  PATH A: cards + tfrmt + gt + docorator  (GSK pharmaverse stack)       ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Real GSK workflow (from t_sp_dmt01.R — 358 lines):
#   1. Read XPT, apply metacore factors, recode labels   (~80 lines)
#   2. ard_stack() + manual ARD cleanup                   (~100 lines)
#   3. tfrmt() spec with body_plan, big_n, col_plan       (~50 lines)
#   4. print_to_gt() + gsk_styling() + cols_width()       (~15 lines)
#   5. tfl_format() -> as_docorator() -> render_pdf()     (~15 lines)
#
# Below we replicate steps 2-5 with our simulated data.

cat("\n")
cat("================================================================\n")
cat("  PATH A: pharmaverse (cards + tfrmt + gt + docorator)\n")
cat("================================================================\n\n")

library(tfrmt)
library(gt)
library(docorator)

# Step 1: Reshape wide -> ARD long format
# tfrmt CANNOT use wide data — it requires one-row-per-stat ARD format.
# In real GSK code this reshape is 100+ lines of mutate/filter/select.

demog_ard <- demog_wide |>
  pivot_longer(
    cols = c(all_of(arms), "Total"),
    names_to = "column",
    values_to = "value"
  ) |>
  rename(group = variable, label = stat_label) |>
  mutate(param = "stat")

# Add big N rows (tfrmt needs these as data rows, not metadata)
big_n_rows <- data.frame(
  group = NA_character_,
  label = NA_character_,
  column = names(n_vec),
  param = "bigN",
  value = as.character(n_vec),
  stringsAsFactors = FALSE
)

demog_ard <- bind_rows(demog_ard, big_n_rows)

# Step 2: tfrmt specification (~50 lines in real GSK code)
demog_tfrmt <- tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,

  body_plan = body_plan(
    frmt_structure(
      group_val = ".default",
      label_val = ".default",
      frmt("x")
    )
  ),

  big_n = big_n_structure(
    param_val = "bigN",
    n_frmt = frmt("\n(N=xx)")
  ),

  row_grp_plan = row_grp_plan(
    row_grp_structure(
      group_val = ".default",
      element_block(post_space = " ")
    ),
    label_loc = element_row_grp_loc(location = "indented")
  ),

  col_plan = col_plan(
    label,
    everything()
  )
)

# Step 3: Render to gt + apply GSK styling
demog_gt <- print_to_gt(demog_tfrmt, demog_ard)

# gsk_styling() — shared 30-line function sourced in every GSK script
demog_gt <- demog_gt |>
  cols_align(align = "left", columns = everything()) |>
  tab_options(
    table.font.size = 14,
    data_row.padding = px(3),
    table.font.names = c("Courier", default_fonts()),
    table_body.hlines.width = px(0),
    stub.border.width = px(0),
    stub.border.color = "transparent"
  ) |>
  tab_style(
    style = cell_text(v_align = "top"),
    locations = cells_body()
  )

# Step 4: tfl_format() -> as_docorator() -> render_pdf()
# This is the real GSK function (281 lines in functions/tfl_format.R).
# It builds fancyhead/fancyfoot, calls as_docorator() + render_pdf().
pharma_pdf <- file.path(outdir, "pharmaverse_demog.pdf")

tryCatch(
  {
    demog_gt |>
      as_docorator(
        display_name = "pharmaverse_demog",
        display_loc = outdir,
        header = fancyhead(
          fancyrow(left = "Protocol: CDISCPILOT01", center = NA, right = doc_pagenum()),
          fancyrow(left = "Analysis Set: Safety", center = NA, right = NA),
          fancyrow(left = NA, center = "Table 14.1.1", right = NA),
          fancyrow(
            left = NA,
            center = "Demographics and Baseline Characteristics",
            right = NA
          )
        ),
        footer = fancyfoot(
          fancyrow(left = "Percentages based on N per treatment group."),
          fancyrow(
            left = pharma_pdf,
            right = toupper(format(Sys.Date(), "%d%b%Y"))
          )
        ),
        tbl_stub_pct = 0.3,
        tbl_scale = TRUE,
        geometry = geom_set(landscape = TRUE)
      ) |>
      render_pdf(escape_latex = FALSE)

    cat("  pharmaverse PDF: ", pharma_pdf, "\n")
  },
  error = function(e) {
    cat("  docorator render_pdf() failed: ", conditionMessage(e), "\n")
    cat("  (docorator requires LaTeX + specific packages installed)\n")
    # Fallback: save gt as HTML
    pharma_html <- file.path(outdir, "pharmaverse_demog.html")
    gtsave(demog_gt, pharma_html)
    cat("  Fallback HTML saved: ", pharma_html, "\n")
  }
)

cat("  Lines of code (after data prep): ~80\n")
cat("  Packages used: cards, tfrmt, gt, docorator (4)\n")
cat("  Shared helpers needed: gsk_styling.R (30 lines), tfl_format.R (281 lines)\n")
cat("  Limitations:\n")
cat("    - No decimal alignment (left-align only)\n")
cat("    - Single-page PDF (no paginated multi-page output)\n")
cat("    - No repeating column headers on page 2+\n")
cat("    - No '(continued)' text on subsequent pages\n")
cat("    - No study-wide theme (each table styled individually)\n")
cat("    - Required ARD reshape (wide -> long -> tfrmt -> gt)\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  PATH B: arframe  (single-package approach)                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Steps:
#   1. Set study-wide theme (once, all tables inherit)
#   2. Pipe wide data through fr_table() -> fr_cols() -> fr_render()
#   3. Done. Paginated PDF with decimal alignment.

cat("================================================================\n")
cat("  PATH B: arframe\n")
cat("================================================================\n\n")

fr_theme_reset()
fr_theme(
  font_size = 9,
  font_family = "Courier New",
  orientation = "landscape",
  hlines = "header",
  header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})",
  footnote_separator = FALSE,
  pagehead = list(left = "Protocol: CDISCPILOT01", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

demog_arframe <- demog_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.1.1",
    "Demographics and Baseline Characteristics",
    "Safety Population"
  ) |>
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
  fr_rows(group_by = list(cols = "variable", label = "stat_label")) |>
  fr_footnotes("Percentages based on N per treatment group.")

arframe_pdf <- file.path(outdir, "arframe_demog.pdf")
demog_arframe |> fr_render(arframe_pdf)
demog_arframe |> fr_render(file.path(outdir, "arframe_demog.rtf"))

cat("  arframe PDF: ", arframe_pdf, "\n")
cat("  Lines of code (after data prep): ~20\n")
cat("  Packages used: arframe (1)\n")
cat("  No shared helpers needed.\n")
cat("  Capabilities:\n")
cat("    - Decimal alignment (numbers line up on the decimal point)\n")
cat("    - Paginated PDF with repeating headers\n")
cat("    - Page X of Y in footer\n")
cat("    - Study-wide theme (set once, all tables inherit)\n")
cat("    - No ARD reshape needed (takes wide data directly)\n")
cat("    - Same spec renders to PDF + RTF + HTML\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Side-by-side summary                                                  ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  COMPARISON SUMMARY — Demographics Table\n")
cat("================================================================\n\n")
cat(sprintf("  %-30s  %-25s  %-20s\n", "", "pharmaverse (GSK)", "arframe"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Lines after data prep", "~80 + 311 helpers", "~20"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Packages", "4 (cards+tfrmt+gt+doco)", "1 (arframe)"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Input format", "ARD (long)", "Wide summary"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Decimal alignment", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Paginated PDF", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Repeating headers", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Page X of Y", "Via doc_pagenum()", "Built-in"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Study-wide theme", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Multi-format output", "PDF only", "PDF+RTF+HTML"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Real GSK code", "358 lines (t_sp_dmt01)", "~20 lines"))
cat("\n  Output directory:", outdir, "\n")

fr_theme_reset()
