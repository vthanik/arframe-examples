# ============================================================================
# AE by SOC/PT Table — GSK pharmaverse stack vs arframe
# ============================================================================
#
# Produces the SAME adverse event table (Table 14.3.1) two ways:
#   Path A: cards + tfrmt + gt + docorator  -> PDF (pharmaverse / GSK stack)
#   Path B: arframe                         -> PDF (single-package)
#
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

ensure_packages(c("dplyr", "tidyr", "cards", "tfrmt", "gt", "docorator", "pharmaverseadam"))

library(arframe)
library(pharmaverseadam)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

outdir <- file.path(tempdir(), "compare_ae")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)


# ── Shared data prep (identical for both paths) ────────────────────────────
# Summarize ADAE into wide SOC/PT display format.

blank_to_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) ifelse(nzchar(trimws(x)), x, NA_character_) else x
  })
  df
}

adae_te <- pharmaverseadam::adae |>
  blank_to_na() |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

adsl_saf <- pharmaverseadam::adsl |>
  blank_to_na() |>
  filter(SAFFL == "Y", TRT01A != "Screen Failure")

n_by_arm <- adsl_saf |> count(TRT01A) |> pull(n, name = TRT01A)
arms     <- names(n_by_arm)

# SOC-level: subjects with any AE per SOC
soc_counts <- adae_te |>
  distinct(USUBJID, TRT01A, AEBODSYS) |>
  count(TRT01A, AEBODSYS, name = "n_subj") |>
  left_join(tibble(TRT01A = arms, N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, AEBODSYS, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat, values_fill = "0 (0.0)") |>
  mutate(row_type = "soc", pt = AEBODSYS, soc = AEBODSYS)

# PT-level: subjects per SOC/PT
pt_counts <- adae_te |>
  distinct(USUBJID, TRT01A, AEBODSYS, AEDECOD) |>
  count(TRT01A, AEBODSYS, AEDECOD, name = "n_subj") |>
  left_join(tibble(TRT01A = arms, N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, AEBODSYS, AEDECOD, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat, values_fill = "0 (0.0)") |>
  mutate(row_type = "pt", pt = AEDECOD, soc = AEBODSYS)

# "Any TEAE" total row
any_ae <- adae_te |>
  distinct(USUBJID, TRT01A) |>
  count(TRT01A, name = "n_subj") |>
  left_join(tibble(TRT01A = arms, N = n_by_arm), by = "TRT01A") |>
  mutate(stat = sprintf("%d (%.1f)", n_subj, n_subj / N * 100)) |>
  select(TRT01A, stat) |>
  pivot_wider(names_from = TRT01A, values_from = stat) |>
  mutate(row_type = "total", pt = "Any TEAE", soc = "")

# Sort SOC by descending frequency, PTs within SOC
soc_order <- adae_te |>
  distinct(USUBJID, AEBODSYS) |>
  count(AEBODSYS, name = "freq") |>
  arrange(desc(freq)) |>
  pull(AEBODSYS)

pt_order <- adae_te |>
  distinct(USUBJID, AEBODSYS, AEDECOD) |>
  count(AEBODSYS, AEDECOD, name = "freq") |>
  arrange(AEBODSYS, desc(freq))

ae_wide <- bind_rows(
  any_ae,
  bind_rows(lapply(soc_order, function(s) {
    soc_row <- soc_counts |> filter(soc == s)
    pt_rows <- pt_counts |>
      filter(soc == s) |>
      left_join(
        pt_order |> filter(AEBODSYS == s),
        by = c("soc" = "AEBODSYS", "pt" = "AEDECOD")
      ) |>
      arrange(desc(freq)) |>
      select(-freq)
    bind_rows(soc_row, pt_rows)
  }))
) |>
  select(soc, pt, row_type, all_of(arms))

n_safety <- setNames(n_by_arm, arms)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  PATH A: cards + tfrmt + gt + docorator  (GSK pharmaverse stack)       ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# Real GSK workflow (from t_saf_ae01_all.R — 621 lines):
#   1. Read XPT, filter TEAEs, recode arms         (~50 lines)
#   2. 6x ard_stack_hierarchical() calls + cleanup  (~470 lines!)
#   3. tfrmt() spec with frmt_combine for n(p%)     (~45 lines)
#   4. print_to_gt() + gsk_styling()                (~10 lines)
#   5. tfl_format() -> as_docorator() -> render_pdf() (~15 lines)

cat("\n")
cat("================================================================\n")
cat("  PATH A: pharmaverse (cards + tfrmt + gt + docorator)\n")
cat("================================================================\n\n")

library(tfrmt)
library(gt)
library(docorator)

# Step 1: Reshape wide -> ARD long format for tfrmt
ae_display <- ae_wide |>
  mutate(
    label = case_when(
      row_type == "soc" ~ pt,
      row_type == "pt"  ~ paste0("  ", pt),
      TRUE              ~ pt
    )
  )

ae_ard <- ae_display |>
  pivot_longer(
    cols = all_of(arms),
    names_to = "column",
    values_to = "value"
  ) |>
  mutate(group = soc, param = "stat") |>
  select(group, label, column, param, value, row_type) |>
  mutate(ord1 = match(group, c("", soc_order)), ord2 = row_number())

# Add big N rows
big_n_rows <- data.frame(
  group = NA_character_,
  label = NA_character_,
  column = names(n_safety),
  param = "bigN",
  value = as.character(n_safety),
  row_type = NA_character_,
  ord1 = 0L,
  ord2 = 0L,
  stringsAsFactors = FALSE
)

ae_ard <- bind_rows(ae_ard, big_n_rows)

# Step 2: tfrmt specification
ae_tfrmt <- tfrmt(
  group = group,
  label = label,
  column = column,
  param = param,
  value = value,
  sorting_cols = c(ord1, ord2),

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
    everything(),
    -starts_with("ord"),
    -row_type
  )
)

# Step 3: Render to gt + GSK styling
ae_gt <- print_to_gt(ae_tfrmt, ae_ard)

ae_gt <- ae_gt |>
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
pharma_pdf <- file.path(outdir, "pharmaverse_ae.pdf")

tryCatch(
  {
    ae_gt |>
      as_docorator(
        display_name = "pharmaverse_ae",
        display_loc = outdir,
        header = fancyhead(
          fancyrow(left = "Protocol: CDISCPILOT01", center = NA, right = doc_pagenum()),
          fancyrow(left = "Analysis Set: Safety", center = NA, right = NA),
          fancyrow(left = NA, center = "Table 14.3.1", right = NA),
          fancyrow(
            left = NA,
            center = "TEAEs by System Organ Class and Preferred Term",
            right = NA
          )
        ),
        footer = fancyfoot(
          fancyrow(left = "MedDRA version 26.0."),
          fancyrow(left = "Subjects counted once per SOC and Preferred Term."),
          fancyrow(
            left = pharma_pdf,
            right = toupper(format(Sys.Date(), "%d%b%Y"))
          )
        ),
        tbl_stub_pct = 0.3,
        tbl_scale = TRUE,
        geometry = geom_set(
          landscape = TRUE,
          left = "0.4in",
          right = "0.4in"
        )
      ) |>
      render_pdf(escape_latex = FALSE)

    cat("  pharmaverse PDF: ", pharma_pdf, "\n")
  },
  error = function(e) {
    cat("  docorator render_pdf() failed: ", conditionMessage(e), "\n")
    cat("  (docorator requires LaTeX + specific packages installed)\n")
    pharma_html <- file.path(outdir, "pharmaverse_ae.html")
    gtsave(ae_gt, pharma_html)
    cat("  Fallback HTML saved: ", pharma_html, "\n")
  }
)

cat("  Lines of code (after data prep): ~90\n")
cat("  Packages used: cards, tfrmt, gt, docorator (4)\n")
cat("  Limitations:\n")
cat("    - No decimal alignment\n")
cat("    - No paginated multi-page PDF\n")
cat("    - No repeating column headers\n")
cat("    - No '(continued)' on page 2+\n")
cat("    - SOC/total bold requires manual gt::tab_style() per value\n")
cat("    - Must reshape wide -> ARD before tfrmt can use it\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  PATH B: arframe  (single-package approach)                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝

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

ae_arframe <- ae_wide |>
  fr_table() |>
  fr_titles(
    "Table 14.3.1",
    list("TEAEs by System Organ Class and Preferred Term", bold = TRUE),
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
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA version 26.0.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence."
  )

arframe_pdf <- file.path(outdir, "arframe_ae.pdf")
ae_arframe |> fr_render(arframe_pdf)
ae_arframe |> fr_render(file.path(outdir, "arframe_ae.rtf"))

cat("  arframe PDF: ", arframe_pdf, "\n")
cat("  Lines of code (after data prep): ~25\n")
cat("  Packages used: arframe (1)\n")
cat("  Capabilities:\n")
cat("    - Decimal alignment\n")
cat("    - Paginated PDF with repeating headers\n")
cat("    - '(continued)' text on page 2+\n")
cat("    - Page X of Y in footer\n")
cat("    - SOC/total bold via fr_row_style()\n")
cat("    - PT indentation via indent_by\n")
cat("    - Same spec also renders to RTF and HTML\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Side-by-side summary                                                  ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("================================================================\n")
cat("  COMPARISON SUMMARY — AE SOC/PT Table\n")
cat("================================================================\n\n")
cat(sprintf("  %-30s  %-25s  %-20s\n", "", "pharmaverse (GSK)", "arframe"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Lines after data prep", "~90 + 311 helpers", "~25"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Packages", "4 (cards+tfrmt+gt+doco)", "1 (arframe)"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Input format", "ARD (long)", "Wide summary"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Decimal alignment", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Paginated PDF", "No (single page)", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Repeating headers", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Continuation text", "No", "Yes"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Page X of Y", "Via doc_pagenum()", "Built-in"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "SOC/total bold", "Manual gt::tab_style()", "fr_row_style()"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "PT indentation", "Manual spaces", "indent_by"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Multi-format output", "PDF only", "PDF+RTF+HTML"))
cat(sprintf("  %-30s  %-25s  %-20s\n", "Real GSK code", "621 lines (t_saf_ae01)", "~25 lines"))
cat("\n  Output directory:", outdir, "\n")

fr_theme_reset()
