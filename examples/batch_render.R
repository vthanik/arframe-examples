# ============================================================================
# Batch Render — Metadata-Driven Table Production
# ============================================================================
#
# Two methods for running an entire CSR table package from a single manifest:
#
#   Method 1: Excel/CSV manifest → loop renders all tables
#   Method 2: R list with inline data + config → functional batch
#
# Both methods share one theme — no per-table formatting boilerplate.
#
# Data source: pharmaverseadam (CDISCPILOT01 study)
# Summary datasets built from raw ADaM using cards + fr_wide_ard.

library(arframe)
library(pharmaverseadam)
library(cards)
library(dplyr, warn.conflicts = FALSE)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Study Setup (shared by both methods)                                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝

fr_theme_reset()
fr_theme(
  font_size = 9, font_family = "Courier New", orientation = "landscape",
  hlines = "header", header = list(bold = TRUE, align = "center"),
  n_format = "{label}\n(N={n})", footnote_separator = FALSE,
  pagehead = list(left = "Protocol: CDISCPILOT01", right = "CONFIDENTIAL"),
  pagefoot = list(left = "{program}", right = "Page {thepage} of {total_pages}")
)

outdir <- file.path(tempdir(), "batch_output")
dir.create(outdir, showWarnings = FALSE)


# ── Helper: convert blank strings to NA (pharmaverseadam convention) ─────

blank_to_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) ifelse(nzchar(trimws(x)), x, NA_character_) else x
  })
  df
}


# ── Build analysis populations ───────────────────────────────────────────

adsl_saf <- blank_to_na(pharmaverseadam::adsl) |>
  filter(SAFFL == "Y", TRT01A != "Screen Failure")

adae_te <- blank_to_na(pharmaverseadam::adae) |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

# Dynamic N-counts from data
arm_n <- adsl_saf |>
  count(TRT01A) |>
  pull(n, name = TRT01A)

n_safety <- c(arm_n, Total = sum(arm_n))


# ── Build summary datasets ───────────────────────────────────────────────

cat("Building summary datasets from pharmaverseadam...\n")

# Demographics: continuous (AGE) + categorical (AGEGR1, SEX, RACE, ETHNIC)
demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "TRT01A",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("AGEGR1", "SEX", "RACE", "ETHNIC")),
  .overall = TRUE
)

demog_wide <- fr_wide_ard(
  demog_ard,
  statistic = list(
    continuous = c(
      "n"          = "{N}",
      "Mean (SD)"  = "{mean} ({sd})",
      "Median"     = "{median}",
      "Min, Max"   = "{min}, {max}"
    ),
    categorical = "{n} ({p}%)"
  ),
  decimals = c(mean = 1, sd = 2, median = 1, p = 0),
  label = c(
    AGE    = "Age (years)",
    AGEGR1 = "Age Group, n (%)",
    SEX    = "Sex, n (%)",
    RACE   = "Race, n (%)",
    ETHNIC = "Ethnicity, n (%)"
  )
)

# AE by SOC and Preferred Term (hierarchical)
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
  decimals = c(p = 0),
  label = c("..ard_hierarchical_overall.." = "Any TEAE")
)

cat("  demog_wide:", nrow(demog_wide), "rows,", ncol(demog_wide), "cols\n")
cat("  ae_wide:   ", nrow(ae_wide),   "rows,", ncol(ae_wide),   "cols\n")
cat("\n")

# Arm column names from actual data (e.g. "Placebo", "Xanomeline Low Dose", ...)
arm_cols <- names(arm_n)


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  METHOD 1: Excel/CSV Manifest                                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# In production, this data frame would come from:
#   readxl::read_excel("TFL_Manifest.xlsx")
# or:
#   read.csv("tfl_manifest.csv")
#
# Each row = one table. Columns define everything needed to render it.

cat("══════════════════════════════════════════════════════════════\n")
cat("  METHOD 1: Excel/CSV Manifest-Driven Batch\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── The manifest (would be an Excel file in production) ──────────────────

manifest <- data.frame(
  # GSK naming: t_{domain}_{shell}{seq}_{variant}
  # domain: sp = study population, saf = safety
  # shell:  dmt = demographics, ae = adverse event
  table_id   = c("t_sp_dmt01",     "t_saf_ae01_all"),
  table_num  = c("Table 14.1.1",   "Table 14.3.1"),
  title      = c("Summary of Demographic and Baseline Characteristics",
                 "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"),
  popfl      = c("SAFFL",          "SAFFL"),
  dataset    = c("demog_wide",     "ae_wide"),
  n_pool     = c("n_safety",       "n_safety"),
  footnote1  = c("Percentages based on N per treatment group.",
                 "MedDRA coding dictionary version used."),
  footnote2  = c("CDISCPILOT01 Safety Population.",
                 "Subjects counted once per SOC and Preferred Term."),
  footnote3  = c(NA, "Sorted by descending total incidence."),
  group_by   = c("variable",       "soc"),
  indent_by  = c(NA,               "pt"),
  bold_rows  = c(NA,               "row_type:soc"),
  blank_after = c("variable",      NA),
  title_bold = c(FALSE,            TRUE),
  continuation = c(FALSE,          TRUE),
  stringsAsFactors = FALSE
)

cat("Manifest (would be read from Excel):\n")
print(manifest[, c("table_id", "table_num", "popfl", "dataset", "n_pool")])
cat("\n")

# ── Column definitions per dataset (could also come from Excel) ──────────

# Helper to build fr_col list for arm columns
make_arm_cols <- function(arm_cols) {
  setNames(
    lapply(arm_cols, function(a) fr_col(a, align = "decimal")),
    arm_cols
  )
}

col_defs <- list(
  demog_wide = list(
    cols = c(
      list(
        variable  = list(visible = FALSE),
        stat_label = list(label = " ", width = 2.5)
      ),
      make_arm_cols(arm_cols),
      list(
        Total = list(label = "Total", align = "decimal")
      )
    ),
    group_label = "stat_label"
  ),
  ae_wide = list(
    cols = c(
      list(
        soc      = list(visible = FALSE),
        pt       = list(label = "System Organ Class\n  Preferred Term", width = 3.5),
        row_type = list(visible = FALSE)
      ),
      make_arm_cols(arm_cols)
    ),
    group_label = "pt"
  )
)

# ── The batch engine: loop over manifest rows ────────────────────────────

build_table_from_manifest <- function(row, col_defs, n_pools) {
  # Get pre-built summary data
  data <- get(row$dataset)

  # Get N-counts
  n_vec <- n_pools[[row$n_pool]]

  # Build fr_col objects from col_defs
  cdef <- col_defs[[row$dataset]]$cols
  fr_cols_list <- lapply(names(cdef), function(nm) {
    d <- cdef[[nm]]
    if (isFALSE(d$visible)) return(fr_col(visible = FALSE))
    do.call(fr_col, d[setdiff(names(d), "visible")])
  })
  names(fr_cols_list) <- names(cdef)

  # Start pipeline
  spec <- data |> fr_table()

  # Population label from flag
  pop_map <- c(SAFFL = "Safety Population", MFASFL = "mFAS Population",
               ENRLFL = "Enrolled Population", FASFL = "FAS Population")
  pop_label <- unname(pop_map[row$popfl]) %||% row$popfl

  # Titles
  if (isTRUE(row$title_bold)) {
    spec <- spec |> fr_titles(row$table_num, list(row$title, bold = TRUE), pop_label)
  } else {
    spec <- spec |> fr_titles(row$table_num, row$title, pop_label)
  }

  # Continuation
  if (isTRUE(row$continuation)) {
    spec <- spec |> fr_page(continuation = "(continued)")
  }

  # Columns (pass fr_col list + N-counts)
  col_args <- c(list(spec = spec), fr_cols_list, list(.n = n_vec))
  spec <- do.call(fr_cols, col_args)

  # Row organization — use group_label from col_defs metadata
  grp_label <- col_defs[[row$dataset]]$group_label
  if (!is.na(row$group_by)) {
    row_args <- list(spec = spec, group_by = row$group_by, group_label = grp_label,
                     group_bold = TRUE)
    if (!is.na(row$indent_by))   row_args$indent_by   <- row$indent_by
    if (!is.na(row$blank_after)) row_args$blank_after  <- row$blank_after
    spec <- do.call(fr_rows, row_args)
  }

  # Conditional bold rows (format: "col:val1,val2")
  if (!is.na(row$bold_rows)) {
    parts <- strsplit(row$bold_rows, ":")[[1]]
    col_name <- parts[1]
    values <- strsplit(parts[2], ",")[[1]]
    styles <- lapply(values, function(v) {
      fr_row_style(rows = fr_rows_matches(col_name, value = v), bold = TRUE)
    })
    spec <- do.call(fr_styles, c(list(spec = spec), styles))
  }

  # Footnotes
  fns <- c(row$footnote1, row$footnote2, row$footnote3)
  fns <- fns[!is.na(fns)]
  if (length(fns) > 0) {
    spec <- do.call(fr_footnotes, c(list(spec = spec), as.list(fns)))
  }

  spec
}

# ── Run the batch ────────────────────────────────────────────────────────

n_pools <- list(n_safety = n_safety)

cat("Running batch render...\n")
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  spec <- build_table_from_manifest(row, col_defs, n_pools)
  spec |> fr_render(file.path(outdir, paste0(row$table_id, ".pdf")))
  spec |> fr_render(file.path(outdir, paste0(row$table_id, ".rtf")))
  cat(sprintf("  [%d/%d] %s -> PDF + RTF\n", i, nrow(manifest), row$table_id))
}

cat("\nMethod 1 complete:", nrow(manifest), "tables rendered.\n")
cat("Output:", outdir, "\n\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  METHOD 2: R List Registry — Functional Batch                            ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# For teams that prefer R over Excel, define a registry of table specs.
# Each entry is a function that returns an fr_spec — lazy evaluation.
# The registry can live in a shared .R file that the team maintains.

cat("══════════════════════════════════════════════════════════════\n")
cat("  METHOD 2: R List Registry — Functional Batch\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── Table registry: each entry is a function returning an fr_spec ────────

table_registry <- list(

  "t_sp_dmt01" = function() {
    demog_wide |>
      fr_table() |>
      fr_titles(
        "Table 14.1.1",
        "Summary of Demographic and Baseline Characteristics",
        "Safety Population"
      ) |>
      fr_cols(
        variable   = fr_col(visible = FALSE),
        stat_label = fr_col(" ", width = 2.5),
        !!!setNames(
          lapply(arm_cols, function(a) fr_col(a, align = "decimal")),
          arm_cols
        ),
        Total = fr_col("Total", align = "decimal"),
        .n = n_safety
      ) |>
      fr_rows(
        group_by    = "variable",
        group_label = "stat_label",
        group_bold  = TRUE,
        blank_after = "variable"
      ) |>
      fr_footnotes(
        "Percentages based on N per treatment group.",
        "CDISCPILOT01 Safety Population."
      )
  },

  "t_saf_ae01_all" = function() {
    ae_wide |>
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
        soc      = fr_col(visible = FALSE),
        pt       = fr_col("System Organ Class\n  Preferred Term", width = 3.5),
        row_type = fr_col(visible = FALSE),
        !!!setNames(
          lapply(arm_cols, function(a) fr_col(a, align = "decimal")),
          arm_cols
        ),
        .n = arm_n
      ) |>
      fr_rows(
        group_by    = "soc",
        group_label = "pt",
        indent_by   = "pt"
      ) |>
      fr_styles(
        fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
      ) |>
      fr_footnotes(
        "MedDRA coding dictionary version used.",
        "Subjects counted once per SOC and Preferred Term.",
        "Sorted by descending total incidence."
      )
  }

)

# ── Batch runner ─────────────────────────────────────────────────────────

outdir2 <- file.path(tempdir(), "batch_registry")
dir.create(outdir2, showWarnings = FALSE)

cat("Running registry batch...\n")
for (nm in names(table_registry)) {
  spec <- table_registry[[nm]]()
  spec |> fr_render(file.path(outdir2, paste0(nm, ".pdf")))
  spec |> fr_render(file.path(outdir2, paste0(nm, ".rtf")))
  cat(sprintf("  %s -> PDF + RTF\n", nm))
}

cat("\nMethod 2 complete:", length(table_registry), "tables rendered.\n")
cat("Output:", outdir2, "\n\n")

# ── Selective render (run just one table) ────────────────────────────────

cat("── Selective render: demographics only ──\n")
table_registry[["t_sp_dmt01"]]() |>
  fr_render(file.path(outdir2, "t_sp_dmt01_rerun.pdf"))
cat("  t_sp_dmt01 rerun -> PDF\n")


# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  Comparison: Method 1 vs Method 2                                        ║
# ╚══════════════════════════════════════════════════════════════════════════╝

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  METHOD COMPARISON\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("Method 1 (Excel Manifest):\n")
cat("  + Titles/footnotes editable by non-programmers (in Excel)\n")
cat("  + Easy to add/remove tables — just edit a spreadsheet row\n")
cat("  + Column defs can also live in Excel (JSON-like)\n")
cat("  + Good for study teams where stats reviews the manifest\n")
cat("  - Requires a build_table_from_manifest() engine function\n")
cat("  - Complex tables (styles, indent_by) need encoding convention\n\n")

cat("Method 2 (R List Registry):\n")
cat("  + Full R expressiveness — any arframe feature available\n")
cat("  + Each table is self-contained and readable\n")
cat("  + Easy to run selectively: table_registry[['t_saf_ae01_all']]()\n")
cat("  + Version-controlled in git alongside the analysis code\n")
cat("  - Titles/footnotes changes require editing R code\n\n")

cat("Both methods share:\n")
cat("  - One fr_theme() call — all tables inherit formatting\n")
cat("  - One for-loop — renders everything to PDF + RTF\n")
cat("  - Summary datasets built once (cards + fr_wide_ard) at the top\n")
cat("  - N-counts computed dynamically from adsl_saf\n")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  vs GSK pharmaverse batch approach\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("In the GSK codebase, batch execution means:\n")
cat("  - One .R file PER table (t_sp_dmt01.R, t_saf_ae01_all.R, ...)\n")
cat("  - Each file: ~300–600 lines (data prep + tfrmt + gt + docorator)\n")
cat("  - Each file sources rx_setup.R, gsk_styling.R, tfl_format.R\n")
cat("  - A shell script (navigate_shell.sh) runs each .R file\n")
cat("  - No shared theme — every table repeats formatting config\n\n")

cat("With arframe:\n")
cat("  - One .R file for ALL tables (this script)\n")
cat("  - ~15 lines per table (Method 2) or ~1 row per table (Method 1)\n")
cat("  - Summary data built once from pharmaverseadam at the top\n")
cat("  - One theme, one loop, done\n")

fr_theme_reset()
