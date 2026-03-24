# ============================================================================
# arframe: Live Demo Script
# ============================================================================
#
# HOW TO USE THIS FILE:
#   1. Open in Positron/RStudio before the meeting.
#   2. Run "SETUP" (Section 0) before they join.
#   3. Walk through Sections 1-7 line-by-line (Ctrl/Cmd + Enter).
#   4. TALKING POINT comments = what to say out loud.
#   5. SAY comments = one-liners to read verbatim if you want.
#   6. Every section ends with a live PDF render you can open on screen.
#
# FLOW (25-30 min):
#   0. Setup                          (pre-meeting, ~1 min)
#   1. The problem — real study code  (~5 min — scroll through actual GSK code)
#   2. Study-wide theme               (~2 min — set once, all tables inherit)
#   3. Demographics table             (~5 min — QC bottleneck + decimal alignment)
#   4. AE by SOC/PT table             (~4 min — hierarchical, bold SOC, indent)
#   5. Vital signs table              (~4 min — page_by, per-page N-counts)
#   6. Column splitting               (~3 min — 8-arm wide table)
#   7. Multi-format output            (~2 min — same spec to PDF/RTF/HTML)
#   8. arbuilder (closer)             (~3 min — no-code UI)
#
# INSTALL (if needed):
#   pak::pak("vthanik/arframe")
#   pak::pak("vthanik/arbuilder")
#
# ============================================================================

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  0. SETUP — Run this before they join the call                           ║
# ╚══════════════════════════════════════════════════════════════════════════╝

library(pharmaverseadam)
library(arframe)
library(cards)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

outdir <- file.path(tempdir(), "arframe_demo")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Convert blanks to NA (pharmaverseadam has blank strings)
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
# ║  1. THE PROBLEM — Real GSK study code (52427_219230)                     ║
# ║     Open these files side-by-side to show the audience                   ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# ACTION: Open these two files in separate tabs. Share your screen.
#         Scroll through each one slowly. The code speaks for itself.
#
# -> TAB 1: Demographics (357 lines)
#    /home/vignesh/R_Projects/verify_compare/52427_219230-INTERNAL_01/prod/tfls/t_sp_dmt01.R
#
# -> TAB 2: AE by SOC/PT (621 lines)
#    /home/vignesh/R_Projects/verify_compare/52427_219230-INTERNAL_01/prod/tfls/t_saf_ae01_all.R
#
# SCROLL GUIDE — Demographics (t_sp_dmt01.R, 357 lines):
# -------------------------------------------------------
#   Lines 17-29   -> 13 library() calls. Point at the screen.
#   Lines 105-135 -> ARD reshaping. Unlist, mutate, case_when.
#                    Point at: unlist(lapply(group1_level, function(x) {...}))
#   Lines 142-159 -> Manual sort columns. ord1 = case_when(variable == ...).
#   Lines 163-192 -> Relabeling. 30 lines of case_when to rename variables.
#   Lines 195-233 -> Edge cases. Duplicate rows, recode subgroups, handle NA.
#   Lines 238-284 -> tfrmt spec. One frmt_structure per variable per stat.
#   Lines 288-298 -> gt post-processing. gsk_styling(), cols_width().
#   Lines 331-346 -> Render.
#
# SAY: "357 lines. 13 packages. One demographics table."
#       (or just let them see it)
#
# SCROLL GUIDE — AE table (t_saf_ae01_all.R, 621 lines):
# -------------------------------------------------------
#   Lines 75-129  -> Manual BigN. Separate stacks per arm, per sex.
#                    if/else to handle missing CAB ULA arm.
#   Lines 136-327 -> SIX ard_stack_hierarchical() calls.
#                    Same call repeated for: TRTSEQA, TRT01A, TRT02A,
#                    then all 3 again for SEXBRTHG = "Overall".
#                    Point at the repetition — same 30 lines, six times.
#   Lines 353-461 -> "ANY EVENT" row. Another 3 + 3 ard_stack calls.
#   Lines 467-506 -> Sort, relabel, factor reorder, dedup.
#   Lines 520-573 -> tfrmt + gt + tfl_format. Finally done.
#
# SAY: "621 lines. Six ard_stack calls. Same code, six times."
#       (or just scroll slowly and pause at the repetition)
#
# IF SOMEONE SAYS: "The pharmaverse website example is only ~87 lines."
# -------------------------------------------------------
# RESPONSE: Those 87 lines produce an unstyled HTML widget.
#   No titles, no footnotes, no page headers/footers,
#   no RTF or PDF output, no pagination, no decimal alignment,
#   no font control, no column widths.
#   The 357 lines in t_sp_dmt01.R is what it takes to produce
#   a submission-ready PDF/RTF with all of that.
#   arframe's ~35 lines produce the same submission-ready output.
#
# WHAT THE PHARMAVERSE WEBSITE EXAMPLES SKIP:
#   - Titles ("Table 14.1.1", study name, population)
#   - Footnotes (methodology notes, MedDRA version)
#   - Page headers (protocol, "CONFIDENTIAL")
#   - Page footers (program path, "Page X of Y", datetime)
#   - RTF/PDF file output (only HTML gt widget)
#   - Pagination (gt cannot paginate — one long scroll)
#   - Repeating column headers on page 2+
#   - Decimal alignment (frmt() is rounding, not alignment)
#   - Font family/size (Courier 9pt for FDA)
#   - Column width control
#   - "(continued)" text on subsequent pages
#
# SAY: "87 lines for an HTML preview. 357 lines for a real submission.
#        arframe does the real submission in 35 lines."
#
# THEN: Switch to this tab. Run the arframe code below.
#        The contrast will be obvious.

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  2. STUDY-WIDE THEME — Set once, every table inherits                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# SAY: "In the current stack, every program repeats font, orientation,
# headers, footers. Change the font size — update 50 programs."
#
# SAY: "arframe: set a theme once. Every table inherits it."

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

# -> Run the block above. Then move on to the tables.

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  3. DEMOGRAPHICS — Table 14.1.1                                          ║
# ║     QC bottleneck + decimal alignment                                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝
# SAY: "Same starting point — cards ARD. Same ard_stack() call."

# Step 1: Build the ARD (same as current stack)
demog_ard <- ard_stack(
  data = adsl_saf,
  .by = "TRT01A",
  ard_continuous(variables = "AGE"),
  ard_categorical(variables = c("AGEGR1", "SEX", "RACE", "ETHNIC")),
  .overall = TRUE
)

# SAY: "In t_sp_dmt01.R, the next 150 lines are manual reshaping.
# In arframe, it's one function call."

# Step 2: ARD to wide — one call replaces ~150 lines of reshape code
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

# -> Show them the clean dataframe:
demog_wide

# -> Point at the output. Clean dataframe. Ready for proc compare / diffdf.

# Step 3: Build the table spec — this is the entire rendering pipeline
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
    group_by = list(cols = "variable", label = "stat_label"),
    blank_after = "variable"
  ) |>
  fr_footnotes(
    "Percentages based on N per treatment group.",
    "CDISCPILOT01 Safety Population."
  )

# -> Run the block above. This replaces lines 105-346 of t_sp_dmt01.R.

# -> Render and open the PDF live:
demog_pdf <- file.path(outdir, "t_14_1_1_demog.pdf")
demog_spec |> fr_render(demog_pdf)

# -> Open the PDF. Point at the decimal alignment.
# SAY: "align = 'decimal'. One argument. No regex, no padding hacks."
# -> Open PDF: browseURL(demog_pdf)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  4. AE BY SOC/PT — Table 14.3.1                                          ║
# ║     Hierarchical structure, bold SOC rows, indentation, continuation     ║
# ╚══════════════════════════════════════════════════════════════════════════╝
# SAY: "AE by SOC/PT. You just scrolled through 621 lines.
# Here's the arframe version."

# Step 1: Hierarchical ARD (same as current stack)
adae_te <- blank_to_na(pharmaverseadam::adae) |>
  filter(TRTEMFL == "Y", TRT01A != "Screen Failure")

ae_ard <- ard_stack_hierarchical(
  data = adae_te,
  variables = c(AEBODSYS, AEDECOD),
  by = TRT01A,
  denominator = adsl_saf,
  id = USUBJID
)

# Step 2: ARD to wide
ae_wide <- fr_wide_ard(
  ae_ard,
  statistic = "{n} ({p}%)",
  decimals = c(p = 0),
  label = c("..ard_hierarchical_overall.." = "Any TEAE")
)

# Step 3: Table spec
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
  fr_rows(group_by = list(cols = "soc", label = "pt"), indent_by = "pt") |>
  fr_styles(
    fr_row_style(rows = fr_rows_matches("row_type", value = "soc"), bold = TRUE)
  ) |>
  fr_footnotes(
    "MedDRA coding dictionary version used.",
    "Subjects counted once per SOC and Preferred Term.",
    "Sorted by descending total incidence."
  )

# -> Render and open:
ae_pdf <- file.path(outdir, "t_14_3_1_ae_soc.pdf")
ae_spec |> fr_render(ae_pdf)

# -> Open the PDF. Point at:
#    - Bold SOC rows
#    - Indented PTs
#    - "(continued)" on page 2+
#    - Column headers repeat every page (gt cannot do this)
# -> Open PDF: browseURL(ae_pdf)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  5. VITAL SIGNS — Table 14.3.6                                           ║
# ║     page_by (1 page per parameter), per-page N-counts                    ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# SAY: "Vital signs. One page per parameter, N-counts update per page.
# tfrmt page_plan can paginate but cannot update N-counts.
# arframe: page_by = 'PARAM'."

# Data prep (same work either way — summarize ADVS)
advs_clean <- blank_to_na(pharmaverseadam::advs) |>
  filter(
    SAFFL == "Y",
    TRT01A != "Screen Failure",
    PARAMCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP"),
    AVISIT %in% c("Baseline", "Week 12", "End of Treatment"),
    ANL01FL == "Y"
  )

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
  mutate(
    baseline = paste0(base_mean, " (", base_sd, ")"),
    value = paste0(val_mean, " (", val_sd, ")"),
    cfb = paste0(chg_mean, " (", chg_sd, ")")
  ) |>
  select(PARAM, AVISIT, TRT01A, n, baseline, value, cfb)

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

n_vs <- advs_clean |>
  filter(AVISIT != "Baseline") |>
  distinct(USUBJID, PARAM, TRT01A) |>
  count(PARAM, TRT01A) |>
  pivot_wider(names_from = TRT01A, values_from = n) |>
  select(-PARAM)
n_vs <- unlist(n_vs[1, ])

# -> The arframe spec:
vs_spec <- vs_long |>
  fr_table() |>
  fr_titles(
    "Table 14.3.6",
    list("Vital Signs -- Summary by Parameter and Visit", bold = TRUE),
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
    blank_after = "AVISIT"
  ) |>
  fr_footnotes(
    "Baseline is the last non-missing value on or before first dose.",
    "Change = Post-baseline value - Baseline value."
  )

vs_pdf <- file.path(outdir, "t_14_3_6_vital_signs.pdf")
vs_spec |> fr_render(vs_pdf)

# -> Open the PDF. Flip through pages. Point at:
#    - Each parameter = own page
#    - N-counts in headers change per page
#    - Bold visit headers, blank rows between visits
# -> Open PDF: browseURL(vs_pdf)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  6. COLUMN SPLITTING — 8-arm wide table                                  ║
# ║     Automatic pagination with stub repeat                                ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# SAY: "8 treatment arms. Too wide for one page.
# gt cannot split columns. arframe: .split = TRUE."

# -> Simulate 8 arms by duplicating columns:
wide_8arm <- demog_wide |>
  mutate(
    `Arm 4` = Placebo,
    `Arm 5` = Placebo,
    `Arm 6` = Placebo,
    `Arm 7` = Placebo,
    `Arm 8` = Placebo
  )

wide_spec <- wide_8arm |>
  fr_table() |>
  fr_titles("Table 14.1.1a", "Demographics — 8-Arm Column Split Demo") |>
  fr_cols(
    variable = fr_col(visible = FALSE),
    stat_label = fr_col("Demographic", width = 2.5, stub = TRUE),
    .split = TRUE
  ) |>
  fr_rows(
    group_by = list(cols = "variable", label = "stat_label"),
    blank_after = "variable"
  )

split_pdf <- file.path(outdir, "demo_column_split.pdf")
wide_spec |> fr_render(split_pdf)

# -> Open the PDF. Point at:
#    - Page 1: stub + arms 1-4
#    - Page 2: stub + arms 5-8 (stub repeats automatically)
# -> Open PDF: browseURL(split_pdf)

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  7. MULTI-FORMAT — Same spec, 3 outputs                                  ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# SAY: "Same spec, three formats. No format-specific code."

demog_spec |> fr_render(file.path(outdir, "t_14_1_1_demog.rtf"))
demog_spec |> fr_render(file.path(outdir, "t_14_1_1_demog.html"))

# -> Open the RTF in Word. Open the HTML in browser.
# SAY: "RTF opens in Word like SAS-generated RTF. HTML works in pkgdown."

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  8. ARBUILDER — No-code UI (the closer)                                  ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# SAY: "For programmers who prefer a UI — arbuilder.
# Point-and-click. Generates the R script automatically."

# -> Run this to open the Shiny app:
# shiny::runApp("../arbuilder")

# ONCE THE APP OPENS:
#   1. Pick a template (Demographics)
#   2. Configure columns, titles, footnotes
#   3. Click "Export" — point at the generated R script
#
# -> The generated code is a working fr_table() pipeline.
#    No syntax to memorize. Starting point they can customize.

# ╔══════════════════════════════════════════════════════════════════════════╗
# ║  SUMMARY — If they ask for the scoreboard                                ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
#   Real study: 52427_219230-INTERNAL_01
#
#                           GSK stack (actual code)    arframe
#   Demographics            357 lines, 13 pkgs        ~35 lines, 2 pkgs
#   AE SOC/PT               621 lines, 8+ pkgs        ~35 lines, 2 pkgs
#   Vital Signs             375 lines, 8+ pkgs        ~35 lines, 2 pkgs
#   tfl_format.R (shared)   327 lines                  0 (built-in theme)
#                           ─────────────────          ──────────────────
#   Total (3 tables)        1,680 lines                ~105 lines
#
#   Where do 1,575 lines go?
#     - ARD reshaping (unlist, mutate, case_when)     ~400 lines -> fr_wide_ard()
#     - BigN manual construction                      ~100 lines -> .n argument
#     - Sort columns (ord1, ord2, ord3)               ~80 lines  -> automatic
#     - Zero-record completion                        ~50 lines  -> automatic
#     - tfrmt frmt_structure per variable             ~80 lines  -> decimals arg
#     - gt post-processing (gsk_styling, cols_width)  ~30 lines  -> fr_theme()
#     - tfl_format wrapper (header/footer/render)     ~327 lines -> fr_theme()
#     - Edge cases (missing arms, sex splits)         ~200 lines -> automatic
#     - Duplicated ard_stack calls (6x in AE)         ~300 lines -> 1 call
#
#   "But the pharmaverse website shows only ~87 lines!"
#   Those 87 lines produce an unstyled HTML widget — no titles,
#   no footnotes, no RTF/PDF, no pagination, no decimal alignment.
#   The fair comparison is submission-ready output vs submission-ready output.
#
#   arframe-only capabilities (not possible in the current stack):
#     - Decimal alignment (native, one argument)
#     - Paginated multi-page PDF with repeating column headers
#     - Column splitting for wide tables (stub repeats per panel)
#     - page_by: one spec, separate pages per parameter
#     - Per-page N-counts (update automatically per page_by group)
#     - Continuation text ("(continued)") on page 2+
#     - Study-wide theme (set once, all tables inherit)
#     - Same spec -> PDF + RTF + HTML (3 formats, zero extra code)
#     - Instant parallel QC (fr_wide_ard output = diffdf/proc compare ready)

cat("\nAll outputs saved to:", outdir, "\n")

fr_theme_reset()
