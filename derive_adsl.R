# derive_adsl.R --------------------------------------------------------------
#
# ADaM Subject-Level Analysis Dataset (ADSL)
#
# Generated with the `admiral-adsl` skill (skills/admiral/admiral-adsl/SKILL.md),
# following the shared conventions in skills/admiral/SKILL.md.
#
# Source data : pharmaversesdtm (study CDISCPILOT01) — benchmark data, not a
#               real study extract.
# ADaM spec   : NONE SUPPLIED. Every cut-point, arm code and population flag
#               definition below is a skill default, not a spec-driven value.
#               All such points carry a `# REVIEW:` annotation.
#
# THIS IS A DRAFTING AID, NOT A VALIDATED DELIVERABLE. QC every derivation
# against your own process before any regulatory, clinical or GxP use.
# --------------------------------------------------------------------------

# -- Step 1 — Setup and domain loading -------------------------------------

library(admiral)
library(dplyr)
library(lubridate)
library(pharmaversesdtm)

data("dm", "ex", "ds", "vs", package = "pharmaversesdtm")

# NOTE: DV (protocol deviations) is not distributed in pharmaversesdtm. See
#   Step 11 for the consequence for PPROTFL.

# Confirm one record per USUBJID in DM before proceeding
stopifnot(nrow(dm) == n_distinct(dm$USUBJID))

# -- Step 2 — Subject spine ------------------------------------------------

# Start from DM. One record per USUBJID is mandatory here and is preserved
# throughout; asserted again at Step 12.
adsl <- dm |>
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    AGE, AGEU, SEX, RACE, ETHNIC, COUNTRY,
    ARM, ARMCD, ACTARM, ACTARMCD,
    DMDTC, RFSTDTC, RFENDTC,
    DTHFL, DTHDTC
  )

# -- Step 3 — Treatment dates ----------------------------------------------
# TRTSDTM / TRTSTMF / TRTEDTM / TRTETMF from EX.EXSTDTC / EX.EXENDTC.
# Datetimes are derived first, then date-only variables are extracted.

ex_dtm <- ex |>
  select(-DOMAIN) |>
  derive_vars_dtm(
    dtc             = EXSTDTC,
    new_vars_prefix = "EXST",
    date_imputation = "first",
    time_imputation = "first",
    flag_imputation = "auto"
  ) |>
  derive_vars_dtm(
    dtc             = EXENDTC,
    new_vars_prefix = "EXEN",
    date_imputation = "last",
    time_imputation = "last",
    flag_imputation = "auto"
  )

# REVIEW: The placebo filter (EXTRT == "PLACEBO") must be confirmed against the
#   protocol. In CDISCPILOT01 EX contains EXTRT values PLACEBO and XANOMELINE,
#   and EXDOSE = 0 records DO occur — so `EXDOSE > 0` alone would silently drop
#   dosed placebo subjects. Confirm the intended condition with the statistician.
adsl <- adsl |>
  # TRTSDTM / TRTSTMF: first dose datetime and its time-imputation flag
  derive_vars_merged(
    dataset_add = ex_dtm,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM),
    mode        = "first",
    filter_add  = (EXDOSE > 0 | EXTRT == "PLACEBO") & !is.na(EXSTDTM)
  ) |>
  # TRTEDTM / TRTETMF: last dose datetime and its time-imputation flag
  # REVIEW: If subjects have non-contiguous EX records, TRTEDTM reflects the
  #   last administration datetime only, not continuous exposure. Flag for QC
  #   if exposure gaps exist in this study.
  # REVIEW: 2 dosed subjects in this study (01-705-1018, 01-705-1382) have EX
  #   records with EXSTDTC populated but EXENDTC missing, so they get TRTSDT but
  #   TRTEDT = NA and therefore TRTDURD = NA — they are SAFFL == "Y" yet drop out
  #   of any treatment-duration summary. Decide with the statistician whether to
  #   query the data or impute TRTEDT (e.g. from EOSDT / last known contact).
  derive_vars_merged(
    dataset_add = ex_dtm,
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM),
    mode        = "last",
    filter_add  = (EXDOSE > 0 | EXTRT == "PLACEBO") & !is.na(EXENDTM)
  ) |>
  # TRTSDT / TRTEDT: date parts of the treatment datetimes. as.Date() is safe
  # here because the inputs are already admiral-derived POSIXct (UTC), not
  # raw --DTC character values.
  mutate(
    TRTSDT = as.Date(TRTSDTM),
    TRTEDT = as.Date(TRTEDTM)
  )

# -- Step 4 — Planned and actual treatment ---------------------------------
# TRT01P/TRT01PN from DM.ARMCD (planned); TRT01A/TRT01AN from DM.ACTARMCD
# (actual). Derived independently via a lookup — the idiomatic admiral
# approach for controlled terminology, preferred over case_when().

# REVIEW: Confirm ARMCD values, treatment labels and numeric codes against the
#   randomisation schedule and ADaM spec before use. Codes 1/2/3 below are a
#   skill default, NOT spec-driven.
arm_lookup <- tibble::tribble(
  ~ARMCD,    ~TRT01P,                   ~TRT01PN,
  "Pbo",     "Placebo",                 1L,
  "Xan_Lo",  "Xanomeline Low Dose",     2L,
  "Xan_Hi",  "Xanomeline High Dose",    3L
  # Screen failure subjects (ARMCD == "Scrnfail") are intentionally absent from
  # the lookup and receive NA. admiral reports them as "not mapped" — expected.
)

adsl <- adsl |>
  derive_vars_merged_lookup(
    dataset_add = arm_lookup,
    by_vars     = exprs(ARMCD),
    new_vars    = exprs(TRT01P, TRT01PN)
  ) |>
  derive_vars_merged_lookup(
    dataset_add = arm_lookup |>
      rename(ACTARMCD = ARMCD, TRT01A = TRT01P, TRT01AN = TRT01PN),
    by_vars     = exprs(ACTARMCD),
    new_vars    = exprs(TRT01A, TRT01AN)
  )

# -- Step 5 — Randomisation and reference dates ----------------------------
# derive_vars_dt() is used for all --DTC conversions so partial dates are
# imputed and flagged rather than silently returning NA.

adsl <- adsl |>
  # RANDDT: date of randomisation from DM.DMDTC
  # REVIEW: Confirm DMDTC is the randomisation date in this study. In some
  #   studies randomisation date comes from a separate SDTM domain.
  derive_vars_dt(
    dtc             = DMDTC,
    new_vars_prefix = "RAND",
    date_imputation = "first",
    flag_imputation = "auto"
  ) |>
  # RFSTDT / RFENDT: subject reference start and end dates from DM
  derive_vars_dt(
    dtc             = RFSTDTC,
    new_vars_prefix = "RFST",
    date_imputation = "first",
    flag_imputation = "auto"
  ) |>
  derive_vars_dt(
    dtc             = RFENDTC,
    new_vars_prefix = "RFEND",
    date_imputation = "last",
    flag_imputation = "auto"
  )

# -- Step 6 — Death variables ----------------------------------------------
# DTHDT from DM.DTHDTC; DTHFL normalised to the CDISC flag convention.

adsl <- adsl |>
  derive_vars_dt(
    dtc             = DTHDTC,
    new_vars_prefix = "DTH",
    date_imputation = "first",
    flag_imputation = "auto"
  ) |>
  mutate(
    # Flag convention: "Y" or NA — never "N"
    DTHFL = if_else(DTHFL == "Y", "Y", NA_character_)
  )

# -- Step 7 — Study day variables ------------------------------------------
# RANDDY relative to TRTSDT. derive_vars_dy() applies the CDISC convention
# (Day 1 is the reference date; there is no Day 0).

adsl <- adsl |>
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars    = exprs(RANDDT)
  )

# -- Step 8 — Treatment duration -------------------------------------------
# TRTDURD from TRTSDT and TRTEDT. NA for untreated subjects.

adsl <- adsl |>
  derive_var_trtdurd()

# -- Step 9 — Disposition (EOSSTT, EOSDT, DCSREAS, DCSREASP) ---------------
# DS is filtered to the disposition event records and the end-of-study status
# is categorised WITHIN the source dataset before merging — DSDECOD holds
# reason values, not status values, so it must never flow into EOSSTT directly.

ds_eos <- ds |>
  select(-DOMAIN) |>
  filter(DSCAT == "DISPOSITION EVENT") |>
  derive_vars_dt(
    dtc             = DSDTC,
    new_vars_prefix = "DS",
    date_imputation = "last",
    flag_imputation = "auto"
  )

# Confirm one DISPOSITION EVENT record per subject before merging
stopifnot(n_distinct(ds_eos$USUBJID) == nrow(ds_eos))

# EOSSTT: end of study status — "COMPLETED" or "DISCONTINUED" only
# REVIEW: The binary mapping below is a skill default and is very likely WRONG
#   for this study. DS.DSDECOD in CDISCPILOT01 contains 10 distinct values,
#   including "STUDY TERMINATED BY SPONSOR" (7 subjects) and "SCREEN FAILURE"
#   (52 subjects). Both are collapsed to "DISCONTINUED" here. Many protocols
#   need a separate sponsor-termination category, and screen failures are
#   commonly excluded from EOSSTT altogether rather than counted as
#   discontinuations. Confirm the full mapping with the statistician.
adsl <- adsl |>
  derive_vars_merged(
    dataset_add = ds_eos |>
      mutate(
        EOSSTT = if_else(DSDECOD == "COMPLETED", "COMPLETED", "DISCONTINUED")
      ),
    by_vars  = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSSTT, EOSDT = DSDT)
  ) |>
  # DCSREAS / DCSREASP: discontinuation reason, NA for completers per CDISC
  # convention. Filtering the source to non-completers avoids a post-merge
  # mutate() cleanup.
  # REVIEW: DCSREAS is the decoded value (DS.DSDECOD); DCSREASP is the verbatim
  #   text (DS.DSTERM). Do not swap these.
  derive_vars_merged(
    dataset_add = ds_eos |>
      filter(DSDECOD != "COMPLETED"),
    by_vars  = exprs(STUDYID, USUBJID),
    new_vars = exprs(DCSREAS = DSDECOD, DCSREASP = DSTERM)
  )

# -- Step 10 — Baseline demographics ---------------------------------------

# REVIEW: Age cut-points MUST come from the ADaM spec — they are study-specific
#   and no spec was supplied for this run. The three groups below are the
#   skill's PLACEHOLDER values. Replace before any use.
adsl <- adsl |>
  mutate(
    AGEGR1 = case_when(
      AGE < 65              ~ "<65",     # PLACEHOLDER — confirm from spec
      AGE >= 65 & AGE <= 80 ~ "65-80",   # PLACEHOLDER — confirm from spec
      AGE > 80              ~ ">80"      # PLACEHOLDER — confirm from spec
    ),
    AGEGR1N = case_when(
      AGEGR1 == "<65"   ~ 1L,
      AGEGR1 == "65-80" ~ 2L,
      AGEGR1 == ">80"   ~ 3L
    )
  )

# HEIGHTBL / WEIGHTBL / BMIBL from the baseline VS records.
#
# WEIGHT uses the SDTM-supplied baseline flag (VSBLFL == "Y"), but HEIGHT
# CANNOT: in CDISCPILOT01 height is collected once at SCREENING 1 and NONE of
# its 254 records carry VSBLFL == "Y". Filtering height on VSBLFL therefore
# matches nothing and silently yields HEIGHTBL = NA for every subject (which in
# turn makes BMIBL all-NA). Height is sourced from its single screening record
# instead.
#
# REVIEW: Baseline definitions are spec-driven and differ per test here. Confirm
#   both: (a) that the single SCREENING 1 height is the intended HEIGHTBL, and
#   (b) that VSBLFL is the intended baseline for weight — if the ADaM spec
#   defines baseline by a visit window or by "last non-missing pre-dose value",
#   both derivations must change.
adsl <- adsl |>
  derive_vars_merged(
    dataset_add = vs |> select(-DOMAIN),
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(HEIGHTBL = VSSTRESN),
    order       = exprs(VSDTC, VSSEQ),
    mode        = "first",
    filter_add  = VSTESTCD == "HEIGHT" & !is.na(VSSTRESN)
  ) |>
  derive_vars_merged(
    dataset_add = vs |> select(-DOMAIN),
    by_vars     = exprs(STUDYID, USUBJID),
    new_vars    = exprs(WEIGHTBL = VSSTRESN),
    filter_add  = VSTESTCD == "WEIGHT" & VSBLFL == "Y"
  ) |>
  # VS carries no BMI record in this study, so BMIBL is computed from the
  # baseline height and weight using admiral's own helper.
  mutate(
    BMIBL = compute_bmi(height = HEIGHTBL, weight = WEIGHTBL)
  )

# -- Step 11 — Population flags (SAFFL, ITTFL, PPROTFL) --------------------
# Population flag definitions are protocol-specific. The logic below is
# standard but MUST be reviewed against the protocol and SAP. Flags are "Y"
# or NA only — never "N".

# SAFFL: received at least one dose
# REVIEW: SAFFL definition is protocol-specific. The condition includes placebo
#   subjects via EXTRT because EXDOSE = 0 occurs for placebo in this study.
#   Verify EXTRT values in EX exhaustively and confirm with the statistician.
adsl <- adsl |>
  derive_var_merged_exist_flag(
    dataset_add   = ex |> select(-DOMAIN),
    by_vars       = exprs(STUDYID, USUBJID),
    new_var       = SAFFL,
    condition     = (EXDOSE > 0 | EXTRT == "PLACEBO") & !is.na(EXSTDTC),
    true_value    = "Y",
    false_value   = NA_character_,
    missing_value = NA_character_
  ) |>
  # ITTFL: randomised subjects. ARMCD is the more reliable test; ARM text is
  # checked too as a safeguard against coding gaps.
  # REVIEW: Confirm ITTFL exclusion criteria with the statistician.
  mutate(
    ITTFL = if_else(
      ARMCD != "Scrnfail" & ARM != "Screen Failure",
      "Y",
      NA_character_
    )
  )

# PPROTFL: per-protocol population — ITT subjects with no major protocol
# deviation.
#
# NOT DERIVED. The DV (protocol deviations) domain is not distributed in
# pharmaversesdtm, so the major-deviation exclusion cannot be evaluated. Per
# the skill's explicit escape hatch, PPROTFL is created and set to NA for all
# subjects so downstream code referencing it does not error.
#
# REVIEW: PPROTFL is NA for every subject because DV was unavailable — this
#   means "per-protocol status NOT ASSESSED", it does NOT mean "no subject had
#   a major deviation". Do not use this variable for any per-protocol analysis.
#   To derive it properly: load DV, flag records meeting the study's major-
#   deviation criterion (DVCAT == "MAJOR", or DVSCAT / a study-specific flag —
#   confirm against the protocol deviation management plan), merge that flag in
#   with derive_vars_merged(filter_add = ..., mode = "first"), then set
#   PPROTFL = if_else(ITTFL == "Y" & is.na(MAJDVFL), "Y", NA_character_).
adsl <- adsl |>
  mutate(PPROTFL = NA_character_)

# -- Step 12 — Dataset attributes and final checks -------------------------

# One record per USUBJID — non-negotiable per ADaMIG
stopifnot(nrow(adsl) == n_distinct(adsl$USUBJID))

# Required variables present
required_vars <- c(
  "STUDYID", "USUBJID", "TRTSDT", "TRTEDT",
  "TRT01P", "TRT01PN", "TRT01A", "TRT01AN",
  "TRTSDTM", "TRTSTMF", "TRTEDTM", "TRTETMF",
  "EOSSTT", "SAFFL", "ITTFL"
)
missing_vars <- setdiff(required_vars, names(adsl))
if (length(missing_vars) > 0) {
  stop("Missing required ADSL variables: ", paste(missing_vars, collapse = ", "))
}

# Conformance checks on the derived values themselves
stopifnot(
  # EOSSTT is restricted to the two permitted values
  all(adsl$EOSSTT %in% c("COMPLETED", "DISCONTINUED", NA)),
  # DCSREAS must be NA for every completer
  all(is.na(adsl$DCSREAS[!is.na(adsl$EOSSTT) & adsl$EOSSTT == "COMPLETED"])),
  # No flag variable may contain "N"
  !any(vapply(
    adsl[c("SAFFL", "ITTFL", "PPROTFL", "DTHFL")],
    function(x) any(x == "N", na.rm = TRUE),
    logical(1)
  ))
)

# Variable labels. xportr_*() is the submission-context route but requires a
# metacore spec, which was not supplied for this run — so labels for the
# derived variables are applied directly instead.
adsl_labels <- c(
  TRTSDT   = "Date of First Study Treatment",
  TRTEDT   = "Date of Last Study Treatment",
  TRTSDTM  = "Datetime of First Study Treatment",
  TRTEDTM  = "Datetime of Last Study Treatment",
  TRTSTMF  = "Time Imputation Flag for TRTSDTM",
  TRTETMF  = "Time Imputation Flag for TRTEDTM",
  TRTDURD  = "Total Treatment Duration (Days)",
  TRT01P   = "Planned Treatment for Period 01",
  TRT01PN  = "Planned Treatment for Period 01 (N)",
  TRT01A   = "Actual Treatment for Period 01",
  TRT01AN  = "Actual Treatment for Period 01 (N)",
  RANDDT   = "Date of Randomization",
  RANDDY   = "Study Day of Randomization",
  EOSSTT   = "End of Study Status",
  EOSDT    = "End of Study Date",
  DCSREAS  = "Reason for Discontinuation from Study",
  DCSREASP = "Reason Specify for Discont from Study",
  DTHDT    = "Date of Death",
  AGEGR1   = "Pooled Age Group 1",
  AGEGR1N  = "Pooled Age Group 1 (N)",
  HEIGHTBL = "Baseline Height (cm)",
  WEIGHTBL = "Baseline Weight (kg)",
  BMIBL    = "Baseline BMI (kg/m^2)",
  SAFFL    = "Safety Population Flag",
  ITTFL    = "Intent-To-Treat Population Flag",
  PPROTFL  = "Per-Protocol Population Flag"
)
for (v in intersect(names(adsl_labels), names(adsl))) {
  attr(adsl[[v]], "label") <- adsl_labels[[v]]
}
attr(adsl, "label") <- "Subject-Level Analysis Dataset"

# For a submission context, replace the block above with:
# adsl <- adsl |>
#   xportr_label(metacore_obj, domain = "ADSL") |>
#   xportr_type(metacore_obj, domain = "ADSL") |>
#   xportr_length(metacore_obj, domain = "ADSL") |>
#   xportr_order(metacore_obj, domain = "ADSL")
# xportr_write(adsl, "adsl.xpt", label = "Subject-Level Analysis Dataset")

message(sprintf(
  "ADSL derived: %d subjects, %d variables.", nrow(adsl), ncol(adsl)
))
