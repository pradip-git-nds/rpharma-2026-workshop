# R/Pharma 2026 Workshop — Agent Instructions

This repository holds four Claude Code skills for clinical trial design and
ADaM dataset derivation. Use them when a task matches.

## Skills

| Skill | Location | Invoke when the task involves |
|---|---|---|
| admiral (parent) | `skills/admiral/SKILL.md` | Shared admiral conventions — library setup, `|>`/`exprs()` style, date rules, `"Y"`/`NA` flags, `# REVIEW:` annotations, `stopifnot()` QC |
| admiral-adsl | `skills/admiral/admiral-adsl/SKILL.md` | "derive ADSL", subject-level dataset, treatment dates, population flags |
| admiral-adae | `skills/admiral/admiral-adae/SKILL.md` | "derive ADAE", adverse events, TEAE / treatment-emergent flag |
| admiral-bds | `skills/admiral/admiral-bds/SKILL.md` | "derive ADVS / ADLB", BDS findings, baseline, change from baseline, visit windowing |
| group-sequential-design | `skills/group-sequential-design/SKILL.md` | "design a Phase 3 trial", group sequential design, alpha spending, interim analysis planning, event/enrollment prediction |

When deriving any ADaM dataset, read `skills/admiral/SKILL.md` first for the
shared conventions, then the dataset-specific child skill. ADSL must be derived
before ADAE or BDS.

## Environment

**Rscript path** — set this to your local Rscript before running the
group-sequential-design skill (it writes and executes R):

    Rscript path: /usr/local/bin/Rscript

Windows example: `C:\Program Files\R\R-4.4.1\bin\Rscript.exe`
macOS/Linux example: `/usr/local/bin/Rscript`

## Notes

- These skills produce **drafting aids, not validated deliverables**. Always QC
  the generated code and outputs against your own process before any regulatory,
  clinical, or GxP-regulated use.
- The skills bundle R and Python scripts that execute in your environment.
  Review a skill's contents before running it.
