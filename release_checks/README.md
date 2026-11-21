# Checks to run before a release

This folder is not part of the package. It contains a thorough comparison of the
outputs of the full IsoriX workflow between two versions of IsoriX, to be run
before each release (it is listed in `release_bullets()` in `IsoriX/R/zzz.R`).

## Why?

The unit tests (including the plot snapshot tests run by `devtools::test()`) use
small toy objects so as to be fast and stable. They check each function in
isolation, but they do not show how all changes since the previous release, put
together, affect what users get at the end of a real workflow (e.g. a change in
a model fit that slightly changes all isoscapes and assignments).

This comparison runs the same full workflow (preparing the sources, fitting the
isoscape, calibrating, assigning, building monthly isoscapes...) with the
previous and the candidate versions of IsoriX, and compares every plot and some
key numbers.

Both versions run on the same computer with exactly the same dependencies, so
any difference comes from IsoriX itself.

## How?

From the root of the repository:

```bash
Rscript release_checks/compare_versions.R
```

By default, the version currently on CRAN is compared to the current state of the
`IsoriX` folder. You can instead compare to any git reference (a tag, a branch
or a commit), e.g.:

```bash
Rscript release_checks/compare_versions.R v0.9.4
```

It takes a few minutes and requires the package **magick**.

The results are written to `release_checks/output/<date-time>/` (ignored by git).
Open `report.html` in a browser: it shows

- every plot produced by both versions side by side, with an image highlighting
  the pixels that differ (plots that differ are listed first);
- the status and duration of each step of the workflow in each version (a step
  that fails in one version, e.g. because an argument changed, is reported but
  does not stop the comparison);
- key numbers (fitted parameters, summaries of the isoscapes and assignments) for
  both versions.

Differences are not necessarily problems: check that each of them is expected
given the changes listed in `NEWS.md`.

## Files

- `compare_versions.R`: installs both versions in temporary libraries, runs the
  workflow with each, compares the outputs and writes the report;
- `workflow.R`: the workflow itself (run once per version, in a fresh R session).
  When adding new features to IsoriX, consider adding them to this workflow too,
  as long as the previous version also supports them (otherwise, the step will
  simply be reported as failing for the previous version).
