# Checks to run before a release

This folder is not part of the package. It contains checks to run before each
release (they are listed in `release_bullets()` in `IsoriX/R/zzz.R`):

- a thorough comparison of the outputs of the full IsoriX workflow between two
  versions of IsoriX (`compare_versions.R`, see below);
- a review of all the links of the bookdown and of the documentation of the
  package (`list_links.R`, see the last section).

## Comparing versions: why?

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

## Comparing versions: how?

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
- `list_links.R`: lists and checks all links (see below);
- `workflow.R`: the workflow itself (run once per version, in a fresh R session).
  When adding new features to IsoriX, consider adding them to this workflow too,
  as long as the previous version also supports them (otherwise, the step will
  simply be reported as failing for the previous version).

## Reviewing the links

From the root of the repository:

```bash
Rscript release_checks/list_links.R
```

This lists every unique link found in the bookdown (`bookdown/*.Rmd` and
`*.yml`) and in the documentation of the package (`IsoriX/R/*.R`,
`DESCRIPTION`, `CITATION`, `README.md`), together with all the places where
each link is used (file:line). It then checks each link (this takes less than a
minute and requires the package **curl**). Use `--no-check` to only list them.

Automatic checks (e.g. by CRAN) only detect broken links, but links can also be
outdated while still working: they then usually work through a redirection
(e.g. a renamed GitHub repository, or a website that moved). The report
(`release_checks/output/links-<date-time>/report.html`) therefore classifies
each link as:

- **broken**: the link does not work (error, or HTTP status >= 400 other than
  the following ones);
- **access denied**: the website refuses requests from scripts (HTTP status
  401, 403 or 429), which is common for publishers and some large websites
  while the link works fine in a browser: open it to check;
- **redirected**: the link works but leads elsewhere (the final address is
  shown, as for any link leading elsewhere): check whether the link should be
  updated;
- **redirected (minor)**: only `http` vs `https`, `www.`, a default port or a
  trailing slash differ (usually harmless);
- **ok**: the link works directly (it may still be worth checking that it
  points to the most relevant page).
