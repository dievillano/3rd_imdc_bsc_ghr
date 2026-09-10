# IMDC 2026 forecast report

Suggested location inside `sprint2026`:

```text
reports/imdc2026/
  _quarto.yml
  index.qmd
```

The report finds the Git repository root automatically, so the RDS paths work even when the Quarto project is nested under `reports/imdc2026/`.

Render locally:

```bash
cd reports/imdc2026
quarto render
```

Preview locally:

```bash
quarto preview
```

For a GitHub Pages deployment, one convenient option is:

```bash
quarto publish gh-pages
```

Run that from the Quarto project directory. Commit the report source before publishing if you want the rendered page to point to the latest source.

Required R packages: `dplyr`, `purrr`, `ggplot2`, `forcats`, `scales`, `fs`, `gt`. `geofacet` is optional; if available, the state plots use the Brazil state grid, otherwise they fall back to `facet_wrap()`.
