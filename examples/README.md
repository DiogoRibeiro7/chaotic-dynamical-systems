# Examples

Short, runnable "notebook-style" walkthroughs of the package.
They sit outside `vignettes/` on purpose: they aren't built into the package
docs, so they're free to evolve and stay light.

| File | What it covers |
|------|----------------|
| [`walkthrough.Rmd`](walkthrough.Rmd) | End-to-end tour: simulate → block maxima/GEV → POT/GPD → extremal index → clusters. ~5 min read. |

## Running locally

```r
# from the repository root
rmarkdown::render("examples/walkthrough.Rmd")
```

This produces `examples/walkthrough.html` next to the source.

You can also open the file in RStudio and use **Knit**, or run the chunks
interactively chunk-by-chunk.

## Why not a vignette?

Vignettes are great for in-depth, teaching-oriented material — and there are
already six of them under [`vignettes/`](../vignettes/). The files here are
deliberately the *opposite*: tight, narrative, high signal-per-line. Think of
them as the "show this to a colleague" tour rather than the textbook chapter.

For the full vignette set, see the package site:
<https://diogoribeiro7.github.io/chaotic-dynamical-systems/articles/>
