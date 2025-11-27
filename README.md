# Swinging, Fast and Slow: Interpreting variation in baseball swing tracking metrics

Scott Powers and Ronald Yurko
[preprint](https://arxiv.org/abs/2507.01238)

2024 Cascadia Symposium on Statistics in Sports (Vancouver)
[slides](https://drive.google.com/file/d/12FuQxjcDzmxETV-RaKdjPA1gbP1BLUaR),
[video](https://www.youtube.com/watch?v=rsJmNvDaHJc&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

2024 Saberseminar (Chicago)
[slides](https://drive.google.com/file/d/1tmKdebUSCWPXwE2q09yf5HCdInPrOIsZ),
[video](https://www.youtube.com/watch?v=rQ1BsjQEWKs&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

In 2024, Major League Baseball released new bat tracking data, reporting swing-by-swing bat speed and swing length measured at the point of contact. While exciting, the data present challenges for their interpretation. The timing of the batter's swing relative to the pitch determines the point of measurement relative to the full swing path. The relationship between swing metrics and swing outcomes is confounded by the batter's pitch recognition. We introduce a framework for interpreting bat tracking data in which we first estimate the batter's intention conditional on ball-strike count and pitch location using a Bayesian hierarchical skew-normal model with random intercept and random slopes for batter. This yields batter-specific effects of count on swing metrics, which we leverage via instrumental variables regression to estimate causal effects of bat speed and swing length on contact and power outcomes. Finally, we valuate the tradeoff between contact and power due to bat speed by modeling a plate appearance as a Markov chain. We conclude that batters can reduce their strikeout rate by reducing bat speed as strikes increase, but the tradeoff in reduced power approximately counteracts the benefit to the average batter.

## Installing the swingfastslow R package

```R
devtools::install_github(
  repo = "saberpowers/swinging-fast-and-slow",
  subdir = "package/swingfastslow"
)
```

## Restoring the reproducible environment

If your goal is to reproduce the results in the paper, start by using renv to install the same package versions we used. The swingfastslow package exists in this repository, so you want to install it before using renv to restore the rest of the packages.

```R
devtools::install("packages/swingfastslow")
renv::restore()
```

## Downloading our input data and models

Our input data and models are available at https://doi.org/10.25611/7QXV-8612. To use them, download and unzip the file from the DOI, and then copy over the `data` and `models` folders into your local copy of the repository.

## Folder structure

```
├── articles                            # LaTeX code for papers and slides
│   ├── arxiv
│   ├── cassis
│   ├── saberseminar
│   └── tas
├── data                                # not tracked by git; see above
├── models                              # not tracked by git; see above
├── output                              # models, figures, tables go here
├── package                             # R package
│   └── swingfastslow
├── renv                                # reproducible environment files
└── scripts                             # R code for reproducing results
    ├── download_data.R
    ├── estimate_models.R
    └── generate_results.R
```
