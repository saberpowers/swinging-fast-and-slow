# Swinging, Fast and Slow: Untangling intention and timing error from bat speed and swing length

Scott Powers and Ron Yurko
2024 Cascadia Symposium on Statistics in Sports
[slides](https://drive.google.com/file/d/12FuQxjcDzmxETV-RaKdjPA1gbP1BLUaR),
[video](https://www.youtube.com/watch?v=rsJmNvDaHJc&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

2024 Saberseminar
[slides](https://drive.google.com/file/d/1tmKdebUSCWPXwE2q09yf5HCdInPrOIsZ),
[video](https://www.youtube.com/watch?v=rQ1BsjQEWKs&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

In May 2024, Major League Baseball (MLB) released a novel dataset of pitch-level swing tracking metrics. The data are limited to bat speed and swing length at contact point, with a teaser of more to come.

Interpretation of bat speed and swing length is complicated by the measurement point. For the exact same swing mechanics, if the batter swings later (or the ball moves faster), the contact point occurs upstream in the swing path. This results in a shorter swing length measurement and a slower bat speed measurement (because the bat accelerates up to and beyond intended contact). If the batter swings earlier (or the ball moves slower), the contact point occurs downstream in the swing path, causing a different measurement bias. Bat speed and swing length, therefore, reflect not only the swing but also the outcome of the swing.

This complication has not discouraged data scientists from making claims about what constitutes a good swing and how batters should try to swing. In the present work, we take a more cautious approach, seeking to understand and classify the sources of swing-to-swing variability in bat speed and swing length.

## Installing the swingfastslow R package

```R
devtools::install_github(
  repo = "saberpowers/swinging-fast-and-slow",
  subdir = "package/swingfastslow"
)
```

## Folder Structure

```
├── figures                             # results in .pdf and .png form
├── package                             # R package
│   └── swingfastslow
├── reports                             # LaTeX code for papers and slides
│   ├── arxiv
│   ├── cassis
│   └── saberseminar
├── scripts                             # R code for reproducing results
|   ├── sandbox
|   ├── download_data.R
|   ├── estimate_models.R
|   └── generate_results.R
└── tables                              # results in .tex form
```
