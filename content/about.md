---
date: "2016-05-05T21:48:51-07:00"
title: About
---

```{r include=FALSE}
source("nfl_expected_wins.R", local = knitr::knit_global())
# or sys.source("your-script.R", envir = knitr::knit_global())
```

This is a "hello world" example website for the [**blogdown**](https://github.com/rstudio/blogdown) package. The theme was forked from [@jrutheiser/hugo-lithium-theme](https://github.com/jrutheiser/hugo-lithium-theme) and modified by [Yihui Xie](https://github.com/yihui/hugo-lithium).

asotnehu asnoteh uasontheu asotneuh asoeuh  asontehu asontehu asontheu asoeuh

```{r fig.cap="Chart"}
  training_years <- c(2015, 2019)
  all_years <- c(2002, 2020)

  data <- load_data_and_build_model(training_years, all_years)
  plot <- plot_wins(data, all_years[1], all_years[2])
```
