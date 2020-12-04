# NFL Predicted Wins Model

An experiment to build a predictive model in R that recreates an article from Advanced Football Analytics.

## Description

An article from 2007 outlined an approach for calculating expected wins based on offensive and defensive efficiency metrics. This project is an attempt to implement the prediction model in R.

- [Part 1](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-part-1.html)
- [Part 2](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-2.html)
- [Part 3](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-3.html)
- [Part 4](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-4.html)

## Chart

![Predicted vs Actual Wins](out/wins.webp)

## Output

Win correlation coefficients from the original Advanced Football Analytics model, this model trained on the 2002-2006 seasons, and this model trained on 2015-2019 seasons.

| Stat        | AFA Cor | GG Cor 2002 | GG Cor 2015 |
| ----------- | ------- | ----------- | ----------- |
| Off Pass    | 0.61    | 0.59        | 0.53        |
| Def Pass    | -0.47   | -0.45       | -0.49       |
| Off Fumble  | -0.46   | -0.38       | -0.41       |
| Off Int     | -0.45   | -0.45       | -0.48       |
| Def FFumble | 0.41    | 0.30        | 0.40        |
| Def Int     | 0.39    | 0.39        | 0.42        |
| Off Pen     | -0.37   | -0.27       | -0.10       |
| Off Run     | 0.18    | 0.19        | 0.13        |
| Def Run     | -0.04   | -0.04       | -0.04       |

The attributes of the multiple linear regression model.

```
Call:
lm(formula = W ~ ZDefPassYardsPerAttempt + ZDefRunYardsPerAttempt +
    ZDefIntRate + ZDefFumbleRate + ZOffPassYardsPerAttempt +
    ZOffRunYardsPerAttempt + ZOffPenYardsPerPlay + ZOffIntRate +
    ZOffFumbleRate, data = data)

Residuals:
    Min      1Q  Median      3Q     Max
-3.5374 -1.1822 -0.0234  1.0455  3.5093

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)
(Intercept)               7.9688     0.1284  62.047  < 2e-16 ***
ZDefPassYardsPerAttempt  -0.9831     0.1457  -6.747 3.08e-10 ***
ZDefRunYardsPerAttempt   -0.3046     0.1435  -2.123 0.035403 *
ZDefIntRate               0.6158     0.1460   4.219 4.23e-05 ***
ZDefFumbleRate            0.5179     0.1387   3.733 0.000268 ***
ZOffPassYardsPerAttempt   1.0497     0.1436   7.312 1.47e-11 ***
ZOffRunYardsPerAttempt    0.1631     0.1351   1.207 0.229318
ZOffPenYardsPerPlay      -0.1637     0.1337  -1.225 0.222559
ZOffIntRate              -0.8797     0.1459  -6.031 1.22e-08 ***
ZOffFumbleRate           -0.7550     0.1349  -5.595 1.02e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.625 on 150 degrees of freedom
Multiple R-squared:  0.7366,	Adjusted R-squared:  0.7208
F-statistic: 46.61 on 9 and 150 DF,  p-value: < 2.2e-16
```

## Reference

Data from:

- [Pro Football Reference](https://www.pro-football-reference.com/)
- [Football Outsiders](https://www.footballoutsiders.com)
