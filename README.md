# NFL Predicted Wins Model

An experiment to build a predictive model in R that recreates an article from Advanced Football Analytics.

## Description

An article from 2007 outlined an approach for calculating expected wins based on offensive and defensive efficiency metrics. This project is an attempt to implement the prediction model in R.

- [Part 1](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-part-1.html)
- [Part 2](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-2.html)
- [Part 3](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-3.html)
- [Part 4](http://archive.advancedfootballanalytics.com/2007/07/what-makes-teams-win-4.html)

## Chart

![Predicted vs Actual Wins](https://topfunky.com/clients/topfunky/github/wins-20201110.png)

## Output

The attributes of the multiple linear regression model.

```
Call:
lm(formula = W ~ ZDefPassYardsPerAttempt + ZDefRunYardsPerAttempt +
    ZDefPenYardsPerPlay + ZDefIntRate + ZDefFumbleRate + ZOffPassYardsPerAttempt +
    ZOffRunYardsPerAttempt + ZOffPenYardsPerPlay + ZOffIntRate +
    ZOffFumbleRate, data = data)

Residuals:
    Min      1Q  Median      3Q     Max
-4.2009 -1.1800 -0.0549  1.1854  4.5311

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)
(Intercept)              7.98264    0.06918 115.393  < 2e-16 ***
ZDefPassYardsPerAttempt -1.10790    0.07856 -14.103  < 2e-16 ***
ZDefRunYardsPerAttempt  -0.31676    0.07521  -4.211 2.95e-05 ***
ZDefPenYardsPerPlay      0.34735    0.07111   4.885 1.35e-06 ***
ZDefIntRate              0.55209    0.07439   7.422 4.27e-13 ***
ZDefFumbleRate           0.30469    0.07077   4.305 1.97e-05 ***
ZOffPassYardsPerAttempt  1.28442    0.07883  16.293  < 2e-16 ***
ZOffRunYardsPerAttempt   0.36407    0.07227   5.038 6.36e-07 ***
ZOffPenYardsPerPlay     -0.39606    0.07135  -5.551 4.37e-08 ***
ZOffIntRate             -0.65387    0.07889  -8.288 8.43e-16 ***
ZOffFumbleRate          -0.47119    0.07353  -6.408 3.11e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.66 on 565 degrees of freedom
Multiple R-squared:  0.7138,	Adjusted R-squared:  0.7087
F-statistic: 140.9 on 10 and 565 DF,  p-value: < 2.2e-16
```

## Reference

Data is from [Pro Football Reference](https://www.pro-football-reference.com/).