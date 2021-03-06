Fitting diagnostics
-------------------

The tool provides three ways to evaluate the goodness of the fit
(besides the comparison of the growth curve against the data). According
to the hypotheses of linear regression, the model residuals should be a
series of independent normal variable with mean zero and constant
variance. Because the data was used to fit the data, the mean of the
residuals should always be zero. If this is no the case, it is very
likely that the fitting algorithm failed to converge.

The module provides three ways to evaluate the hypothesis of normality
of the residuals. The `Residuals plot` shows the residual as a faction
of the storage time. The cloud of points should be randomly distributed
around zero (horizontal, dashed line) without any visible trend. The
plot includes a trend line (solid blue line) to help visualizing
deviations with respect to the theoretical result. Any deviation will
indicate that some part of the growth curve is not properly described by
the model (e.g. the data points had a lag phase but *λ* was fixed to
zero).

The second one is a `Histogram of the residuals`. This plot overlaps the
probability density of a normal distribution with the same mean and
standard deviation as the residuals. Strong deviations between the
histogram and this line likely indicates model deficiencies.

The final tool to evaluate the goodness of the fit is a Shapiro-Wilk
test of the residuals. According to this test, p-values lower than the
value of *α* (0.05) indicate that the residuals significantly deviate
from normality. Nonetheless, the results of this test should be analyzed
together with the results of the other 2 tools. A p-value of the
Shapiro-Wilk test &gt; 0.05 does not indicate a good model fit when
there is a clear trend in the residuals (e.g. this test does not check
for autocorrelation).
