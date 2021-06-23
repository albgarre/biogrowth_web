Method
------

The model is fitted to the data by non-linear regression. The method
uses an iterative algorithm, so it needs initial values for every
parameter to be estimated from the data. These values are defined in the
`Model parameters` box. It is common for this type of model to show
convergence issues. For that reason, the tool allows to fit any model
parameter to known values. Be mindful when doing so, as it may result in
unrealistic inference (see doi: 10.1111/risa.13386)

Note that this modeling approach is designed for data gathered under
isothermal conditions. In case of dynamic fits, please use the
appropiate module.

Output
------

The module generates a plot comparing the fitted model against the
observations. The data was used to fit the model, so the points should
be ‘reasonably’ close to the model. If they are not, it is quite likely
that the starting values of the parameters were not appropriate.
Alternatively, it is possible some parameter(s) was fixed to unrealistic
values.

It also generates a table with the estimated values and standard errors
of the model parameters, as well as 95% confidence intervals. This table
also includes several error indexes:

-   Mean Error: $ME = \\frac{\\sum{e}}{n}$
-   Mean Squared Error: $MSE = \\frac{\\sum{e^2}}{n}$
-   Root Mean Squared Error: $RMSE = \\sqrt{ \\frac{\\sum{e^2}}{n}}$
-   Standard Error of the Residuals:
    $SER = \\sqrt{ \\frac{\\sum{e^2}}{n-p}}$
-   Bias factor:
-   Accuracy factor:
-   And number of degrees of freedom of the model, estimated as
    *d**f* = *n* − *p*.
