Growth prediction under dynamic conditions
------------------------------------------

Method
------

The prediction is calculated by approximating the solution of the
differential equation using numerical methods. Namely, the `ode`
function from the deSolve package is used with default control
parameters.

Predicted growth
----------------

This plot shows the predicted microbial count under dynamic conditions.
The curve may deviate from the sigmoidal curve due to the variation of
the environmental factors. Also, the curve may have a lag or stationary
phase due to the inhibition by the environmental fators, not because of
the usual interpretation under static conditions.

Variation of the gamma factors
------------------------------

This plot shows the predicted variation of the gamma factors throughout
storage. In the gamma approach, each environmental factor acts as a
correction factor between 0 and 1 that reduces the growth rate. Hence,
this plot illustrates which is the most limiting factor for each time
point, and how large is its impact.

Note that this plot only shows the effect of the gamma factors. The lag
phase can also be a very relevant limiting factor.
