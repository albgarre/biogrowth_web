Growth prediction under static conditions with parameter uncertainty
--------------------------------------------------------------------

Stochastic growth curve
-----------------------

This plot shows the prediction intervals according to the variation in
the model parameters. The solid line represents the median of the
simulations. The two dashed areas represent the area between the 10th
and 90th, and 5th and 95th intervals. The intervals are estimated by
forward propagation using Monte Carlo simulations. It is advised to
repeat the calculations for with the same number of simulations. If the
results vary, the number of simulations should be increased.

Note that the simulations are valid only for static conditions. Also be
mindful about what the variation of the parameters represent.

Time to reach a microbial count
-------------------------------

This plot shows an estimation of the treatment time needed to reach some
microbial count. Because simulations are stochastic, the result is a
probability distribution according to the variation in the model
parameters. The distribution is estimated using forward propagation with
Monte Carlo simulations. You should increase the number of iterations
until the histogram is ‘reasonably’ smooth.

The read line shows the median of the histogram, and the grey lines the
90th and and 10th quantiles. Note that the simulations are valid only
for static conditions. Also be mindful about what the variation of the
parameters represent.
