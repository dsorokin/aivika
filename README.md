Aivika -- a multi-paradigm simulation library

Aivika is a multi-paradigm simulation library which has 
the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths via the recursive do-notation);

* has a basic support of the event-driven paradigm of 
  the Discrete Event Simulation (DES);

* has a basic support of the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with limited resources;

* supports the activity-oriented paradigm of DES;

* supports the basic constructs for the agent-based modeling;

* allows creating combined discrete-continuous models;

* the arrays of simulation variables are supported by design 
  (this is mostly a feature of Haskell itself);

* supports the Monte-Carlo simulation;

* the simulation model can depend on external parameters;

* uses extensively the signals to notify the model about changing 
  the reference and variable values;

* allows gathering statistics in time points;

* hides the technical details in high-level simulation monads
  (two of them support the recursive do-notation).

Aivika itself is a light-weight engine with minimal dependencies. 
However, it has additional packages [Aivika Experiment] [1] and 
[Aivika Experiment Chart] [2] that offer the following features:

* automating the simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary by statistical data;

* parallel execution of the Monte-Carlo simulation;

* have an extensible architecture.

All three libraries were tested on Linux, Windows and OS X.

Please refer to the PDF document [Introduction to 
Aivika Simulation Library] [3] for more details 
(the document is not finished yet).

[1]: http://hackage.haskell.org/package/aivika-experiment  "Aivika Experiment"
[2]: http://hackage.haskell.org/package/aivika-experiment-chart  "Aivika Experiment Chart"
[3]: https://github.com/dsorokin/aivika/blob/master/doc/aivika.pdf  "Introduction to Aivika Simulation Library"
