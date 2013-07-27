Aivika -- a multi-paradigm simulation library for 
the Haskell programming language and licensed under BSD3

[Aivika] [1] is a multi-paradigm simulation library which has 
the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths via the recursive do-notation);

* has a basic support of the event-driven paradigm of 
  the Discrete Event Simulation (DES);

* has a basic support of the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with limited resources (you can define your own behaviour
  or use the predefined queue strategies);

* allows customizing the queues (you can define your own behaviour
  or use the predefined queue strategies);

* supports the activity-oriented paradigm of DES;

* supports the basic constructs for the agent-based modeling;

* allows creating combined discrete-continuous models;

* the arrays of simulation variables are inherently supported 
  (this is mostly a feature of Haskell itself);

* supports the Monte-Carlo simulation;

* the simulation model can depend on external parameters;

* uses extensively the signals to notify the model about changing 
  the reference and variable values;

* allows gathering statistics in time points;

* hides the technical details in high-level simulation monads
  (two of them support the recursive do-notation).

Aivika itself is a light-weight engine with minimal dependencies. 
However, it has additional packages [Aivika Experiment] [2] and 
[Aivika Experiment Chart] [3] that offer the following features:

* automating the simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary of statistical data;

* parallel execution of the Monte-Carlo simulation;

* have an extensible architecture.

All three libraries were tested on Linux, Windows and OS X.

Please read the PDF document [An Introduction to 
Aivika Simulation Library] [4] for more details. 
This document is included in the distributive of Aivika 
but you can usually find a more recent version by the link provided.

[1]: http://hackage.haskell.org/package/aivika  "Aivika"
[2]: http://hackage.haskell.org/package/aivika-experiment  "Aivika Experiment"
[3]: http://hackage.haskell.org/package/aivika-experiment-chart  "Aivika Experiment Chart"
[4]: https://github.com/dsorokin/aivika/blob/master/doc/aivika.pdf  "An Introduction to Aivika Simulation Library"
