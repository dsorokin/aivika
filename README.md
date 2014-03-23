Aivika -- a multi-paradigm simulation library for 
the Haskell programming language with a strong emphasis on
the Discrete Event Simulation (DES) in the first order and
System Dynamics (SD) in the second one.

It is licensed under BSD3.

[Aivika] [1] is a multi-paradigm simulation library which has 
the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths via the recursive do-notation);

* supports the event-driven paradigm of DES as a basic core
  for implementing other paradigms;

* supports extensively the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with the resources (you can define your own behaviour
  or use the predefined queue strategies);

* allows customizing the queues (you can define your own behaviour
  or use the predefined queue strategies);

* allows defining an infinite stream of data based on the
  process-oriented computation, where we can define a complex enough
  behaviour just in a few lines of code;

* allows defining processors (actually, the Haskell arrows) that
  operate on the infinite streams of data, because of which some models
  can remind of their high-level graphical representation on the
  diagram used by visual simulation software tools;

* supports the activity-oriented paradigm of DES;

* supports the basic constructs for the agent-based modeling;

* allows creating combined discrete-continuous models as all parts
  of the library are very well integrated and this is reflected
  directly in the type system;

* the arrays of simulation variables are inherently supported 
  (this is mostly a feature of Haskell itself);

* supports the Monte-Carlo simulation;

* the simulation model can depend on external parameters;

* uses extensively the signals to notify the model about changing 
  the reference and variable values;

* allows gathering statistics in time points;

* hides the technical details in high-level simulation monads
  and even one arrow (some of these monads support the recursive
  do-notation).

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
Aivika Simulation Library] [4] for more details, although it is
outdated and contains a very basic description only. The most
powerful features of Aivika are not yet described in this PDF document.

[1]: http://hackage.haskell.org/package/aivika  "Aivika"
[2]: http://hackage.haskell.org/package/aivika-experiment  "Aivika Experiment"
[3]: http://hackage.haskell.org/package/aivika-experiment-chart  "Aivika Experiment Chart"
[4]: https://github.com/dsorokin/aivika/blob/master/doc/aivika.pdf  "An Introduction to Aivika Simulation Library"

P.S. Aivika is actually a genuine female Mari name which is pronounced 
with stress on the last syllable as in French, but the Russians usually 
pronounce it wrong :)
