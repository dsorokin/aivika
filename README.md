A multi-method simulation library

**Aivika** is a discrete event simulation (DES) framework with support of activity-oriented, 
event-oriented and process-oriented paradigms. It supports resource preemption and other 
improved simulation techniques. There is also a partial support of system dynamics and
agent-based modelling. All the complexity is hidden under easy-to-use computations.

The represented [aivika](http://hackage.haskell.org/package/aivika) package is a basic 
simulation library optimized for sequential execution.
There are also packages for nested and parallel distributed simulation.
All they are licensed under BSD3.

The library has the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths via the recursive do-notation);

* supports the event-driven paradigm of DES as a basis
  for implementing other paradigms;

* supports extensively the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with the resources based on specified queue strategies 
  (FCFS/FIFO, LCFS/LIFO, SIRO, static priorities and so on);

* allows customizing the unbounded and bounded queues based on strategies too;

* supports the resource preemption;

* allows defining a queue network based on streams of data (transacts);

* allows using a GPSS-like DSL with help of the additional library;

* allows simulating circuits with recursive links and delays;

* supports the activity-oriented paradigm of DES;

* supports basic constructs for the agent-based modeling such as 
  agents, states, timeout and timer handlers;

* allows creating combined discrete-continuous models as all parts
  of the library are well integrated and this is reflected directly in
  the type system;

* the arrays of simulation variables are inherently supported;

* supports the Monte-Carlo simulation;

* the simulation model can depend on external parameters;

* uses extensively signals for notification;

* allows gathering statistics in time points;

* hides technical details in high-level simulation computations
  (monads, streams and arrows).

The simulation engine itself has minimal dependencies. 
However, there are additional packages 
[aivika-experiment](http://hackage.haskell.org/package/aivika-experiment) 
and [aivika-experiment-chart](http://hackage.haskell.org/package/aivika-experiment-chart) 
that offer the following features:

* automating simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary of statistical data;

* parallel execution of the Monte-Carlo simulation;

* has an extensible architecture.

The charting package has two interchangeable back-ends 
[aivika-experiment-cairo](http://hackage.haskell.org/package/aivika-experiment-cairo) 
and [aivika-experiment-diagrams](http://hackage.haskell.org/package/aivika-experiment-diagrams),
where the former uses Cairo and it creates small PNG images, while the latter
creates more detailed SVG files and it can be used on Windows.

There are also additional packages that allow saving the results of simulation 
in SQL databases. Then the results can be accessed from other software applications.
For example, it can be useful when creating flight simulators in other programming languages
such as C\# or Java.

Moreover, the method was generalized in package 
[aivika-transformers](http://hackage.haskell.org/package/aivika-transformers) and applied to 
nested simulation, package [aivika-branches](http://hackage.haskell.org/package/aivika-branches), 
and parallel distributed simulation, 
package [aivika-distributed](http://hackage.haskell.org/package/aivika-distributed). 

The libraries were tested on Linux, Windows and OS X.

The PDF documentation, installation instructions and a more full information about Aivika 
are available on the the [AivikaSoft](http://www.aivikasoft.com) website.

P.S. Aivika is actually a genuine female Mari name which is pronounced with stress on the last syllable.
