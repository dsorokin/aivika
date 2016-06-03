**Aivika** is a multi-method simulation library focused on 
discrete event simulation (DES) with partial support of 
system dynamics and agent-based modeling.

It is licensed under BSD3.

[Aivika] [1] has the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths via the recursive do-notation);

* supports the event-driven paradigm of DES as a basic core
  for implementing other paradigms;

* supports extensively the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with the resources based on specified queue strategies 
  (FCFS/FIFO, LCFS/LIFO, SIRO, static priorities and so on);

* allows customizing the infinite and finite queues based on strategies too;

* supports the resource preemption;

* allows defining a queue network based on streams of data (transacts) 
  and their processors;

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

Aivika itself is a light-weight engine with minimal dependencies. 
However, it has additional packages [aivika-experiment] [2] and 
[aivika-experiment-chart] [3] that offer the following features:

* automating simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary of statistical data;

* parallel execution of the Monte-Carlo simulation;

* has an extensible architecture.

The charting package has two interchangeable back-ends 
[aivika-experiment-cairo] [4] and [aivika-experiment-diagrams] [5],
where the former uses Cairo and it is more preferable.

The PDF documentation and installation instructions are 
available on the [Aivika Wiki] [6] website. 

Moreover, the method was generalized in package [aivika-transformers] [7] and applied to 
nested simulation, package [aivika-branches] [8], and parallel distributed simulation,
package [aivika-distributed] [9]. 

The libraries were tested on Linux, Windows and OS X.

A more full information about Aivika is available on the [AivikaSoft] [10] website.

[1]: http://hackage.haskell.org/package/aivika  "aivika"
[2]: http://hackage.haskell.org/package/aivika-experiment  "aivika-experiment"
[3]: http://hackage.haskell.org/package/aivika-experiment-chart  "aivika-experiment-chart"
[4]: http://hackage.haskell.org/package/aivika-experiment-cairo  "aivika-experiment-cairo"
[5]: http://hackage.haskell.org/package/aivika-experiment-diagrams  "aivika-experiment-diagrams"
[6]: https://github.com/dsorokin/aivika/wiki  "Aivika Wiki"
[7]: http://hackage.haskell.org/package/aivika-transformers "aivika-transformers"
[8]: http://hackage.haskell.org/package/aivika-branches "aivika-branches"
[9]: http://hackage.haskell.org/package/aivika-distributed "aivika-distributed"
[10]: http://www.aivikasoft.com/en/products/aivika.html "AivikaSoft"

P.S. Aivika is actually a genuine female Mari name which is pronounced 
with stress on the last syllable.
