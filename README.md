**Aivika** -- a multi-paradigm simulation library for 
the Haskell programming language with a strong emphasis on
Discrete Event Simulation (DES) and System Dynamics (SD).

It is licensed under BSD3.

### Simulation Library

[Aivika] [1] is a multi-paradigm simulation library which has 
the following features:

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

* allows defining a queue network based on infinite streams of data
  and their processors, where we can define a complex enough
  behaviour just in a few lines of code;

* allows simulating circuits with recursive links and delays;

* supports the activity-oriented paradigm of DES;

* supports the basic constructs for the agent-based modeling;

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
However, it has additional packages [Aivika Experiment] [2] and 
[Aivika Experiment Chart] [3] that offer the following features:

* automating simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary of statistical data;

* parallel execution of the Monte-Carlo simulation;

* has an extensible architecture.

The charting library has two interchangeable back-ends:
[Aivika Experiment Cairo] [4] and [Aivika Experiment Diagrams] [5].

All libraries were tested on Linux, Windows and OS X.

The PDF documentation and installation instructions are available on the [Aivika Wiki] [6] website.

### Consulting

I am ready to consider your offer related to developing new modules or customizing Aivika, or related to developing simulation models. Please contact me at <david.sorokin@gmail.com>.

### Aivika for .NET

Regarding the simulation library, I also released [Aivika for .NET] [7] of similar capabilities but written in F#. It literally means that you can create simulation models in F# and integrate them with the code written in C#. Also it means that you can edit your models in Visual Studio on Microsoft Windows, in Xamarin Studio on OS X and in MonoDevelop on Linux. 

Please let me know too if you have an interest in .NET version of the Aivika library.

[1]: http://hackage.haskell.org/package/aivika  "Aivika"
[2]: http://hackage.haskell.org/package/aivika-experiment  "Aivika Experiment"
[3]: http://hackage.haskell.org/package/aivika-experiment-chart  "Aivika Experiment Chart"
[4]: http://hackage.haskell.org/package/aivika-experiment-cairo  "Aivika Experiment Cairo"
[5]: http://hackage.haskell.org/package/aivika-experiment-diagrams  "Aivika Experiment Diagrams"
[6]: https://github.com/dsorokin/aivika/wiki  "Aivika Wiki"
[7]: https://github.com/dsorokin/aivika-fsharp-ce "Aivika for .NET"

P.S. Aivika is actually a genuine female Mari name which is pronounced 
with stress on the last syllable.
