
Version 4.2.1
-----

* Added optimised queues which have no counters nor signals.

* Added assembling functions for streams.

Version 4.2
-----

* Added new random distributions: lognormal, Gamma, Beta, Weibull and 
  a generic discrete by pdf.

* The items can be removed from the queue; moreover, the queue can be 
  cleared.

* Added a simplified API for accessing the results of simulation.

* Added the Gate entity.

Version 4.1.1
-----

* More counters and statistics for the new resources.

Version 4.1
-----

* Added new resource types with counters and statistics.

Version 4.0.2
-----

* Minor changes in the Statistics module: replacing functions 
  resetSamplingCounter and resetTimingCounter with their general analogs.

* Unifying process preemption signals in modules Server and Activity: 
  renaming four signals like that how they are named in the Process module.
  
* The timing statistics (time persistent one) can be normalized to 
  a sampling-based statistics (based upon observation), which allows, 
  for example, building a deviation chart for the queue size.

Version 4.0.1
-----

* Fixed build issues on GHC 7.10.1.

Version 4.0
-----

* Added resource preemption.

* Fixed misprint in the activityProcessor function.

Version 3.1
-----

* Added functions failoverStream and failoverProcessor to model
servers with failures (temporary cancellation of the process with the
further repairing of the server).

* Added functions joinStream and joinProcessor to simplify the
modeling of servers.

* Checking the argument, i.e. time period, in the holdProcess
function. It must not be negative.

* The taskProcess computation behaves exactly like the background
  process, i.e. now its cancellation leads to immediate cancelling the background
  process too.

* Functions setStateActivation, setStateDeactivation and
  setStateTransition are redefined as the Event computations instead
  of Simulation computations.

* Refactored and simplified module Results.
