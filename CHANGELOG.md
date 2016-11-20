
Version 4.6
-----

* Updated module DoubleLinkedList.

* Breaking change: arrows Net and Processor are trying to perform computations
  in parallel as possible, when using the proc notation. Earlier they executed
  sequentially.

Version 4.5
-----

* The Transform computation seems to be not ArrowLoop.

* Added the enqueueEventWithStartTime and enqueueEventWithStopTime functions.

Version 4.3.5
-----

* Removed the obsolete preprocessor instructions for conditional compilation.

Version 4.3.4
-----

* Yet more safe the resource preemption.

* Introducing exception SimulationRetry, which is needed for parallel distributed simulation.

Version 4.3.3
-----

* Added function vectorDeleteRange to remove the range of elements from the mutable vector.

* Fixed the resource preemption when releasing and requesting again for the resource at
  the same modeling time.

Version 4.3.2
-----

* Added functions splitStreamFiltering, splitStreamFilteringQueueing to filter when 
  splitting the input stream.

* Explicit exporting function newRandomGenerator01 for generating random numbers by 
  the specified custom generator returning numbers from 0 to 1.
  
* Added function freezeList for the double linked list.

* Added an immutable priority queue.

Version 4.3.1
-----

* Improved the timeoutProcessUsingId function: no need in additional cancellation signal.
  Thanks to Gabriel Garcia who pointed to this issue and suggested a possible solution.

* Added functions delaySignal and delaySignalM to delay a signal in time through 
  the event queue.

* Added function runSimulationByIndex to run the simulation with an arbitrary run index.

Version 4.3
-----

* Added optimised queues which have no counters nor signals.

* Added assembling functions for streams.

* Added the operation activity as a simplification of server.

* Added new functions for the queues.

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
