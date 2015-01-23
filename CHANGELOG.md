Version 3.1
-----

* Added functions failoverStream and failoverProcessor to model
servers with failures (temporary cancellation of the process with the
further repairing of the server).

* Added functions joinStream and joinProcessor to simplify the
modeling of servers.

* Checking the argument, i.e. time period, in the holdProcess
function. It must be not negative.

* The taskProcess computation behaves exactly like the background
  process, i.e. now its cancellation leads to immediate cancelling the background
  process too.

* Functions setStateActivation, setStateDeactivation and
  setStateTransition are redefined as the Event computations instead
  of Simulation computations.

* Refactored and simplified module Results.
