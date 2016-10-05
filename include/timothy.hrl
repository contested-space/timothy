-type size() :: non_neg_integer().

%% A survey is made up of four components:
%%
%% - A list of experiments: the precise definition of an experiment
%%     is given in the next section, but roughly an experiment is
%%     a function to measure for performance.
%%
%% - An input generator function: a function used to create input
%%     terms that will be fed to the experiment functions.  The input
%%     generator accepts a non-negative integer argument that
%%     specifies the size of the term to generate.  For example, if
%%     the argument is 5, a generator could generate a list containing
%%     5 elements, a tree with 5 leaf nodes, a binary with 5 bytes,
%%     etc.
%%
%%     When an input term has been generated, it will be fed to all
%%     the benchmarking functions; this ensures that the measurements
%%     stem from the same inputs and that no benchmark may have
%%     accidentally been biased by giving it "nicer" terms (e.g., a
%%     sorted list for insertion sort vs. a reversed list for quick
%%     sort).
%%
%%     Because the inputs are passed to all the benchmarking
%%     functions, it is okay (and even recommended) to generate
%%     randomized inputs.
%%
%% - A list of input sizes: an important data point when doing
%%     performance measurements is "how does the size of the input
%%     affect the speed of a function?"  Some algorithms offer good
%%     performance on smaller inputs while others only start showing
%%     their advantage once the input size becomes quite large.  The
%%     values in this list will be passed to the input generator
%%     function.
%%
%% - A number of iterations: by sampling each experiment multiple times,
%%     we can compute useful statistics such as the mean, the standard
%%     deviation, etc., which help us determine if the results
%%     obtained can be trusted to be close to reality or if they might
%%     have been affected by external noise.
%%
%% The other fields, the tallied statistics, regroup the statistics of
%% the experiments.
-record(survey, {
          %% Parameters
          experiments :: [experiment()],
          generator   :: fun((size()) -> any()),
          input_sizes :: [size()],
          iterations  :: non_neg_integer(),

          %% Tallied statistics
          minimums    :: #{size() => [float()]},
          maximums    :: #{size() => [float()]},
          medians     :: #{size() => [float()]},
          means       :: #{size() => [float()]},
          variances   :: #{size() => [float()]},
          std_devs    :: #{size() => [float()]}
}).

-type survey() :: #survey{}.


%% An experiment has a name to identify it, and a function that we
%% wish to benchmark.  This function should accept a term created
%% by a survey's generator function; the return value is ignored
%% by timothy.  (It it our philosophy that validating that two
%% functions are semantically identical is outside the scope of
%% a benchmarking tool, and should be done with unit and property
%% tests.)
%%
%% Each time the experiment is run, the time it took to execute the
%% function is added to the `times` field (technically, the function
%% `run_experiment` returns a new experiment with an updated `times`
%% field); once all the timings have been gathered, the function
%% `compile_experiment_stats` will fill the remaining fields.
-record(experiment, {
          %% Parameters
          name     :: atom(),
          function :: fun((any()) -> any()),

          %% Computed statistics
          times    :: [float()],
          n        :: undefined | non_neg_integer(),
          minimum  :: undefined | float(),
          maximum  :: undefined | float(),
          median   :: undefined | float(),
          mean     :: undefined | float(),
          variance :: undefined | float(),
          std_dev  :: undefined | float()
}).

-type experiment() :: #experiment{}.
