-module(timothy).

-compile(export_all).

-export([
         new_survey/4,
         new_experiment/2,
         run_survey/1,
         report_survey/1
]).

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

%% Public functions
new_experiment(Name, Fun) ->
    #experiment{
       name = Name,
       function = Fun,
       times = []
    }.

new_survey(Experiments, Gen, Sizes, Iters) ->
    #survey{
       experiments = Experiments,
       generator   = Gen,
       input_sizes = Sizes,
       iterations  = Iters,
       minimums    = #{},
       maximums    = #{},
       medians     = #{},
       means       = #{},
       variances   = #{},
       std_devs    = #{}
    }.


%% Run a survey, and tally the statistics of its experiments.
%% In pseudo-code, we have (roughly) the following algorithm:
%%
%%     for size in survey.sizes do
%%         for _ in 1 to survey.iterations do
%%             x = survey.generator(size)
%%             for exp in survey.experiments do
%%                 t = time exp.function(x)
%%                 exp.times.append(t)
%%             done
%%         done
%%         for exp in survey.experiments do
%%             compile_experiment_stats(exp)
%%         done
%%         tally(survey, survey.experiments)
%%     done
run_survey(S = #survey{experiments = Experiments,
                       generator = Gen,
                       input_sizes = Sizes,
                       iterations = Iters}) ->
    lists:foldl(
      fun(Size, Survey) ->
              Exps = run_experiments(Experiments, Gen, Size, Iters),
              Exps2 = lists:map(fun compile_experiment_stats/1, Exps),
              tally(Survey, Exps2, Size)
      end,
      S,
      Sizes).


report_survey(S) ->
    ExpNames = [E#experiment.name || E <- S#survey.experiments],

    rule(),
    io:format("| ~8s | ~10s | ~16s | ~16s | ~16s | ~16s | ~16s |~n",
              ["Size", "Experiment", "Minimum", "Maximum",
               "Median", "Mean", "Std. Dev."]),
    rule(),
    lists:foreach(
      fun(Size) ->
              line(Size, ExpNames, S),
              rule()
      end,
      S#survey.input_sizes).

rule() ->
    IoStr = [$+,
             lists:duplicate(10, $-), $+,
             lists:duplicate(12, $-), $+,
             lists:duplicate(18, $-), $+,
             lists:duplicate(18, $-), $+,
             lists:duplicate(18, $-), $+,
             lists:duplicate(18, $-), $+,
             lists:duplicate(18, $-), $+],
    io:format("~s~n", [IoStr]).

line(Size, Names, Survey) ->
    lists:foreach(
      fun({I, Name}) ->
              io:format("| ~8B | ~10s | ~16B | ~16B | ~16.3f | ~16.3f | ~16.3f |~n",
                        [Size,
                         Name,
                         survey_get(Survey, Size, I, #survey.minimums),
                         survey_get(Survey, Size, I, #survey.maximums),
                         survey_get(Survey, Size, I, #survey.medians),
                         survey_get(Survey, Size, I, #survey.means),
                         survey_get(Survey, Size, I, #survey.std_devs)
                        ]
                       )
      end,
      lists:zip(lists:seq(1, length(Names)), Names)).


%% Private functions
run_experiments(Experiments, Gen, Size, Iters) ->
    lists:foldl(
      fun(_Iter, Experiments2) ->
              X = Gen(Size),
              lists:map(fun(E) -> run_experiment(E, X) end, Experiments2)
      end,
      Experiments,
      lists:seq(1, Iters)
     ).

run_experiment(E = #experiment{function = Fun, times = Times}, X) ->
    {T, _} = timer:tc(Fun, [X]),
    E#experiment{times = [T | Times]}.

compile_experiment_stats(E = #experiment{times = Times}) ->
    SortedTimes = lists:sort(Times),
    N = length(SortedTimes),
    HalfN = N div 2,
    Median = case N rem 2 of
                 0 -> (lists:nth(HalfN, SortedTimes)
                       + lists:nth(HalfN+1, SortedTimes)) / 2;
                 1 -> lists:nth(HalfN, SortedTimes)
             end,
    Mean = lists:foldl(fun(X, Acc) -> Acc + X/N end,
                       0.0,
                       SortedTimes),
    Variance = lists:foldl(fun(X, Acc) -> Acc + math:pow(X - Mean, 2) end,
                           0.0,
                           SortedTimes) / (N - 1),
    StdDev = math:sqrt(Variance),
    E#experiment {
      times    = SortedTimes,
      n        = N,
      minimum  = hd(SortedTimes),
      maximum  = lists:last(SortedTimes),
      median   = Median,
      mean     = Mean,
      variance = Variance,
      std_dev  = StdDev
    }.

tally(Survey = #survey{
         minimums = SurveyMinimums,
         maximums = SurveyMaximums,
         medians = SurveyMedians,
         means = SurveyMeans,
         variances = SurveyVariances,
         std_devs = SurveyStdDevs
      }, Experiments, Size) ->
    Minimums  = [E#experiment.minimum  || E <- Experiments],
    Maximums  = [E#experiment.maximum  || E <- Experiments],
    Medians   = [E#experiment.median   || E <- Experiments],
    Means     = [E#experiment.mean     || E <- Experiments],
    Variances = [E#experiment.variance || E <- Experiments],
    StdDevs   = [E#experiment.std_dev  || E <- Experiments],
    Survey#survey{
      minimums  = maps:put(Size, Minimums, SurveyMinimums),
      maximums  = maps:put(Size, Maximums, SurveyMaximums),
      medians   = maps:put(Size, Medians, SurveyMedians),
      means     = maps:put(Size, Means, SurveyMeans),
      variances = maps:put(Size, Variances, SurveyVariances),
      std_devs  = maps:put(Size, StdDevs, SurveyStdDevs)
    }.


survey_get(S, Size, Index, Field) ->
    lists:nth(Index, maps:get(Size, element(Field, S))).
