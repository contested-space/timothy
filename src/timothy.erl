-module(timothy).

-include("timothy.hrl").

-export([
         new_survey/4,
         new_experiment/2,
         run_survey/1
]).


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
