-module(timothy).

-include("timothy.hrl").

-export([
    new_experiment/2,
    new_survey/4,
    run_survey/1,
    show_survey/2
]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% Public functions
new_experiment(Name, Fun) ->
    #experiment{
        name = Name,
        function = Fun,
        times = []
    }.


new_survey(Experiments, Gen, Size, Iters) ->
    #survey{
        experiments = Experiments,
        generator = Gen,
        input_size = Size,
        iterations = Iters
    }.


%% Run a survey, and tally the statistics of its experiments.
%% In pseudo-code, we have (roughly) the following algorithm:
%%
%%     for _ in 1 to survey.iterations do
%%         x = survey.generator(size)
%%         for exp in survey.experiments do
%%             t = time exp.function(x)
%%             exp.times.append(t)
%%         done
%%     done
%%     for exp in survey.experiments do
%%         compile_experiment_stats(exp)
%%     done
%%     tally(survey, survey.experiments)
run_survey(S = #survey{experiments = Experiments,
                       generator = Gen,
                       input_size = Size,
                       iterations = Iters}) ->
    Experiments2 = run_experiments(Experiments, Gen, Size, Iters),
    S#survey{experiments = Experiments2}.


show_survey(#survey{ experiments = Experiments }, Windows) ->
    Max = 1 + lists:max([lists:max(Times) || #experiment{ times=Times } <- Experiments]),
    SliceSice = Max / Windows,
    Histograms = lists:map(fun(Exp) ->
        H = new_histogram(Windows, SliceSice),
        lists:foldl(fun tally/2, H, Exp#experiment.times)
    end, Experiments),
    lists:foreach(fun({Exp, H}) ->
        io:format("~p~n", [Exp#experiment.name]),
        show_histogram(H)
    end, lists:zip(Experiments, Histograms)).

%% Private functions

%% run_experiments doesn't use lists:seq/2 anymore, because for a large
%% number of iterations (which can be common when measuring fast experiments),
%% the Erlang VM would run out of heap space.
run_experiments(Experiments, Gen, Size, Iters) ->
    run_experiments_loop(Experiments, Gen, Size, Iters).

run_experiments_loop(Experiments, _Gen, _Size, 0) ->
    Experiments;
run_experiments_loop(Experiments, Gen, Size, Iters) ->
    X = Gen(Size),
    Experiments2 = lists:map(fun(E) -> run_experiment(E, X) end, Experiments),
    run_experiments_loop(Experiments2, Gen, Size, Iters - 1).


run_experiment(E = #experiment{function = Fun, times = Times}, X) ->
    {T, _} = timer:tc(Fun, [X]),
    E#experiment{times = [T | Times]}.



-spec new_histogram(non_neg_integer(), float()) -> histogram().

new_histogram(Windows, SliceSize) ->
    EmptyTally = maps:from_list([
        {N, 0} || N <- lists:seq(0, Windows - 1)
    ]),
    #histogram{
        slice_size = SliceSize,
        tally = EmptyTally
    }.


-spec tally(float(), histogram()) -> histogram().

tally(Entry, H = #histogram{ slice_size=Slice, tally=CurrTally }) ->
    Bucket = trunc(Entry / Slice),
    Count = maps:get(Bucket, CurrTally),
    NewTally = maps:put(Bucket, Count + 1, CurrTally),
    H#histogram{
        tally = NewTally
    }.


-spec show_histogram(histogram()) -> ok.

show_histogram(#histogram{ slice_size = SliceSize, tally = Tally }) ->
    lists:foreach(
        fun(I) ->
            io:format("~.2f\t~.2f\t~p~n", [SliceSize*I, SliceSize*(I+1), maps:get(I, Tally)])
        end,
        lists:seq(0, maps:size(Tally) - 1)).
