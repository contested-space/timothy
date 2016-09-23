-module(timothy).

-export([
         new_experiment/3,
         run_experiment/2
]).


%% An experiment is a structure that contains three ingredients:
%% - A list of benchmarks: this list contains pairs where the
%%     first element is an atom giving the benchmark a name,
%%     and the second element is the function to measure; this
%%     function accepts one input term and returns a result.
%%     If we wish to benchmark a function that requires more
%%     than one parameter, a small wrapper function will be
%%     necessary.
%%
%%     As a point of convention, the first element of the benchmark
%%     list is the basis, the reference against which the other
%%     benchmarks will be compared.  Therefore, the benchmark list
%%     should contain at least two elements.
%%
%% - An input generator function: this function is used to create
%%     input terms that will be fed to the benchmarks.  The input
%%     generator accepts a non-negative integer argument that
%%     specifies the size of the term to generate.  For example, if
%%     the argument is 5, a generator could generate a list containing
%%     5 elements, a tree with 5 leaf nodes, a binary with 5 bytes, etc.
%%
%%     When an input term has been generated, it will be fed to all the
%%     benchmarking functions; this ensures that the measurements obtained
%%     stem from the same inputs and that no benchmark may have been
%%     biased by giving it "nicer" terms (e.g., a sorted list for insertion
%%     sort vs. a reversed list for quick sort).
%%
%% - A list of input sizes: an important data point when doing performance
%%     measurements is "how does the size of the input affect the speed of
%%     a function?"  Some algorithms offer good performance on smaller inputs
%%     while others only start showing their advantage once the input size
%%     becomes quite large.
-record(experiment, {
          benchmarks      :: [{atom(), fun((term()) -> term())}],
          input_generator :: fun((non_neg_integer()) -> term()),
          input_sizes     :: [non_neg_integer()]
}).


new_experiment(Benchmarks, Gen, Sizes) ->
    #experiment{
       benchmarks = Benchmarks,
       input_generator = Gen,
       input_sizes = Sizes
    }.


%% Obtain time measurements for the benchmarks in an experiment.
%% The second argument, Iterations, controls how many time each
%% benchmark is executed per input size.  By sampling each function
%% multiple times, we can compute useful statistics such as the mean,
%% the standard deviation, etc., which help us determine if the
%% results obtained can be trusted to be close to reality or if they
%% have been affected by external noise.
%%
%% The following pseudo-code summarizes the process.
%%
%% for size in input_sizes do
%%   for _ in 1..iterations do
%%     x = gen(size)
%%     for id, bench in benchmarks do
%%       t = time bench(x)
%%       results[size, id].push(t)
%%     done
%%   done
%% done
run_experiment(#experiment{benchmarks = Benchmarks,
                           input_generator = Gen,
                           input_sizes = Sizes},
               Iterations) ->
    lists:foldl(
      fun(Size, Acc) ->
              R = benchmark_n_times(Benchmarks, Gen, Size, Iterations),
              dict:store(Size, R, Acc)
      end,
      dict:new(),
      Sizes).


benchmark_n_times(Benchmarks, Gen, Size, Iterations) ->
    lists:foldl(
      fun(_, Acc) ->
              R = benchmark_once(Benchmarks, Gen, Size),
              lists:foldl(
                fun({Name, Time}, Acc2) -> dict:append(Name, Time, Acc2) end,
                Acc,
                R)
      end,
      dict:new(),
      lists:seq(1, Iterations)).


benchmark_once(Benchmarks, Gen, Size) ->
    Input = Gen(Size),
    [begin
         {T, _} = timer:tc(Fun, [Input]),
         {Name, T}
     end || {Name, Fun} <- Benchmarks].
