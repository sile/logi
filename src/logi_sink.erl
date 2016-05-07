%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sinks
%%
%% A sink has the specification of a sink process ({@link logi_sink_proc}).
%% A sink process manages a sink writer ({@link logi_sink_writer}).
%%
%% See `logi_builtin_sink_XXX' modules for usage examples.
%%
%% @end
-module(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([from_writer/2]).
-export([is_sink/1]).

-export([get_id/1]).
-export([get_spec/1]).
-export([get_sup_flags/1]).

-export_type([sink/0]).
-export_type([id/0]).
-export_type([spec/0]).
-export_type([sup_flags/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(SINK, ?MODULE).
-record(?SINK,
        {
          spec      :: spec(),
          sup_flags :: sup_flags()
        }).

-opaque sink() :: #?SINK{}.
%% A sink

-type id() :: term().
%% The identifier of a sink
%%
%% The scope of an identifier is limited in siblings with the same parent.

-type spec() :: supervisor:child_spec().
%% The specification of a sink process.
%%
%% See official documents of `supervisor' for more information.
%%
%% NOTE: `restart' field is ignored (always regarded as `temporary').

-type sup_flags() :: supervisor:sup_flags().
%% The supervise flags of a sink process.
%%
%% See official documents of `supervisor' for more information.
%%
%% NOTE: `strategy' field is ignored.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Spec, #{})
-spec new(spec()) -> sink().
new(Spec) ->
    new(Spec, #{}).

%% @doc Creates a new sink
-spec new(spec(), sup_flags()) -> sink().
new(Spec, Flags) ->
    #?SINK{
        spec      = normalize_spec(Spec),
        sup_flags = normalize_sup_flags(Flags)
       }.

%% @doc Creates a sink from standalone a writer instance
%%
%% No specific sink process is needed by `Writer' to write log messages.
-spec from_writer(id(), logi_sink_writer:writer()) -> sink().
from_writer(Id, Writer) ->
    new(#{id => Id, start => {logi_sink_standalone_writer_proc, start_link, [Writer]}}).

%% @doc Returns `true' if `X' is a sink, `false' otherwise
-spec is_sink(X :: (sink() | term())) -> boolean().
is_sink(X) ->
    is_record(X, ?SINK).

%% @equiv maps:get(id, get_spec(Sink))
-spec get_id(Sink :: sink()) -> id().
get_id(#?SINK{spec = Spec}) ->
    maps:get(id, Spec).

%% @doc Gets the process specification of `Sink'
%%
%% The type of the return value is always map.
-spec get_spec(Sink :: sink()) -> spec().
get_spec(#?SINK{spec = Spec}) ->
    Spec.

%% @doc Gets the supervise flags of `Sink'
%%
%% The type of the return value is always map.
-spec get_sup_flags(sink()) -> sup_flags().
get_sup_flags(#?SINK{sup_flags = Flags}) ->
    Flags.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec normalize_sup_flags(sup_flags()) -> sup_flags().
normalize_sup_flags({_, Intensity, Period}) ->
    normalize_sup_flags(#{intensity => Intensity, period => Period});
normalize_sup_flags(Flags) ->
    _ = is_map(Flags) orelse error(badarg, [Flags]),
    _ = logi_utils:is_non_neg_integer(maps:get(intensity, Flags, 1)) orelse error(badarg, [Flags]),
    _ = logi_utils:is_pos_integer(maps:get(period, Flags, 5)) orelse error(badarg, [Flags]),
    maps:with([intensity, period], Flags).

-spec normalize_spec(spec()) -> spec().
normalize_spec({Id, StartFunc, _, Shutdown, Type, Modules}) ->
    normalize_spec(#{id => Id, start => StartFunc, shutdown => Shutdown, type => Type, modules => Modules});
normalize_spec(Spec) ->
    _ = supervisor:check_childspecs([Spec]) =:= ok orelse error(badarg, [Spec]),
    maps:with([id, start, shutdown, type, modules], Spec).
