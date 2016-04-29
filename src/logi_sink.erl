%% @copyright 2014-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
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

-type sink() :: #?SINK{}.
-type id() :: term().
-type spec() :: supervisor:child_spec().
%% TODO: note: restart is ignored
-type sup_flags() :: supervisor:sup_flags().
%% TODO: note: strategy is ignored

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(spec()) -> sink().
new(Spec) ->
    new(Spec, #{}).

-spec new(spec(), sup_flags()) -> sink().
new(Spec, Flags) ->
    %% TODO: validate
    #?SINK{
        spec      = normalize_spec(Spec),
        sup_flags = normalize_sup_flags(Flags)
       }.

-spec from_writer(id(), logi_sink_writer:writer()) -> sink().
from_writer(Id, Writer) ->
    new(#{id => Id, start => {logi_sink_standalone_writer_proc, start_link, [Writer]}}).

-spec is_sink(sink() | term()) -> boolean().
is_sink(X) ->
    is_record(X, ?SINK).

-spec get_id(sink()) -> id().
get_id(#?SINK{spec = Spec}) ->
    maps:get(id, Spec).

-spec get_spec(sink()) -> spec().
get_spec(#?SINK{spec = Spec}) ->
    Spec.

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
