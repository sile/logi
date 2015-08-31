%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc backend object
-module(logi_backend).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback write(logi_backend:backend(), logi_msg_info:info(), io:format(), [term()]) -> any().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([make/1, make/4]).
-export([is_backend/1]).
-export([get_id/1, get_process/1, get_module/1, get_data/1]).

-export_type([backend/0]).
-export_type([spec/0]).
-export_type([id/0, process/0, data/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(BACKEND, ?MODULE).

-record(?BACKEND,
        {
          id      :: id(),
          process :: process(),
          module  :: module(),
          data    :: data()
        }).
%% TODO: formatter field (?): {module(), term()}

-opaque backend() :: #?BACKEND{}.

-type spec() :: {process(), module(), data()} % TODO: `process()'は不要かも (`data()'だけで十分?)
              | {id(), process(), module(), data()}.

-type id()      :: term().
-type process() :: atom().
-type data()    :: term(). % user defined data

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Makes a new backend object
-spec make(id(), process(), module(), data()) -> backend().
make(Id, Process, Module, Data) ->
    case is_atom(Process) andalso is_atom(Module) of
        false -> error(badarg, [Id, Process, Module, Data]);
        true  ->
            #?BACKEND{
                id      = Id,
                process = Process,
                module  = Module,
                data    = Data
               }
    end.

%% @doc Makes a new backend object from `Spec'
-spec make(Spec::spec()) -> backend().
make({Process, Module, Data})     -> make(Process, Process, Module, Data);
make({Id, Process, Module, Data}) -> make(Id, Process, Module, Data);
make(Arg)                         -> error(badrag, [Arg]).

%% @doc Returns `true' if `Term' appears to be a backend, otherwise `false'
-spec is_backend(Term::term()) -> boolean().
is_backend(X) -> is_record(X, ?BACKEND).

%% @doc Gets the backend ID
-spec get_id(backend()) ->  id().
get_id(#?BACKEND{id = Id}) -> Id.

%% @doc Gets the backend process
-spec get_process(backend()) -> process().
get_process(#?BACKEND{process = Process}) -> Process.

%% @doc Gets the backend module
-spec get_module(backend()) -> module().
get_module(#?BACKEND{module = Module}) -> Module.

%% @doc Gets the backend data
-spec get_data(backend()) -> data().
get_data(#?BACKEND{data = Data}) -> Data.
