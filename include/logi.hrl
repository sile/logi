%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-define(LOGI_DEFAULT_BACKEND_MANAGER, logi_default_backend_manager).

-record(logi_log_context,
        {
          manager = ?LOGI_DEFAULT_BACKEND_MANAGER :: logi:backend_manager_id(),
          application                             :: atom(),
          module                                  :: module(),
          function                                :: atom(),
          line                                    :: pos_integer()
        }).

-record(logi_log_option,
        {
          header    = []                            :: [logi:header_entry()],
          metadata  = []                            :: [logi:metadata_entry()],
          frequency = always                        :: logi:frequency_policy()
        }).

-record(logi_format_option,
        {
          severity = info            :: logi:severity(),
          timestamp = os:timestamp() :: erlang:timestamp(),
          header = []                :: [logi:header_entry()],
          metadata = []              :: [logi:metadata_entry()],
          omitted_count = 0          :: non_neg_integer()
        }).
