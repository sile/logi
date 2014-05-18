%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc header file

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-define(LOGI_DEFAULT_EVENT_MANAGER, logi_default_event_manager).

-record(logi_log_option,
        {
          manager      = ?LOGI_DEFAULT_EVENT_MANAGER :: logi:event_manager_ref(),
          header       = []                          :: [logi:header_entry()],
          metadata     = []                          :: [logi:metadata_entry()],
          frequency    = always                      :: logi:frequency_policy(),
          frequency_id = self()                      :: term()
        }).

-record(logi_format_option,
        {
          severity = info :: logi:severity(),
          timestamp = os:timestamp() :: erlang:timestamp(),
          header = [] :: [logi:header_entry()],
          metadata = [] :: [logi:metadata_entry()],
          omitted_count = 0 :: non_neg_integer()
        }).
