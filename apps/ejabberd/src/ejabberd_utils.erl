%%==============================================================================
%%%% Copyright 2012 Erlang Solutions Ltd.
%%%%%
%%%%% Licensed under the Apache License, Version 2.0 (the "License");
%%%%% you may not use this file except in compliance with the License.
%%%%% You may obtain a copy of the License at
%%%%%
%%%%% http://www.apache.org/licenses/LICENSE-2.0
%%%%%
%%%%% Unless required by applicable law or agreed to in writing, software
%%%%% distributed under the License is distributed on an "AS IS" BASIS,
%%%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%%% See the License for the specific language governing permissions and
%%%%% limitations under the License.
%%%%%==============================================================================

-module(ejabberd_utils).
-export([get_value/2,
         get_value/3]).

get_value(Key, List) ->
    get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        _ -> Default
    end.
