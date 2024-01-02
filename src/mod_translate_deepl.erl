%% @author Driebit BV
%% @copyright 2024 Driebit BV
%% @doc Translation service using DeepL
%% @end

%% Copyright 2024 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_translate_deepl).

-mod_title("Translate with DeepL").
-mod_description("Translation service using DeepL").
-mod_author("Driebit BV").
-mod_depends([ mod_translation ]).

-author("Driebit BV").

-export([
    observe_translate/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

observe_translate(#translate{
        from = From,
        to = To,
        texts = Texts
    }, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            m_translate_deepl:translate(From, To, Texts, Context);
        false ->
            ?LOG_INFO(#{
                in => ?MODULE,
                text => <<"Not allowed to use DeepL for translations">>,
                result => error,
                reason => eacces
            }),
            undefined
    end.
