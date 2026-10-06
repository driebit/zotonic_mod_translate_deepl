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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "module", "localization_and_translation",
        "api_and_integration", "configuration", "authorization_and_access_control"
    ]
}).
-moduledoc("
Provides automatic text translation through DeepL as a translation service for
Zotonic's `mod_translation` module.

## Configuration

Enable `mod_translate_deepl` and set the `mod_translate_deepl.api_key`
configuration value to the site's DeepL API key. The integration selects the
free API endpoint when the key contains `:fx`; otherwise it uses the paid API
endpoint. No separate endpoint setting is needed.

Grant `use.mod_translate_deepl` to the user groups allowed to request
translations. The observer checks this permission before contacting DeepL.
Translation requests send the supplied texts to the external DeepL service
using the site's API key.

## Translation service

The module handles Zotonic's `translate` notification and delegates the source
language, target language, and list of texts to `m_translate_deepl:translate/4`.
This integrates with the translation option in the admin's add-language dialog.

Texts are sent with HTML tag handling enabled, and text inside `code` elements
is excluded from translation. Omitting the source language requests automatic
language detection. Supported language combinations depend on DeepL.

On success the observer returns `{ok, TranslatedTexts}`. If access is denied,
the API key is missing, or the request fails, it returns `undefined`, leaving
the notification available to other translation providers.
").

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
            case m_translate_deepl:translate(From, To, Texts, Context) of
                {ok, _} = Ok ->
                    Ok;
                {error, _} ->
                    undefined
            end;
        false ->
            ?LOG_INFO(#{
                in => ?MODULE,
                text => <<"Not allowed to use DeepL for translations">>,
                result => error,
                reason => eacces
            }),
            undefined
    end.
