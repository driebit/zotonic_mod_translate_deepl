%% @author Driebit BV
%% @copyright 2023-2024 Driebit BV
%% @doc Model for translating texts using DeepL
%% @end

%% Copyright 2023-2024 Driebit BV
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

-module(m_translate_deepl).

-export([
    m_get/3,
    is_configured/1,
    translate/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


m_get([ <<"is_configured">> | Rest ], _Msg, Context) ->
    {ok, {is_configured(Context), Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, enoent}.


-spec is_configured( Context ) -> boolean() when
    Context :: z:context().
is_configured(Context) ->
    case api_key(Context) of
        <<>> -> false;
        "" -> false;
        undefined -> false;
        _ -> true
    end.

-spec translate(SourceLanguage, TargetLanguage, Texts, Context) -> {ok, TranslatedTexts} | {error, Reason} when
    SourceLanguage :: z_language:language() | undefined,
    TargetLanguage :: z_language:language(),
    Texts :: list( binary() ),
    Context :: z:context(),
    TranslatedTexts :: list( binary() ),
    Reason :: term().
translate(SourceLanguage, TargetLanguage, Texts, Context) when is_list(Texts) ->
    translate_1(api_key(Context), SourceLanguage, TargetLanguage, Texts, Context).

translate_1(<<>>, _SourceLanguage, _TargetLanguage, _Texts, _Context) ->
    {error, api_key};
translate_1(ApiKey, SourceLanguage, TargetLanguage, Texts, Context) ->
    %% checklist:
    %% - exponential backoff on 429 or 500 errors
    %% - preferably use a persistent http connection
    %% - use Content-Type: application/x-www-form-urlencoded or application/json
    %% - only for document/file upload Content-Type: multipart/form-data
    %% - providing translation context might improve translations
    Endpoint = api_endpoint(Context),
    Options = [
        {authorization, <<"DeepL-Auth-Key ", ApiKey/binary>>},
        {content_type, <<"application/json">>}
    ],
    Url = <<Endpoint/binary, "/v2/translate">>,
    Args = #{
        <<"text">> => Texts,
        <<"target_lang">> => z_convert:to_binary(TargetLanguage),
        <<"tag_handling">> => <<"html">>,
        <<"ignore_tags">> => [
            <<"code">>
        ]
    },
    Args1 = case source_lang(z_convert:to_binary(SourceLanguage)) of
        undefined ->
            Args;
        SLang ->
            Args#{
                <<"source_lang">> => SLang
            }
    end,
    case z_fetch:fetch_json(post, Url, Args1, Options, Context) of
        {ok, #{<<"translations">> := Translations }} when is_list(Translations) ->
            Tr1 = [ Text || #{ <<"text">> := Text } <- Translations ],
            {ok, Tr1};
        {ok, Response} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_driebit_deepl,
                text => <<"Unexpected result from DeepL">>,
                result => error,
                reason => unknown_response,
                response => Response,
                source_language => SourceLanguage,
                target_language => TargetLanguage
            }),
            {error, unknown_response};
        {error, {400, _Url, _Hs, _Size, ErrorBody}} ->
            Reason = case jsx:decode(ErrorBody) of
                #{ <<"message">> := <<"Value for 'source_lang' not supported.">> } ->
                    source_language;
                #{ <<"message">> := <<"Value for 'target_lang' not supported.">> } ->
                    target_language;
                #{ <<"message">> := Message } ->
                    Message;
                R ->
                    R
            end,
            ?LOG_ERROR(#{
                in => zotonic_mod_driebit_deepl,
                text => <<"Error result from DeepL">>,
                result => error,
                reason => Reason,
                source_language => SourceLanguage,
                target_language => TargetLanguage
            }),
            {error, Reason};
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_driebit_deepl,
                text => <<"Error result from DeepL">>,
                result => error,
                reason => Reason,
                source_language => SourceLanguage,
                target_language => TargetLanguage
            }),
            {error, Reason}
    end.

source_lang(<<>>) ->
    undefined;
source_lang(<<"x-default">>) ->
    undefined;
source_lang(<<"x-none">>) ->
    undefined;
source_lang(Lang) ->
    Lang.

api_key(Context) ->
    z_convert:to_binary(m_config:get_value(mod_translate_deepl, api_key, Context)).

api_endpoint(Context) ->
    Key = api_key(Context),
    case binary:match(Key, <<":fx">>) of
        nomatch ->
             <<"https://api.deepl.com">>; % (Paid endpoint)
        {_, _} ->
             <<"https://api-free.deepl.com">>
    end.
