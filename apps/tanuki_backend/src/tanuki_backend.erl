%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(tanuki_backend).
-export([all_tags/0, by_date/1, by_date/2, by_tag/1, by_tags/1, fetch_document/1]).

%%
%% Client API
%%

%
% @doc Retrieves the document with the given identifier as an ejson object
%      (where ejson is defined in couchbeam as essentially a proplist of
%      binary strings).
%
-spec fetch_document(string()) -> {document, term()}.
fetch_document(DocId) ->
    gen_server:call(tanuki_backend_db, {fetch_document, DocId}).

%
% @doc Retrieves all known tags as couchbeam view results.
%
-spec all_tags() -> [Rows::term()].
all_tags() ->
    gen_server:call(tanuki_backend_db, all_tags).

%
% @doc Retrieves all documents with a given tag, as couchbeam view results.
%
-spec by_tag(string()) -> [Rows::term()].
by_tag(Tag) when is_list(Tag) ->
    gen_server:call(tanuki_backend_db, {by_tag, Tag}).

%
% @doc Retrieves all documents with the given tags, as couchbeam view results.
%
-spec by_tags([string()]) -> [Rows::term()].
by_tags(Tags) when is_list(Tags) ->
    gen_server:call(tanuki_backend_db, {by_tags, Tags}).

%
% @doc Retrieves all documents whose most relevant date is within the given year.
%      The date used will be exif_date, or file_date, or import_date, in that
%      order. Results are as from couchbeam_view:fetch/3.
%
-spec by_date(integer()) -> [Rows::term()].
by_date(Year) when is_integer(Year) ->
    StartDate = [Year, 0, 0, 0, 0],
    EndDate = [Year + 1, 0, 0, 0, 0],
    gen_server:call(tanuki_backend_db, {by_date, StartDate, EndDate}).

%
% @doc Retrieves all documents whose most relevant date is within the given month.
%      The date used will be exif_date, or file_date, or import_date, in that
%      order. Results are as from couchbeam_view:fetch/3.
%
-spec by_date(integer(), integer()) -> [Rows::term()].
by_date(Year, Month) when is_integer(Year), is_integer(Month) ->
    StartDate = [Year, Month, 0, 0, 0],
    EndDate = [Year, Month + 1, 0, 0, 0],
    gen_server:call(tanuki_backend_db, {by_date, StartDate, EndDate}).
