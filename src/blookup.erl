%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Binary Lookup Data Structure.==
%%% Keep all binary key/value data in a single continuous binary to
%%% minimize Erlang memory usage.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(blookup).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([find/2,
         new/0,
         store/3,
         test/0]).

-ifndef(KEY_SIZE_MAX_BYTES).
-define(KEY_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(VALUE_SIZE_MAX_BYTES).
-define(VALUE_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(INDEX_SIZE_MAX_BYTES).
-define(INDEX_SIZE_MAX_BYTES, 4).
-endif.
-define(INDEXES_ELEMENT_SIZE_BYTES,
        (?INDEX_SIZE_MAX_BYTES + ?KEY_SIZE_MAX_BYTES + ?VALUE_SIZE_MAX_BYTES)).
-define(INDEX_SIZE_TYPE,
        :?INDEX_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).
-define(KEY_SIZE_TYPE,
        :?KEY_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).
-define(VALUE_SIZE_TYPE,
        :?VALUE_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).

-record(blookup,
    {
        size = 0 :: non_neg_integer(),
        indexes = <<>> :: binary(),
        data = <<>> :: binary()
    }).

-type state() :: #blookup{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in the binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec find(Key :: binary(),
           Lookup :: #blookup{}) ->
    {ok, Value :: binary()} |
    error.

find(<<>>, _) ->
    error;
find(Key,
     #blookup{size = Size,
              indexes = Indexes,
              data = Data})
    when is_binary(Key) ->
    case lookup(Key, Size, Indexes, Data) of
        {true, _, Value} ->
            {ok, Value};
        {false, _} ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    #blookup{}.

new() ->
    #blookup{}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key/value pair in the binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: binary(),
            Value :: binary(),
            Lookup :: #blookup{}) ->
    #blookup{}.

store(<<>>, _, _) ->
    erlang:exit(badarg);
store(Key, Value,
      #blookup{size = SizeOld,
               indexes = IndexesOld,
               data = DataOld} = Lookup)
    when is_binary(Key), is_binary(Value) ->
    {SizeNew, IndexesNew, DataNew} = case lookup(Key, SizeOld,
                                                 IndexesOld, DataOld) of
        {true, I, _} ->
            store_key_value_replace(Key, Value, I, SizeOld,
                                    IndexesOld, DataOld);
        {false, I} ->
            store_key_value_insert(Key, Value, I, SizeOld,
                                   IndexesOld, DataOld)
    end,
    Lookup#blookup{size = SizeNew,
                   indexes = IndexesNew,
                   data = DataNew}.

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    Lookup0 = blookup:new(),
    error = blookup:find(<<>>, Lookup0),
    {'EXIT', badarg} = (catch blookup:store(<<>>, <<"ignored">>, Lookup0)),
    Lookup1 = blookup:store(<<"abcd">>, <<"value0">>, Lookup0),
    {ok, <<"value0">>} = blookup:find(<<"abcd">>, Lookup1),
    error = blookup:find(<<"abc">>, Lookup1),
    Lookup2 = blookup:store(<<"abcc">>, <<"value1">>, Lookup1),
    Lookup3 = blookup:store(<<"abcb">>, <<"value2">>, Lookup2),
    Lookup4 = blookup:store(<<"bc">>, <<"value__3">>, Lookup3),
    Lookup5 = blookup:store(<<"aaaaaaaaaaaa">>, <<"value00004">>, Lookup4),
    Lookup6 = blookup:store(<<"a">>, <<"value5">>, Lookup5),
    {ok, <<"value0">>} = blookup:find(<<"abcd">>, Lookup6),
    {ok, <<"value2">>} = blookup:find(<<"abcb">>, Lookup6),
    {ok, <<"value__3">>} = blookup:find(<<"bc">>, Lookup6),
    error = blookup:find(<<"aaaaaaaaaaa">>, Lookup6),
    {ok, <<"value00004">>} = blookup:find(<<"aaaaaaaaaaaa">>, Lookup6),
    {ok, <<"value5">>} = blookup:find(<<"a">>, Lookup6),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

lookup(_, 0, _, _) ->
    {false, 0};
lookup(Key, Size, Indexes, Data) ->
    I = Size div 2,
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            IMax = Size - 1,
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Indexes, Data)
            end;
        Key < KeyCompare ->
            IMin = 0,
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Indexes, Data)
            end
    end.

lookup_loop(I, I, Key, Indexes, Data) ->
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            {false, I + 1};
        Key < KeyCompare ->
            {false, I}
    end;
lookup_loop(IMin, IMax, Key, Indexes, Data) ->
    I = IMin + (IMax - IMin) div 2,
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Indexes, Data)
            end;
        Key < KeyCompare ->
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Indexes, Data)
            end
    end.

lookup_key_value(0,
                 <<0?INDEX_SIZE_TYPE,
                   KeySize?KEY_SIZE_TYPE,
                   ValueSize?VALUE_SIZE_TYPE>>,
                 Data) ->
    <<Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8>> = Data,
    {Key, Value};
lookup_key_value(I, Indexes, Data) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    <<_:Offset/binary-unit:8,
      Index?INDEX_SIZE_TYPE,
      KeySize?KEY_SIZE_TYPE,
      ValueSize?VALUE_SIZE_TYPE,
      _/binary>> = Indexes,
    <<_:Index/binary-unit:8,
      Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8,
      _/binary>> = Data,
    {Key, Value}.

store_key_value_replace(Key, Value, I, Size, Indexes, Data) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    <<IndexesPart0:Offset/binary-unit:8,
      Index?INDEX_SIZE_TYPE,
      KeySizeOld?KEY_SIZE_TYPE,
      ValueSizeOld?VALUE_SIZE_TYPE,
      IndexesPart1Old/binary>> = Indexes,
    IndexesPart1New = indexes_adjust(IndexesPart1Old,
                                     (KeySizeNew + ValueSizeNew) -
                                     (KeySizeOld + ValueSizeOld)),
    <<DataPart0:Index/binary-unit:8,
      _:KeySizeOld/binary-unit:8,
      _:ValueSizeOld/binary-unit:8,
      DataPart1/binary>> = Data,
    {Size,
     <<IndexesPart0:Offset/binary-unit:8,
       Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       IndexesPart1New/binary>>,
     <<DataPart0:Index/binary-unit:8,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       DataPart1/binary>>}.

store_key_value_insert(Key, Value, 0, 0, <<>>, <<>>) ->
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    Index = 0,
    {1,
     <<Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE>>,
     <<Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8>>};
store_key_value_insert(Key, Value, I, Size, Indexes, Data) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    <<IndexesPart0:Offset/binary-unit:8,
      IndexesPart1Old/binary>> = Indexes,
    Index = case IndexesPart1Old of
        <<>> ->
            OffsetPrevious = (I - 1) * ?INDEXES_ELEMENT_SIZE_BYTES,
            <<_:OffsetPrevious/binary-unit:8,
              IndexPrevious?INDEX_SIZE_TYPE,
              KeySizePrevious?KEY_SIZE_TYPE,
              ValueSizePrevious?VALUE_SIZE_TYPE>> = IndexesPart0,
            IndexPrevious + KeySizePrevious + ValueSizePrevious;
        <<IndexValue?INDEX_SIZE_TYPE,
          _/binary>> ->
            IndexValue
    end,
    IndexesPart1New = indexes_adjust(IndexesPart1Old,
                                     KeySizeNew + ValueSizeNew),
    <<DataPart0:Index/binary-unit:8,
      DataPart1/binary>> = Data,
    {Size + 1,
     <<IndexesPart0:Offset/binary-unit:8,
       Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       IndexesPart1New/binary>>,
     <<DataPart0:Index/binary-unit:8,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       DataPart1/binary>>}.

indexes_adjust(<<>> = Indexes, _) ->
    Indexes;
indexes_adjust(Indexes, 0) ->
    Indexes;
indexes_adjust(Indexes, Difference) ->
    indexes_adjust(<<>>, Indexes, Difference).

indexes_adjust(IndexesNew, <<>>, _) ->
    IndexesNew;
indexes_adjust(IndexesNew,
               <<IndexOld?INDEX_SIZE_TYPE,
                 KeySize?KEY_SIZE_TYPE,
                 ValueSize?VALUE_SIZE_TYPE,
                 IndexesOld/binary>>,
               Difference) ->
    IndexNew = IndexOld + Difference,
    indexes_adjust(<<IndexesNew/binary,
                     IndexNew?INDEX_SIZE_TYPE,
                     KeySize?KEY_SIZE_TYPE,
                     ValueSize?VALUE_SIZE_TYPE>>,
                   IndexesOld,
                   Difference).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

-endif.

