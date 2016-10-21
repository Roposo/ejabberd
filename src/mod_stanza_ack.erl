%%%----------------------------------------------------------------------
%%% File    : mod_stanza_ack.erl
%%% Author  : Kay Tsar <kay@mingism.com>
%%% Purpose : Message Receipts XEP-0184 0.5
%%% Created : 25 May 2013 by Kay Tsar <kay@mingism.com>
%%% Usage   : Add the following line in modules section of ejabberd.cfg:
%%%              {mod_stanza_ack,  [{host, "zilan"}]}
%%%
%%%
%%% Copyright (C) 2013-The End of Time   Mingism
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_stanza_ack).

-behaviour(gen_mod).


-include("logger.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

-type host()    :: string().
-type name()    :: string().
-type value()   :: string().
-type opts()    :: [{name(), value()}, ...].

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).
%-define(EJABBERD_DEBUG, true).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).
-export([on_user_send_packet/4, check_should_acknowledge_and_send_ack_response/5]).

-spec start(host(), opts()) -> ok.
start(Host, Opts) ->
	mod_disco:register_feature(Host, ?NS_RECEIPTS),
	ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 12),
	ok.

-spec stop(host()) -> ok.
stop(Host) ->
	ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 12),
	ok.

on_user_send_packet(Packet, _C2SState, From, To) ->
    Key = check_should_acknowledge_and_send_ack_response_async(From, To, Packet, From#jid.lserver, From),
%    ?INFO_MSG("Async task initiated to send acknowledgement packet (key: ~p)!", [Key]),
    Packet.

check_should_acknowledge_and_send_ack_response(From, To, Packet, RegisterFromJid, RegisterToJid) ->
    case should_acknowledge(Packet) of 
        S when (S==true) ->
            send_ack_response(From, To, Packet, RegisterFromJid, RegisterToJid);
        false ->
            ok
    end,
    Packet.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_should_acknowledge_and_send_ack_response_async(From, To, Packet, RegisterFromJid, RegisterToJid) ->
    Key = rpc:async_call(node(), mod_stanza_ack, check_should_acknowledge_and_send_ack_response, [From, To, Packet, RegisterFromJid, RegisterToJid]),
    Key.

should_acknowledge(#xmlel{name = <<"message">>} = Packet) ->
    case {fxml:get_tag_attr_s(<<"type">>, Packet),
          fxml:get_subtag_cdata(Packet, <<"body">>)} of
        {<<"error">>, _} ->
            false;
        {_, <<>>} ->
            %% Empty body
            false;
        _ ->
            true
    end;
should_acknowledge(#xmlel{}) ->
    false.

send_ack_response(From, To, Packet, RegisterFromJid, RegisterToJid) ->
    ReceiptId = fxml:get_tag_attr_s(<<"id">>, Packet),
%    SentTo = jlib:jid_to_string(To),
    XmlBody = #xmlel{name = <<"message">>,
                     attrs = [{<<"from">>, RegisterFromJid}, {<<"to">>, jlib:jid_to_string(RegisterToJid)}],
                     children = [#xmlel{name = <<"received">>,
                                        attrs = [{<<"xmlns">>, ?NS_RECEIPTS}, {<<"id">>, ReceiptId}],
              				children = []}]},
    TimeStamp = fxml:get_path_s(Packet, [{elem, <<"delay">>}, {attr, <<"stamp">>}]),
    case TimeStamp of
      <<>> ->
        XmlN = jlib:add_delay_info(XmlBody, From#jid.lserver, erlang:timestamp(), <<"Chat Acknowledgement">>);
      _ ->
        TimeStampValue = jlib:datetime_string_to_timestamp(TimeStamp),
        XmlN = jlib:add_delay_info(XmlBody, From#jid.lserver, TimeStampValue, <<"Chat Acknowledgement">>)
    end,
    ejabberd_router:route(jlib:string_to_jid(RegisterFromJid), RegisterToJid, XmlN).
