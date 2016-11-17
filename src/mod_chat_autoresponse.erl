%%%-------------------------------------------------------------------
%%% @author Vishal Singh <vishal@roposo.com>
%%% @copyright (C) 2016, Vishal Singh
%%% @doc
%%%
%%% @end
%%% Created : 25 Oct 2016 by Vishal Singh <vishal@roposo.com>
%%%-------------------------------------------------------------------

-module(mod_chat_autoresponse).

-behavior(gen_mod).

%% API

-export([start/2, stop/1, depends/2, mod_opt_type/1, on_offline_message_hook/3, route_auto_reply/6,
    on_user_send_packet/4, route_auto_accept_chat_request/2]).

-include("logger.hrl").
-include_lib("ejabberd.hrl").
-include_lib("jlib.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, on_offline_message_hook, 12),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 16),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, on_offline_message_hook, 12),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 16),
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(auto_reply_url_post) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [auto_reply_url_post].

on_offline_message_hook(From, To, Packet) ->
    {_Xmlel, _Type, _Details, _Body} = Packet,
    case _Type of
        <<"message">> ->
            Body = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
            case Body of
                <<>> ->
                    ok;
                _ ->
                    {BodyJSON} = jiffy:decode(Body),
                    {BodyBlock} = proplists:get_value(<<"block">>, BodyJSON),
                    MessageType = proplists:get_value(<<"ty">>, BodyBlock),
                    ?INFO_MSG("Message type: ~p", [binary_to_list(MessageType)]),
                    case MessageType of
                        <<"cs_rp">> ->
                            SSI = proplists:get_value(<<"ssi">>, BodyBlock),
                            case SSI of
                                true ->
                                    ID = fxml:get_tag_attr_s(<<"id">>, Packet),
                                    ?INFO_MSG("Initiating async task to send auto reply...", []),
                                    Key = route_auto_reply_async(BodyBlock, To, From, ID, auto_reply_url_post, MessageType),
                                    ?INFO_MSG("Async task initiated to send auto reply (key: ~p)!", [Key]);
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end
            end;
        _ ->
          ok
    end.

route_auto_reply(BodyBlock, From, To, ID, PostUrlConfig, MessageType) ->
    PostUrl = binary_to_list(gen_mod:get_module_opt(From#jid.lserver, ?MODULE, PostUrlConfig, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
    UserP = string:concat("user=", binary_to_list(From#jid.luser)),
    MsgTyP = string:concat("msgtype=", binary_to_list(MessageType)),
    case MessageType of
        <<"cr_cp">> ->
            PayAmt = proplists:get_value(<<"pay">>, BodyBlock),
            PayAmtP = string:concat("pay=", binary_to_list(PayAmt)),
            Data = string:join([UserP, MsgTyP, PayAmtP], "&");
        _ ->
            Data = string:join([UserP, MsgTyP], "&")
    end,
    ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
    Response = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
    case Response of
        {ok, {_, _, ResponseBody}} ->
            ?INFO_MSG("Response received: {ok, ~s}", [ResponseBody]),
            {ResponseJSON} = jiffy:decode(ResponseBody),
            {ChatBodyJSONData} = proplists:get_value(<<"data">>, ResponseJSON),
            case proplists:lookup(<<"block">>, ChatBodyJSONData) of
                none -> ok;
                _ ->
                    ChatBody = jiffy:encode({ChatBodyJSONData}),
                    route_auto_reply_to_both(ChatBody, From, To, ID)
            end;
        {error, {ErrorReason, _}} -> ?ERROR_MSG("Response received: {error, ~s}", [ErrorReason]);
        _ -> ok
    end.

on_user_send_packet(Packet, _, From, To) ->
    {_Xmlel, _Type, _Details, _Body} = Packet,
    case _Type of
        <<"message">> ->
            Body = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
            case Body of
                <<>> ->
                  ok;
                _ ->
                  {BodyJSON} = jiffy:decode(Body),
                  {BodyBlock} = proplists:get_value(<<"block">>, BodyJSON),
                  MessageType = proplists:get_value(<<"ty">>, BodyBlock),
                  ?INFO_MSG("Message type: ~p", [binary_to_list(MessageType)]),
                  case MessageType of
                      <<"cr_cp">> ->
                          ID = fxml:get_tag_attr_s(<<"id">>, Packet),
                          ?INFO_MSG("Initiating async task to send auto reply...", []),
                          Key = route_auto_reply_async(BodyBlock, To, From, ID, auto_reply_url_post, MessageType),
                          ?INFO_MSG("Async task initiated to send auto reply (key: ~p)!", [Key]);
                      <<"cr_pm">> ->
                          ID = fxml:get_tag_attr_s(<<"id">>, Packet),
                          ?INFO_MSG("Initiating async task to send auto reply...", []),
                          Key = route_auto_reply_async(BodyBlock, To, From, ID, auto_reply_url_post, MessageType),
                          ?INFO_MSG("Async task initiated to send auto reply (key: ~p)!", [Key]);
                      _ ->
                          ok
                  end
            end;
        _ ->
          ok
    end,
    Packet.

route_auto_accept_chat_request(From, To) ->
    XmlPacket1 = #xmlel{name = <<"presence">>,
                        attrs = [{<<"to">>, jid:to_string(To)},
                                 {<<"type">>, <<"subscribed">>}]},
    Result = ejabberd_router:route(From, To, XmlPacket1),
    case Result of
        ok ->
            XmlPacket2 = #xmlel{name = <<"presence">>,
                                attrs = [{<<"to">>, jid:to_string(To)},
                                         {<<"type">>, <<"subscribe">>}]},
            ejabberd_router:route(From, To, XmlPacket2);
        _ -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

route_auto_reply_async(BodyBlock, From, To, ID, PostUrlConfig, MessageType) ->
    Key = rpc:async_call(node(), mod_chat_autoresponse, route_auto_reply, [BodyBlock, From, To, ID, PostUrlConfig, MessageType]),
    Key.

route_auto_reply_to_both(Body, From, To, ID) ->
    IDR = list_to_binary(binary_to_list(ID) ++ "_auto-reply"),
    XmlReply = #xmlel{name = <<"message">>,
                      attrs = [{<<"from">>, jid:to_string(From)}, {<<"to">>, jid:to_string(To)}, {<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"chat">>}, {<<"id">>, IDR}],
                      children = [#xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}]},
    Server = From#jid.lserver,
    TimestampTag = gen_mod:get_module_opt(Server, mod_chat_interceptor, timestamp_tag, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    XmlPacket = jlib:add_delay_info(XmlReply, Server, erlang:timestamp(), TimestampTag),
    ejabberd_router:route(From, To, XmlPacket),
    ejabberd_router:route(To, From, XmlPacket).
