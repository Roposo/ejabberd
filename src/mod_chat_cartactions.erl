%%%-------------------------------------------------------------------
%%% @author Vishal Singh <vishal@roposo.com>
%%% @copyright (C) 2016, Vishal Singh
%%% @doc
%%%
%%% @end
%%% Created : 26 Oct 2016 by Vishal Singh <vishal@roposo.com>
%%%-------------------------------------------------------------------

-module(mod_chat_cartactions).

-behavior(gen_mod).

%% API

-export([start/2, stop/1, depends/2, mod_opt_type/1, on_user_send_packet/4, send_cart_action_result_packet/4]).

-include("logger.hrl").
-include_lib("ejabberd.hrl").
-include_lib("jlib.hrl").

-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).

%%%===================================================================
%%% API
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 14),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 14),
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(cart_action_add_url_post) -> fun iolist_to_binary/1;
mod_opt_type(cart_action_remove_url_post) -> fun iolist_to_binary/1;
mod_opt_type(cart_action_oos_url_post) -> fun iolist_to_binary/1;
mod_opt_type(cart_get_url_get) -> fun iolist_to_binary/1;
mod_opt_type(cart_action_token) -> fun iolist_to_binary/1;
mod_opt_type(cart_action_timeout_millis) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [cart_action_add_url_post, cart_action_remove_url_post, cart_action_oos_url_post, cart_get_url_get, cart_action_token, cart_action_timeout_millis].

on_user_send_packet(Packet, _, From, To) ->
    XmlP = Packet,
    {_Xmlel, _Type, _Details, _Body} = XmlP,
    case _Type of
        <<"message">> ->
            Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
            case Type of
                <<"chat">> ->
                    Body = fxml:get_path_s(XmlP, [{elem, list_to_binary("body")}, cdata]),
                    case Body of
                        <<"">> ->
                            ProcessPacket = false;
                        _ ->
                            {BodyJSON} = jiffy:decode(Body),
                            {BodyBlock} = proplists:get_value(<<"block">>, BodyJSON),
                            MessageType = proplists:get_value(<<"ty">>, BodyBlock),
                            ?INFO_MSG("Message type: ~p", [binary_to_list(MessageType)]),
                            if
                                MessageType == <<"cs_sp">>; MessageType == <<"cs_rmp">>; MessageType == <<"cs_oos">> ->
                                    ?INFO_MSG("Cart action packet received: ~s", [fxml:element_to_binary(XmlP)]),
                                    CartActionResponse = cart_action(From, To, BodyJSON, MessageType),
                                    Key = send_cart_action_result_packet_async(From#jid.lserver, From, CartActionResponse, XmlP),
                                    {CartActionResponseJSON} = jiffy:decode(CartActionResponse),
                                    GSCCode = proplists:get_value(<<"gsc">>, CartActionResponseJSON),
                                    case GSCCode of
                                        <<"700">> ->
                                            ProcessPacket = true;
                                        _ ->
                                            ProcessPacket = false
                                    end;
                                MessageType == <<"cr_get">> ->
                                    ?INFO_MSG("Cart action packet received: ~s", [fxml:element_to_binary(XmlP)]),
                                    ProcessPacket = false,
                                    CartGetResponse = cart_get(From, BodyJSON),
                                    case CartGetResponse of
                                        <<"Failure">> ->
                                            ?INFO_MSG("Error getting cart info", []);
                                        _ ->
%                                            ?INFO_MSG("Cart get response: ~p", [binary_to_list(CartGetResponse)]),
                                            send_cart_get_data_packet(From#jid.lserver, From, CartGetResponse, XmlP)
                                    end;
                                true ->
                                    ProcessPacket = true
                            end
                    end;
                _ ->
                    ProcessPacket = true
            end;
        _ ->
            ProcessPacket = true
    end,
    case ProcessPacket of
        true ->
            ?INFO_MSG("Processing packet", []),
            XmlP;
        false ->
            ?INFO_MSG("Skipping packet", [])
    end.

send_cart_action_result_packet(From, To, CartActionResponse, Packet) ->
    ID = fxml:get_tag_attr_s(<<"id">>, Packet),
    IDR = list_to_binary(binary_to_list(ID) ++ "_result"),
    Body = list_to_binary("{\"block\":{\"ty\":\"cr_ack\",\"data\":" ++ binary_to_list(CartActionResponse) ++ "}}"),
    XmlBody = #xmlel{name = <<"message">>,
                     attrs = [{<<"from">>, From}, {<<"to">>, jid:to_string(To)}, {<<"xml:lang">>, <<"en">>}, {<<"id">>, IDR}],
                     children = [#xmlel{name = <<"body">>,
                                        children = [{xmlcdata, Body}]},
                                 #xmlel{name = <<"received">>,
                                        attrs = [{<<"xmlns">>, ?NS_RECEIPTS}, {<<"id">>, ID}],
                                        children = []}]},
    TimeStamp = fxml:get_path_s(Packet, [{elem, <<"delay">>}, {attr, <<"stamp">>}]),
    case TimeStamp of
        <<>> ->
            XmlN = jlib:add_delay_info(XmlBody, From, erlang:timestamp(), <<"Cart Action Acknowledgement">>);
        _ ->
            TimeStampValue = jlib:datetime_string_to_timestamp(TimeStamp),
            XmlN = jlib:add_delay_info(XmlBody, From, TimeStampValue, <<"Cart Action Acknowledgement">>)
    end,
    ejabberd_router:route(jlib:string_to_jid(From), To, XmlN).

%%%===================================================================
%%% Internal functions
%%%===================================================================

cart_action(From, To, BodyJSON, Action) ->
    {BodyBlock} = proplists:get_value(<<"block">>, BodyJSON),
    ProductId = proplists:get_value(<<"id">>, BodyBlock),
    Server = From#jid.lserver,
    Token = gen_mod:get_module_opt(Server, ?MODULE, cart_action_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Timeout = gen_mod:get_module_opt(Server, ?MODULE, cart_action_timeout_millis, fun(S) -> iolist_to_binary(S) end, 5000),
    case Action of
        <<"cs_sp">> ->
            Buyer = From#jid.luser,
            Seller = To#jid.luser,
            {BodyDet} = proplists:get_value(<<"det">>, BodyJSON),
            ProductJSON = jiffy:encode(proplists:get_value(ProductId, BodyDet)),
            CartActionAddUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, cart_action_add_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
            Data = jiffy:encode({[{<<"usereid">>,Buyer},{<<"sellereid">>,Seller},{<<"sdata">>,ProductJSON},{<<"token">>,Token}]}),
            ?INFO_MSG("Sending post request to ~s with body \"~s\"", [CartActionAddUrl, Data]),
            Response = httpc:request(post, {CartActionAddUrl, [{"Content-Type", "application/json"}], "application/json", Data}, [{timeout, Timeout}], []);
        <<"cs_rmp">> ->
            Buyer = From#jid.luser,
            Seller = To#jid.luser,
            CartActionRemoveUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, cart_action_remove_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
            Data = jiffy:encode({[{<<"usereid">>,Buyer},{<<"sellereid">>,Seller},{<<"seid">>,ProductId},{<<"token">>,Token}]}),
            ?INFO_MSG("Sending post request to ~s with body \"~s\"", [CartActionRemoveUrl, Data]),
            Response = httpc:request(post, {CartActionRemoveUrl, [{"Content-Type", "application/json"}], "application/json", Data}, [{timeout, Timeout}], []);
        <<"cs_oos">> ->
            Buyer = To#jid.luser,
            Seller = From#jid.luser,
            CartActionOOSUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, cart_action_oos_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
            Data = jiffy:encode({[{<<"usereid">>,Buyer},{<<"sellereid">>,Seller},{<<"seid">>,ProductId},{<<"token">>,Token}]}),
            ?INFO_MSG("Sending post request to ~s with body \"~s\"", [CartActionOOSUrl, Data]),
            Response = httpc:request(post, {CartActionOOSUrl, [{"Content-Type", "application/json"}], "application/json", Data}, [{timeout, Timeout}], []);
        _ ->
            Response = {error, {<<"Incorrect type for Cart Action">>, Action}}
    end,
    case Response of
        {ok, {_, _, ResponseBody}} ->
            ?INFO_MSG("Response body: ~p", [ResponseBody]);
        {error, {ErrorReason, _}} ->
            ?INFO_MSG("Response received: {error, ~s}", [ErrorReason]),
            ResponseBody = "{\"gsc\":\"600\",\"message\":\"" ++ binary_to_list(ErrorReason) ++ "\"}";
        _ ->
            ResponseBody = "{\"gsc\":\"600\",\"message\":\"Unknown error\"}"
    end,
    list_to_binary(ResponseBody).

cart_get(From, BodyJSON) ->
    {BodyBlock} = proplists:get_value(<<"block">>, BodyJSON),
    Buyer = From#jid.luser,
    Seller = proplists:get_value(<<"uid">>, BodyBlock),
    Server = From#jid.lserver,
    Token = gen_mod:get_module_opt(Server, ?MODULE, cart_action_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Timeout = gen_mod:get_module_opt(Server, ?MODULE, cart_action_timeout_millis, fun(S) -> iolist_to_binary(S) end, 5000),
    CartGetUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, cart_get_url_get, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
    CartGetUrlFull = CartGetUrl ++ "/" ++ binary_to_list(Token) ++ "/" ++ binary_to_list(Buyer) ++ "/" ++ binary_to_list(Seller),
    ?INFO_MSG("Sending get request to ~s", [CartGetUrlFull]),
    Response = httpc:request(get, {CartGetUrlFull, []}, [{timeout, Timeout}], []),
    case Response of
        {ok, {_, _, ResponseBody}} ->
            ?INFO_MSG("Response body: ~p", [ResponseBody]);
        {error, {ErrorReason, _}} ->
            ?INFO_MSG("Response received: {error, ~s}", [ErrorReason]),
            ResponseBody = "{\"gsc\":\"600\",\"message\":\"" ++ binary_to_list(ErrorReason) ++ "\"}";
        _ ->
            ResponseBody = "{\"gsc\":\"600\",\"message\":\"Unknown error\"}"
    end,
    list_to_binary(ResponseBody).

send_cart_action_result_packet_async(From, To, CartActionResponse, Packet) ->
    ?INFO_MSG("Send error packet from ~p to ~p", [binary_to_list(From), binary_to_list(To#jid.luser)]),
    Key = rpc:async_call(node(), mod_chat_cartactions, send_cart_action_result_packet, [From, To, CartActionResponse, Packet]),
    Key.

send_cart_get_data_packet(From, To, Data, Packet) ->
    ID = fxml:get_tag_attr_s(<<"id">>, Packet),
    IDR = list_to_binary(binary_to_list(ID) ++ "_result"),
    Body = list_to_binary("{\"block\":{\"ty\":\"cr_res\",\"data\":" ++ binary_to_list(Data) ++ "}}"),
    XmlBody = #xmlel{name = <<"message">>,
                     attrs = [{<<"from">>, From}, {<<"to">>, jid:to_string(To)}, {<<"xml:lang">>, <<"en">>}, {<<"id">>, IDR}],
                     children = [#xmlel{name = <<"body">>,
                                        children = [{xmlcdata, Body}]},
                                 #xmlel{name = <<"received">>,
                                        attrs = [{<<"xmlns">>, ?NS_RECEIPTS}, {<<"id">>, ID}],
                                        children = []}]},
    TimeStamp = fxml:get_path_s(Packet, [{elem, <<"delay">>}, {attr, <<"stamp">>}]),
    case TimeStamp of
        <<>> ->
            XmlN = jlib:add_delay_info(XmlBody, From, erlang:timestamp(), <<"Cart Info">>);
        _ ->
            TimeStampValue = jlib:datetime_string_to_timestamp(TimeStamp),
            XmlN = jlib:add_delay_info(XmlBody, From, TimeStampValue, <<"Cart Info">>)
    end,
    ejabberd_router:route(jlib:string_to_jid(From), To, XmlN).

