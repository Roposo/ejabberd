%%%-------------------------------------------------------------------
%%% @author tarunkumar
%%% @copyright (C) 2015, <ROPOSO>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2015 7:44 PM
%%%-------------------------------------------------------------------
-module(mod_chat_interceptor).
-author("tarunkumar").

-behavior(gen_mod).
%% Required by ?INFO_MSG macros
-include("logger.hrl").
%-include("lager.hrl").
-include_lib("ejabberd.hrl").
-include_lib("jlib.hrl").
%-ifdef(NO_EXT_LIB).
%-include("fxml.hrl").
%-else.
%-include_lib("fast_xml/include/fxml.hrl").
%-endif.
-include("mod_muc_room.hrl").
%-include_lib("ejabberd/include/jlib.hrl").
%-include_lib("jiffy_util.hrl").

%% API
% Log chat program
% It is based on user comments
-export([task/0, task_chat/1, start/2, stop/1, to_a_text_file/1, chat_to_text_file/2, add_timestamp/3,
  send_push_notification_to_user/5, update_vcard/2, depends/2, mod_opt_type/1, auto_accept_chat_request/3]).

%-export([on_filter_packet/1, post_to_server/5]).
-export([on_filter_packet/1, post_to_server/6, on_filter_group_chat_packet/5, on_filter_group_chat_presence_packet/5,
  on_update_presence/3, on_user_send_packet/4, on_user_receive_packet/5]).

%-record(state, {config}).

%-define(INFO_MSG, "custom_plugin_ROPOSO").

-ifdef(TEST).
-compile(export_all).
-endif.

to_a_text_file( File ) ->
  {ok, IO} = file:open( File, [append] ),
  file:write(IO, "This is new text sent by chatter!\n"),
  file:close( IO ).

task() ->
  File = "chat_history.log",
  to_a_text_file( File ).

start(Host, _Opts) ->
%%  task(),
%%  capture packets sent by user
  ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, on_filter_group_chat_packet, 0),
%%  ejabberd_hooks:add(muc_filter_presence, Host, ?MODULE, on_filter_group_chat_presence_packet, 1),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 2),
%%  ejabberd_hooks:add(c2s_update_presence, Host, ?MODULE, on_update_presence, 3),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_user_send_packet, 10),
%%  capture packets received by user
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, on_user_receive_packet, 100),
%%  ejabberd_hooks:add(privacy_iq_set, Host, ?MODULE, process_iq_set, 27),
  ok.

stop(Host) ->
  % ?INFO_MSG("chat logging is stopping", []),
  % delete packets sent by user
  ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, on_filter_group_chat_packet, 0),
%%  ejabberd_hooks:delete(muc_filter_presence, Host, ?MODULE, on_filter_group_chat_presence_packet, 1),
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, task, 2),
%%  ejabberd_hooks:delete(c2s_update_presence, Host, ?MODULE, on_update_presence, 3),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_user_send_packet, 10),
  % delete packets received by user
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, on_user_receive_packet, 100),
%%  ejabberd_hooks:delete(privacy_iq_set, Host, ?MODULE, process_iq_set, 27),
  ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(push_url_post) -> fun iolist_to_binary/1;
mod_opt_type(vcard_url_post) -> fun iolist_to_binary/1;
mod_opt_type(vcard_image_type) -> fun iolist_to_binary/1;
mod_opt_type(block_url_post) -> fun iolist_to_binary/1;
mod_opt_type(unblock_url_post) -> fun iolist_to_binary/1;
mod_opt_type(block_unblock_token) -> fun iolist_to_binary/1;
mod_opt_type(block_list_url_post) -> fun iolist_to_binary/1;
mod_opt_type(timestamp_tag) -> fun iolist_to_binary/1;
mod_opt_type(auto_accept_check_url_post) -> fun iolist_to_binary/1;
mod_opt_type(_) -> [push_url_post, vcard_url_post, vcard_image_type, block_url_post, unblock_url_post,
  block_unblock_token, block_list_url_post, timestamp_tag, auto_accept_check_url_post].

add_timestamp(Pkt, LServer, TimestampTag) ->
%  ?INFO_MSG("**************** Adding timestamp to packet ****************", []),
  jlib:add_delay_info(Pkt, LServer, erlang:timestamp(), TimestampTag).

send_push_notification_to_user(From, To, Body, Type, Server) ->
  PostUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, push_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
  ToP = string:concat("to=", To),
  FrP = string:concat("from=", From),
  BoP = string:concat("body=", Body),
  TyP = string:concat("type=", Type),
  Data = string:join([ToP, FrP, BoP, TyP], "&"),
  ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
%  {Flag, {_, _, ResponseBody}} = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  Response = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  case Response of
    {ok, {_, _, ResponseBody}} -> ?INFO_MSG("Response received: {ok, ~s}", [ResponseBody]);
    {error, {ErrorReason, _}} -> ?ERROR_MSG("Response received: {error, ~s}", [ErrorReason]);
    _ -> ok
  end,
  ok.

update_vcard(User, Server) ->
%  ?INFO_MSG("Update VCard called with arguments: ~p, ~p", [binary_to_list(User), binary_to_list(Server)]),
  PostUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, vcard_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
  ImageTypeValue = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, vcard_image_type, fun(S) -> iolist_to_binary(S) end, list_to_binary("raw"))),
  UserP = string:concat("user=", binary_to_list(User)),
  ImageType = string:concat("img_type=", ImageTypeValue),
  Data = string:join([UserP, ImageType], "&"),
  ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
%  {Flag, {_, _, ResponseBody}} = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  Response = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  case Response of
    {ok, {_, _, ResponseBody}} ->
      ?INFO_MSG("Response received: {ok, ~s}", [ResponseBody]),
      VCard = fxml_stream:parse_element(list_to_binary(ResponseBody)),
%      ?INFO_MSG("Converted string to vcard successfully: ~p", [binary_to_list(fxml:element_to_binary(VCard))]),
      mod_vcard:set_vcard(User, Server, VCard);
    {error, {ErrorReason, _}} -> ?ERROR_MSG("Response received: {error, ~s}", [ErrorReason]);
    _ -> ok
  end,
  ok.

update_vcard_of_user(User, Server) ->
  Key = rpc:async_call(node(), mod_chat_interceptor, update_vcard, [User, Server]),
  Key.

should_auto_accept(From, To, Server) ->
  PostUrl = binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, auto_accept_check_url_post, fun(S) -> iolist_to_binary(S) end, <<"">>)),
  FromP = string:concat("sender=", binary_to_list(From#jid.luser)),
  ToP = string:concat("receiver=", binary_to_list(To#jid.luser)),
  Data = string:join([FromP, ToP], "&"),
  ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
  Response = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  case Response of
    {ok, {_, _, ResponseBody}} ->
      ?INFO_MSG("Response received: {ok, ~s}", [ResponseBody]),
      case ResponseBody of
        "true" -> true;
        _ -> false
      end;
    {error, {ErrorReason, _}} ->
      ?ERROR_MSG("Response received: {error, ~s}", [ErrorReason]),
      false;
    _ -> false
  end.

auto_accept_chat_request(From, To, Server) ->
  case should_auto_accept(From, To, Server) of
    true -> mod_chat_autoresponse:route_auto_accept_chat_request(To, From);
    _ -> ok
  end.

auto_accept_chat_request_async(From, To, Server) ->
  Key = rpc:async_call(node(), mod_chat_interceptor, auto_accept_chat_request, [From, To, Server]),
  Key.

on_user_send_packet(Pkt, _, _, To) ->
  XmlP = Pkt,
  {_Xmlel, _Type, _Details, _Body} = XmlP,
  case _Type of
    <<"message">> ->
      Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
      case Type of
        <<"chat">> ->
          Body = fxml:get_path_s(XmlP, [{elem, list_to_binary("body")}, cdata]),
          case Body of
            <<"">> ->
              XmlN = XmlP;
            _ ->
              Server = To#jid.lserver,
              TimestampTag = gen_mod:get_module_opt(Server, ?MODULE, timestamp_tag, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
              XmlN = add_timestamp(XmlP, Server, TimestampTag)
%              ?INFO_MSG("**************** Added timestamp to packet, new packet: ~p ****************", [binary_to_list(fxml:element_to_binary(XmlN))])
          end;
        _ ->
          XmlN = XmlP
      end;
    _ ->
      XmlN = XmlP
  end,
  XmlN.

on_user_receive_packet(Pkt, _, _, From, To) ->
  XmlP = Pkt,
  case XmlP of
    {_Xmlel, _Type, _Details, _Body} ->
      case _Type of
        <<"presence">> ->
          Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
          case Type of
            <<"subscribe">> ->
              ?INFO_MSG("********************* Subscribe packet from ~s to ~s is ~s *********************",
                [From#jid.luser, To#jid.luser, fxml:element_to_binary(XmlP)]),
              case should_auto_accept(From, To, From#jid.lserver) of
                true ->
                  #xmlel{name = Name, attrs = Attrs, children = Els} = XmlP,
                  NewEls = [#xmlel{name = <<"autoaccept">>, children = [{xmlcdata, <<"true">>}]} | Els],
                  XmlN = #xmlel{name = Name, attrs = Attrs, children = NewEls};
                false ->
                  XmlN = XmlP
              end;
            _ ->
              XmlN = XmlP
          end;
        _ ->
          XmlN = XmlP
      end;
    _ ->
      XmlN = XmlP
  end,
  XmlN.

on_update_presence(Packet, _, _) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** c2s_update_presence ******\n"),
  Packet.

on_filter_group_chat_presence_packet(Packet, _, RoomJID, From, FromNick) ->
  on_filter_group_chat_presence_packet_helper(Packet, RoomJID, From, FromNick).
%  Packet.

on_filter_group_chat_presence_packet_helper(Packet, _, _, FromNick) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** entering muc_filter_presence ******\n"),
  XmlP = Packet,

  XmlStr = binary_to_list(fxml:element_to_binary(XmlP)),
  chat_to_text_file(File, "\n****** Presence XML starts ******\n"),
  chat_to_text_file(File, XmlStr),
  chat_to_text_file(File, "\n****** Presence XML ends ******\n"),

  To = fxml:get_tag_attr_s(<<"to">>, XmlP),
  case To of
    <<>> ->
      ResponseBody = "Success";
    _ ->
      Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
      case Type of
        <<"unavailable">> ->
          ResponseBody = post_to_server(FromNick, To, <<"leave">>, "presence", To, <<"Sample">>);
        _ ->
          ResponseBody = post_to_server(FromNick, To, <<"join">>, "presence", To, <<"Sample">>)
      end
%      update_pub_sub_node(RoomJID, From, Type)
  end,
  case string:equal(ResponseBody, "Success") of
    true ->
      chat_to_text_file(File, "\n****** Processing presence packet and exiting muc_filter_presence ******\n"),
      Packet;
    false ->
      chat_to_text_file(File, "\n****** Skipping presence packet and exiting muc_filter_presence ******\n")
  end.

%update_pub_sub_node(RoomJID, _, _) ->
%  Do something!
%  RoomJID.

on_filter_group_chat_packet(Packet, _, _, _, _) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** inside muc_filter_message ******\n"),
%  on_filter_packet(Packet).
%  ToS = element(2, To),
%  FromS = element(2, From),
%  Body = fxml:get_tag_cdata(fxml:get_subtag(XmlP, <<"body">>)),
%  X = string:concat("\n****** from= ", FromS),
%  Y = string:concat("\n****** to= ", ToS),
%  Z = string:concat("\n****** message=", Body),
%  chat_to_text_file(File, X),
%  chat_to_text_file(File, Y),
%  chat_to_text_file(File, Z),
%  chat_to_text_file(File, "\n****** exiting muc_filter_message ******\n"),
  Packet.

on_filter_packet(Packet) ->
  ?INFO_MSG("************ Packet processing starts ************", []),
  PacketN = task_chat(Packet),
  ?INFO_MSG("************ Packet processing ends ************~n~n", []),
  PacketN.

task_chat({From, To, XmlP}) ->
  XmlStr = binary_to_list(fxml:element_to_binary(XmlP)),
  ?INFO_MSG("XML begins:", []),
  ?INFO_MSG("~p", [XmlStr]),
  ?INFO_MSG("XML ends!", []),
  {_Xmlel, _Type, _Details, _Body} = XmlP,
  ?INFO_MSG("Packet type: ~p", [binary_to_list(_Type)]),
  FromS = element(2, From),
  ?INFO_MSG("Packet from: ~p", [binary_to_list(FromS)]),
  ToS = element(2, To),
  ?INFO_MSG("Packet to: ~p", [binary_to_list(ToS)]),
  case _Type of
    <<"message">> ->
      XmlN = XmlP,
      STB = fxml:get_subtag(XmlP, <<"body">>),
      case STB of
        false ->
%          Body = <<"">>,
%          XmlN = XmlP,
          ProcessPacket = true;
%          ToN = To;
        _ ->
          Body = fxml:get_tag_cdata(STB),
          ?INFO_MSG("Message body: ~p", [binary_to_list(Body)]),
          Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
          case Type of
            <<"groupchat">> ->
               ProcessPacket = false;
%              ToInXml = fxml:get_tag_attr_s(<<"to">>, XmlP),
%              ID = fxml:get_tag_attr_s(<<"id">>, XmlP),
%              ResponseBody = post_to_server(FromS, ToS, Body, Type, ToInXml, ID),
%              ProcessPacket = string:equal(ResponseBody, "Success"),
%              ToN = To;
            _ ->
              ProcessPacket = true
          end
      end;
    <<"iq">> ->
      ProcessPacket = true,
%      ToN = To,
      Type = fxml:get_tag_attr_s(<<"type">>, XmlP),
      case Type of
        <<"set">> ->
          Ask = fxml:get_path_s(XmlP, [{elem, list_to_binary("query")}, {elem, list_to_binary("item")}, {attr, list_to_binary("ask")}]),
          case Ask of
            <<"subscribe">> ->
              Subscription = fxml:get_path_s(XmlP, [{elem, list_to_binary("query")}, {elem, list_to_binary("item")}, {attr, list_to_binary("subscription")}]),
              ToJID = binary_to_list(fxml:get_path_s(XmlP, [{elem, list_to_binary("query")}, {elem, list_to_binary("item")}, {attr, list_to_binary("jid")}])),
              ToUid = lists:nth(1, string:tokens(ToJID, "@")),
              ToServer = lists:nth(2, string:tokens(ToJID, "@")),
              case Subscription of
                <<"none">> ->
                  send_push_notification_to_user(binary_to_list(FromS), ToUid, "Chat invitation!", "subscribe_request", list_to_binary(ToServer)),
                  Key = update_vcard_of_user(list_to_binary(ToUid), list_to_binary(ToServer)),
                  ?INFO_MSG("Async task initiated to update VCard (key: ~p)!", [Key]),
                  KeyAutoAccept = auto_accept_chat_request_async(From, jid:from_string(list_to_binary(ToJID)), list_to_binary(ToServer)),
                  ?INFO_MSG("Async task initiated to auto-accept chat request if required (key: ~p)!", [KeyAutoAccept]);
                <<"from">> ->
                  send_push_notification_to_user(binary_to_list(FromS), ToUid, "Chat acceptance!", "subscribe_accept", list_to_binary(ToServer));
                _ ->
                  ok
              end;
            _ ->
              ok
          end,
          VCard = fxml:get_path_s(XmlP, [{elem, list_to_binary("vCard")}]),
          case VCard of
            <<>> ->
              XmlN = XmlP;
            _ ->
              VCardPhotoType = fxml:get_path_s(VCard, [{elem, list_to_binary("PHOTO")}, {elem, list_to_binary("TYPE")}, cdata]),
              case VCardPhotoType of
                <<>> ->
%                  ImageTypeValue = list_to_binary(string:concat("image/", binary_to_list(gen_mod:get_module_opt(Server, ?MODULE, vcard_image_type, fun(S) -> iolist_to_binary(S) end, list_to_binary("url"))))),
                  ImageTypeValue = <<"image/url">>,
                  VCardPhotoXml = fxml:get_path_s(VCard, [{elem, list_to_binary("PHOTO")}]),
                  VCardPhotoXmlN = fxml:append_subtags(VCardPhotoXml, [#xmlel{name = <<"TYPE">>, children = [{xmlcdata, ImageTypeValue}]}]),
                  VCardN = fxml:replace_subtag(VCardPhotoXmlN, VCard),
                  XmlN = fxml:replace_subtag(VCardN, XmlP),
%                  XmlStrN = binary_to_list(fxml:element_to_binary(XmlN)),
                  ?INFO_MSG("VCard being set to: ~p", [binary_to_list(fxml:element_to_binary(VCardN))]);
                _ ->
                  XmlN = XmlP
              end
          end;
        _ ->
          XmlN = XmlP
      end;
    _ ->
      XmlN = XmlP,
      ProcessPacket = true,
%      ToN = To,
      ok
  end,
  ?INFO_MSG("New XML: ~p", [binary_to_list(fxml:element_to_binary(XmlN))]),
  case ProcessPacket of
    true ->
      ?INFO_MSG("Processing packet", []),
%      Packet;
%      {From, ToN, XmlP};
      {From, To, XmlN};
    false ->
      ?INFO_MSG("Skipping packet", [])
  end.

%get_to_list_from_json_string(Body) ->
%  File = "chat_history.log",
%  Map = jiffy:decode(Body, [return_maps]),
%  DetMap = maps:get(<<"det">>, Map),
%  MapArray = maps:get(<<"to">>, DetMap),
%  ToList = get_nickname_list_from_map_array(MapArray),
%  ToList.

%get_nickname_list_from_map_array([Head|Rest]) ->
%  HeadAsList = binary_to_list(maps:get(<<"nickname">>, Head)),
%  lists:append([HeadAsList], get_nickname_list_from_map_array(Rest));
%get_nickname_list_from_map_array([]) ->
%  [].

%get_text_from_json_string(Body) ->
%  Map = jiffy:decode(Body, [return_maps]),
%  DetMap = maps:get(<<"det">>, Map),
%  Text = binary_to_string(maps:get(<<"text">>, DetMap)),
%  Text.

post_to_server(From, To, Body, ChatType, GroupChatFullName, ID) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** Posting to backend server ******\n"),
  application:start(inets),
  Method = post,
  URL = "http://localhost:9000/api",
  Header = [],
  Type = "application/x-www-form-urlencoded",
  F = string:concat("f=", lists:flatten(io_lib:format("~p",[From]))),
  T = string:concat("t=", lists:flatten(io_lib:format("~p",[To]))),
%  B = string:concat("msg=", Body),
  B = string:concat("msg=", lists:flatten(io_lib:format("~p",[Body]))),
  C = string:concat("c=", lists:flatten(io_lib:format("~p",[list_to_binary(ChatType)]))),
  G = string:concat("g=", lists:flatten(io_lib:format("~p",[GroupChatFullName]))),
  I = string:concat("i=", lists:flatten(io_lib:format("~p",[ID]))),
  Data = string:join([F, T, B, C, G, I], "&"),
  chat_to_text_file(File, "Data: "),
  chat_to_text_file(File, Data),
  chat_to_text_file(File, "\n\n"),
  HTTPOptions = [],
  Options = [],
%  {Flag, {_, _, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Data}, HTTPOptions, Options),
  Response = httpc:request(Method, {URL, Header, Type, Data}, HTTPOptions, Options),
  case Response of
    {ok, {_, _, ResponseBody}} ->
      ?INFO_MSG("Response received: {ok, ~s}", [ResponseBody]),
      ResponseBody;
    {error, {ErrorReason, _}} ->
      ?ERROR_MSG("Response received: {error, ~s}", [ErrorReason]),
      ErrorReason;
    _ ->
      ok
  end.
%  File = "chat_history.log",
%  case Flag of
%    ok ->
%      chat_to_text_file(File, "\n****** HTTP request response \"ok\" ******\n"),
%      chat_to_text_file(File, "\n****** HTTP request response body start ******\n"),
%      chat_to_text_file(File, ResponseBody),
%      chat_to_text_file(File, "\n****** HTTP request response body end ******\n");
%    error ->
%      chat_to_text_file(File, "\n****** HTTP request response \"error\" ******\n")
%  end,
%  ResponseBody.

chat_to_text_file(File, Data) ->
  EnableLogging = false,
  case EnableLogging of
    true ->
      {ok, IO} = file:open(File, [append]),
      file:write(IO, Data),
%      file:write(IO, "\n"),
      file:close( IO );
    false ->
      ok
  end.

