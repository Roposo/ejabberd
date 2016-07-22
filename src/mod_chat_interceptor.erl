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
-export([task/0, task_chat/1, start/2, stop/1, to_a_text_file/1, chat_to_text_file/2]).

%-export([on_filter_packet/1, post_to_server/5]).
-export([on_filter_packet/1, post_to_server/6, on_filter_group_chat_packet/5, on_filter_group_chat_presence_packet/5, on_update_presence/3]).

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
  ejabberd_hooks:add(muc_filter_presence, Host, ?MODULE, on_filter_group_chat_presence_packet, 1),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, on_filter_packet, 2),
  ejabberd_hooks:add(c2s_update_presence, Host, ?MODULE, on_update_presence, 3),
%%  capture packets received by user
%%  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, task, 50),
  ok.

stop(Host) ->
  % ?INFO_MSG("chat logging is stopping", []),
  % delete packets sent by user
  ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, on_filter_group_chat_packet, 0),
  ejabberd_hooks:delete(muc_filter_presence, Host, ?MODULE, on_filter_group_chat_presence_packet, 1),
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, task, 2),
  ejabberd_hooks:delete(c2s_update_presence, Host, ?MODULE, on_update_presence, 3),
  % delete packets received by user
  %ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, task, 50),
  ok.

on_update_presence(Packet, User, Server) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** c2s_update_presence ******\n"),
  Packet.

on_filter_group_chat_presence_packet(Packet, MUCState, RoomJID, From, FromNick) ->
  on_filter_group_chat_presence_packet_helper(Packet, RoomJID, From, FromNick).
%  Packet.

on_filter_group_chat_presence_packet_helper(Packet, RoomJID, From, FromNick) ->
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
      end,
      update_pub_sub_node(RoomJID, From, Type)
  end,
  case string:equal(ResponseBody, "Success") of
    true ->
      chat_to_text_file(File, "\n****** Processing presence packet and exiting muc_filter_presence ******\n"),
      Packet;
    false ->
      chat_to_text_file(File, "\n****** Skipping presence packet and exiting muc_filter_presence ******\n")
  end.

update_pub_sub_node(RoomJID, From, Type) ->
%  Do something!
  RoomJID.

on_filter_group_chat_packet(Packet, MUCState, RoomJID, From, FromNick) ->
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
  ?INFO_MSG("*** in filter packet ***", []),
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** entering on_filter_packet ******\n"),
  task_chat(Packet).
%  Packet.

task_chat({From, To, XmlP} = Packet) ->
  File = "chat_history.log",
  chat_to_text_file(File, "\n****** entering task_chat ******\n"),

  XmlStr = binary_to_list(fxml:element_to_binary(XmlP)),
  ?INFO_MSG("XML begins:", []),
  ?INFO_MSG("~p", [XmlStr]),
  ?INFO_MSG("XML ends!", []),
  chat_to_text_file(File, "\nXML begins:\n"),
  chat_to_text_file(File, XmlStr),
  chat_to_text_file(File, "\nXML ends!\n"),

  ToS = element(2, To),
  FromS = element(2, From),
  ?INFO_MSG("Message from: ~p", [binary_to_list(FromS)]),
  ?INFO_MSG("Message to: ~p", [binary_to_list(ToS)]),
  X = string:concat("\nFrom: ", FromS),
  Y = string:concat("\nTo: ", ToS),
  chat_to_text_file(File, X),
  chat_to_text_file(File, Y),
  STB = fxml:get_subtag(XmlP, <<"body">>),
  case STB of
    false ->
      Body = <<"">>;
    _ ->
      Body = fxml:get_tag_cdata(STB)
  end,
%  Body = fxml:get_tag_cdata(fxml:get_subtag(XmlP, <<"body">>)),
  ?INFO_MSG("Message body: ~p~n~n", [binary_to_list(Body)]),
  Z = string:concat("\nMessage: ", Body),
  chat_to_text_file(File, Z),

  Type = binary_to_list(fxml:get_tag_attr_s(<<"type">>, XmlP)),
  case string:equal(Type, "groupchat") of
    true ->
      ToInXml = fxml:get_tag_attr_s(<<"to">>, XmlP),
      ID = fxml:get_tag_attr_s(<<"id">>, XmlP),
      ResponseBody = post_to_server(FromS, ToS, Body, Type, ToInXml, ID),
      ProcessPacket = string:equal(ResponseBody, "Success");
    false ->
      ProcessPacket = true
  end,
  case ProcessPacket of
    true ->
      chat_to_text_file(File, "\n****** Processing packet and exiting task_chat ******\n"),
      Packet;
    false ->
      chat_to_text_file(File, "\n****** Skipping packet and exiting task_chat ******\n")
  end.

get_to_list_from_json_string(Body) ->
%  File = "chat_history.log",
  Map = jiffy:decode(Body, [return_maps]),
  DetMap = maps:get(<<"det">>, Map),
  MapArray = maps:get(<<"to">>, DetMap),
  ToList = get_nickname_list_from_map_array(MapArray),
  ToList.

get_nickname_list_from_map_array([Head|Rest]) ->
  HeadAsList = binary_to_list(maps:get(<<"nickname">>, Head)),
  lists:append([HeadAsList], get_nickname_list_from_map_array(Rest));
get_nickname_list_from_map_array([]) ->
  [].

%get_text_from_json_string(Body) ->
%  Map = jiffy:decode(Body, [return_maps]),
%  DetMap = maps:get(<<"det">>, Map),
%  Text = binary_to_string(maps:get(<<"text">>, DetMap)),
%  Text.

chat_to_text_file(File, Data) ->
  EnableLogging = true,
  case EnableLogging of
    true ->
      {ok, IO} = file:open(File, [append]),
      file:write(IO, Data),
%      file:write(IO, "\n"),
      file:close( IO );
    false ->
      LTB = list_to_binary(File)
  end.

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
  {Flag, {_, _, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Data}, HTTPOptions, Options),
  File = "chat_history.log",
  case Flag of
    ok ->
      chat_to_text_file(File, "\n****** HTTP request response \"ok\" ******\n"),
      chat_to_text_file(File, "\n****** HTTP request response body start ******\n"),
      chat_to_text_file(File, ResponseBody),
      chat_to_text_file(File, "\n****** HTTP request response body end ******\n");
    error ->
      chat_to_text_file(File, "\n****** HTTP request response \"error\" ******\n")
  end,
  ResponseBody.

