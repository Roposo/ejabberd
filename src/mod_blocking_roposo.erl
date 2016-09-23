%%%-------------------------------------------------------------------
%%% @author Vishal Singh <vishal@roposo.com>
%%% @copyright (C) 2016, Vishal Singh
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2016 by Vishal Singh <vishal@roposo.com>
%%%-------------------------------------------------------------------

-module(mod_blocking_roposo).

-behaviour(mod_blocking).

%% API

-export([process_blocklist_block/3, unblock_by_filter/3,
	 process_blocklist_get/2]).

-include("jlib.hrl").
-include("mod_privacy.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

process_blocklist_block(LUser, LServer, Filter) ->
    List = process_blocklist_get(LUser, LServer),
    UIDList = listitem_list_to_uid_list(List),
    ?INFO_MSG("Old UID list: ~p", [UIDList]),
    NewList = Filter(List),
    NewUIDList = listitem_list_to_uid_list(NewList),
    ?INFO_MSG("New UID list: ~p", [NewUIDList]),
    lists:foreach(fun (UID) ->
                    case lists:member(UID, UIDList) of
                      true ->
                        ok;
                      false ->
                        block_unblock_user(binary_to_list(LUser), UID, "true", LServer)
                    end
                  end,
                  NewUIDList),
    Default = <<"Blocked contacts">>,
    {atomic, {ok, Default, NewList}}.

unblock_by_filter(LUser, LServer, Filter) ->
    List = process_blocklist_get(LUser, LServer),
    UIDList = listitem_list_to_uid_list(List),
    ?INFO_MSG("Old UID list: ~p", [UIDList]),
    NewList = Filter(List),
    NewUIDList = listitem_list_to_uid_list(NewList),
    ?INFO_MSG("New UID list: ~p", [NewUIDList]),
    lists:foreach(fun (UID) ->
                    case lists:member(UID, NewUIDList) of
                      true ->
                        ok;
                      false ->
                        block_unblock_user(binary_to_list(LUser), UID, "false", LServer)
                    end
                  end,
                  UIDList),
    Default = <<"Blocked contacts">>,
    {atomic, {ok, Default, NewList}}.

process_blocklist_get(LUser, LServer) ->
%    PostUrl = "http://localhost:9020/chat/get_block_list",
    PostUrl = binary_to_list(gen_mod:get_module_opt(LServer, mod_chat_interceptor, block_list_url_post, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
    UserP = string:concat("user=", binary_to_list(LUser)),
    Data = string:join([UserP], "&"),
    ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
    {_, {_, _, ResponseBody}} = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
    ?INFO_MSG("Blocked list for ~s: ~s", [LUser, ResponseBody]),
    ResponseLen = string:len(ResponseBody),
    if
      ResponseLen > 2 -> [BlockedUsers] = string:tokens(ResponseBody, "[]");
      true -> BlockedUsers = ""
    end,
%    [BlockedUsers] = string:tokens(ResponseBody, "[]"),
    ?INFO_MSG("Blocked users list: ~p", [BlockedUsers]),
    UIDs = string:tokens(BlockedUsers, ","),
    uid_list_to_listitem_list(UIDs, binary_to_list(LServer)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

uid_list_to_listitem_list([], _) ->
    [];
uid_list_to_listitem_list([UID | RemainingUIDs], Server) ->
    ?INFO_MSG("UID: ~p", [UID]),
%    JID = #jid{user = list_to_binary(UID), server = list_to_binary(Server), resource = <<"">>},
    JID = {list_to_binary(UID), list_to_binary(Server), <<"">>},
    ?INFO_MSG("JID string: ~p", [jid:to_string(JID)]),
    [#listitem{type = jid, action = deny, value = JID, match_all = true} | uid_list_to_listitem_list(RemainingUIDs, Server)].

listitem_list_to_uid_list(List) ->
    lists:map(fun (Item) ->
                #listitem{type = jid, value = {User, _, _}} = Item,
                ?INFO_MSG("UID in item: ~p", [User]),
                binary_to_list(User)
              end,
              List).

%blocked_uids_from_lists(List, NewList) ->
%    UIDList = lists:sort(listitem_list_to_uid_list(List)),
%    ?INFO_MSG("UID list: ~p", [UIDList]),
%    NewUIDList = lists:sort(listitem_list_to_uid_list(NewList)),
%    ?INFO_MSG("New UID list: ~p", [NewUIDList]),
%    ok.

block_unblock_user(Blocker, Blockee, Block, Server) ->
  GetUrl = binary_to_list(gen_mod:get_module_opt(Server, mod_chat_interceptor, block_url_get, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
%  GetUrl = "http://localhost:9020/chat/block_unblock",
  Token = binary_to_list(gen_mod:get_module_opt(Server, mod_chat_interceptor, block_token, fun(S) -> iolist_to_binary(S) end, list_to_binary(""))),
%  Token = "",
  BlockerP = string:concat("blocker=", Blocker),
  BlockeeP = string:concat("blockee=", Blockee),
  BlockP = string:concat("block=", Block),
  TokenP = string:concat("token=", Token),
  Data = string:join([BlockerP, BlockeeP, BlockP, TokenP], "&"),
  GetUrlFull = string:concat(GetUrl, string:concat("?", Data)),
  ?INFO_MSG("Sending get request to ~s", [GetUrlFull]),
%  {Flag, {_, _, ResponseBody}} = httpc:request(get, {GetUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
  {Flag, {_, _, ResponseBody}} = httpc:request(GetUrlFull),
  ?INFO_MSG("Response received: {~s, ~s}", [Flag, ResponseBody]),
%  ?INFO_MSG("**************** ~p has blocked ~p ****************~n~n", [Blocker, Blockee]),
  ok.
