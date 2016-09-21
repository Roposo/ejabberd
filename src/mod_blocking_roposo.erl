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
    F = fun () ->
		Default = case mod_privacy_sql:sql_get_default_privacy_list_t(LUser) of
			      {selected, []} ->
				  Name = <<"Blocked contacts">>,
				  case mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Name) of
				      {selected, []} ->
					  mod_privacy_sql:sql_add_privacy_list(LUser, Name);
				      {selected, [{_ID}]} ->
					  ok
				  end,
				  mod_privacy_sql:sql_set_default_privacy_list(LUser, Name),
				  Name;
			      {selected, [{Name}]} -> Name
			  end,
		{selected, [{ID}]} =
		    mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Default),
		case mod_privacy_sql:sql_get_privacy_list_data_by_id_t(ID) of
		    {selected, RItems = [_ | _]} ->
			List = lists:flatmap(fun mod_privacy_sql:raw_to_item/1, RItems);
		    _ ->
			List = []
		end,
		NewList = Filter(List),
		NewRItems = lists:map(fun mod_privacy_sql:item_to_raw/1,
				      NewList),
		mod_privacy_sql:sql_set_privacy_list(ID, NewRItems),
		{ok, Default, NewList}
	end,
    ejabberd_sql:sql_transaction(LServer, F).

unblock_by_filter(LUser, LServer, Filter) ->
    F = fun () ->
		case mod_privacy_sql:sql_get_default_privacy_list_t(LUser) of
		    {selected, []} -> ok;
		    {selected, [{Default}]} ->
			{selected, [{ID}]} =
			    mod_privacy_sql:sql_get_privacy_list_id_t(LUser, Default),
			case mod_privacy_sql:sql_get_privacy_list_data_by_id_t(ID) of
			    {selected, RItems = [_ | _]} ->
				List = lists:flatmap(fun mod_privacy_sql:raw_to_item/1,
						     RItems),
				NewList = Filter(List),
				NewRItems = lists:map(fun mod_privacy_sql:item_to_raw/1,
						      NewList),
				mod_privacy_sql:sql_set_privacy_list(ID, NewRItems),
				{ok, Default, NewList};
			    _ -> ok
			end;
		    _ -> ok
		end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

uid_list_to_listitem_list([], Server) ->
    [];
uid_list_to_listitem_list([UID | RemainingUIDs], Server) ->
    ?INFO_MSG("UID: ~p", [UID]),
%    JID = #jid{user = list_to_binary(UID), server = list_to_binary(Server), resource = <<"">>},
    JID = {list_to_binary(UID), list_to_binary(Server), <<"">>},
    ?INFO_MSG("JID string: ~p", [jid:to_string(JID)]),
    [#listitem{type = jid, action = deny, value = JID, match_all = true} | uid_list_to_listitem_list(RemainingUIDs, Server)].

process_blocklist_get(LUser, LServer) ->
    PostUrl = "http://localhost:9020/chat/get_block_list",
    UserP = string:concat("user=", binary_to_list(LUser)),
    Data = string:join([UserP], "&"),
    ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Data]),
    {Flag, {_, _, ResponseBody}} = httpc:request(post, {PostUrl, [], "application/x-www-form-urlencoded", Data}, [], []),
    ?INFO_MSG("Blocked list for ~s: ~s", [LUser, ResponseBody]),
    [BlockedUsers] = string:tokens(ResponseBody, "[]"),
    ?INFO_MSG("Blocked users list: ~p", [BlockedUsers]),
    UIDs = string:tokens(BlockedUsers, ","),
    uid_list_to_listitem_list(UIDs, binary_to_list(LServer)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
