-module(couch_custom_uuids_httpd).

-author("ahmetturk").
-export([handle_req/1]).

-record(httpd, {
  mochi_req,
  peer,
  method,
  requested_path_parts,
  path_parts,
  db_url_handlers,
  user_ctx,
  req_body = undefined,
  design_url_handlers,
  auth,
  default_fun,
  url_handlers
}).

handle_req(#httpd{method='GET'}=Req) ->
  Max = list_to_integer(couch_config:get("custom_uuids","max_count","1000")),
  Count = list_to_integer(couch_httpd:qs_value(Req, "count", "1")),
  case Count > Max of
    true -> throw({forbidden, <<"count parameter too large">>});
    false -> ok
  end,
  UUIDs = [couch_custom_uuids:new() || _ <- lists:seq(1, Count)],
  Etag = couch_httpd:make_etag(UUIDs),
  couch_httpd:etag_respond(Req, Etag, fun() ->
    CacheBustingHeaders = [
      {"Date", couch_util:rfc1123_date()},
      {"Cache-Control", "no-cache"},
      {"Expires", "Fri, 01 Jan 1990 00:00:00 GMT"},
      {"Pragma", "no-cache"},
      {"ETag", Etag}
    ],
    send_json(Req, 200, CacheBustingHeaders, {[{<<"uuids">>, UUIDs}]})
                                      end);
handle_req(Req) ->
  send_method_not_allowed(Req, "GET").
