-module(tpapi).

-export([get_tx_status/2, get_tx_status/3, get_wallet_info/2, commit_transaction/2,
  mine_sha512/3, get_register_wallet_transaction/2, register_wallet/2, get_wallet_seq/2,
  get_block/2, get_blockinfo/2, get_last_block/1, get_last_blockinfo/1, get_height/1,
  get_fullblock/2,
  ping/1, get_settings/1]).


%% Wait for transaction commit and get it's status
get_tx_status(TxId, BaseUrl) ->
  get_tx_status(TxId, BaseUrl, 40).

get_tx_status(_TxId, _BaseUrl, 0 = _Trys) ->
  {ok, timeout, _Trys};

get_tx_status(TxId, BaseUrl, Try) ->
  Res = make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/tx/status/" ++ make_list(TxId)
  ),
  Status = maps:get(<<"res">>, Res, null),

  case application:get_env(tpapi, "verbose", false) of
    true -> io:format("got tx status: ~p ~n * raw: ~p~n", [Status, Res]);
    _ -> skip
  end,
  
  case Status of
    null ->
      timer:sleep(1000),
      get_tx_status(TxId, BaseUrl, Try - 1);
    AnyValidStatus ->
      {ok, AnyValidStatus, Try}
  end.

%% -------------------------------------------------------------------------------------

% get chain settings
get_settings(BaseUrl) ->
  make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/settings"
  ).


%% -------------------------------------------------------------------------------------

% get info for wallet
get_wallet_info(Wallet, BaseUrl) ->
  make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/address/" ++ make_list(Wallet)
  ).


%% -------------------------------------------------------------------------------------

% get wallet sequence
get_wallet_seq(Wallet, BaseUrl) ->
  WalletData = get_wallet_info(Wallet, BaseUrl),
  Info = maps:get(<<"info">>, WalletData, #{}),
  maps:get(<<"seq">>, Info, 0).

%% -------------------------------------------------------------------------------------

% get block contents
get_block(Hash, BaseUrl) ->
  make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/block/" ++ make_list(Hash)
  ).

get_fullblock(Hash, BaseUrl) ->
  block:unpack(
  make_raw_http_request( get,
                         make_list(BaseUrl) ++ "/api/binblock/" ++ make_list(Hash)
                       )
   ).

%% -------------------------------------------------------------------------------------

% get last block contents
get_last_block(BaseUrl) ->
  get_block(last, BaseUrl).

%% -------------------------------------------------------------------------------------

% get blockinfo for a block
get_blockinfo(Hash, BaseUrl) ->
  make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/blockinfo/" ++ make_list(Hash)
  ).

%% -------------------------------------------------------------------------------------

% get blockinfo for the last block
get_last_blockinfo(BaseUrl) ->
  get_blockinfo(last, BaseUrl).

%% -------------------------------------------------------------------------------------

% get network height
get_height(BaseUrl) ->
  #{<<"block">> := #{<<"header">> := #{<<"height">> := Height}}} =
    get_last_blockinfo(BaseUrl),
  Height.

%% -------------------------------------------------------------------------------------

% post encoded and signed transaction using API
commit_transaction(SignedTransaction, BaseUrl) ->
  make_http_request(
    post,
    make_list(BaseUrl) ++ "/api/tx/new",
    #{tx=>base64:encode(SignedTransaction)}
  ).

%% -------------------------------------------------------------------------------------

% register new wallet using API
register_wallet(PrivKey, BaseUrl) ->
  RegisterTx = get_register_wallet_transaction(PrivKey),
  register_wallet_by_tx(RegisterTx, BaseUrl).

register_wallet_by_tx(RegisterTx, BaseUrl) ->
  Res = commit_transaction(RegisterTx, BaseUrl),
  TxId = maps:get(<<"txid">>, Res, unknown),
  {ok, TxStatus, _} = get_tx_status(TxId, BaseUrl),
  case TxStatus of
    timeout ->
      {error, timeout, TxId};
    #{<<"res">> := Wallet} ->
      {ok, Wallet, TxId};
    _ ->
      {error, TxStatus, TxId}
  end.


get_register_wallet_transaction(PrivKey) ->
  get_register_wallet_transaction(PrivKey, #{}).

get_register_wallet_transaction(PrivKey, Options) ->
  Promo = maps:get(promo, Options, <<"TEST5">>),
  PowDiff = maps:get(pow_diff, Options, 8),
  PubKey = tpecdsa:calc_pub(PrivKey, true),
  T1 = #{
    kind => register,
    t => os:system_time(millisecond),
    ver => 2,
    inv => make_binary(Promo),
    keys => [PubKey]
  },
  TXConstructed = tx:sign(tx:construct_tx(T1, [{pow_diff, PowDiff}]), PrivKey),
  tx:pack(TXConstructed).


%% -------------------------------------------------------------------------------------

make_http_request(post, Url, Params) when is_list(Url) andalso is_map(Params) ->
  RequestBody = jsx:encode(Params),
  Query = {Url, [{"connection", "close"}], "application/json", RequestBody},
  {ok, {{_, 200, _}, _, ResponceBody}} =
    httpc:request(
      post,
      Query,
      [],
      [{body_format, binary}, {ipv6_host_with_brackets, true}]
    ),
  process_http_answer(ResponceBody).

make_http_request(get, Url) when is_list(Url) ->
  ResponceBody=make_raw_http_request(get, Url),
  process_http_answer(ResponceBody).

make_raw_http_request(get, Url) when is_list(Url) ->
  Query = {Url, [{"connection", "close"}]},
  {ok, {{_, 200, _}, _, ResponceBody}} =
    httpc:request(
      get,
      Query,
      [],
      [{body_format, binary}, {ipv6_host_with_brackets, true}]
    ),
    ResponceBody.


process_http_answer(AnswerBody) ->
  Answer = jsx:decode(AnswerBody, [return_maps]),
  check_for_success(Answer),
  Answer.

check_for_success(Data) when is_map(Data) ->
  % answers without ok are temporary assumed successfully completed
  Success = maps:get(<<"ok">>, Data, true),
  if
    Success =/= true ->
      Code = maps:get(<<"code">>, Data, 0),
      Msg = maps:get(<<"msg">>, Data, <<"">>),
      throw({apierror, Code, Msg});
    true ->
      ok
  end.

%% -------------------------------------------------------------------------------------
ping(BaseUrl) ->
  Res = make_http_request(
    get,
    make_list(BaseUrl) ++ "/api/status"
  ),
  case maps:get(<<"ok">>, Res, false) of
    false -> false;
    _ -> true
  end.

%% -------------------------------------------------------------------------------------

mine_sha512(Str, Nonce, Diff) ->
  DS = <<Str/binary, (integer_to_binary(Nonce))/binary>>,
  Act = if Diff rem 8 == 0 ->
    <<Act1:Diff/big, _/binary>> = crypto:hash(sha512, DS),
    Act1;
          true ->
            Pad = 8 - (Diff rem 8),
            <<Act1:Diff/big, _:Pad/big, _/binary>> = crypto:hash(sha512, DS),
            Act1
        end,
  if Act == 0 ->
    DS;
    true ->
      mine_sha512(Str, Nonce + 1, Diff)
  end.

%% -------------------------------------------------------------------------------------

make_binary(Arg) when is_binary(Arg) ->
  Arg;

make_binary(Arg) when is_atom(Arg) ->
  atom_to_binary(Arg, utf8);

make_binary(Arg) when is_list(Arg) ->
  list_to_binary(Arg);

make_binary(_Arg) ->
  throw(badarg).


%% -------------------------------------------------------------------------------------

make_list(Arg) when is_list(Arg) ->
  Arg;

make_list(Arg) when is_atom(Arg) ->
  atom_to_list(Arg);

make_list(Arg) when is_binary(Arg) ->
  binary_to_list(Arg);

make_list(_Arg) ->
  throw(badarg).


