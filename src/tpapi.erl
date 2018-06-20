-module(tpapi).

%%-define(DEBUG, 1).

-export([get_tx_status/2, get_tx_status/3, get_wallet_info/2, commit_transaction/2,
    mine_sha512/3, get_register_wallet_transaction/2, register_wallet/2, get_wallet_seq/2,
    get_last_block/1, get_height/1]).


%% Wait for transaction commit and get it's status
get_tx_status(TxId, BaseUrl) ->
    get_tx_status(TxId, BaseUrl, 20).

get_tx_status(_TxId, _BaseUrl, 0 = _Trys) ->
    {ok, timeout, _Trys};

get_tx_status(TxId, BaseUrl, Try)->
    Query = {make_list(BaseUrl) ++ "/api/tx/status/" ++ make_list(TxId), []},
    {ok, {{_, 200, _}, _, ResBody}} = httpc:request(get, Query, [], [{body_format, binary}]),
    Res = jsx:decode(ResBody, [return_maps]),
    Status = maps:get(<<"res">>, Res, null),

%%-ifdef(DEBUG).
%%    io:format("got tx status: ~p ~n * raw: ~p", [Status, Res]),
%%-endif.

    case Status of
        null ->
            timer:sleep(1000),
            get_tx_status(TxId, BaseUrl, Try-1);
        AnyValidStatus ->
            {ok, AnyValidStatus, Try}
    end.

%% -------------------------------------------------------------------------------------

% get info for wallet
get_wallet_info(Wallet, BaseUrl) ->
    Query = {make_list(BaseUrl) ++ "/api/address/" ++ make_list(Wallet), []},
    {ok, {{_, 200, _}, _, ResBody}} =
        httpc:request(get, Query, [], [{body_format, binary}]),
    jsx:decode(ResBody, [return_maps]).


%% -------------------------------------------------------------------------------------

% get wallet sequence
get_wallet_seq(Wallet, BaseUrl) ->
    WalletData = get_wallet_info(Wallet, BaseUrl),
    Info = maps:get(<<"info">>, WalletData, #{}),
    maps:get(<<"seq">>, Info, 0).

%% -------------------------------------------------------------------------------------

% get last block
get_last_block(BaseUrl) ->
    Query = {make_list(BaseUrl) ++ "/api/block/last", []},
    {ok, {{_, 200, _}, _, ResBody}} =
        httpc:request(get, Query, [], [{body_format, binary}]),
    jsx:decode(ResBody, [return_maps]).

%% -------------------------------------------------------------------------------------

% get network height
get_height(BaseUrl) ->
    #{<<"block">> := #{<<"header">> := #{<<"height">> := Height}}} = get_last_block(BaseUrl),
    Height.

%% -------------------------------------------------------------------------------------

% post encoded and signed transaction using API
commit_transaction(SignedTransaction, BaseUrl) ->
    Body = jsx:encode(#{
        tx=>base64:encode(SignedTransaction)
    }),
    Query = {make_list(BaseUrl) ++ "/api/tx/new", [], "application/json", Body},
    {ok, {{_, 200, _}, _, ResBody}} = httpc:request(post, Query, [], [{body_format, binary}]),
    jsx:decode(ResBody, [return_maps]).

%% -------------------------------------------------------------------------------------

% register new wallet using API
register_wallet(PubKey, BaseUrl) ->
    RegisterTx = get_register_wallet_transaction(PubKey),
    register_wallet_by_tx(RegisterTx, BaseUrl).

register_wallet_by_tx(RegisterTx, BaseUrl) ->
    Res = commit_transaction(RegisterTx, BaseUrl),
    TxId = maps:get(<<"txid">>, Res, unknown),
    {ok, Status, _} = get_tx_status(TxId, BaseUrl),
    Wallet = maps:get(<<"res">>, Status, unknown),
    {ok, Wallet, TxId}.


get_register_wallet_transaction(PubKey) ->
    Promo = <<"TEST5">>,
    get_register_wallet_transaction(PubKey, Promo).

get_register_wallet_transaction(PubKey, Promo) ->
    PromoBin = make_binary(Promo),
    Now = os:system_time(second),
    Pow = mine_sha512(<<PromoBin/binary, " ", (integer_to_binary(Now))/binary, " ">>, 0, 8),
    tx:pack(#{
        type => register,
        register => PubKey,
        timestamp => Now,
        pow => Pow
    }).

%% -------------------------------------------------------------------------------------

mine_sha512(Str, Nonce, Diff) ->
    DS = <<Str/binary,(integer_to_binary(Nonce))/binary>>,
%%    if
%%        Nonce rem 1000000 == 0 ->
%%        io:format("nonce ~w~n",[Nonce]);
%%        true -> ok
%%    end,
    Act=if Diff rem 8 == 0 ->
        <<Act1:Diff/big,_/binary>>=crypto:hash(sha512,DS),
        Act1;
            true ->
                Pad=8-(Diff rem 8),
                <<Act1:Diff/big,_:Pad/big,_/binary>>=crypto:hash(sha512,DS),
                Act1
        end,
    if Act==0 ->
        DS;
        true ->
            mine_sha512(Str,Nonce+1,Diff)
    end.

%% -------------------------------------------------------------------------------------

make_binary(Arg) when is_binary(Arg) ->
    Arg;

make_binary(Arg) when is_list(Arg) ->
    list_to_binary(Arg);

make_binary(_Arg) ->
    throw(badarg).


%% -------------------------------------------------------------------------------------

make_list(Arg) when is_list(Arg) ->
    Arg;

make_list(Arg) when is_binary(Arg) ->
    binary_to_list(Arg);

make_list(_Arg) ->
    throw(badarg).


