module Balatro.Bot.Game

type Game =
    { Menu: unit -> Result<Api.GameState, Api.ApiError>
      Start: Api.Deck -> Api.Stake -> string option -> Result<Api.GameState, Api.ApiError>
      State: unit -> Result<Api.GameState, Api.ApiError>
      Select: unit -> Result<Api.GameState, Api.ApiError>
      Play: int list -> Result<Api.GameState, Api.ApiError>
      Discard: int list -> Result<Api.GameState, Api.ApiError>
      Buy: Api.BuyTarget -> Result<Api.GameState, Api.ApiError>
      Sell: Api.SellTarget -> Result<Api.GameState, Api.ApiError>
      Reroll: unit -> Result<Api.GameState, Api.ApiError>
      CashOut: unit -> Result<Api.GameState, Api.ApiError>
      NextRound: unit -> Result<Api.GameState, Api.ApiError>
      Pack: Api.PackAction -> Result<Api.GameState, Api.ApiError>
      Use: int -> int list -> Result<Api.GameState, Api.ApiError>
      Rearrange: Api.Rearrangement -> Result<Api.GameState, Api.ApiError> }

let live (client: Api.Client) : Game =
    { Menu = fun () -> Api.menu client
      Start = fun deck stake seed -> Api.start client deck stake seed
      State = fun () -> Api.gamestate client
      Select = fun () -> Api.select client
      Play = Api.play client
      Discard = Api.discard client
      Buy = Api.buy client
      Sell = Api.sell client
      Reroll = fun () -> Api.reroll client
      CashOut = fun () -> Api.cashOut client
      NextRound = fun () -> Api.nextRound client
      Pack = Api.pack client
      Use = Api.useConsumable client
      Rearrange = Api.rearrange client }
