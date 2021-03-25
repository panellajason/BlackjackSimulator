exception CheckFailedException of string
//-------------------Card representations-------------------------.
type CardSuit = 
  | Spades 
  | Clubs
  | Diamonds
  | Hearts

type Card = {suit : CardSuit; kind : int}

type PlayerHand = {
  cards: Card list; 
  doubled: bool
}

type PlayerState = {
  activeHands: PlayerHand list; 
  finishedHands: PlayerHand list
}

type GameState = {
  deck : Card list; 
  player : PlayerState; 
  dealer: Card list
}

type GameLog = {playerWins : int; dealerWins : int; draws : int}

type HandOwner = 
  | Player 
  | Dealer

type PlayerAction = 
  | Hit
  | Stand
  | DoubleDown
  | Split

type HandResult = 
  | Win
  | Lose
  | Draw

let rand = new System.Random()

//---------------------------UTILITY METHODS--------------------------

let cardToString card =
  let kind = card.kind
  match kind with 
  | 1 -> sprintf "Ace of %A" card.suit 
  | 11 -> sprintf "Jack of %A" card.suit 
  | 12 -> sprintf "Queen of %A" card.suit 
  | 13 -> sprintf "King of %A" card.suit 
  | _ -> sprintf "%A of %A" kind card.suit
  
let handToString hand =
  let combineCards acc element =  
    match acc with
    | "" -> cardToString(element)
    | _ -> acc + ", " + cardToString(element)
  List.fold combineCards "" hand
    
let cardValue card =
  match card.kind with
  | 1 -> 11
  | 11 | 12 | 13 -> 10  
  | n -> n
    
let handTotal hand =
  let sum = List.sumBy (fun c -> cardValue c) hand
  let numAces = hand |> List.filter (fun c -> c.kind = 1) |> List.length
  if sum <= 21 then
    sum
  else 
    let maxAces = (float sum - 21.0) / 10.0 |> ceil |> int
    sum - (10 * (min numAces maxAces))

//------------FUNCTIONS THAT CREATE OR UPDATE GAME STATES--------------

let makeDeck () =
    List.init 52 (fun i -> let s = match i / 13 with
                                   | 0 -> Spades
                                   | 1 -> Clubs
                                   | 2 -> Diamonds
                                   | _ -> Hearts //to make warning go away
                           {suit = s; kind = i % 13 + 1})

let shuffleDeck deck =
  let arr = List.toArray deck
  let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp
  Array.iteri (fun i _ -> swap arr i (rand.Next(i, Array.length arr))) arr
  Array.toList arr

let newGame (deck : Card list) =
  let playerCards = [deck.Head ; List.item 2 deck] 
  let dealerCards = [deck.Tail.Head ; List.item 3 deck]
  {deck = List.skip 4 deck;
   player = {activeHands = [{cards = playerCards; doubled = false}]; finishedHands = []}
   dealer = dealerCards}

let hit handOwner gameState = 
  let topCard = gameState.deck.Head
  let newDeck = gameState.deck.Tail
  if handOwner = Dealer then
    let newDealerHand = topCard :: gameState.dealer
    {gameState with deck = newDeck; dealer = newDealerHand}
  else
    let playerState = gameState.player
    let newActiveHandCards = topCard::playerState.activeHands.Head.cards
    let newPlayerHand = {cards = newActiveHandCards; doubled = playerState.activeHands.Head.doubled}
    let addPlayerHand = newPlayerHand :: playerState.activeHands.Tail
    {gameState with deck = newDeck; player = {activeHands = addPlayerHand; finishedHands = playerState.finishedHands}}

//------------------------PLAYER STRATEGIES----------------------

let inactivePlayerStrategy gameState =  
  printfn "Player Stands"
  Stand

let coinFlipPlayerStrategy gameState =
  if rand.NextDouble() * 101.0 < 50.0 then
    printfn "Coin Heads"
    printfn "Player Hits"
    Hit
  else
    printfn "Coin Tails"
    printfn "Player Stands"
    Stand

let greedyPlayerStrategy gameState = 
  let playerState = gameState.player
  let score = handTotal playerState.activeHands.Head.cards
  if score < 21 then
    printfn "Player Hits"
    Hit
  else
    Stand

let basicPlayerStrategy gameState =
  let playerState = gameState.player
  let firstActiveHand = playerState.activeHands.Head.cards
  let playerScore = handTotal firstActiveHand
  let dealer = gameState.dealer
  let dealerScore = handTotal dealer

  let dealerFirstCard = cardValue dealer.Head
  let firstCard = cardValue firstActiveHand.Head
  let secondCard = cardValue firstActiveHand.Tail.Head

  if firstCard = 5 && secondCard = 5 && firstActiveHand.Length = 2 then
    printfn "Player Double Downs (Two 5s)"
    DoubleDown
  elif firstCard + secondCard = 11 && firstActiveHand.Length = 2 then
    printfn "Player Double Downs (Total of 11)"
    DoubleDown
  elif firstCard + secondCard = 10 && firstActiveHand.Length = 2 then
    if dealerFirstCard = 10 || dealerFirstCard = 11 then
      printfn "Player Hits (Total of 10)"
      Hit
    else
      printfn "Player Double Downs (Total of 10)"
      DoubleDown
  elif firstCard + secondCard = 9 && firstActiveHand.Length = 2 then
    if dealerFirstCard = 2 || dealerFirstCard >= 7 then
      printfn "Player Hits (Total of 9)"
      Hit
    else
      printfn "Player Double Downs (Total of 9)"
      DoubleDown
  elif firstCard = secondCard && firstActiveHand.Length = 2 then
    if playerScore = 20 then
      printfn "Player Stands (Total of 20)"
      Stand
    else
      printfn "Player Splits (1st card = 2nd card)"
      Split
  elif dealerFirstCard >= 2 && dealerFirstCard <= 6 then
    if playerScore >= 12 then
      printfn "Player Stands (Dealer's 1st Card is 2 - 6)"
      Stand
    else
      printfn "Player Hits (Dealer's 1st Card is 2 - 6)"
      Hit
  elif dealerFirstCard >= 7 && dealerFirstCard <= 13 then
    if playerScore <= 16 then
      printfn "Player Hits (Dealer's First Card is 7 - King)"
      Hit 
    else
      printfn "Player Stands (Dealer's First Card is 7 - King)"
      Stand 
  elif dealerFirstCard = 1 then
    if (firstCard = 11 || secondCard = 11) && playerScore <= 16 then
      printfn "Player Hits (Dealer's First Card is Ace)"  
      Hit
    elif playerScore <= 11 then
      printfn "Player Hits (Dealer's First Card is Ace)"  
      Hit
    else
      printfn "Player Stands (Dealer's First Card is Ace)"
      Stand
  else
    printfn "Player Stands"
    Stand

//------------------------PLAYER/DEALER TURNS------------------------

let rec playerTurn (playerStrategy : GameState->PlayerAction) (gameState : GameState) =
  let playerState = gameState.player
  if playerState.activeHands.IsEmpty then
    gameState
  else
    let score = handTotal playerState.activeHands.Head.cards
    let currentPlayerHand = playerState.activeHands.Head 
    printfn "Player's hand: %s; %d points" (handToString currentPlayerHand.cards) score
    if score > 21 then
      printfn "Player busts!"
      gameState
    elif score = 21 then
      printfn "Player blackjack!"
      gameState
    else
      let action = playerStrategy gameState
      match action with
      |Hit -> gameState
              |> hit Player
              |> playerTurn playerStrategy
      |Stand -> gameState 
      |DoubleDown -> let newPlayerHand = {cards = gameState.deck.Head::currentPlayerHand.cards; doubled = true}
                     let newActiveHands = newPlayerHand :: playerState.activeHands.Tail
                     printfn "Player's hand: %s; %d points" (handToString newPlayerHand.cards) (handTotal newPlayerHand.cards)
                     {gameState with deck = gameState.deck.Tail; player = {activeHands = newActiveHands; finishedHands = playerState.finishedHands}}
      |Split -> let splitHand1 = currentPlayerHand.cards.Head::[]
                let splitHand2 = currentPlayerHand.cards.Tail.Head::[]
                let newHand1 = {cards = gameState.deck.Head::splitHand1; doubled = false}
                let newHand2 = {cards = gameState.deck.Tail.Head::splitHand2; doubled = false}
                {gameState with deck = gameState.deck.Tail.Tail; player = {activeHands = newHand1 :: newHand2 :: playerState.activeHands.Tail; finishedHands = playerState.finishedHands}}
                |> playerTurn playerStrategy

let rec dealerTurn gameState =
  let dealer = gameState.dealer
  let score = handTotal dealer
  printfn "Dealer's hand: %s; %d points" (handToString dealer) score
  if score > 21 then
    printfn "Dealer busts!"
    gameState
  elif score = 21 then
    printfn "Dealer blackjack!"
    gameState
  elif score < 17 then
    printfn "Dealer Hits"
    gameState
    |> hit Dealer
    |> dealerTurn
  else
    printfn "Dealer must stay"
    gameState
                      
//--------------------------Game Functions--------------------------

let transferHand gameState =
  {gameState with player = {activeHands = gameState.player.activeHands.Tail; finishedHands = gameState.player.activeHands.Head::gameState.player.finishedHands}}

let determineWinner playerScore dealerScore =
  if playerScore = 21 && dealerScore = 21 then
    Draw
  elif dealerScore = 21 then
    Lose
  elif playerScore = 21 then
    Win
  elif dealerScore > 21 && playerScore > 21 then
    Draw
  elif dealerScore > 21 then
    Win
  elif playerScore > 21 then
    Lose
  elif playerScore = dealerScore then
    Draw
  elif playerScore > dealerScore then
    Win 
  else    
    Lose
    
let oneGame playerStrategy gameState =
  let dealer = gameState.dealer
  let playerState = gameState.player
  let dealerFirst = cardToString(dealer.Head)
  printfn "\nDealer is showing: %s" dealerFirst

  let combineActiveHands gameState = 
    let rec combine acc gameState =
      match gameState.player.activeHands with
      |[]-> acc 
      |_ -> printfn "Player's turn"
            let nextState = playerTurn playerStrategy gameState
            printfn "Dealer's turn"
            let newGameState = dealerTurn nextState

            let dealerScore = handTotal newGameState.dealer
            let playerScore = handTotal newGameState.player.activeHands.Head.cards
            let result = determineWinner playerScore dealerScore
            match result with
            |Win-> printfn "Player wins"
                   if newGameState.player.activeHands.Head.doubled = true then
                    combine {acc with playerWins = acc.playerWins + 2} (transferHand newGameState)
                   else
                    combine {acc with playerWins = acc.playerWins + 1} (transferHand newGameState)
            |Lose-> printfn "Player loses"
                    if newGameState.player.activeHands.Head.doubled = true then
                      combine {acc with dealerWins = acc.dealerWins + 2} (transferHand newGameState)
                    else
                      combine {acc with dealerWins = acc.dealerWins + 1} (transferHand newGameState)
            |Draw-> printfn "Draw"  
                    combine {acc with draws = acc.draws + 1} (transferHand newGameState) 
    combine {playerWins = 0; dealerWins = 0; draws = 0} gameState
  combineActiveHands gameState

let manyGames n playerStrategy =
  List.init n (fun i -> oneGame playerStrategy (makeDeck() |> shuffleDeck |> newGame))
  |> List.reduce (fun g1 g2 -> {playerWins = g1.playerWins + g2.playerWins; dealerWins = g1.dealerWins + g2.dealerWins; draws = g1.draws + g2.draws})
                
//---------------------INTERACTIVE STRATEGIES--------------------
let legalPlayerActions playerHand =
  let legalActions = [Hit; Stand; DoubleDown; Split]
  let requirements = [
    handTotal playerHand < 21; 
    true; 
    playerHand.Length = 2;
    playerHand.Length = 2 && cardValue playerHand.Head = cardValue playerHand.Tail.Head
  ]
  List.zip legalActions requirements 
  |> List.filter (fun (_, req) -> req) 
  |> List.map (fun (act, _) -> act) 

let actionToString = function
  | Hit -> "(H)it"
  | Stand -> "(S)tand"
  | DoubleDown -> "(D)ouble down"
  | Split -> "S(p)lit"

let rec interactivePlayerStrategy gameState =
  let playerHand = gameState.player.activeHands.Head
  let legalActions = legalPlayerActions playerHand.cards
  legalActions
  |> List.map actionToString
  |> String.concat ", "
  |> printfn "What do you want to do? %s" 

  let answer = System.Console.ReadLine()
  match answer.ToLower() with
  | "h" when List.contains Hit legalActions -> Hit
  | "s" -> Stand
  | "d" when List.contains DoubleDown legalActions -> DoubleDown
  | "p" when List.contains Split legalActions -> Split
  | _ -> printfn "Please choose one of the available options, dummy."
         interactivePlayerStrategy gameState

//------------------------Testing Functions----------------------
let check actual expected =
  if actual <> expected then
      raise (CheckFailedException(sprintf "Check failed. Expected: '%A' got: '%A'." expected actual))
  else
      ()
        
// This is the test from the PDF for doubling down.
let testFromPDF() =
  let deck = [{suit = Hearts; kind = 5}; {suit = Clubs; kind = 13};
              {suit = Spades; kind = 5}; {suit = Clubs; kind = 13};
              {suit = Clubs; kind = 1}]
  let result = deck |> newGame |> oneGame basicPlayerStrategy
  let expected = {dealerWins = 0; playerWins = 2; draws = 0}
  assert (result = expected)
  check result expected

// This is a test for splitting with two 9's. The strategy should Stand on both hands after the split. One hand wins, one draws.
let splitTest() =
  let deck = [{suit = Hearts; kind = 9}; {suit = Clubs; kind = 13};
              {suit = Spades; kind = 9}; {suit = Diamonds; kind = 7};
              {suit = Clubs; kind = 1}; {suit = Clubs; kind = 8}]
  let result = deck |> newGame |> oneGame basicPlayerStrategy
  let expected = {dealerWins = 0; playerWins = 1; draws = 1}
  assert (result = expected)
  check result expected

// This test splits two 8's. One hand gets a 2 and doubles down but loses. The other hand gets an Ace, Stands, and wins.
let splitIntoDouble() =
  let deck = [{suit = Hearts; kind = 8}; {suit = Clubs; kind = 7};
              {suit = Spades; kind = 8}; {suit = Diamonds; kind = 13};
              {suit = Clubs; kind = 2}; {suit = Clubs; kind = 1}; {suit = Diamonds; kind = 2}]
  let result = deck |> newGame |> oneGame basicPlayerStrategy
  let expected = {dealerWins = 2; playerWins = 1; draws = 0}
  assert (result = expected)
  check result expected

//-------------------------------Main-------------------------------    
[<EntryPoint>]
let main argv =
  // makeDeck() 
  // |> shuffleDeck
  // |> newGame
  // |> oneGame basicPlayerStrategy
  // |> printfn "%A"

  //manyGames 1000 inactivePlayerStrategy |> printfn "\nInactive Player Strategy: \n%A"
  //manyGames 1000 coinFlipPlayerStrategy |> printfn "\nCoin Flip Player Strategy: \n%A"
  //manyGames 1000 greedyPlayerStrategy |> printfn "\nGreedy Player Strategy: \n%A"
  manyGames 1000 basicPlayerStrategy |> printfn "\nBasic Player Strategy: \n%A"
  
  //testFromPDF()
  //splitTest()
  //splitIntoDouble()
  0 


