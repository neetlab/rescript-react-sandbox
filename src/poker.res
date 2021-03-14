
type suit =
  | Spade
  | Diamond
  | Club
  | Heart

type card =
  | Joker
  | Standard(suit, int)

let createCard = (suit: suit, rank: int): card => {
  Standard(suit, rank)
}

let sum = (a: card, b: card): int => {
  switch (a, b) {
  | (Standard(_, x), Standard(_, y)) => x + y
  | _ => 0
  }
}

let prev = (a: card): card => {
  switch a {
  | Standard(s, x) => createCard(s, x == 1 ? 13 : x + 1)
  | Joker => Joker
  }
}

let ord = (a: card, b: card): int => {
  switch (a, b) {
  | (Standard(_, x), Standard(_, y)) => x - y
  | _ => 0
  }
}


let rankEq = (a: card, b: card): bool => {
  switch (a, b) {
  | (Standard(_, x), Standard(_, y)) => x == y
  | (Joker, Joker) => true
  | _ => false
  }
}

let isFlush = (deck: list<card>): bool => {
  open Belt.List

  switch deck {
  | list{} => false
  | list{x, ...xs} => xs->every(v => rankEq(x, v))  
  }
}

let rec isStraight = (deck: list<card>): bool => {
  open Belt.List

  let sortedDeck = deck
    ->map(card => prev(card))
    ->sort(ord)

  switch sortedDeck {
  | list{Standard(_, x), Standard(ys, yr), ...xs} => {
      if (Js.Math.abs_int(x - yr) <= 1) {
        isStraight(list{Standard(ys, yr), ...xs})
      } else {
        false
      }
    }
  | _ => false
  }
}

Js.log(
  isFlush(list{
    Standard(Spade, 1),
    Standard(Heart, 1),
    Standard(Diamond, 1),
    Standard(Club, 1),
  })
)
