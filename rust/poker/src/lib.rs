/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.

use std::cmp::{Ordering, PartialOrd};
use std::str::FromStr;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

pub fn winning_hands<'a>(hands: &[&'a str]) -> Vec<&'a str> {
    if hands.len() == 1 {return hands.to_vec();}
    let mut hands = hands.to_vec();
    hands.sort_by(|b, a| {
        let a_hand = Hand::from_str(a);
        let b_hand = Hand::from_str(b);
        a_hand.partial_cmp(&b_hand).unwrap()
    });
    let mut idx = 0;
    println!("{hands:?}");
    while Hand::from_str(hands[0]).partial_cmp(&Hand::from_str(hands[idx])).unwrap() == Ordering::Equal {
        idx += 1;
        if idx == hands.len() {
            break;
        }
    }
    println!("{hands:?}");
    hands[0..idx].to_vec()
}


#[derive(Clone, Copy, Debug, Eq)]
struct Card {
    rank: i8,
    suit: char
}

impl PartialEq for Card {
    fn eq(&self, other: &Self) -> bool {
        self.rank == other.rank
    }
}

impl Hash for Card {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rank.hash(state);
    }
}

impl Card {
    fn new(s: &str) -> Self {
        let (rank, suit) = s.split_at(s.len()-1);
        let rank = ["2","3","4","5","6","7","8","9","10","J","Q","K","A"]
            .iter()
            .position(|r| *r == rank)
            .unwrap() as i8; // should never fail in any of the tests, so fuck it :D
        let suit = char::from_str(suit).unwrap();
        Card {rank, suit}
    }

    fn make_low(&mut self) -> Result<(), String> {
        if self.rank == 12 {
            self.rank = -1;
            Ok(())
        } else {
            Err("not an ace".into())
        }
    }
}

#[test]
fn test_new_card() {
    let card_str = "QH";
    let card = Card::new(card_str);
    assert_eq!(card, Card {rank: 10, suit: 'H'});
    assert_eq!(card.suit, 'H');
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.rank.cmp(&other.rank))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Hand {
    HighCard{cards: Vec<Card>},
    Pair{pair_card: Card, kickers: Vec<Card>},
    TwoPair{high_pair: Card, low_pair: Card, kicker: Card},
    ThreeOfAKind{three_card: Card, kickers: Vec<Card>},
    Straight{cards: Vec<Card>},
    Flush{cards: Vec<Card>},
    FullHouse{three_card: Card, pair_card: Card},
    FourOfAKind{four_card: Card, kicker: Card},
    StraightFlush{cards: Vec<Card>},
    RoyalFlush
}

impl FromStr for Hand {
    type Err = ();
    fn from_str(v: &str) -> Result<Self, Self::Err> {
        let mut cards: Vec<Card> = v.split(" ").map(Card::new).collect();
        cards.sort_by(|a,b| a.partial_cmp(b).unwrap());
        let is_flush = {
            let suit = cards[0].suit;
            cards[1..].iter().all(|c| c.suit == suit)
        };
        if is_flush && is_straight(&cards) {
            if cards[0].rank == 8 {
                cards.reverse();
                return Ok(Hand::RoyalFlush)
            } else {
                cards.reverse();
                return Ok(Hand::StraightFlush{cards});
            }
        }
        match find_groups(&cards, 4) {
            Some(c) => {
                let four_card = c[0];
                return Ok(Hand::FourOfAKind{four_card, kicker: cards.into_iter().filter(|c| *c != four_card).take(1).nth(0).unwrap()});
            },
            None => ()
        };
        let pairs = find_groups(&cards, 2);
        let three = find_groups(&cards, 3);
        if three.is_some() && pairs.is_some() {
            let three_card = three.unwrap()[0];
            let pair_card = pairs.unwrap()[0];
            return Ok(Hand::FullHouse{three_card, pair_card});
        }
        if is_flush {
            cards.reverse();
            return Ok(Hand::Flush{cards});
        }
        if is_straight(&cards) {
            cards.reverse();
            return Ok(Hand::Straight{cards});
        }
        if three.is_some() {
            let three_card = three.unwrap()[0];
            for _ in 0..3 {
                cards.pop();
            }
            cards.reverse();
            return Ok(Hand::ThreeOfAKind { three_card: three_card, kickers: cards })
        }
        if pairs.is_some() {
            let mut pairs = pairs.unwrap();
            pairs.sort_by(|a, b| b.partial_cmp(a).unwrap());
            if pairs.len() == 2 {
                for _ in 0..4 {
                    cards.pop();
                }
                return Ok(Hand::TwoPair { high_pair: pairs[0], low_pair: pairs[1], kicker: cards.into_iter().last().unwrap() })
            } else if pairs.len() == 1 {
                for _ in 0..2 {
                    cards.pop();
                }
                cards.reverse();
                return Ok(Hand::Pair { pair_card: pairs[0], kickers: cards })
            }
        }
        cards.reverse();
        Ok(Hand::HighCard { cards: cards })
    }
}

#[test]
fn test_from_str() {
    let royal = "10H AH QH JH KH";
    assert_eq!(Hand::from_str(royal), Ok(Hand::RoyalFlush), "failed to create royal flush");
    let straight_flush = "2H 3H 4H 5H 6H";
    assert_eq!(Hand::from_str(straight_flush), Ok(Hand::StraightFlush{ cards: vec![Card::new("6H"),Card::new("5H"),Card::new("4H"),Card::new("3H"),Card::new("2H"),]}), "failed to create straight flush");
    let four = "2H 2D 2S 2C 3H";
    assert_eq!(Hand::from_str(four), Ok(Hand::FourOfAKind{four_card: Card::new("2H"), kicker: Card::new("3H")}), "failed to create four of a kind");
    let fh = "2H 2D 2S 3D 3H";
    assert_eq!(Hand::from_str(fh), Ok(Hand::FullHouse{three_card: Card::new("2H"), pair_card: Card::new("3D")}), "failed to create full house");
    let flush = "7D 8D 3D KD AD";
    assert_eq!(Hand::from_str(flush), Ok(Hand::Flush{cards: vec![Card::new("AD"), Card::new("KD"), Card::new("8D"), Card::new("7D"), Card::new("3D")]}), "failed to create flush");
    let straight = "AS 2H 3D 4S 5H";
    assert_eq!(Hand::from_str(straight), Ok(Hand::Straight{cards: vec![Card::new("AS"), Card::new("5H"), Card::new("4S"), Card::new("3D"), Card::new("2H")]}), "failed to create straight");
    let three = "7H 7D 7S 2H 3D";
    assert_eq!(Hand::from_str(three), Ok(Hand::ThreeOfAKind { three_card: Card::new("7H"), kickers: vec![Card::new("3D"), Card::new("2H")] }), "failed to create three of a kind");
    let two = "7H 7D 5S 5H 3D";
    assert_eq!(Hand::from_str(two), Ok(Hand::TwoPair { high_pair: Card::new("7H"), low_pair: Card::new("5S"), kicker: Card::new("3D") }), "failed to create two pairs");
    let pair = "7H 7D 5S 2H 3D";
    assert_eq!(Hand::from_str(pair), Ok(Hand::Pair { pair_card: Card::new("7H"), kickers: vec![Card::new("5S"), Card::new("3D"), Card::new("2H")] }), "failed to create pair");
    let high = "7H 9D 5S 2H 3D";
    assert_eq!(Hand::from_str(high), Ok(Hand::HighCard { cards: vec![Card::new("9D"), Card::new("7H"), Card::new("5S"), Card::new("3D"), Card::new("2H")] }), "failed to create high card");
}

fn is_straight(cards: &[Card]) -> bool {
    let mut cards = cards.to_vec();
    cards.iter().enumerate().all(|(i, c)| {
        c.rank == (i as i8) + cards[0].rank
    })
}

#[test]
fn test_is_straight() {
    let cards = vec![
        Card{rank: 5, suit: 'H'},
        Card{rank: 6, suit: 'H'},
        Card{rank: 7, suit: 'H'},
        Card{rank: 8, suit: 'H'},
        Card{rank: 9, suit: 'H'},
    ];
    let cards2 = vec![
        Card{rank: 5, suit: 'H'},
        Card{rank: 7, suit: 'H'},
        Card{rank: 7, suit: 'H'},
        Card{rank: 8, suit: 'H'},
        Card{rank: 9, suit: 'H'},
    ];
    let cards3 = vec![
        Card::new("10D"),
        Card::new("JH"),
        Card::new("QS"),
        Card::new("KD"),
        Card::new("AC"),
    ];
    assert!(is_straight(&cards));
    assert!(is_straight(&cards3));
    assert!(!is_straight(&cards2));
}

fn find_groups(cards: &[Card], group_size: i32) -> Option<Vec<Card>> {
    let mut c_map = HashMap::<Card, i32>::new();
    for c in cards {
        match c_map.get_mut(&c) {
            Some(cv) => *cv += 1,
            None => {c_map.insert(*c, 1);}
        };
    }
    let found: Vec<Card> = c_map.into_iter().filter(|(_, v)| *v == group_size).map(|(k, _)| k).collect();
    match found.len() {
        0 => None,
        _ => Some(found)
    }
}

#[test]
fn test_find_groups() {
    let cards = vec![
        Card{rank: 5, suit: 'H'},
        Card{rank: 5, suit: 'D'},
        Card{rank: 8, suit: 'D'},
        Card{rank: 8, suit: 'H'},
        Card{rank: 8, suit: 'S'},
    ];
    assert_eq!(find_groups(&cards, 2), Some(vec![Card{rank: 5, suit: 'D'}]));
    assert_eq!(find_groups(&cards, 3), Some(vec![Card{rank: 8, suit: 'H'}]));
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut comparison = hand_rank(self).cmp(&hand_rank(other));
        if comparison == Ordering::Equal {
            match (self, other) {
                (Self::RoyalFlush, Self::RoyalFlush) => (),
                (Self::HighCard{cards: lcards}, Self::HighCard{cards: rcards}) |
                (Self::Straight{cards: lcards}, Self::Straight{cards: rcards}) |
                (Self::Flush{cards: lcards}, Self::Flush{cards: rcards}) |
                (Self::StraightFlush{cards: lcards}, Self::StraightFlush{cards: rcards}) => {
                    comparison = compare_many(lcards, rcards);
                },
                (Self::FourOfAKind{four_card: lmain, kicker: lk}, Self::FourOfAKind{four_card: rmain, kicker: rk}) => {
                    comparison = compare_one(lmain, rmain);
                    if comparison == Ordering::Equal {
                        comparison = compare_one(lk, rk);
                    }
                },
                (Self::FullHouse{three_card: lmain, pair_card: lk}, Self::FullHouse{three_card: rmain, pair_card: rk}) => {
                    comparison = compare_one(lmain, rmain);
                    if comparison == Ordering::Equal {
                        comparison = compare_one(lk, rk);
                    }
                },
                (Self::TwoPair{high_pair: lh, low_pair: ll, kicker: lk}, Self::TwoPair{high_pair: rh, low_pair: rl, kicker: rk}) => {
                    comparison = compare_one(lh, rh);
                    if comparison == Ordering::Equal {
                        comparison = compare_one(ll, rl);
                    }
                    if comparison == Ordering::Equal {
                        comparison = compare_one(lk, rk);
                    }
                },
                (Self::Pair{pair_card: lmain, kickers: lk}, Self::Pair{pair_card: rmain, kickers: rk})  |
                (Self::ThreeOfAKind{three_card: lmain, kickers: lk}, Self::ThreeOfAKind{three_card: rmain, kickers: rk})  => {
                    comparison = compare_one(lmain, rmain);
                    if comparison == Ordering::Equal {
                        comparison = compare_many(lk, rk);
                    }
                },
                _ => unreachable!() // If ordering is equal then the two hands in the tuple must be the same
            }
        }
        Some(comparison)
    }
}

fn compare_one(l: &Card, r: &Card) -> Ordering {
    l.partial_cmp(r).unwrap()
}

#[test]
fn test_compare_one() {
    let tests = vec![
        (Card::new("5H"), Card::new("5S"), Ordering::Equal),
        (Card::new("5H"), Card::new("3S"), Ordering::Greater),
        (Card::new("5H"), Card::new("7S"), Ordering::Less),
    ];
    for (left, right, exp) in tests {
        assert_eq!(compare_one(&left, &right), exp);
    }
}

fn compare_many(l: &Vec<Card>, r: &Vec<Card>) -> Ordering {
    let mut result = Ordering::Equal;
    let mut idx = 0_usize;
    while result == Ordering::Equal && idx < l.len() {
        result = l[idx].partial_cmp(&r[idx]).unwrap();
        idx += 1;
    }
    result
}

#[test]
fn test_compare_many() {
    let tests = vec![
        (vec![Card::new("5H"), Card::new("7H")], vec![Card::new("5S"), Card::new("7S")], Ordering::Equal),
        (vec![Card::new("5H"), Card::new("7H")], vec![Card::new("5S"), Card::new("6S")], Ordering::Greater),
        (vec![Card::new("5H"), Card::new("7H")], vec![Card::new("5S"), Card::new("8S")], Ordering::Less),
    ];
    for (left, right, exp) in tests {
        assert_eq!(compare_many(&left, &right), exp);
    }
}

fn hand_rank(hand: &Hand) -> u8 {
    match hand {
        Hand::HighCard{cards: _} => 0,
        Hand::Pair{pair_card: _, kickers: _} => 1,
        Hand::TwoPair{high_pair: _, low_pair: _, kicker: _} => 2,
        Hand::ThreeOfAKind{three_card: _, kickers: _} => 3,
        Hand::Straight{cards: _} => 4,
        Hand::Flush{cards: _} => 5,
        Hand::FullHouse{three_card: _, pair_card: _} => 6,
        Hand::FourOfAKind{four_card: _, kicker: _} => 7,
        Hand::StraightFlush{cards: _} => 8,
        Hand::RoyalFlush => 9
    }
}








