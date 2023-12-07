# frozen_string_literal: true

Hand = Data.define(:cards, :bid) do
  self::RANKS = "23456789TJQKA"

  def self.parse(line)
    cards, bid = line.split
    self[cards: cards.chars.map { self::RANKS.index(_1) }, bid: bid.to_i]
  end

  def value
    occurences = cards.tally
    values = occurences.empty? ? [0] : occurences.values
    # A hand contains at most 4 identical cards
    # Hand values are based on the number of identical cards they have, nothing else
    # Since the value of the identical cards doesn't matter, the value
    # of a hand is uniquely described by the number of identical cards it contains
    # 4, 1 -> 4^3 + 4^0 = 65
    # 3, 2 -> 4^2 + 4^1 = 20
    # and so on
    values.map { 4**(_1 - 1) }.sum
  end

  def <=>(other)
    self_value = value
    other_value = other.value
    # If the two hand don't have the same value, the greater hand is the
    # one with the greater value
    return self_value - other_value if self_value != other_value

    # Otherwise, returns the difference between the two first cards that are
    # different
    cards.zip(other.cards).map { _1 - _2 }.find { !_1.zero? }
  end
end

class JokerHand < Hand
  self::RANKS = "X23456789TJQKA"

  def self.parse(line)
    cards, bid = line.split
    self[cards: cards.chars.map { _1 == "J" ? 0 : JokerHand::RANKS.index(_1) }, bid: bid.to_i]
  end

  def value
    occurences = cards.tally
    # Same as before but we now need to gather the jokers and add
    # their count to the highest occurrence
    # Joker having the value 0, we delete the
    # nb_joker here is nil or an int
    nb_jokers = occurences.delete(0)
    # Since we want to add the jokers to the maximum number of occurrences
    # we need to sort the occurrences
    values = occurences.empty? ? [0] : occurences.values.sort_by(&:-@)
    # nil being the only falsey value, this adds either
    # the number of jokers if non nil or 0
    values[0] += nb_jokers || 0
    # We can't make powers of 4 since the maximum number of
    # occurences is now 5
    values.map { 5**(_1 - 1) }.sum
  end
end

def parse(file, hand_class)
  File.readlines(file, chomp: true).map { hand_class.parse(_1) }
end

def part1(file)
  parse(file, Hand).sort.each_with_index.map { |hand, rank| hand.bid * (rank + 1) }.sum
end

def part2(file)
  parse(file, JokerHand).sort.each_with_index.map { |hand, rank| hand.bid * (rank + 1) }.sum
end

puts(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
