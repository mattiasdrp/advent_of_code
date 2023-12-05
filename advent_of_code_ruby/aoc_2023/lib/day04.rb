# frozen_string_literal: true

def infos(line)
  parts = line.split(/[:|]/)
  card = parts[0].scan(/\d+/)[0].to_i
  winning = parts[1].scan(/\d+/)
  numbers = parts[2].scan(/\d+/)
  [card, winning, numbers]
end

def compute_line1(line, points)
  _, winning, numbers = infos(line)
  total = 0
  numbers.each do |number|
    if winning.include? number
      total = total.zero? ? 1 : total * 2
    end
  end
  points + total
end

def part1(file)
  score = 0
  File.readlines(file).each do |line|
    score = compute_line1(line, score)
  end
  score
end

def compute_line2(line, cards)
  card, winning, numbers = infos(line)
  cards[card] = cards.fetch(card, 1)
  copies = cards[card]
  total = 0
  numbers.each do |number|
    total += 1 if winning.include? number
  end
  (0..total - 1).each do |i|
    cards[card + 1 + i] = copies + cards.fetch(card + 1 + i, 1)
  end
  cards
end

def part2(file)
  cards = { 1 => 1 }
  File.readlines(file).each do |line|
    cards = compute_line2(line, cards)
  end
  total = 0
  cards.each_value { |value| total += value }
  total
end

puts ARGV[0] == "1" ? part1(ARGV[1]) : part2(ARGV[1])
