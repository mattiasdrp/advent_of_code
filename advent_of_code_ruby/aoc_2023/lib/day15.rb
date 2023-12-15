# frozen_string_literal: true

class String
  def aoc_hash
    each_char.reduce(0) { |total, char| ((total + char.ord) * 17) % 256 }
  end
end

def part1(file)
  File.readlines(file, chomp: true)[0].split(",").sum(&:aoc_hash)
end

Lens = Data.define(:label, :length)

class Lens
  def pretty_print(pp)
    pp.pp [label, length]
  end
end

def execute(operation, map)
  label, length = operation.split("=")
  case length
  in nil
    # label- case
    label = label.chomp("-")
    box = label.aoc_hash
    map[box]&.delete_if { _1.label == label }
  in length
    box = label.aoc_hash
    length = length.to_i
    # label = length case
    if map[box].nil?
      map[box] = [Lens[label:, length:]]
    else
      case map[box].index { _1.label == label }
      in nil
        map[box].push(Lens[label:, length:])
      in i
        map[box][i] = Lens[label:, length:]
      else
      end
    end
  else
  end
  map
end

def score(map)
  map.each_pair.sum(0) do |box, array|
    array.each_with_index.sum(0) do |lens, index|
      lens.length * (index + 1) * (box + 1)
    end
  end
end

def part2(file)
  map = {}
  File
    .readlines(file, chomp: true)[0]
    .split(",")
    .each { |operation| execute(operation, map) }
  score map
end

pp(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
