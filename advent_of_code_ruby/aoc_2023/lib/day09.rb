# frozen_string_literal: true

def next_sequence(array)
  prev, *tail = array
  tail.map do |v|
    new_v = v - prev
    prev = v
    new_v
  end
end

def get_history(array)
  history = [array]
  sequence = array
  until sequence.all?(&:==)
    sequence = next_sequence(sequence)
    history << sequence
  end
  history
end

def down_and_up(array, part)
  sequence, *tail = get_history(array).reverse
  tail.each do |seq|
    sequence =
      if part == 1
        seq << seq.last + sequence.last
      else
        seq.unshift(seq.first - sequence.first)
      end
  end
  part == 1 ? sequence.last : sequence.first
end

def part(file, part)
  File.readlines(file, chomp: true).sum do |line|
    down_and_up(
      line
      .split
      .map(&:to_i),
      part
    )
  end
end

puts(ARGV[1] == "1" ? part(ARGV[0], 1) : part(ARGV[0], 2))
