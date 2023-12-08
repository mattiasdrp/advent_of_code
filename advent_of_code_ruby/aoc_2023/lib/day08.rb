# frozen_string_literal: true

Dest = Data.define(:left_dest, :right_dest)

def parse(file, predicate)
  instr, _, *graph = File.readlines(file, chomp: true)
  map = {}
  starts = []
  graph.each do |line|
    source, left_dest, right_dest = /(\w+) = \((\w+), (\w+)\)/.match(line).captures
    source = source.to_sym
    starts << source if predicate.call(source)
    map[source] = Dest[left_dest: left_dest.to_sym, right_dest: right_dest.to_sym]
  end
  [instr.chars.map(&:to_sym), map, starts]
end

def next_dest(map, instructions, source, count, predicate)
  until predicate.call(source)
    instr = instructions[0]
    source = instr == :R ? map[source].right_dest : map[source].left_dest
    instructions.rotate!(1)
    count += 1
  end
  count
end

def part1(file)
  instructions, map, = parse(file, ->(_) { false })
  next_dest(map, instructions, :AAA, 0, -> { _1 == :ZZZ })
end

def part2(file)
  instructions, map, starts = parse(file, -> { _1.end_with?("A") })
  starts
    .map { next_dest(map, instructions, _1, 0, ->(source) { source.end_with?("Z") }) }
    .reduce(&:lcm)
end

puts(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
