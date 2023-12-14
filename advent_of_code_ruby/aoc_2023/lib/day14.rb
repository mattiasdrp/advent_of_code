# frozen_string_literal: true

def weight1(line)
  sum, = line.each_char.with_index.reverse_each.reduce([0, line.size]) do |(sum, last), (char, index)|
    case char
    in "#"
      [sum, index]
    in "O"
      [sum + last, last - 1]
    else
      [sum, last]
    end
  end
  sum
end

def part1(file)
  array = File.readlines(file, chomp: true)
  rows = array.length - 1
  (0..array[0].size - 1).reduce(0) do |sum, col|
    line = (0..rows).reduce(+"") do |acc, row|
      acc << array[rows - row][col]
    end
    sum + weight1(line)
  end
end

def move_vertically(array, col, rstart, rend, increment)
  (if rstart < rend
     (rstart..rend)
   else
     rstart.downto(rend)
   end).reduce(rstart) do |last, row|
    case array[row][col]
    in "#"
      row + increment
    in "O"
      if last != row
        array[last][col] = "O"
        array[row][col] = "."
      end
      last + increment
    else
      last
    end
  end
end

def move_horizontally(array, row, rstart, rend, increment)
  (if rstart < rend
     (rstart..rend)
   else
     rstart.downto(rend)
   end).reduce(rstart) do |last, col|
    case array[row][col]
    in "#"
      col + increment
    in "O"
      if last != col
        array[row][last] = "O"
        array[row][col] = "."
      end
      last + increment
    else
      last
    end
  end
end

def weight(array)
  top = array.length
  array.each_with_index.sum do |line, index|
    line.sum { _1 == "O" ? top - index : 0 }
  end
end

def cycle(array)
  (0..array[0].length - 1).each { move_vertically(array, _1, 0, array.length - 1, 1) }
  (0..array.length - 1).each { move_horizontally(array, _1, 0, array[0].length - 1, 1) }
  (0..array[0].length - 1).each { move_vertically(array, _1, array.length - 1, 0, -1) }
  (0..array.length - 1).each { move_horizontally(array, _1, array[0].length - 1, 0, -1) }
end

def process(array)
  memo = {}
  cycles = 0
  while memo[array].nil?
    memo[array] = cycles
    cycle array
    cycles += 1
  end
  # At this point:
  # - memo[array] points to the first time we saw an array
  # - cycles points to the second time we saw the exact same array
  # We just need to know the remaining number of cycles we need to do
  (1..(1_000_000_000 - memo[array]) % (cycles - memo[array])).each { cycle(array) }
  array
end

def pp_array(array)
  array.each { puts _1.join }
  puts
end

# Part1 using the functions from part2 for testing purposes
# def part1(file)
#   array = File.readlines(file, chomp: true).map(&:chars)
#   (0..array[0].length - 1).each { move_north(array, _1) }
#   # pp_array array
#   weight(array)
# end

def part2(file)
  array = File.readlines(file, chomp: true).map(&:chars)
  # pp_array array
  array = process(array)
  weight(array)
end

puts(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
